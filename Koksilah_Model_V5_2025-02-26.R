#Koksilah Model V1
#2025-02-26
#Author: Kristina Deenik
#previous edits: originally created by Kristina Deenik 2020-2021 for MSc thesis modelling wetlands in the Okanagan

#master list of libraries
library(raster)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(randomForest)
library(caret)
library(tidyverse)
library(ranger)
library(rsample)
library(corrplot)
library(sp)
library(vip)
library(pdp)
library(ggplot2)
library(mlr3)
library(mlr3spatiotempcv)
library(mlr3learners)
library(mlr3tuning)
library(mlr3verse)
library(whitebox)
library(spatstat)
library(leaflet)
library(mapview)
library(terra)
library(DiceKriging)
library(mlr3mbo)
library(rgenoud)
library(lhs)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>>>STEP 1: Stack rasters<<<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#V5 is just removing depth to water_rivers
#Move all parameters into one folder then select it here:
tif_folder <- "C:/TINA_WORKING_FOLDER/Kok/Param"

# List TIFF files in the folder
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE)

param <- stack(tif_files)

names(param) #V5
# [1] "CHM"                            "dev_from_mean_elev_1000m"       "dev_from_mean_elev_300m"       
# [4] "dev_from_mean_elev_50m"         "downslope_index"                "geomorphons"                   
# [7] "Mx_Band4"                       "NDWI"                           "num_downslope_neighbours"      
# [10] "num_upslope_neighbours"         "plan_curvature"                 "profile_curvature"             
# [13] "stochastic_depression_analysis"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>>>STEP 2: Training points<<<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#for V2 I went through and cleaned up the point layer. There were several places that were no longer wetland due to develpments.
#also areas that were confusing with agriculture.

#for V3 I created classes: Openwater, Wetland, Urban, Agriculture and Forest. I cleaned up points more.

#For V4 I added a few more training points and re-classified for the following classes: wetland, openwater, up-veg (forest and ag), up-non-veg (urban, sand, rocks)
#Replace and reclass some classes! Need to drop Upland class in the points shapefile.

#for V5 all I am doing is dropping the depth to rivers as it is influencing the model too much I think.
#For V5, I also want to change the classes to Wetland, upland, water again...

pts <- read_sf("C:/TINA_WORKING_FOLDER/Kok/pts/Training_Points_Wetland_OpenWater_Up-Ag_Up-NonVeg_Up-Veg.shp")

# Rename the column "class" to "Class"
names(pts)[names(pts) == "class"] <- "Class"

#select only fields of interest
pts <- pts %>% select(Class)

# Remove rows where Class is "Upland"
#pts <- pts %>% filter(Class != "Upland")

unique(pts$Class)

# Reclassify the Class column
pts <- pts %>%
  mutate(Class = case_when(
    Class %in% c("Ag", "Forest", "Up-Veg") ~ "Upland",
    Class == "Urban" ~ "Up-NonVeg",
    TRUE ~ Class  # Keep other classes unchanged
  ))

pts <- pts %>%
  mutate(Class = case_when(
    Class %in% c("Ag", "Forest", "Up-Veg") ~ "Upland",
    Class == "Up-NonVeg" ~ "Upland",
    TRUE ~ Class  # Keep other classes unchanged
  ))

# Check unique values to confirm
unique(pts$Class)

#match pts to param
param_df <- raster::extract(param, pts, df=TRUE)
param_out <- cbind(pts, param_df)
head(param_out) #looks good

param_out <- param_out %>% drop_na()

#save dataframe: 
write.csv(param_out,"C:/TINA_WORKING_FOLDER/Kok/param_out/param_out_wetland_water_upland_V5.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>>>#CHECK FOR COFOUNDING VARIABLES - done for V2#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>>>
# #check for correlated variables --> this should have been done after the training and testing datasets were made before
# #training the model and running it.
# rp_p <- param_out
# 
# # partition only relevant columns:
# var_cor_test <- rp_p[, 3:18]
# 
# #drop rows with na
# var_cor_test <- var_cor_test %>% drop_na()
# 
# st_geometry(var_cor_test) <- NULL
# 
# C = cor(var_cor_test)
# corrplot(C, method = 'number')
# 
# corrplot(C)
# #hard to read
# 
# # Shorten column names to the first three letters
# colnames(C) <- substr(colnames(C), 1, 3)
# 
# 
# # Create a custom color palette
# my_color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
# 
# # Create the correlation plot with custom options
# corrplot(C,
#          method = 'number',
#          col = my_color_palette,  # Use your custom color palette
#          tl.cex = 0.25,            # Adjust the font size
#          tl.col = "black",        # Set text label color
#          tl.srt = 45,             # Rotate text labels by 45 degrees
#          diag = FALSE,            # Exclude diagonal elements
#          addCoef.col = "black",   # Set coefficient color
#          type = "upper"          # Display upper triangle of the correlation matrix
# )
# 
# 
# # Compute correlation matrix
# df <- var_cor_test
# colnames(df) <- substr(colnames(df), 1, 10)
# cor_matrix <- cor(df, use = "complete.obs")  # Excludes missing values
# # Plot the correlation matrix
# corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
# 
# # Find pairs with correlation greater than 0.7 (excluding self-correlations)
# high_cor <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
# 
# # Convert to a readable format
# correlated_vars <- data.frame(
#   Var1 = rownames(cor_matrix)[high_cor[, 1]],
#   Var2 = colnames(cor_matrix)[high_cor[, 2]],
#   Correlation = cor_matrix[high_cor]
# )
# 
# # View correlated variable pairs
# print(correlated_vars)
# Var1       Var2 Correlation
# # 1       NDWI Mx_2_4_1.2  -0.8614436
# # 2 Mx_2_4_1.2       NDWI  -0.8614436
# 
# 
# #DROP: Mx2_4_1.2
# 
# #I need to separate the bands
# r <- stack("C:/TINA_WORKING_FOLDER/Kok/Param/Correlated_or_unwanted/Mosaic_Mx_reproj_resample_clipped-016.tif")
# 
# # Keep only layer and 4
# r_band4 <- r[[4]]
# 
# # Check the result
# print(r_band4)
# plot(r_band4)
# writeRaster(r_band4, "C:/TINA_WORKING_FOLDER/Kok/Param/Mx_Band4.tif", overwrite = TRUE)
# #Now go back to step one and re-dp pt_param

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Step 4: Train Model~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(12345)
index_wet <- createDataPartition(param_out$Class, p = .8, 
                                 list = FALSE, 
                                 times = 1)

train_wet <- param_out[ index_wet,]
test_wet  <- param_out[-index_wet,]

#this should preserve the proportions of each wetland class. Check:
prop.table(table(param_out$Class)) # this is def not representative of the landscape. Heavily weighted toward Wetlands
# OpenWater  Up-NonVeg     Up-Veg    Wetland 
# 0.25865209 0.03825137 0.10673953 0.59635701 8 

indat <- train_wet

#drop rows with na, should not be any left
indat <- indat %>% drop_na()
indat %>% group_by(Class) %>% count()
#same counts as above
#1 OpenWater   568
#2 Upland 1159
#3 Wetland 1310

data_sf <- st_as_sf(indat)

#drop Id column
data_sf <- data_sf[,-(2)]

#wetland response as factor
data_sf$Class <- as.factor(data_sf$Class)

#get ranger classification learner
mlr_learners$get("classif.ranger")

#set up the classification task
# sf
task <- as_task_classif_st(data_sf, target = "Class", backend = data_sf)

# Verify the result
print(task)

#create the ranger classification learner
learner = mlr3::lrn("classif.ranger", oob.error = TRUE,
                    importance = 'impurity', predict_type = 'prob')

#hyperparameter search space
search_space = ps(
  mtry = p_int(lower = 4, upper = 13),  #13 is the max number of param I have: mtry can not be larger than number of variables in data. 
  num.trees = p_int(lower = 500, upper = 1500), #changed from 1000 to 1500
  sample.fraction = p_dbl(lower = 0.5, upper = 0.7), #changed from 0.8 to 0.7
  max.depth = p_int(lower = 20, upper = 50), #cahnged from 100 to 50 to prevent overfitting
  min.node.size = p_int(lower = 10, upper = 100) #Consider lowering the lower limit to 5 or 10, as smaller node sizes can help capture fine-scale patterns. changed from 20
)

#create spatial resampling k-fold 10
resampling_sp = rsmp("repeated_spcv_coords", folds = 10, repeats = 1)

#classification error
#measure = msr("classif.ce") #this is not working for some reason

measure = msrs(c('classif.acc')) #tried this instead and it worked...

#terminate after 100 evaluations
evals100 = trm("evals", n_evals = 100)

# Create the tuning instance (updated to new function)
instance = TuningInstanceBatchSingleCrit$new(
  task = task,
  learner = learner,
  resampling = resampling_sp,
  measure = measure,
  search_space = search_space,
  terminator = evals100
)


#creat the tuner
tuner = tnr("mbo") #CHANGED FROM GRID SEARCH ******

#optimize the hyperparameters THIS WILL TAKE A LONG TIME TO RUN #NEXT TIME TRY PARALLEL PROCESSING: #future::plan(backend)
tuner$optimize(instance)

save.image('C:/TINA_WORKING_FOLDER/Kok/Save_workspaces/optimize_tuner_02252026_V5b.Rda')
#load("C:/Users/kdeenik/Documents/Thesis/R/optimize_tuner.RData")

#take the optimize hyperparameters and apply to the learner
learner$param_set$values = instance$result_learner_param_vals

#train the model using the selected hyperparameters THIS IS YOUR MODEL
learner$train(task)

##~~~~~~~~~~~~~~~~~~~~~~#Running the RF model~~~~~~~~~~~~~~~~~~~~~~~~~~~

#identify hyperparameters that were selected
instance$result_learner_param_vals

#calculate variable importance
filter = flt("importance", learner = learner)
filter$calculate(task)

print(filter)

#this is the variable importance plot!
plot(filter)

#hm, it is still relying heavily on Mx and NDWI... I really want it to lean more on Stoch. dep analysis
#I will go back and change the classes.

# #ONCE YOU GET TO HERE WE CAN RUN THE PREDICTION. NEEDS YOUR RASTER STACKS
# #I want to see how this does on a small study area
# # Read the study area shapefile

#study_area <- st_read("C:/TINA_WORKING_FOLDER/Kok/small_study_area/small_study_area_26910.shp")  # Replace with actual file path
# 
# # Ensure both raster and vector are in the same CRS
# #study_area <- st_transform(study_area, crs(param))
# 
# # Crop and mask the raster stack
#clipped_rasters <- mask(crop(param, study_area), study_area)
# 
# plot(clipped_rasters)
# 
# # Save the clipped rasters
# writeRaster(clipped_rasters, "C:/TINA_WORKING_FOLDER/Kok/small_study_area/clipped_rasters.tif", overwrite = TRUE)
# 
# 
#RF_mod2 <- learner$train(task)

#ALL_stacked <- clipped_rasters

#for the small study area, this takes 39 steps ~11:50- (more than 1 min per step)
#Index 1 = Ag
#model_output1 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/small_study_area/Model_OUTPUTS/RF_Index1_Ag_Classpredict_V3.tif", predict_type="prob",
#                         index=1, na.rm=TRUE, progress="window", overwrite=FALSE)
# #Index 2 = Forest
# model_output2 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/small_study_area/Model_OUTPUTS/RF_Index2_Forest_Classpredict_V3.tif", predict_type="prob", 
#                          index=2, na.rm=TRUE, progress="window", overwrite=FALSE)
# #Index 3 = OW
# model_output3 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/small_study_area/Model_OUTPUTS/RF_Index3_OW_Classpredict_V3.tif", predict_type="prob", 
#                          index=3, na.rm=TRUE, progress="window", overwrite=FALSE)
# 
# #Index 4 = Urban
# model_output4 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/small_study_area/Model_OUTPUTS/RF_Index4_Urban_Classpredict_V3.tif", predict_type="prob", 
#                          index=4, na.rm=TRUE, progress="window", overwrite=FALSE)
# 
# #Index 5 = Wetland
# model_output5 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/small_study_area/Model_OUTPUTS/RF_Index5_Wetland_Classpredict_V3.tif", predict_type="prob", 
#                          index=5, na.rm=TRUE, progress="window", overwrite=FALSE)
# 
# save.image("C:/TINA_WORKING_FOLDER/Kok/small_study_area/Save_workspace/RFmodel_small_study_area_2025-02-24.Rda")


#Run model on the whole study area:
#1 OpenWater   568
#2 Up-NonVeg 84
#3 Up-Veg 235
#4 Wetland  1310

RF_mod2 <- learner$train(task)

ALL_stacked <- param

names(ALL_stacked)

# #Index 1 = OW
# model_output1 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/RF_Index1_Classpredict.tif", predict_type="prob", 
#                          index=1, na.rm=TRUE, progress="window", overwrite=FALSE)

# #Index 2 = Up-NonVeg
# model_output2 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/RF_Index2_Classpredict.tif", predict_type="prob", 
#                          index=2, na.rm=TRUE, progress="window", overwrite=FALSE)
# #Index 3 = Up-veg
model_output3 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/Model_OUTPUTS/RF_Index3_Classpredict_wetland_prob_V5.tif", predict_type="prob", 
                          index=3, na.rm=TRUE, progress="window", overwrite=FALSE)

# #Index 4 = Wetland
# model_output4 <- predict(ALL_stacked, learner, filename="C:/TINA_WORKING_FOLDER/Kok/Model_OUTPUTS/RF_Index4_Classpredict_wetland_prob_V5.tif", predict_type="prob", 
#                          index=4, na.rm=TRUE, progress="window", overwrite=FALSE)



save.image('C:/TINA_WORKING_FOLDER/Kok/Save_workspaces/after_model_run_2025-02-26_V5.Rda')

#~~~~~~~~~~~~~Produce a classification raster where the max class is assigned to each pixel
# Load the three probability rasters
upland <- rast(model_output1)    # Probability raster for upland
open_water <- rast(model_output2) # Probability raster for open water
wetland <- rast(model_output3)  # Probability raster for wetland

# Stack the rasters into a single SpatRaster object
prob_stack <- c(upland, open_water, wetland)

# Assign class IDs (1 = upland, 2 = open water, 3 = wetland)
class_ids <- 1:3

# Create the classification raster
classification <- which.max(prob_stack)
values(classification) <- class_ids[values(classification)]

# Plot for visualization
plot(classification, col = c("brown", "blue", "green"),
     main = "Classification Raster",
     legend = TRUE)

# Save the output classification raster
writeRaster(classification, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification_raster_by_Class.tif", overwrite = TRUE)

class(classification)
classification_raster2 <- raster(classification)
writeRaster(classification_raster2, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification_raster_by_Class.tif", overwrite = TRUE)


#Now do it for 50% min prob
# Find the maximum probability and the corresponding class
max_prob <- app(prob_stack, max)       # Maximum probability for each pixel
max_class <- which.max(prob_stack)     # Class with the maximum probability

# Apply the 50% threshold
classification50 <- ifel(max_prob >= 0.5, max_class, NA)  # Assign class or NA

classification50 <- raster(classification50)

# Save the output classification raster
writeRaster(classification50, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification50.tif", overwrite = TRUE)


#Now do it for 70% min prob
# Apply the 70% threshold
classification70 <- ifel(max_prob >= 0.7, max_class, NA)  # Assign class or NA

classification70 <- raster(classification70)

# Save the output classification raster
writeRaster(classification70, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification70.tif", overwrite = TRUE)


#Now do it for 90% min prob
# Apply the 90% threshold
classification90 <- ifel(max_prob >= 0.9, max_class, NA)  # Assign class or NA

classification90 <- raster(classification90)

# Save the output classification raster
writeRaster(classification90, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification90.tif", overwrite = TRUE)


#Now do it for 85% min prob
classification85 <- ifel(max_prob >= 0.85, max_class, NA)  # Assign class or NA

classification85 <- raster(classification85)

# Save the output classification raster
writeRaster(classification85, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification85.tif", overwrite = TRUE)




#THIS DID NOT WORK:
# Load your rasters
raster1 <- model_output1
raster2 <- model_output2
raster3 <- model_output3

# Compute the classification raster with the maximum value at each pixel
classification_raster <- overlay(raster1, raster2, raster3, fun = max)

# Save the output
writeRaster(classification_raster, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification_raster.tif", format = "GTiff", overwrite = TRUE)

#assign a class only if the highest pixel value exceeds a confidence threshold of 0.5

# Compute the classification raster with conditions
classification_raster2 <- overlay(raster1, raster2, raster3, fun = function(x, y, z) {
  max_value <- max(x, y, z, na.rm = TRUE) # Calculate the maximum value
  if (max_value > 0.5) {
    return(max_value) # Assign the class if confidence > 0.5
  } else {
    return(NA) # Otherwise, assign NA
  }
})

# Save the output
writeRaster(classification_raster2, "C:/TINA_WORKING/CPCC/OK_rerun/OUTPUT/classification_raster.tif", format = "GTiff", overwrite = TRUE)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~MODEL ACCURACY~~~~~~~~~~~~~~~~~~~~~~~~

#read in test data
#load('G:/R-Code/Workspace/test_wet_V2_09-05_08-29-2023.Rda')
indat_test <- test_wet

#drop rows with na
indat_test <- indat_test %>% drop_na()


data_sf_test <- st_as_sf(indat_test, coords = c('X', 'Y'), crs = 26910)
data_sf_test <- data_sf_test[,-(2)]

#response used as factor
data_sf_test$Class <- as.factor(data_sf_test$Class)

#learner is our model from before. load model now
#load('G:/R-Code/Workspace/learner_V2_09-06-2023.RData')

PREDICT_NEW = learner$predict_newdata(newdata = data_sf_test, task = task)

#get scores
PREDICT_NEW$score
PREDICT_NEW

#pull out values from the predictclassif object
tab = as.data.table(PREDICT_NEW)
tab


#confusion matrix
con_mat = confusionMatrix(tab$truth,tab$response)
con_mat
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  OpenWater Upland Wetland
# OpenWater       136      0       6
# Upland            0    277      12
# Wetland           4     29     294
# 
# Overall Statistics
# 
# Accuracy : 0.9327          
# 95% CI : (0.9125, 0.9495)
# No Information Rate : 0.4116          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8939          
# 
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: OpenWater Class: Upland Class: Wetland
# Sensitivity                    0.9714        0.9052         0.9423
# Specificity                    0.9903        0.9735         0.9260
# Pos Pred Value                 0.9577        0.9585         0.8991
# Neg Pred Value                 0.9935        0.9382         0.9582
# Prevalence                     0.1847        0.4037         0.4116
# Detection Rate                 0.1794        0.3654         0.3879
# Detection Prevalence           0.1873        0.3813         0.4314
# Balanced Accuracy              0.9809        0.9393         0.9342


save.image('C:/TINA_WORKING_FOLDER/Kok/Save_workspaces/after_model_run_2025-02-26_V4.Rda')
#load("C:/Users/kdeenik/Documents/Thesis/R/optimize_tuner.RData")