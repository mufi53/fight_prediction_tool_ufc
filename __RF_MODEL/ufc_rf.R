# ************************************************
# No License.
# The model is for educational purposes only
# ************************************************
# 2019 PRATICAL BUSINESS ANALYTICS GROUP PROJECT
#
# Group Name   : The Deepest Learners
# Group Members: Reza Naghshineh
#                Shayan Salehi   
#                Vikas 
#                Mufaddal
#                
# November-December 2019
# ************************************************
# Data Analysis for UFC fight data 1993-2019
# About This Script:   - Builds and Evaluates a Random Forest Classifier
#
# ************************************************
gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

#  clears all objects in "global environment"
rm(list=ls())

print("~~ RF STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "UFC_FINAL.CSV"


# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "caret",
  "randomForest"
) 


library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

# ************************************************
# NreadDataset() : (Tweaked)
# Read a CSV file from working directory
#
# INPUT: string  - csvFilename - CSV filename
#        Boolean - optional stringAsFactors (default TRUE)
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset <- function(csvFilename, stringAsFactors=TRUE){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = stringAsFactors)
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# prepare_data()
# Prepares a given data set for models by generating training/testing set (includes randomisation & normalisation)
#
# INPUT:    data frame         - data set to be handled
#           integer            - output_col_n, index of the output 
#           double             - ratio, train to test size ratio
#
# OUTPUT :  list (of 4)        - toReturn, returns the traing & testing set along with target and testing categories
# ************************************************
prepare_data <- function(df, output_col_n, ratio){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}
  df_normalised <- as.data.frame(lapply(df[,-output_col_n], normalised))
  
  # Extract training set
  df_train <- df_normalised[random,] 
  # Extract testing set
  df_test <- df_normalised[-random,]
  
  # Output category
  df_target_category <- df[random,output_col_n]
  df_test_category <- df[-random,output_col_n]
  
  toReturn <- list("training_set" = df_train, "testing_set" = df_test,"target_category" = df_target_category,"test_category"= df_test_category)
  
  return(toReturn)
}


# ************************************************
# main code goes below:
# ************************************************

UFC_DATA <- NreadDataset(DATA_FILE) # Normal dataset
UFC_PCA <- NreadDataset("UFC_PCA.csv") # PCA data
PCA_cols <- names(UFC_PCA)[2:length(names(UFC_PCA))] # PCA column names excluding Winner
print("Running ... ")

# Train/test split with 0.8 ratio
prepared_dataset <- prepare_data(df = UFC_DATA,output_col_n = 1, ratio = 0.8)

# PCA train/test are subset of prepared_dataset with PCA_cols only
pca_train <- prepared_dataset$training_set %>% select(PCA_cols)
pca_test <- prepared_dataset$testing_set %>% select(PCA_cols)

# Concat X and y for RF training
rf_data <- data.frame(Winner=prepared_dataset$target_category, prepared_dataset$training_set)
pca_rf_data <- data.frame(Winner=prepared_dataset$target_category, pca_train)

# RF Classifier
rf_classifier <- randomForest(Winner~., data = rf_data, norm.votes = TRUE, proximity = TRUE)

# RF With PCA
pca_rf_classifier <- randomForest(Winner~., data = pca_rf_data, norm.votes = TRUE, proximity = TRUE)

# RF Predictions on unseen testing set
rfpred <- predict(rf_classifier, prepared_dataset$testing_set)
pca_rfpred <- predict(pca_rf_classifier, pca_test)

# Confusion Matrix
print("Normal RF Consufion Matrix:")
cm <- confusionMatrix(rfpred, prepared_dataset$test_category)
print(cm)

print("---------------------------------------------------------")

print("PCA RF Consufion Matrix:")
pca_cm <- confusionMatrix(pca_rfpred, prepared_dataset$test_category)
print(pca_cm)

print("---------------------------------------------------------")

# Print accuracy
print(paste("RF Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print(paste("PCA_RF Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))

# Save SVM model
#saveRDS(rf_classifier, file = "RF_MODEL-test.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)


print("~~ RF ENDED:")


