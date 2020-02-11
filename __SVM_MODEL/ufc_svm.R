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
#
# November-December 2019
# ************************************************
# Data Analysis for UFC fight data 1993-2019
# About This Script:   - Builds and Evaluates a SVM-Radial Basis Function Classifier
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

print("~~ SVM STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "UFC_FINAL.CSV"


# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "caret",
  "e1071"
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

UFC_DATA <- NreadDataset("UFC_FINAL.csv") # Normal data
UFC_PCA <- NreadDataset("UFC_PCA.csv") # PCA data
PCA_cols <- names(UFC_PCA)[2:length(names(UFC_PCA))] # PCA column names excluding Winner
print("Running ...")

# Train/test split with 0.5 ratio
prepared_dataset <- prepare_data(df = UFC_DATA,output_col_n = 1, ratio = 0.5)

# PCA train/test are subset of prepared_dataset with PCA_cols only
pca_train <- prepared_dataset$training_set %>% select(PCA_cols)
pca_test <- prepared_dataset$testing_set %>% select(PCA_cols)

# Concat X and y for SVM training
svm_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), prepared_dataset$training_set)
pca_svm_data <- data.frame(Winner=as.factor(prepared_dataset$target_category), pca_train)

# SVM Classifier
svm_classifier <- svm(Winner ~ ., 
                 data = svm_data, 
                 type = 'C-classification', 
                 kernel = 'radial',
                 cost=10,
                 scale=TRUE,
                 probability=TRUE)

# SVM With PCA
pca_svm_classifier <- svm(Winner ~ ., 
                      data = pca_svm_data, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      cost=10,
                      scale=TRUE,
                      probability=TRUE)

# SVM Predictions on unseen testing set
svmpred <- predict(svm_classifier, newdata=prepared_dataset$testing_set, probability = TRUE)
pca_svmpred <- predict(pca_svm_classifier, newdata=pca_test, probability = TRUE)

# Confusion Matrix
print("Normal SVM Consufion Matrix:")
cm <- confusionMatrix(svmpred, prepared_dataset$test_category)
print(cm)

print("---------------------------------------------------------")

print("PCA SVM Consufion Matrix:")
pca_cm <- confusionMatrix(pca_svmpred, prepared_dataset$test_category)
print(pca_cm)

print("---------------------------------------------------------")

# Print accuracy
print(paste("SVM Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print(paste("PCA_SVM Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))

# Save SVM model
#saveRDS(svm_classifier, file = "SVM_MODEL.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)


print("~~ SVM ENDED:")


