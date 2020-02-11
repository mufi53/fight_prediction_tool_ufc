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
# About This Script:   - Builds and Evaluates a Deep Neural Network Classifier
#                      - note: Initially 0.7 train/test ratio was and got 
#                              ~68% accuracy,
#                              but for representation purposes a ratio of 0.1
#                              is substituted to train faster. Therefore, a 
#                              lower accuracy may be shown.
#                      - note: Contains DNN Architecture diagrams in Plots
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

print("~~ DNN STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "UFC_FINAL.CSV"


# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "caret",
  "neuralnet",
  "caTools"
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
# prepare_data()    (tweaked)
# Prepares a given data set for models by generating training/testing set (includes randomisation & normalisation)
#
# INPUT:    data frame         - data set to be handled
#           integer            - output_col_n, index of the output 
#           double             - ratio, train to test size ratio
#           Boolean            - optional normalized (default TRUE), performs normalisation
#
# OUTPUT :  list (of 4)        - toReturn, returns the traing & testing set along with target and testing categories
# ************************************************
prepare_data <- function(df, output_col_n, ratio, normalized = TRUE){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}
  if(normalized == TRUE){
    df_normalised <- as.data.frame(lapply(df[,-output_col_n], normalised))
  }
  else{
    df_normalised <- df[,-output_col_n]
  }
  
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

UFC_DATA <- NreadDataset("UFC_FINAL.csv") # Normal dataset
UFC_PCA <- NreadDataset("UFC_PCA.csv") # PCA data
PCA_cols <- names(UFC_PCA)[2:length(names(UFC_PCA))] # PCA column names excluding Winner
print("Running ... ")

UFC_DATA$Winner = as.character(UFC_DATA$Winner) # Factor to chr
UFC_DATA$Winner[UFC_DATA$Winner == "Blue"] <- 1 # encode blue as 1
UFC_DATA$Winner[UFC_DATA$Winner == 'Red']  <- 0 # encode red as 0
UFC_DATA$Winner = as.numeric(UFC_DATA$Winner) # as numeric

output_col_n = 1 # output target
UFC_DATA[-output_col_n]<-scale(UFC_DATA[-output_col_n]) # centre dataset

# Train/test split with 0.7 ratio with no normalisation
prepared_dataset <- prepare_data(UFC_DATA, output_col_n = output_col_n,ratio = 0.1, normalized = FALSE)

# PCA train/test are subset of prepared_dataset with PCA_cols only
pca_train <- prepared_dataset$training_set %>% select(PCA_cols)
pca_test <- prepared_dataset$testing_set %>% select(PCA_cols)

# Concat X and y for NN training
nn_data <- data.frame(Winner=prepared_dataset$target_category, prepared_dataset$training_set)
pca_nn_data <- data.frame(Winner=prepared_dataset$target_category, pca_train)

# DNN Classifier
dnn_classifier <- neuralnet(Winner ~ . , data=nn_data, hidden=c(3,2),stepmax=1e6, linear.output=FALSE, threshold=0.01,lifesign = "full")
print("---------------------------------------------------------")
# DNN with PCA
pca_dnn_classifier <- neuralnet(Winner ~ . , data=pca_nn_data, hidden=c(3,2),stepmax=1e6, linear.output=FALSE, threshold=0.01,lifesign = "full")
print("---------------------------------------------------------")

# Plot Neural Network
plot(dnn_classifier)
plot(pca_dnn_classifier)

# DNN Predictions on unseen testing set
nnpred <- neuralnet::compute(dnn_classifier, prepared_dataset$testing_set)[["net.result"]]
nnpred[nnpred > 0.5] <- 1 # if probability > 0.5, classify as 1 (Blue) otherwise 0 (Red)
nnpred[nnpred <= 0.5] <- 0
nnpred <- as.factor(nnpred) # convert to factor

pca_nnpred <- neuralnet::compute(pca_dnn_classifier, pca_test)[["net.result"]]
pca_nnpred[pca_nnpred > 0.5] <- 1 # if probability > 0.5, classify as 1 (Blue) otherwise 0 (Red)
pca_nnpred[pca_nnpred <= 0.5] <- 0
pca_nnpred <- as.factor(pca_nnpred) # convert to factor

# Confusion Matrix
print("Normal DNN Consufion Matrix:")
cm <- confusionMatrix(nnpred, as.factor(prepared_dataset$test_category))
print(cm)

print("---------------------------------------------------------")

print("PCA DNN Consufion Matrix:")
pca_cm <- confusionMatrix(pca_nnpred, as.factor(prepared_dataset$test_category))
print(pca_cm)

print("---------------------------------------------------------")

# Print accuracy
print(paste("DNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print(paste("PCA_DNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))

#save(dnn_classifier, file="test.rda")
# Save DNN model
#saveRDS(dnn_classifier, file = "DNN_MODEL.rds"
#        ,ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)

print("~~ DNN ENDED:")
