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
# About This Script:     - K Nearest Neighbour Classification on processed data and PCA data
#                        - Testing a range of K
#                        - note: related graphs/analysis is in visualisation script
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

print("~~ KNN STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "UFC_FINAL.CSV"
PCA_FILE <- "UFC_PCA.csv"


# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "caret",
  "class"
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
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
#
# INPUT: String - fieldName - name of field
#
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
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
# perform_knn()
# Perform K Nearest Neighbours Classification on a given traing and testing dataset
#
# REQUIRES: class
#
# INPUT: data frame         - df_train, traing data set
#        data frame         - df_test, testing data set
#        Factor             - df_target_category, output target used in training
#        factor             - df_test_category, output target used in accuracy estimation in testing
#        integer            - k, k value
#
# OUTPUT : data frame       - df_KNN, knn predictions
# ************************************************
perform_knn <- function(df_train, df_test, df_target_category,
                        df_test_category, k){
  
  #Run the knn function
  df_KNN <- knn(train = df_train,test = df_test,cl = df_target_category,k = k)
  df_TAB <- table(df_KNN,df_test_category)
  
  #Out put the accuracy.
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  
  toReturn <- list("accuracy" = accuracy(df_TAB), "predicted" = df_KNN)
  return(toReturn)
  
}


# ************************************************
# main code goes below:
# ************************************************

# accuracy table related:
k_value <- c()
avg_accuracy <- c()
pca_k_value <- c()
pca_avg_accuracy <- c() 

UFC_DATA <- NreadDataset(DATA_FILE) # Normal dataset
UFC_PCA <- NreadDataset("UFC_PCA.csv") # PCA data
PCA_cols <- names(UFC_PCA)[2:length(names(UFC_PCA))] # PCA column names excluding Winner

# Train/test split with 0.75 ratio
prepared_dataset <- prepare_data(df = UFC_DATA,output_col_n = 1, ratio = 0.75)

# PCA train/test are subset of prepared_dataset with PCA_cols only
pca_train <- prepared_dataset$training_set %>% select(PCA_cols)
pca_test <- prepared_dataset$testing_set %>% select(PCA_cols)

# KNN: Experiment tange of K values:    # note: initially, values 1 to 100 were tested as k.
for(k in 50:53){
  k_value <- c(k_value,k)
  
  # accuracy per run
  accuracies <- c()
  
  # number of runs per K    # note: initially, tested for 30 runs
  for(i in 1:5){
    accuracies<- c(accuracies,perform_knn(df_train = prepared_dataset$training_set, df_test = prepared_dataset$testing_set, df_target_category = prepared_dataset$target_category,
                                          df_test_category = prepared_dataset$test_category, k=k)$accuracy)
  }
  print(paste("KNN K=", k, "Average Accuracy in 30 run",mean(accuracies)))
  avg_accuracy <- c(avg_accuracy,mean(accuracies))
}

print("---------------------------------------------------------")

# PCA KNN: Experiment tange of K values:    # note: initially, values 1 to 100 were tested as k.
for(k in 50:53){
  pca_k_value <- c(pca_k_value,k)
  
  # accuracy per run
  accuracies <- c()
  
  # number of runs per k    # note: initially, tested for 30 runs
  for(i in 1:5){
    accuracies<- c(accuracies,perform_knn(df_train = pca_train, df_test = pca_test, df_target_category = prepared_dataset$target_category,
                                          df_test_category = prepared_dataset$test_category, k=k)$accuracy)
  }
  print(paste("PCA-KNN K=", k, "Average Accuracy in 30 run",mean(accuracies)))
  pca_avg_accuracy <- c(pca_avg_accuracy,mean(accuracies))
}
print("---------------------------------------------------------")

# Table of k-values & average accuracies
acc_df <- data.frame(k_value,avg_accuracy,pca_k_value,pca_avg_accuracy)

#write.csv(acc_df,"acc_df_2_70_e.csv", row.names = FALSE) #export df

# KNN and PCA-KNN Evaluation on K = 70
knn_pred <- perform_knn(df_train = prepared_dataset$training_set, df_test = prepared_dataset$testing_set, df_target_category = prepared_dataset$target_category,
            df_test_category = prepared_dataset$test_category, k=70)$predicted

pca_knn_pred <- perform_knn(df_train = pca_train, df_test = pca_test, df_target_category = prepared_dataset$target_category,
                        df_test_category = prepared_dataset$test_category, k=70)$predicted

# Confusion Matrix
print("Normal KNN Consufion Matrix:")
cm <- confusionMatrix(knn_pred, prepared_dataset$test_category)
print(cm)

print("---------------------------------------------------------")

print("PCA KNN Consufion Matrix:")
pca_cm <- confusionMatrix(pca_knn_pred, prepared_dataset$test_category)
print(pca_cm)

print("---------------------------------------------------------")

# Print accuracy
print(paste("KNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))
print(paste("PCA_KNN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(pca_cm$overall[1], 4)*100,"%" ))

print("~~ KNN ENDED:")

