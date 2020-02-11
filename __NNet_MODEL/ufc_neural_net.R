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
#                Prat
#
# November-December 2019
# ************************************************
# Data Analysis for UFC fight data 1993-2019
# About This Script:     - note: running this script with default hyper-parameters 
#                                my take up to 3 minutes depending on computer
#                        - note: some ideas here taken from kaggle
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

print("~~ STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "UFC_FINAL.CSV"


# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "caret",
  "keras",
  "ggplot2",
  "ggthemes",
  "corrplot",
  "nnet",
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
NreadDataset <- function(csvFilename, stringAsFactors=FALSE){
  
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
# prepare_data_nn()
# Prepares a given data set by generating training/testing set (includes randomisation & normalisation)
#
# INPUT:    data frame         - data set to be handled
#           integer            - output_col_n, index of the output 
#           double             - ratio, train to test size ratio
#
# OUTPUT :  list (of 4)        - toReturn, returns the traing & testing set along with target and testing categories
# ************************************************
prepare_data_nn <- function(df, output_col_n, ratio){
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
# perform_nnet()
# Performs single-hidden-layer Neural Network classification along with cross validation
#
# INPUT:    data frame         - training_set
#           list               - target_category, target output of training_set
#           Boolean            - trace, displays trace output (default FALSE)
#           String             - method, method of validation (default "repeatedcv")
#           integer            - number, number of folds in cross-folding (default 3)
#           integer            - repeats, number of repeats in validation before taking average (default 3)
#
# OUTPUT :  list (of 20)       - nnetFit, model specification
# ************************************************
perform_nnet <- function(training_set,target_category,trace=FALSE, method="repeatedcv", number=3,repeats=3 ){
  fitControl <- trainControl(method = method,
                             number = number, 
                             repeats = repeats)
  nnetFit <- train(training_set, target_category,
                   method = "pcaNNet", # performs with PCA
                   trControl = fitControl,
                   verbose = FALSE,
                   trace = T)
}


# ************************************************
# main code goes below:
# ************************************************


UFC_DATA <- NreadDataset(DATA_FILE) # read data
prepared_data <- prepare_data_nn(UFC_DATA, output_col_n = 1, 0.85) # prepare test/train dataset

# Train NN # note: NNet does cross validation 
nnetFit <- perform_nnet(training_set = prepared_data$training_set,
                        target_category = prepared_data$target_category,
                        trace = T, number = 7, repeats = 5)

results <- nnetFit$results

write.csv(results,"result_nn_123.csv", row.names = FALSE)
nnetplot3 = plot(varImp(nnetFit, scale = F),main="Neural Network: Factor Importance")
print(nnetplot3)

# Export trained model
saveRDS(nnetFit, "./NNet_Model_123.rds")

# Testing on separate data:
nnetpred <- predict(nnetFit, newdata=prepared_data$testing_set ) # testing_set predictions 
test_nn <- data.frame("Real_Winner" = prepared_data$test_category, "Model_Prediction" = nnetpred)
test_nn$Real_Winner <- as.numeric(test_nn$Real_Winner) # Factor to Numeric
test_nn$Model_Prediction <- as.numeric(test_nn$Model_Prediction) # Factor to Numeric

# check if predicted correctly
isCorrect <- c()
for (k in 1:nrow(test_nn)){
  # if both output were same, add 1, otherwise add 0
  if(test_nn[k,1]==test_nn[k,2]){isCorrect <- c(isCorrect, 1)}else{isCorrect <- c(isCorrect, 0)}
}
test_nn$isCorrect <- isCorrect # concat to table

# calculate accuracy
accuracy <- as.double(sum(test_nn$isCorrect) / nrow(test_nn)) * 100
print(paste("Accuracy in",k,"Unseen Tests: ",accuracy))
print(table(test_nn[,1] ,nnetpred))






