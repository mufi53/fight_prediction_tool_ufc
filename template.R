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
# About This Script:     - 
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

print("~~ STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "UFC_FINAL.CSV"

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "formattable",
  "PerformanceAnalytics",
  "dplyr",
  "caret"
) 


library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

# ************************************************
# NreadDataset() :
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset <- function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
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
# main code goes below:
# ************************************************

# Load UFC Unproccessed Data
UFC_DATA <- NreadDataset(DATA_FILE)


print("~~ END")


