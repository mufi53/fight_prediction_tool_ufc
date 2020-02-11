# ************************************************
# No License.
# The model is for educational purposes only
#
# NOTES:
# Function prefixed the name with an “N”, highlights that it was written by Prof. Nick ryman-tub
# Style & Documentation of code follows same patterns as PBA module at University of Surrey
# Functions have been decleared and documented on top of each script
# Most visualisations are performed in a dedicated folder for visualisation
# Codes have been tested to run without issue in Surrey Labs. Should you encounter any issues,
# please email rn00177@surrey.ac.uk
#
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
# About This Script:     - Loading original (raw) Data
#                        - Droping unnecessary Columns
#                        - Handling Missing Value
#                        - Some Visualisation & Field Analysis (in Plots and Viewer)
#                        - Exporting final version of processed dataset
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

print("~~ DATA PRE-PROCESSING STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "data.CSV"
TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
MAX_LITERALS      <- 55                   # Maximum numner of hotcoding new fields

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "formattable",
  "PerformanceAnalytics",
  "dplyr",
  "ggplot2",
  "reshape2",
  "caret",
  "corrplot",
  "sqldf",
  "lattice"
) 


library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

# ************************************************
# NreadDataset() :  (tweaked : not including removePunctuation)
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset <- function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
NPREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

# ************************************************
# NPREPROCESSING_initialFieldType() :
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}

# ************************************************
# NPREPROCESSING_setInitialFieldType() :
# Set  each field for NUMERIC or SYNBOLIC
#
# INPUT:
#        String - name - name of the field to manually set
#        String - type - manual type
#
# OUTPUT : None
# ************************************************
NPREPROCESSING_setInitialFieldType<-function(name,type){
  
  #Sets in the global environment
  manualTypes<<-rbind(manualTypes,data.frame(name=name,type=type,stringsAsFactors = FALSE))
}

# ************************************************
# remove_constant_cols()
# Removes numeric columns which have a constant value (e.g: all zeros)
#
# INPUT: data frame        - df to be handled
#
# OUTPUT : data frame      - df without constant columns
# ************************************************
remove_constant_cols <- function(df){
  print("Removing Constant Columns:")
  numerics <- unlist(lapply(df, is.numeric))  
  df_numeric<-df[,numerics]
  constantCols <- c()
  for(col in names(df_numeric)){
    # if min=max it is constant
    if(min(df_numeric[,col],na.rm=TRUE)==max(df_numeric[,col],na.rm=TRUE)){
      constantCols <- c(constantCols, col)
    }
    
  }
  if(length(constantCols)>0){
    print(paste("Constant Column Found:",constantCols))
    df<- select(df,-constantCols)
  }
  else if(length(constantCols)==0){
    print("No Constants: Nothing to Remove")
    df <- df
  }
  else{print("Error in remove_constant_cols Function")}
  
  return(df)
}

# ************************************************
# Handle_Missing_median()
# Handles missing values by substituting them with meadian of the column
#
# REQUIRES: dplyr
#
# INPUT: data frame    - full dataset to be handled
#
# OUTPUT : none
# ************************************************
Handle_Missing_median <- function(dataset){
  
  data.frame(
    lapply(
      dataset,
      function(x) ifelse(is.na(x),
                         median(x, na.rm = TRUE),
                         x)))
  
}

# ************************************************
# draw_correlation_heatmap()
# Computes the correlation matrix, reorders them and generates correlation heatmap
# * idea taken from sthda.com *
#
# REQUIRES: reshape2, ggplot2
#
# INPUT: data frame    - dataset of numeric values only
#        double        - optional midpoint (dedault 0)
#        double        - optional lower (dedault -1)
#        Boolean       - optional coef (dedault FALSE) #display coefficients
#        String       - optional title (dedault empty) #display title
#
# OUTPUT : none
# ************************************************
draw_correlation_heatmap <- function(numeric_df, midpoint = 0, below=-1, coef=FALSE, title=""){
  reorder_matrix <- function(correlation_matrix){
    # Use correlation between variables as distance
    dd <- as.dist((1-correlation_matrix)/2)
    hc <- hclust(dd)
    correlation_matrix <-correlation_matrix[hc$order, hc$order]
  }
  # Omit upper triangle due to redundancy
  get_upper_tri <- function(correlation_matrix){
    correlation_matrix[lower.tri(correlation_matrix)]<- NA
    return(correlation_matrix)
  }
  correlation_matrix <- reorder_matrix(round(cor(numeric_df),3))
  
  upper_tri <- get_upper_tri(correlation_matrix)
  melted_cormat <- melt(upper_tri,na.rm = TRUE)
  correlation_heatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = midpoint, limit = c(below,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed()
  if(coef==TRUE){correlation_heatmap<-correlation_heatmap+ geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)}
  if(length(title)>0){correlation_heatmap<-correlation_heatmap+ggtitle(title)}
  print(correlation_heatmap)
}

# ************************************************
# get_percent_missing()
# Prints the percent of missings for columns with at least 1 NA and returns these columns
#
# INPUT: data frame    - dataset
#        double        - optional threshold (dedault 0)
#
# OUTPUT : vector      - column names with missing data larger than threshold
# ************************************************
get_percent_missing <- function(dataset, threshold=0){
  # input validation
  if(threshold<0 | threshold>100 ){threshold=0}
  
  missing_cols <- c()
  for(col in names(dataset)){
    prcnt<-format(round(sum(is.na(dataset[,col]))/nrow(dataset)*100, 2), nsmall = 2)
    if(as.numeric(prcnt)>0){print(paste("col: ", col," | Percent Missing -->", prcnt))}
    if(as.numeric(prcnt) > threshold){missing_cols<-c(missing_cols, col)}
  }
  return(missing_cols)
}

# ************************************************
# fill_with_LR()
# Fills the NA values of the 'toPredict' Column using a Linear Regression on the 'Predictor'
#
# INPUT: data frame    - dataset
#        String        - predictor : prediction is based on this column * can NOT contain NA * 
#        String        - toPredict : the column to predict its NAs
#        object        - LR : the Linear Regression Model
#
# OUTPUT : data frame  - a data frame of toPredict column with filled NA
# ************************************************
fill_with_LR<-function(df,predictor,toPredict,LR){
  x<-df[,predictor][is.na(df[,toPredict])]
  
  y<-LR$coefficients[2]*x + LR$coefficients[1]
  return(df[,toPredict][is.na(df[,toPredict])] <- y)
}

# ************************************************
# perform_PCA()
# Performs Principal Component Analysis on a given numeric data set and return most important columns
#
# INPUT: data frame    - dataset
#
# OUTPUT : chr list  - list of important columns' name
# ************************************************
perform_PCA <- function(df) {
  
  # PCA and Plotting Component Variance
  df.prc = prcomp(df, center = TRUE,scale = TRUE)
  
  # Variance
  variance = df.prc$sdev ^ 2
  
  # Kaiser Criterion
  pca_vars = variance[variance >= 1] 
  number_of_PCAs = length(pca_vars)
  
  #Scree Plot
  screeplot(df.prc,type = "line",main = "PCA: Scree Plot")
  
  #Varimax Rotation
  df.varimax_rotation = varimax(df.prc$rotation)
  
  test = data.frame(unclass(df.varimax_rotation$loadings))
  test = cbind(rownames(test),test)
  row.names(test)<-1:nrow(test)
  
  colnames_test = names(test)
  colnames_test = colnames_test[2:number_of_PCAs]
  selected_variables = c()
  for(i in colnames_test){
    for (k in 1:nrow(test)){
      if (test[k,i]>0.2 | test[k,i]<=-0.2){
        selected_variables <- c(selected_variables,paste(test[k,1]))
      }
    }
  }
  return(selected_variables)
}

# ************************************************
# normalised()
# Normalises the data to a number between 0 to 1
#
# INPUT: data frame    - dataset
#
# OUTPUT : data framet  - normalised dataset
# ************************************************
normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}

# ************************************************
# main code goes below:
# ************************************************

# Load UFC Unproccessed Data
UFC_DATA <- NreadDataset(DATA_FILE)

# Export a comma separated table of columns' name
# df <- data.frame(names(UFC_DATA))
# df$Description <- "n/a"
# write.table(df,"table.txt",quote = FALSE, sep = ",")

# Exclude match level data; only consider data related to opponents
UFC_Preprocessed <- UFC_DATA[,c(6,10:12,37:38,63,65:79,104:105,130,132:145)]

# Rename columns causing trouble in SQLdf
UFC_Preprocessed <- UFC_Preprocessed %>% 
  rename(
    B_win_by_KO_TKO = B_win_by_KO.TKO,
    R_win_by_KO_TKO = R_win_by_KO.TKO,
    B_Stance_ = B_Stance,
    R_Stance_ = R_Stance
  )
# Replace empty string with NA
UFC_Preprocessed[UFC_Preprocessed==""]<-NA

# We are only considering Matches where there was a winner because:
# 1- Draw rate is very low and this occurs rarely
# 2- This is a binary classification
print(paste("Draw Rate is:",round(sum(UFC_Preprocessed$Winner=="Draw")/nrow(UFC_Preprocessed)*100,3),"% | ", "Omitting 'Draw' Matches"))
UFC_Preprocessed$Winner[UFC_Preprocessed$Winner == "Draw"] <- NA  # Draw become NA to be droped later

# Distinguish numeric & symbolic fields
field_types<-NPREPROCESSING_initialFieldType(UFC_Preprocessed)
numeric_fields<-names(UFC_Preprocessed)[field_types=="NUMERIC"]
symbolic_fields<-names(UFC_Preprocessed)[field_types=="SYMBOLIC"]

# Remove columns with sum 0 and update dataset
removableCols <- c()
for(field in numeric_fields){ if(sum(UFC_Preprocessed[,field],na.rm = TRUE) == 0){removableCols <- c(removableCols, field)}}
UFC_Preprocessed<- select (UFC_Preprocessed,-removableCols)
for (element in removableCols ){numeric_fields <- numeric_fields[ numeric_fields != element]}
cat("[1] Columns with sum 0 removed:", removableCols,"\n")

# Format data to 3 Decimal Points
is.num <- sapply(UFC_Preprocessed, is.numeric)
UFC_Preprocessed [is.num] <- lapply(UFC_Preprocessed [is.num], round, 3)

# 1-hot-encode categoricals 'BStance'&'RStance'
toBeEncoded <- c(15,32)
UFC_encoded <- UFC_Preprocessed[,toBeEncoded]
UFC_encoded<-transform(UFC_encoded)
dmy <- dummyVars(" ~ .", data = UFC_encoded) #dummyVars uses caret
UFC_encoded <- data.frame(predict(dmy, newdata = UFC_encoded))

# Renaming
UFC_encoded <- UFC_encoded %>% 
  rename(
    B_Stance_Open_Stance = B_Stance_Open.Stance,
    R_Stance_Open_Stance = R_Stance_Open.Stance
  )

# Concat NOT-encoded columns with encoded ones
UFC_Preprocessed<- data.frame(UFC_Preprocessed[,-toBeEncoded],UFC_encoded)

# Get columns with missing data
print("_._._._._._._._. Before Immutation: NA % in Columns _._._._._._._._. ")
cols_with_missings <- get_percent_missing(UFC_Preprocessed)
print("_._._._._._._._._._._._._._._._._.._._._._._._._._._._._._._._._._._ ")

# ************************************************
# immute_missings()
# Function handles missing values (i.e: NA) using different techniques specific to this problem 
# Treatments:   1- Replace NA with Median
#               2- Predict NA using Linear Regression (LR)
#               3- Omit NA
# INPUT: data frame  - dataset to be handled
#
# OUTPUT : data frame  - dataset after handling NAs
# ************************************************
immute_missings<-function(UFC_Preprocessed){
  print("STARTED Missing Value Immutation")
  
  # fighter_measures include height, weight and reach of both fighters
  fighter_measures <- UFC_Preprocessed[,c(15,16,17,31,32,33)]
  fighter_measures <- na.omit(fighter_measures)
  fighter_measures <- sqldf("Select B_Height_cms as fighter_height,B_Reach_cms as fighter_reach,B_Weight_lbs as fighter_weight from fighter_measures UNION ALL Select R_Height_cms,R_Reach_cms,R_Weight_lbs from fighter_measures")
  
  # >>>>>>>>>>>> 1- 'Height' Immutation <<<<<<<<<<<<
  # Boxplots in a 1x2 grid
  par(mfrow=c(1, 2))
  boxplot(fighter_measures$fighter_height, main="Height_cms")
  boxplot(fighter_measures$fighter_reach, main="Reach_cms")
  
  # 'heights' have ~0.12% missings; Immute their NA with MEDIAN of each:
  UFC_Preprocessed$R_Height_cms[is.na(UFC_Preprocessed$R_Height_cms)] <- round(median(UFC_Preprocessed$R_Height_cms, na.rm = TRUE),3)
  UFC_Preprocessed$B_Height_cms[is.na(UFC_Preprocessed$B_Height_cms)] <- round(median(UFC_Preprocessed$B_Height_cms, na.rm = TRUE),3)
  # >>>>>>>>>>>> END of 'Height' Immutation <<<<<<<<<<<<
  
  
  # >>>>>>>>>>>> 2- 'Reach' Immutation <<<<<<<<<<<<
  # Heatmap: find highest correlation for 'reach'?
  draw_correlation_heatmap(fighter_measures, midpoint = 0.8, below = 0.7, coef = TRUE, title = "Body Measures Correlation Heatmap")
  # > Answer: 'height' ~89% correlated)
  
  # 'Reach' vs 'Height' scatterplot & smooth line
  print(ggplot(span=0.1,fighter_measures, aes(y=fighter_reach, x=fighter_height)) + geom_point()+ geom_smooth(method = 'loess')+ggtitle("Smooth Fitted Line on fighter measures"))
  
  # Immute 'reach' NA using LR:
  linearMod <- lm(fighter_reach ~ fighter_height, data=fighter_measures)
  par(mfrow=c(1, 1))
  plot(fighter_measures$fighter_height,fighter_measures$fighter_reach,main="Our Linear Regression Plot",pch=16)
  abline(linearMod, col="red")
  UFC_Preprocessed$B_Reach_cms[is.na(UFC_Preprocessed$B_Reach_cms)]<-fill_with_LR(UFC_Preprocessed,predictor = "B_Height_cms",toPredict = "B_Reach_cms",LR = linearMod)
  UFC_Preprocessed$R_Reach_cms[is.na(UFC_Preprocessed$R_Reach_cms)]<-fill_with_LR(UFC_Preprocessed,predictor = "R_Height_cms",toPredict = "R_Reach_cms",LR = linearMod)
  # >>>>>>>>>>>> END of 'Reach' Immutation <<<<<<<<<<<<
  #get_percent_missing(UFC_Preprocessed) # Reduced 19 to 15 columns with missings  **UnComment to Check**
  
  
  # >>>>>>>>>>>> 3- Rest: NA.Omit <<<<<<<<<<<<
  UFC_Preprocessed <- na.omit(UFC_Preprocessed)
  # >>>>>>>>>>>> END of 'NA.Omit' Immutation <<<<<<<<<<<<
  
  
  print("FINISHED Missing Value Immutation")
  return(UFC_Preprocessed)
}
# Calling the function
UFC_Preprocessed<-immute_missings(UFC_Preprocessed)

# remove columns without variation
UFC_Preprocessed <- remove_constant_cols(UFC_Preprocessed)
# "R_StanceSideways" was constant, therefor corresponding "B_StanceSideways" must also be removed
UFC_Preprocessed<- select (UFC_Preprocessed,-c(B_Stance_Sideways))

# Print some confirmation messages:
print(paste("Data Loss due to NA after Immutation: ",round(100-nrow(UFC_Preprocessed)/nrow(UFC_DATA)*100,2),"%")) # Loss Data %
list_na<-colnames(UFC_Preprocessed)[apply(UFC_Preprocessed,2, anyNA)]
if(length(list_na) == 0){print("No Missing Values Left in UFC_Preprocessed")} else {print(paste("Missing value in:",list_na))} # NA Cols

# ** Analysis and Visualisation: ** 
# Generate table for fields analysis 
NPREPROCESSING_prettyDataset(UFC_Preprocessed)

# Dataframe of only numeric values ** Excludes encoded 'Stance' **
UFC_numeric_DF <- UFC_Preprocessed[,numeric_fields]

# Considering Blue player numeric-data only
UFC_Blue_Player <- UFC_numeric_DF[,c(1:16, 33)] 

# perform PCA (exclude Winner)
PCA_cols <- perform_PCA(UFC_Preprocessed[,-1])
UFC_PCA<-data.frame(UFC_Preprocessed[1],UFC_Preprocessed[,PCA_cols]) # concat 1st column
cat("[1] Principle Components: \n", PCA_cols,"\n",sep =" | ")

# BoxPlot
par(mfrow=c(1, 1))
op <- par(mar= c(15,4,4,2))
boxplot(UFC_Blue_Player, use.cols = TRUE, main="Data Summary In BoxPlot", horiz=F,las=2)

# Correlation Matrix Heatmap
draw_correlation_heatmap(UFC_Blue_Player, title = "Blue Fighter Correlation Heatmap")

# Convert 'Winner' from 'chr' to 'Factor'
UFC_Preprocessed$Winner <- as.factor(UFC_Preprocessed$Winner)
UFC_PCA$Winner <- as.factor(UFC_PCA$Winner)
# Reorder columns to match SQLdf ordering
UFC_Preprocessed <- UFC_Preprocessed[,c(1:17,34,36:39,18:33,35,40:43)]
UFC_FINAL<- UFC_Preprocessed #rename

# Export processed dataframes as csv
#write.csv(UFC_numeric_DF,"UFC_numeric_DF.csv", row.names = FALSE)
#write.csv(UFC_Blue_Player,"UFC_Blue_Player.csv", row.names = FALSE)
#write.csv(UFC_FINAL,"UFC_FINAL.csv", row.names = FALSE)
#write.csv(UFC_PCA,"UFC_PCA.csv", row.names = FALSE)

print("~~ DATA PRE-PROCESSING ENDED")



