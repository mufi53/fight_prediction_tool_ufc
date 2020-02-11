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
# About This Script:  - Generates the most recent fighter stat using SQL commands within R.   
#                     - Exports a csv file to be used as a database for the App
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

print("~~ FIGHTER STAT STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "data.CSV"
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
MAX_LITERALS      <- 55                   # Maximum numner of hotcoding new fields

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "sqldf",
  "caret",
  "dplyr"
  
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
# main code goes below:
# ************************************************

# Load UFC Unproccessed Data
UFC_DATA <- NreadDataset(DATA_FILE)

# Needed fields to create fighter stat
UFC_Preprocessed <- UFC_DATA[,c(1,2,4,6,10:12,37:38,63,65:79,104:105,130,132:145)]

# Replace empty string with NA
UFC_Preprocessed[UFC_Preprocessed==""]<-NA

# Draw become NA to be droped later
UFC_Preprocessed$Winner[UFC_Preprocessed$Winner == "Draw"] <- NA  

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
toBeEncoded <- c(18,35)
UFC_encoded <- UFC_Preprocessed[,toBeEncoded]
UFC_encoded<-transform(UFC_encoded)
dmy <- dummyVars(" ~ .", data = UFC_encoded) #dummyVars uses caret
UFC_encoded <- data.frame(predict(dmy, newdata = UFC_encoded))
# Concat NOT-encoded columns with encoded ones
UFC_Preprocessed<- data.frame(UFC_Preprocessed[,-toBeEncoded],UFC_encoded)

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
  fighter_measures <- UFC_Preprocessed[,c(18,19,20,34,35,36)]
  fighter_measures <- na.omit(fighter_measures)
  fighter_measures <- sqldf("Select BHeightcms as fighter_height,BReachcms as fighter_reach,BWeightlbs as fighter_weight from fighter_measures UNION ALL Select RHeightcms,RReachcms,RWeightlbs from fighter_measures")
  
  # 'heights' have ~0.12% missings; Immute their NA with MEDIAN of each:
  UFC_Preprocessed$RHeightcms[is.na(UFC_Preprocessed$RHeightcms)] <- round(median(UFC_Preprocessed$RHeightcms, na.rm = TRUE),3)
  UFC_Preprocessed$BHeightcms[is.na(UFC_Preprocessed$BHeightcms)] <- round(median(UFC_Preprocessed$BHeightcms, na.rm = TRUE),3)

  # Immute 'reach' NA using LR:
  linearMod <- lm(fighter_reach ~ fighter_height, data=fighter_measures)
  UFC_Preprocessed$BReachcms[is.na(UFC_Preprocessed$BReachcms)]<-fill_with_LR(UFC_Preprocessed,predictor = "BHeightcms",toPredict = "BReachcms",LR = linearMod)
  UFC_Preprocessed$RReachcms[is.na(UFC_Preprocessed$RReachcms)]<-fill_with_LR(UFC_Preprocessed,predictor = "RHeightcms",toPredict = "RReachcms",LR = linearMod)

  UFC_Preprocessed <- na.omit(UFC_Preprocessed)

  print("FINISHED Missing Value Immutation")
  return(UFC_Preprocessed)
}
# Calling the function
UFC_Preprocessed<-immute_missings(UFC_Preprocessed)

# Print some confirmation messages:
print(paste("Data Loss due to NA after Immutation: ",round(100-nrow(UFC_Preprocessed)/nrow(UFC_DATA)*100,2),"%")) # Loss Data %
list_na<-colnames(UFC_Preprocessed)[apply(UFC_Preprocessed,2, anyNA)]
if(length(list_na) == 0){print("No Missing Values in UFC_Preprocessed")} else {print(paste("Missing value in:",list_na))} # NA Cols

# Convert 'Winner' from 'chr' to 'Factor'
UFC_Preprocessed$Winner <- as.factor(UFC_Preprocessed$Winner)

# rename columns having conflict with sql commands
UFC_Preprocessed <- UFC_Preprocessed %>% 
  rename(
    RStanceOpen_Stance = RStanceOpen.Stance,
    BStanceOpen_Stance = BStanceOpen.Stance
  )

# SQLdf commands below find the latest information of each fighter:
test_df <- UFC_Preprocessed

# Red Fighter
Red_fighter = sqldf("Select Rfighter,date,Rcurrentlosestreak,	Rcurrentwinstreak,	Rlongestwinstreak,	Rlosses,	Rtotalroundsfought,	Rtotaltitlebouts,	RwinbyDecisionMajority,	RwinbyDecisionSplit,	RwinbyDecisionUnanimous,	RwinbyKOTKO,	RwinbySubmission,	RwinbyTKODoctorStoppage,	Rwins,	RHeightcms,	RReachcms,	RWeightlbs, Rage,RStanceOpen_Stance,	RStanceOrthodox,	RStanceSideways,	RStanceSouthpaw,	RStanceSwitch from test_df")

max_date_red_fighters = sqldf("select Rfighter,MAX(date) as max_date from Red_fighter group by 1")

distinct_most_recent_red_fighter_stats = sqldf("Select A.*,B.* from max_date_red_fighters as A LEFT JOIN Red_fighter as B on A.Rfighter=B.Rfighter and A.max_date=B.date ")

distinct_most_recent_red_fighter_stats = distinct_most_recent_red_fighter_stats[,c(1,4:26)]

# Blue Fighter
Blue_fighter = sqldf("Select Bfighter,date,Bcurrentlosestreak,	Bcurrentwinstreak,	Blongestwinstreak,	Blosses,	Btotalroundsfought,	Btotaltitlebouts,	BwinbyDecisionMajority,	BwinbyDecisionSplit,	BwinbyDecisionUnanimous,	BwinbyKOTKO,	BwinbySubmission,	BwinbyTKODoctorStoppage,	Bwins,	BHeightcms,	BReachcms,	BWeightlbs, Bage,BStanceOpen_Stance,	BStanceOrthodox,	BStanceSideways,	BStanceSouthpaw,	BStanceSwitch from test_df")

max_date_blue_fighters = sqldf("select Bfighter,MAX(date) as max_date from Blue_fighter group by 1")

distinct_most_recent_blue_fighter_stats = sqldf("Select A.*,B.* from max_date_blue_fighters as A LEFT JOIN Blue_fighter as B on A.Bfighter=B.Bfighter and A.max_date=B.date ")

distinct_most_recent_blue_fighter_stats = distinct_most_recent_blue_fighter_stats[,c(1,4:26)]

# Merge
blue_red_mix = sqldf("Select * from distinct_most_recent_red_fighter_stats UNION ALL Select * from distinct_most_recent_blue_fighter_stats")

max_date_all_fighters = sqldf("select Rfighter,MAX(date) as max_date from blue_red_mix group by 1")

distinct_most_recent_all_fighter_stats = sqldf("Select A.*,B.* from max_date_all_fighters as A LEFT JOIN blue_red_mix as B on A.Rfighter=B.Rfighter and A.max_date=B.date order by Rfighter")

master_fighter_recent_stats = distinct_most_recent_all_fighter_stats[,c(1,4:26)]

# add index
Fighter_ID <- seq.int(nrow(master_fighter_recent_stats))
master_fighter_recent_stats <- data.frame(Fighter_ID, master_fighter_recent_stats)
master_fighter_recent_stats<- select(master_fighter_recent_stats, -RStanceSideways)
names(master_fighter_recent_stats) <- c("Fighter_ID", "Fighter",	"Date",
                                        'current_lose_streak',
                                        'current_win_streak',
                                        'longest_win_streak',
                                        'losses',
                                        'total_rounds_fought',
                                        'total_title_bouts',
                                        'win_by_Decision_Majority',
                                        'win_by_Decision_Split',
                                        'win_by_Decision_Unanimous',
                                        'win_by_KO_TKO',
                                        'win_by_Submission',
                                        'win_by_TKO_Doctor_Stoppage',
                                        'wins',
                                        'Height_cms',
                                        'Reach_cms',
                                        'Weight_lbs',
                                        'age',
                                        'Stance_Open_Stance',
                                        'Stance_Orthodox',
                                        'Stance_Southpaw',
                                        'Stance_Switch')
# Export processed dataframes as csv
#write.csv(master_fighter_recent_stats,"master_fighter_recent_stats.csv", row.names = FALSE)
print("~~ FIGHTER STAT ENDED")



