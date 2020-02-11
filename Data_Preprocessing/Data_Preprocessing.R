#Data Preprocessing
#Converting Raw_Data to MRD Data

#reading UFC raw_data to the environment
raw_data <- read.csv("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Data_Preprocessing/Raw_Data_to_MRD_Data/raw_data_1993_2019.csv",header=TRUE,stringsAsFactors = FALSE)
raw_data = raw_data[,c(6:7,10:12,37:38,63,65:79,104:105,130,132:145)]

#Each row in the data contains information of a bout between two players
#take a look at the first few rows of the data  and the summary of each column to understand the information given
head(raw_data)
summary(raw_data)


#Most of the variables are present in "numeric" data type which is good and might reduce the transformations required to create a MRD dataset.
#there are many variables which are given at match level detail, we are trying to predict before a match happens if a particular fighter historical stats could decide the outcome of a bout.

#Missing values in each column
sapply(raw_data, function(x) sum(is.na(x)))
#B_Reach_cms - 666 missing
#Red_Reach - 316 missing 
#R_Weight_lbs - 3 missing
#B_Weight_lbs - 6 missing
#B_age - 172 missing
#R_age - 64 missing
#B_Height_cms - 8 missing
#R_Height_cms - 4 missing


resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

library(tidyr)
#test = raw_data[,c(-1,-2,-5,-17,-23,-35)]
test = raw_data[,c(18,19,20)]
test = test %>% drop_na()

#Cheking for body attributes correlation for fighter B stats
library(corrplot)
correlation_matrix <- cor(test)
corrplot(correlation_matrix, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)


#Cheking for body attributes correlation for fighter A stats
test = raw_data[,c(36,37,38)]
test = test %>% drop_na()
correlation_matrix <- cor(test)
corrplot(correlation_matrix, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)


#Reach and Height have 88% for Blue fighters and 89% for Red fighters correlation
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(raw_data$B_Height_cms, main="Blue_Height")
boxplot(raw_data$B_Reach_cms, main="Blue_Reach")

par(mfrow=c(1, 2))
boxplot(raw_data$R_Height_cms, main="Red_Height")
boxplot(raw_data$R_Reach_cms, main="Red_Reach")

par(resetPar())

#Since height has less number of missing values and reach contains comparitively more missing values 
#Replaceing missing values by median of the columns
#NA2median <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#raw_data$R_Height_cms = replace(raw_data$R_Height_cms, TRUE, lapply(raw_data$R_Height_cms, NA2median))

#Replaceing Missing values with Mean in height columns
raw_data$R_Height_cms[is.na(raw_data$R_Height_cms)] <- round(median(raw_data$R_Height_cms, na.rm = TRUE))
raw_data$B_Height_cms[is.na(raw_data$B_Height_cms)] <- round(median(raw_data$B_Height_cms, na.rm = TRUE))


linearMod <- lm(scale(R_Reach_cms) ~ scale(R_Height_cms), data=test)
plot(scale(test$R_Reach_cms),scale(test$R_Height_cms))
abline(linearMod)
summary(linearMod)

raw_data_height_red_df = data.frame(R_Height_cms = raw_data$R_Height_cms)
distPred_df <- setNames(data.frame(predict(linearMod, raw_data_height_red_df)),c('reach'))

#Replacing Missing values with models prediction
raw_data$R_Reach_cms[is.na(raw_data$R_Reach_cms)] <- distPred_df$reach[is.na(raw_data$R_Reach_cms)]


#Fighter B variables subset as test
test = raw_data[,c(18,19,20)]
test = test %>% drop_na()

#Plotting data points with regression line

linearMod_2 <- lm(scale(B_Reach_cms) ~ scale(B_Height_cms), data=test)
with(test,plot(scale(B_Reach_cms), scale(B_Height_cms)))
abline(linearMod_2)

#print(linearMod)
#summary(linearMod) 

raw_data_height_red_df = data.frame(B_Height_cms = raw_data$B_Height_cms)
distPred_df <- setNames(data.frame(predict(linearMod_2, raw_data_height_red_df)),c('reach'))

#Replacing Missing values with models prediction
raw_data$B_Reach_cms[is.na(raw_data$B_Reach_cms)] <- distPred_df$reach[is.na(raw_data$B_Reach_cms)]









