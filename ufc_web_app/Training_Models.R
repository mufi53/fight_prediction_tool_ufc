library(sqldf)
# Importing the dataset 
dataset = read.csv('UFC_FINAL.csv') 
#dataset = read.csv('/Users/vikasnair/Documents/PBA_Project/pba_coursework/Modeling_Nick/UFC_PCA.csv')

# Encoding the target feature as factor 
dataset$Winner = factor(dataset$Winner)

# Splitting the dataset into the Training set and Test set 
#install.packages('caTools') 

#Scaling the dataset
dataset[-1]<-scale(dataset[-1])

#Normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- cbind(dataset$Winner, as.data.frame(lapply(dataset[-1], normalize)))

library(caTools) 
set.seed(123) 
split = sample.split(dataset$Winner, SplitRatio = 0.80) 

training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE) 

#####################################################################################################################
# Fitting SVM to the Training set 
#install.packages('e1071') 
library(e1071) 
summary(training_set)
svm_classifier = svm(formula = Winner ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'radial',
                 cost=10,
                 scale=TRUE,
                 probability=TRUE)

# Predicting the Test set results 
#svm_classifier = readRDS("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/SVM_RBF_Classifier.rds", refhook = NULL)

y_pred = predict(svm_classifier, newdata = test_set,probability=TRUE)
red_prob = head(attr(y_pred, "probabilities"))[1]
blue_prob = head(attr(y_pred, "probabilities"))[2]
# Making the Confusion Matrix 
cm = table(test_set[, 1], y_pred) 

head(attr(y_pred, "probabilities"))

saveRDS(svm_classifier, file = "/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/SVM_RBF_Classifier.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)

##########Random Forest###############################
library(randomForest)

rf = readRDS("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/RF_Classifier.rds", refhook = NULL)

rf = randomForest(Winner~., data = training_set, norm.votes = TRUE, proximity = TRUE)
#p1 = predict(rf, row_data_to_be_predicted, type = "prob")
predictions = predict(rf, test_set, type = "vote", norm.votes = TRUE)
blue_prob = predictions[1]
red_prob = predictions[2]
#y_pred = predict(rf, row_data_to_be_predicted)
#cm = table(test_set[, 1], y_pred) 

saveRDS(rf, file = "/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/RF_Classifier.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)


identical(p1,p2)

##################




###Neural Nets#################################################################################################################
####################################################################################################################
#Neural Network
library(neuralnet)
library(caTools) 

dataset$Winner = as.character(dataset$Winner)
dataset$Winner[dataset$Winner == 'Blue']  <- 1
dataset$Winner[dataset$Winner == 'Red']  <- 0
dataset$Winner = as.numeric(dataset$Winner)


test_set$Winner = as.character(test_set$Winner)
test_set$Winner[test_set$Winner == 'Blue']  <- 1
test_set$Winner[test_set$Winner == 'Red']  <- 0
test_set$Winner = as.numeric(test_set$Winner)


training_set$Winner = as.character(training_set$Winner)
training_set$Winner[training_set$Winner == 'Blue']  <- 1
training_set$Winner[training_set$Winner == 'Red']  <- 0
training_set$Winner = as.numeric(training_set$Winner)

#Neural Computing
nn <- neuralnet(Winner ~ . , data=training_set, hidden=c(40,),stepmax=1e6, linear.output=FALSE, threshold=0.01)
#nn$result.matrix

#Neural Network Plot
plot(nn)


#Garsons Reletive Importance
#library(NeuralNetTools)
#library(RSNNS)
#data(training_set)
#garson(nn)
#lekprofile(nn)
# garson
#garson(mod, 'Y1')


#Test Predict
#Test the resulting output
temp_test <- subset(test_set, select = c(-1))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test_set$Winner, prediction = nn.results$net.result)
#print(results)


#Confusion Matrix
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
#attach(roundedresultsdf)
table(actual,prediction)

#Saving the Model
saveRDS(nn, file = "/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/NN_classifier.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)



pred_neural_network_call < - function(dataset){
#Predicting New values/ Future Predictions
nn = readRDS("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/NN_classifier.rds", refhook = NULL)


dataset$Winner = as.character(dataset$Winner)
dataset$Winner[dataset$Winner == 'Blue']  <- 1
dataset$Winner[dataset$Winner == 'Red']  <- 0
dataset$Winner = as.numeric(dataset$Winner)

nn.results <- compute(nn, row_data_to_be_predicted)
row_data_to_be_predicted <- data.frame(prediction = nn.results$net.result)
roundedresults<-sapply(results,round,digits=0)
return (list(int(sapply(results,round,digits=0)),results$prediction))
}
#####################################################################################################################
#####################################################################################################################

#Data Extraction and Passing a row of data and win predictions

pred_neural_network_call<-function(row_data_to_be_predicted){
  nn = readRDS("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/NN_classifier.rds", refhook = NULL)
  nn.results <- compute(nn, row_data_to_be_predicted)
  results <- data.frame(prediction = nn.results$net.result)
  roundedresults<-sapply(results,round,digits=0)
  return (list(as.integer(sapply(results,round,digits=0)),results$prediction))
}


pred_random_forest_call<-function(row_data_to_be_predicted){
  rf = readRDS("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/RF_Classifier.rds", refhook = NULL)
  predictions = predict(rf, row_data_to_be_predicted, type = "vote", norm.votes = TRUE)
  blue_prob = predictions[1]
  red_prob = predictions[2]
  return(list(blue_prob,red_prob))
}

pred_svm_call <- function(row_data_to_be_predicted){
  svm_classifier = readRDS("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/RDS_Files/SVM_RBF_Classifier.rds", refhook = NULL)
  y_pred = predict(svm_classifier, newdata = row_data_to_be_predicted,probability=TRUE)
  red_prob = head(attr(y_pred, "probabilities"))[1]
  blue_prob = head(attr(y_pred, "probabilities"))[2]
  return(list(blue_prob,red_prob))
}
  
data_predictions_from_models <- function(row_data_to_be_predicted) {
  neural_net_output = pred_neural_network_call(row_data_to_be_predicted)
  random_forest_output = pred_random_forest_call(row_data_to_be_predicted)
  svm_output = pred_svm_call(row_data_to_be_predicted)
  outputs = list(neural_net_output,random_forest_output,svm_output)
  return (outputs)
}


winner_ensemble_analysis <- function(model_outputs){
  nn_output = as.numeric(model_outputs[[1]][1])
  nn_probability = as.numeric(model_outputs[[1]][2])
  nn_winner = 'null'
  nn_winner = ifelse(as.integer(nn_output) == 1,'blue','red')
  nn_blue_prob = ifelse(nn_winner=='blue',nn_probability,1-nn_probability)
  nn_red_prob = ifelse(nn_winner=='red',nn_probability,1-nn_probability)
  
  #nn_winner_prob = nn_probability
  #nn_looser_prob = 1-nn_winner_prob
  
  rf_blue_prob = as.numeric(model_outputs[[2]][1])
  rf_red_prob = as.numeric(model_outputs[[2]][2])
  rf_winner = ifelse(rf_blue_prob>rf_red_prob,'blue','red')
  rf_winner_prob = ifelse(rf_winner=='blue',rf_blue_prob,rf_red_prob)

  svm_blue_prob = as.numeric(model_outputs[[3]][1])
  svm_red_prob = as.numeric(model_outputs[[3]][2])
  svm_winner = ifelse(svm_blue_prob>svm_red_prob,'blue','red')
  svm_winner_prob = ifelse(svm_winner=='blue',svm_blue_prob,svm_red_prob)

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  winner = Mode(list(svm_winner,rf_winner,nn_winner))
  
  average_prob_red = (2/3)*100#round((rf_red_prob+nn_red_prob+svm_red_prob)/3,digits = 4)*100
  average_prob_blue = (1/3)*100#round((rf_blue_prob+nn_blue_prob+svm_blue_prob)/3,digits = 4)*100
  
  return(list(winner,average_prob_blue,average_prob_red))
}

get_fighter_details <- funtion(fighter_id_B,fighter_id_R){
  fighter_stats = read.csv("/Users/vikasnair/Documents/PBA_Project/pba_coursework/Final_Modules/Datasets/Revised_Final_data_V2/master_fighter_recent_stats.csv")
  row_data_to_be_predicted_1 = cbind(sqldf(paste("Select * from fighter_stats where Fighter_ID  ='",as.character(fighter_id_B),"'",sep="")),sqldf(paste("Select * from fighter_stats where Fighter_ID  ='",as.character(fighter_id_R),"'",sep="")))
  row_data_to_be_predicted = row_data_to_be_predicted_1[,c(-1,-2,-3,-25,-26,-27,-28,-50)]
  hardcoded_column_names = list("B_current_lose_streak","B_current_win_streak"
                                ,"B_longest_win_streak","B_losses","B_total_rounds_fought"
                                ,"B_total_title_bouts","B_win_by_Decision_Majority","B_win_by_Decision_Split"
                                ,"B_win_by_Decision_Unanimous","B_win_by_KO_TKO","B_win_by_Submission"
                                ,"B_win_by_TKO_Doctor_Stoppage","B_wins","B_Height_cms"
                                ,"B_Reach_cms","B_Weight_lbs","B_age","B_Stance_Open_Stance","B_Stance_Orthodox","B_Stance_Southpaw","B_Stance_Switch"
                                ,"R_current_lose_streak","R_current_win_streak","R_longest_win_streak","R_losses"
                                ,"R_total_rounds_fought","R_total_title_bouts","R_win_by_Decision_Majority"
                                ,"R_win_by_Decision_Split","R_win_by_Decision_Unanimous","R_win_by_KO_TKO"
                                ,"R_win_by_Submission","R_win_by_TKO_Doctor_Stoppage","R_wins"
                                ,"R_Height_cms","R_Reach_cms","R_Weight_lbs"
                                ,"R_age","R_Stance_Open_Stance","R_Stance_Orthodox","R_Stance_Southpaw","R_Stance_Switch")
  names(row_data_to_be_predicted) = hardcoded_column_names
  model_outputs = data_predictions_from_models(row_data_to_be_predicted)
  winner_output = winner_ensemble_analysis(model_outputs)
  winner_id = ifelse(winner_output[[1]][1]=='blue',fighter_id_B,fighter_id_R)
  winner_name = as.character(sqldf(paste("Select Fighter from fighter_stats where Fighter_ID ='",as.character(winner_id),"'",sep=""))[1,1])
  
  return(list(winner_name,as.numeric(winner_output[[2]][1]),as.numeric(winner_output[[3]][1])))
}


# "B_current_lose_streak","B_current_win_streak"
# ,"B_longest_win_streak","B_losses","B_total_rounds_fought"
# ,"B_total_title_bouts","B_win_by_Decision_Majority","B_win_by_Decision_Split"
# ,"B_win_by_Decision_Unanimous","B_win_by_KO_TKO","B_win_by_Submission"
# ,"B_win_by_TKO_Doctor_Stoppage","B_wins","B_Height_cms"
# ,"B_Reach_cms","B_Weight_lbs","B_age","B_Stance_Open_Stance","B_Stance_Orthodox","B_Stance_Southpaw","B_Stance_Switch"
# ,"R_current_lose_streak","R_current_win_streak","R_longest_win_streak","R_losses"
# ,"R_total_rounds_fought","R_total_title_bouts","R_win_by_Decision_Majority"
# ,"R_win_by_Decision_Split","R_win_by_Decision_Unanimous","R_win_by_KO_TKO"
# ,"R_win_by_Submission","R_win_by_TKO_Doctor_Stoppage","R_wins"
# ,"R_Height_cms","R_Reach_cms","R_Weight_lbs"
# ,"R_age","R_Stance_Open_Stance","R_Stance_Orthodox","R_Stance_Southpaw","R_Stance_Switch"
