library(data.table)
library(caret)
library(doSNOW)
library(plyr)
library(ggplot2)
library(cluster)
library(randomForest)
library(rlist)

# --------------------------
# Requirement:
# Run clean_data.R 
# Run CART.R
# There is preshuffle_data.2 in your object variables
# --------------------------

train <- NULL
test <- NULL

training_percent = 0.8
number_of_threads = 4
nTree = 1000 # Optimal trees = 1000, based on RFmultitree.R

results.table <- data.table(c(37596, 12345, 95824, 56932, 10514, 20095, 23420, 20515, 31925, 29392))
names(results.table)[1] <- 'seed'
results.table$OOB_Accuracy <- 0
results.table$test_Accuracy <- 0
results.table$Accuracy <- 0
conf_matrix_list <- list()

prepare_train_test <- function(training_percent, edited_data, seed) {
  
  training_count = round(training_percent * nrow(edited_data))
  testing_count = nrow(edited_data) - training_count
  
  # Shuffling the data, because the order of the data input might matter
  # Sometimes the model you are using do it for you, sometimes they don't
  set.seed(seed)
  # Remember to set seed everytime before you shuffle, so that it will result
  # in same shuffle data everytime
  edited_data <- edited_data[sample(.N, .N)]
  
  # Double arrow means global variables
  train <<- edited_data[1:training_count]
  test <<- edited_data[(training_count+1):(training_count+testing_count)]
}

# Create utility function to ease the processsing
randF <- function(seed, training, labels, testing, testing_labels) {
  
  cl <- parallel::makeCluster(number_of_threads)
  registerDoSNOW(cl)
  
  set.seed(seed)
  rf <- randomForest(x = training, y = labels, importance = TRUE, ntree = nTree, xtest = testing, ytest = testing_labels, keep.forest = TRUE)
  
  stopCluster(cl)
  
  return (rf)
}

randF.seeds <- function(preshuffle_data.3) {
  results.table <- data.table(c(37596, 12345, 95824, 56932, 10514, 20095, 23420, 20515, 31925, 29392))
  names(results.table)[1] <- 'seed'
  results.table$OOB_Accuracy <- 0
  results.table$test_Accuracy <- 0
  results.table$Accuracy <- 0
  conf_matrix_list <<- list()
  for (k in 1:10) {
    seed = results.table$seed[k]
    
    # ------------------------------------------
    # Run 1. Using ToPromote as label, and does NOT include PerformanceRating in feature
    # ------------------------------------------
    
    edited_data <- preshuffle_data.3
    prepare_train_test(training_percent, edited_data, seed)
    data.label <- train$ToPromote
    
    # Setting the data for train and test
    rf.train.1 <- train
    rf.train.1$ToPromote <- NULL # Remove away the label
    rf.train.1$PerformanceRating <- NULL
    
    rf.test.1 <- test
    rf.test.1$ToPromote <- NULL # Remove away the label
    rf.test.1$PerformanceRating <- NULL
    
    # Running of the randomForest
    rf.1 <- randF(seed, rf.train.1, data.label, rf.test.1, test$ToPromote)
    
    # Saving OOB_accuracy
    results.table$OOB_Accuracy[k] <- confusionMatrix(rf.1$predicted, data.label)$overall["Accuracy"]
    # Saving test_accuracy
    results.table$test_Accuracy[k] <- confusionMatrix(rf.1$test$predicted, test$ToPromote)$overall["Accuracy"]
    
    rf.preds <- predict(rf.1, rf.test.1, type = "class")
    conf_matrix <- confusionMatrix(rf.preds, test$ToPromote)
    conf_matrix_list <<- list.append(conf_matrix_list, conf_matrix)
    # Saving accuracy
    results.table$Accuracy[k] <- conf_matrix$overall["Accuracy"]
    
  }
  return (results.table)
}

# ---------------------------
# 6.5 Use of different seeds
# ---------------------------

for (k in 1:10) {
  seed = results.table$seed[k]
  
  # ------------------------------------------
  # Run 1. Using ToPromote as label, and does NOT include PerformanceRating in feature
  # ------------------------------------------
  edited_data <- preshuffle_data.2
  prepare_train_test(training_percent, edited_data, seed)
  data.label <- train$ToPromote
  
  # Setting the data for train and test
  rf.train.1 <- train
  rf.train.1$ToPromote <- NULL # Remove away the label
  rf.train.1$PerformanceRating <- NULL
  
  rf.test.1 <- test
  rf.test.1$ToPromote <- NULL # Remove away the label
  rf.test.1$PerformanceRating <- NULL
  
  # Running of the randomForest
  rf.1 <- randF(seed, rf.train.1, data.label, rf.test.1, test$ToPromote)
  
  # Saving OOB_accuracy
  results.table$OOB_Accuracy[k] <- confusionMatrix(rf.1$predicted, data.label)$overall["Accuracy"]
  # Saving test_accuracy
  results.table$test_Accuracy[k] <- confusionMatrix(rf.1$test$predicted, test$ToPromote)$overall["Accuracy"]
  
  rf.preds <- predict(rf.1, rf.test.1, type = "class")
  conf_matrix <- confusionMatrix(rf.preds, test$ToPromote)
  # Saving accuracy
  results.table$Accuracy[k] <- conf_matrix$overall["Accuracy"]
  
}

# Check the highest accuracy based on different random starting points
results.table

results.table[max(OOB_Accuracy) == OOB_Accuracy]
results.table[max(test_Accuracy) == test_Accuracy]
results.table[max(Accuracy) == Accuracy]

# -------------------------------------
# Initial benchmark (seed: 37596)
# OOB_accuracy: 0.996246
# test_accuracy: 0.9967825
# Accuracy: 0.9967825
# -------------------------------------
# Best OOB_Accuracy (seed: 37596)
# OOB_accuracy: 0.996246
# test_accuracy: 0.99678245
# Accuracy: 0.9967825
# --------------------------------------
# Best test_accuracy (seed: 20515)
# OOB_accuracy: 0.9954416
# test_accuracy: 0.997426
# Accuracy: 0.997426
# --------------------------------------
# Best Accuracy (seed: 20515)
# OOB_accuracy: 0.9954416
# test_accuracy: 0.997426
# Accuracy: 0.997426
# --------------------------------------

results.table
# And then we look at the table again,
# we can kind of conclude that randomForest was the right model to be used to predict
# and we did not get such high accuracy with just pure random luck of picking the right seed
# and the accuracy is high across differnt seeds

# Save it to the folder
write.csv(results.table, file = "rf_seed_table.csv", row.names = FALSE)

# Lastly, we want to run randomForest on our new features that we get based off refine_data.R
# Hence, there are two options
# 1. Recode JobRole only
# 2. Recode JobRole, StockOptionLevel, and Attrition
#

# ---------------------------
# 6.6.1 Using Recoded JobRole
# ---------------------------

# --------------------------------------------------------
#  1. Recode JobRole
# --------------------------------------------------------
#
# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Convert back to characters
preshuffle_data.3$JobRole <- as.character(preshuffle_data.3$JobRole)

# Recode them
preshuffle_data.3$JobRole[which(preshuffle_data.3$JobRole == 'Manufacturing Director'| preshuffle_data.3$JobRole == 'Research Director' | preshuffle_data.3$JobRole == 'Healthcare Representative' )] <- 'High' 
preshuffle_data.3$JobRole[which(preshuffle_data.3$JobRole == 'Sales Executive'| preshuffle_data.3$JobRole == 'Research Scientist' | preshuffle_data.3$JobRole == 'Manager' )] <- 'Medium' 
preshuffle_data.3$JobRole[which(preshuffle_data.3$JobRole == 'Human Resources'| preshuffle_data.3$JobRole == 'Sales Representative' | preshuffle_data.3$JobRole == 'Laboratory Technician' )] <- 'Low' 

# Then factor the feature again
preshuffle_data.3$JobRole <- as.factor(preshuffle_data.3$JobRole)

# Check if it is factored properly
summary(preshuffle_data.3$JobRole)

# Run with different seeds
results.table <- randF.seeds(preshuffle_data.3)

# Look at the results
results.table

results.table[max(OOB_Accuracy) == OOB_Accuracy]
results.table[max(test_Accuracy) == test_Accuracy]
results.table[max(Accuracy) == Accuracy]

# -------------------------------------
# Initial benchmark (seed: 37596)
# OOB_accuracy: 0.996246
# test_accuracy: 0.9967825
# Accuracy: 0.9967825
# -------------------------------------
# Best OOB_Accuracy (seed: 37596)
# OOB_accuracy: 0.9964606 (Original Best OOB_accuracy: 0.996246)
# test_accuracy: 0.996568
# Accuracy: 0.996568
# --------------------------------------
# Best test_accuracy (seed: 56932)
# OOB_accuracy: 0.9963533
# test_accuracy: 0.9976405 (Original Best test_accuracy: 0.997426)
# Accuracy: 0.9976405
# --------------------------------------
# Best Accuracy (seed: 56932)
# OOB_accuracy: 0.9963533
# test_accuracy: 0.9976405
# Accuracy: 0.9976405 (Original Best Accuracy: 0.997426)
# --------------------------------------
# Comment:
# THe accuracy did increased marginally, but it is actually quite crucial
# As it get closer to 100%, it becomes harder and harder to increase each percent

# Save it to the folder
write.csv(results.table, file = "rf_seed_table_recoded_1.csv", row.names = FALSE)

# ---------------------------
# 6.6.2 Using Recoded JobRole, StockOptionLevel, and Attrition
# ---------------------------
# --------------------------------------------------------
#  2. Recode JobRole, StockOptionLevel, and Attrition
# --------------------------------------------------------
#
# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Convert back to characters
preshuffle_data.3$JobRole <- as.character(preshuffle_data.3$JobRole)

# Recode them
preshuffle_data.3$JobRole[which(preshuffle_data.3$JobRole == 'Manufacturing Director'| preshuffle_data.3$JobRole == 'Research Director' | preshuffle_data.3$JobRole == 'Healthcare Representative' )] <- 'High' 
preshuffle_data.3$JobRole[which(preshuffle_data.3$JobRole == 'Sales Executive'| preshuffle_data.3$JobRole == 'Research Scientist' | preshuffle_data.3$JobRole == 'Manager' )] <- 'Medium' 
preshuffle_data.3$JobRole[which(preshuffle_data.3$JobRole == 'Human Resources'| preshuffle_data.3$JobRole == 'Sales Representative' | preshuffle_data.3$JobRole == 'Laboratory Technician' )] <- 'Low' 

# Then factor the feature again
preshuffle_data.3$JobRole <- as.factor(preshuffle_data.3$JobRole)

# Check if it is factored properly
summary(preshuffle_data.3$JobRole)

# Convert to characters
preshuffle_data.3$StockOptionLevel <- as.character(preshuffle_data.3$StockOptionLevel)

# Recode them
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '0')] <- 'None' 
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '1')] <- 'Low' 
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '2'| preshuffle_data.3$StockOptionLevel == '3')] <- 'High' 

# Then factor the feature again
preshuffle_data.3$StockOptionLevel <- as.factor(preshuffle_data.3$StockOptionLevel)

# Check if it is factored properly
summary(preshuffle_data.3$StockOptionLevel)

# Convert to characters
preshuffle_data.3$Attrition <- as.character(preshuffle_data.3$Attrition)

# Recode them
preshuffle_data.3$Attrition[which(preshuffle_data.3$Attrition == 'Current employee')] <- 'No' 
preshuffle_data.3$Attrition[which(preshuffle_data.3$Attrition == 'Voluntary Resignation' | preshuffle_data.3$Attrition == 'Termination')] <- 'Yes' 

# Then factor the feature again
preshuffle_data.3$Attrition <- as.factor(preshuffle_data.3$Attrition)

# Check if it is factored properly
summary(preshuffle_data.3$Attrition)

# Run with different seeds
results.table <- randF.seeds(preshuffle_data.3)

# Look at the results
results.table

results.table[max(OOB_Accuracy) == OOB_Accuracy]
results.table[max(test_Accuracy) == test_Accuracy]
results.table[max(Accuracy) == Accuracy]

# -------------------------------------
# Initial benchmark (seed: 37596)
# OOB_accuracy: 0.996246
# test_accuracy: 0.9967825
# Accuracy: 0.9967825
# -------------------------------------
# Best OOB_Accuracy (seed: 37596)
# OOB_accuracy: 0.996836 (Original Best OOB_accuracy: 0.996246)
# test_accuracy: 0.996139
# Accuracy: 0.996139
# --------------------------------------
# Best test_accuracy (seed: 56932)
# OOB_accuracy: 0.995817
# test_accuracy: 0.9976405 (Original Best test_accuracy: 0.997426)
# Accuracy: 0.9976405
# --------------------------------------
# Best Accuracy (seed: 56932)
# OOB_accuracy: 0.995817
# test_accuracy: 0.9976405
# Accuracy: 0.9976405 (Original Best Accuracy: 0.997426)
# --------------------------------------
# Comments:
# This results is the same as when only JobRole is recoded
# But either ways, we can determine this model is the one with the highest accuracy so far
#
# Save it to the folder
write.csv(results.table, file = "rf_seed_table_recoded_2.csv", row.names = FALSE)

# Additional code to see the confusion matrix of best model
conf_matrix_list[[which(results.table$Accuracy == max(results.table$Accuracy))]]
# Additional code to see the most important variables when RF ran on recoded data
# Seeing which features matter the most
varImpPlot(rf.1)

partialPlot(rf.1, rf.train.1, 'EmployeeSource', las=2, mar=c(4,10,10,2), xlab="")
partialPlot(rf.1, rf.train.1, 'MonthlyIncome')
partialPlot(rf.1, rf.train.1, 'JobRole', las=2, mar=c(4,10,10,2), xlab="")
partialPlot(rf.1, rf.train.1, 'TotalWorkingYears')

# partialPlot(rf.1, rf.train.1, 'EmployeeNumber')
partialPlot(rf.1, rf.train.1, 'MonthlyRate')
partialPlot(rf.1, rf.train.1, 'RelationshipSatisfaction')
partialPlot(rf.1, rf.train.1, 'YearsInCurrentRole')
partialPlot(rf.1, rf.train.1, 'YearsWithCurrManager')
partialPlot(rf.1, rf.train.1, 'Age')

