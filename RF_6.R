library(data.table)
library(caret)
library(doSNOW)
library(plyr)
library(ggplot2)
library(cluster)
library(randomForest)

# --------------------------
# Requirement:
# Run clean_data.R 
# Run CART.R
# There is preshuffle_data.2 in your object variables
# --------------------------

# ---------------------------
# 6.1 Introduction
# ---------------------------

# Explanation of randomForest
# -------------------------------------------
# TL;DR it is just multiple decision-trees (CART) and average the results
# and it is better than CART in generalisation because of its large number
#
# It is creating multiple trees, and another concept of bagging
# where you pick a data entry from the bag of training data,
# and then you record the data entry and use it to train,
# then you put the data back into the 'bag' of training data
# and you repeat the process until you get the number of datas same as your dataset
# -------------------------------------------

train <- NULL
test <- NULL

training_percent = 0.8
number_of_threads = 4
seed = 37596
nTree = 1000 # number of trees in randomForest

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

# ------------------------------------------
# Start of training of Random Forest
# ------------------------------------------

# ---------------------------
# 6.2 RF Run #1
# ---------------------------

# ------------------------------------------
# Run 1. Using ToPromote as label, and does NOT include PerformanceRating in feature
# ------------------------------------------

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
set.seed(seed)
rf.1 <- randF(seed, rf.train.1, data.label, rf.test.1, test$ToPromote)
rf.1

# OOB_accuracy: 0.997246
# test_accuracy: 0.9967825

rf.preds <- predict(rf.1, rf.test.1, type = "class")
conf_matrix <- confusionMatrix(rf.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.9967825
# RandomForest is surprisingly good at predicting

# Code for getting OOB_accuracy
confusionMatrix(rf.1$predicted, data.label)$overall["Accuracy"]
# Code for getting test_accuracy
confusionMatrix(rf.1$test$predicted, test$ToPromote)$overall["Accuracy"]

# Seeing which features matter the most
varImpPlot(rf.1)

# Interpretation of MeanDecreaseAccuracy and MeanDecreaseGini
# MeanDecreaseAccuracy: Indicates the average loss in accuracy without the feature
# MeanDecreaseGini: Indicates the loss in 'sorting ability' of trees based on the feature

# partialPlot can be used to can show the marginal effect of a variable
# on the predicting ability (probability) of the class/label
partialPlot(rf.1, rf.train.1, 'EmployeeSource')
partialPlot(rf.1, rf.train.1, 'MonthlyIncome')

# Plot number of trees against the error
plot(rf.1)
# Some might claim a case of overfitting, because of too many trees
# But, you must remember, the model actually have never seen the test data at all
# Yet the accuracy is still high. Hence, it is actually not overfitting
# The only arguement can be made is if the number of trees is optimized
# because more trees, would only mean more time is needed to train the data
# Anyway, another experiment of using different number of trees will be run
# to check the optimal number of trees needed to get the best accuracy


# -----------------------------------------------
# Understanding of the results
# -----------------------------------------------
# OOB error rate:
# So OOB error rate is the estimated error based on those samples,
# which are not picked by the bagging process. Those samples are used as the test data,
# since technically they have not been 'seen' by the training model
# so it still makes valid sense for them to be used as test data
#
#
# -----------------------------------------------
# Test set error rate:
# The interesting thing is that randomForest has its own testing mechanism,
# however, its own testing mechanism is done "in place" as the trees are grown
# which means, it does not wait until the whole forest is developed before
# predicting, which may lead to varying results, but in just minor cases
# but in both cases, we try to record this accuracy as test_accuracy
# 
# ------------------------------------------------
# Our own manual accuracy:
#
# Previously, we splitted the data into training and testing,
# so in this case, we still use the whole (complete) forest as a model to predict
# rather than "predicting while growing the forest" which is used by test set error rate
# Hence, in this case, a complete model should reflect a better indication of the accuracy
# as the final model is the one to use to predict values, if this model ever get used in the future
# -------------------------------------------------


