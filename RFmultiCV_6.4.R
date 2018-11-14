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

train <- NULL
test <- NULL

training_percent = 0.8
number_of_threads = 4
seed = 37596
nTree = 1000 # Optimal trees = 1000, based on RFmultitree.R

results.table <- data.table(2:40)
names(results.table)[1] <- 'Multifold_k'
results.table$OOB_Accuracy <- 0
results.table$test_Accuracy <- 0
results.table$Accuracy <- 0

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
randF.cv <- function(seed, training, labels, testing, testing_labels, k) {
  
  set.seed(seed)
  cv.k.folds <- createMultiFolds(labels, k = k, times = 10)
  
  # Set up caret's trainControl object per above.
  ctrl.k <- trainControl(method = "repeatedcv", number = k, repeats = 10,
                         index = cv.k.folds)
  
  cl <- parallel::makeCluster(number_of_threads)
  registerDoSNOW(cl)
  
  set.seed(seed)
  rf <- train(x = training, y = labels, method = "rf", tuneLength = 3,
                     ntree = nTree, trControl = ctrl.k)
  
  stopCluster(cl)
  
  return (rf)
}

# ---------------------------
# 6.4 Addition of cross validation
# ---------------------------

for (k in 2:40) {
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
  rf.1 <- randF(seed, rf.train.1, data.label, rf.test.1, test$ToPromote, k)
  
  # Saving OOB_accuracy
  results.table$OOB_Accuracy[k-1] <- confusionMatrix(rf.1$predicted, data.label)$overall["Accuracy"]
  # Saving test_accuracy
  results.table$test_Accuracy[k-1] <- confusionMatrix(rf.1$test$predicted, test$ToPromote)$overall["Accuracy"]
  
  rf.preds <- predict(rf.1, rf.test.1, type = "class")
  conf_matrix <- confusionMatrix(rf.preds, test$ToPromote)
  # Saving accuracy
  results.table$Accuracy[k-1] <- conf_matrix$overall["Accuracy"]
}

# Check to see the accuracy and which k is most ideal for the accuracy
results.table[max(Accuracy) == Accuracy]

# -------------------------------------
# Initial benchmark
# OOB_accuracy: 0.997246
# test_accuracy: 0.9967825
# Accuracy: 0.9967825
# -------------------------------------
# Best score based on cross-validation (k=30)
# OOB_accuracy: 0.9936183
# test_accuracy: 0.99571
# Accuracy: 0.99571
# --------------------------------------
#
# Surprisingly, in this case, cross-validation is not that effective
# Even the best accuracy out of 39 different cross-validation values
# The original model without cross-validation is still more accuracy in terms of all 3 accuracy

# Save it to the folder
write.csv(results.table, file = "rf_cv_table.csv", row.names = FALSE)
