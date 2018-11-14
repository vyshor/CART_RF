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
nTree_size = 100 # number of trees per unit_size (which means that we going to have a multiplier factor k)
# (Total number of trees = k * nTree_size)

results.table <- data.table(1:11)
names(results.table)[1] <- 'k'
results.table$OOB_Accuracy <- 0
results.table$test_Accuracy <- 0
results.table$Accuracy <- 0
results.table$Time_taken <- 0.

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
randF <- function(seed, training, labels, testing, testing_labels, nTree) {
  
  cl <- parallel::makeCluster(number_of_threads)
  registerDoSNOW(cl)
  
  set.seed(seed)
  rf <- randomForest(x = training, y = labels, importance = TRUE, ntree = nTree, xtest = testing, ytest = testing_labels, keep.forest = TRUE)
  
  stopCluster(cl)
  
  return (rf)
}

# ---------------------------
# 6.3 Varying number of trees
# ---------------------------

for (k in 1:11) {
  
  
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
time_needed <- system.time(rf.1 <- randF(seed, rf.train.1, data.label, rf.test.1, test$ToPromote, nTree_size * k))
# Only keep track of the time for running the RandomForest, because it is the one that takes the most time

# Saving OOB_accuracy
results.table$OOB_Accuracy[k] <- confusionMatrix(rf.1$predicted, data.label)$overall["Accuracy"]
# Saving test_accuracy
results.table$test_Accuracy[k] <- confusionMatrix(rf.1$test$predicted, test$ToPromote)$overall["Accuracy"]

rf.preds <- predict(rf.1, rf.test.1, type = "class")
conf_matrix <- confusionMatrix(rf.preds, test$ToPromote)
# Saving accuracy
results.table$Accuracy[k] <- conf_matrix$overall["Accuracy"]
# Saving the time used
results.table$Time_taken[k] <- time_needed[3]

}

# Check to see the accuracy and which k is most ideal for number of trees compared to time
# which is k=10 highest accuracy with 0.9967825, thus nTree = 1000
results.table

# One thing to note, there is actually no overfitting, because
# the accuracy is actually not much dependent on the number of trees
# And the accuracy is actually based off a set of results that the randomForest has yet to see
# Hence, the only trade off is the time taken to build the tree
# And which nTree will give the optimal accuacy, if you scrutinise down to every percent of accuracy


# Save it to the folder
write.csv(results.table, file = "tree_table.csv", row.names = FALSE)
