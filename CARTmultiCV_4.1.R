library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(doSNOW)
library(plyr)
library(ggplot2)
library(cluster)

# --------------------------
# Requirement:
# Run clean_data.R 
# Run CART.R
# There is preshuffle_data.2 in your object variables
# --------------------------


training_percent = 0.8
number_of_threads = 4
seed = 37596
multifolds_times = 10

results.table <- data.table(2:11)
names(results.table)[1] <- 'k'
results.table$CV_Accuracy <- 0
results.table$Accuracy <- 0

# Set the preparation of train and test into a function to ease future configurations
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

# ------------------------------------------
# Start of training of CART
# ------------------------------------------

# Create utility function to ease the processsing
rpart.cv <- function(seed, training, labels, ctrl) {
  # This is to enable parallel processing
  # Meaning using multithread, each to run the CART training
  # Because normally R will limit the CPU running capacity
  # Hence it is necessary to run multithreads in parallel
  # to save time
  cl <- parallel::makeCluster(number_of_threads)
  # Need to register for the cluster to
  registerDoSNOW(cl)
  
  # Remember to set the seed everytime before you run, because
  # the starting point of where you pick before creating the tree
  # actually matters here
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# So cross validation is splitting the whole records into k parts, then set one of the part
# as the testing part, and the rest of them are used for training
# Times is just the number of times it is repeated, because different starting points matter
prepare_ctrl <- function(data.label, multifolds_k, multifolds_times) {
  cv.folds <- createMultiFolds(data.label, k = multifolds_k, times = multifolds_times)
  ctrl <- trainControl(method = "repeatedcv", number = multifolds_k, repeats = multifolds_times,
                       index = cv.folds)
  return (ctrl)
}

# ------------------------------
# Start of the looping
# ------------------------------

for (multifolds_k in 2:11) {

# --------------------------------------
# Run 3, with ToPromote as the label,
# And remove PerformanceRating as variables
# Because performance rating is highly correlated, and it feels like it is something
# that we are predicting
# --------------------------------------

# Remember to set your working data, reset to original, to preshuffle period
edited_data <- preshuffle_data.2

prepare_train_test(training_percent, edited_data, seed)
data.label <- train$ToPromote

# Grab features
rpart.train.CV <- train
rpart.train.CV$ToPromote <- NULL # Remove away the label
rpart.train.CV$PerformanceRating <- NULL

rpart.test.CV <- test
rpart.test.CV$ToPromote <- NULL # Remove away the label
rpart.test.CV$PerformanceRating <- NULL

# Create multi-folds
set.seed(seed)
ctrl <- prepare_ctrl(data.label, multifolds_k, multifolds_times)


# Run CV and check out results
rpart.cv.results <- rpart.cv(seed, rpart.train.CV, data.label, ctrl)

# Save it to the table
results.table$CV_Accuracy[multifolds_k-1] <- max(rpart.cv.results$results$Accuracy)


# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.cv.preds <- predict(rpart.cv.results$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.cv.preds, test$ToPromote)

# Save it to the table
results.table$Accuracy[multifolds_k-1] <- conf_matrix$overall["Accuracy"]

}

# -------------------------------
# 4.1 Cross-validation k
# ------------------------------

# Check to see the accuracy and which k is most ideal for cross validation
# which is k=7 highest accuracy with 0.7838748
results.table

# The possiblity why the accuracy is all the same, because the model they created are very similar
# but still differs abit in accuracy when it comes to CV_accuracy

# Save it to the folder
write.csv(results.table, file = "CV_table.csv", row.names = FALSE)
