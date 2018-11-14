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
multifolds_k = 7
multifolds_times = 10

seed.table <- data.table(c(37596, 12345, 95824, 56932, 10514, 20095, 23420, 20515, 31925, 29392))
names(seed.table)[1] <- 'seed'
seed.table$CV_Accuracy <- 0
seed.table$Accuracy <- 0

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

prepare_ctrl <- function(data.label, multifolds_k, multifolds_times) {
  
  cv.folds <- createMultiFolds(data.label, k = multifolds_k, times = multifolds_times)
  
  ctrl <- trainControl(method = "repeatedcv", number = multifolds_k, repeats = multifolds_times,
                       index = cv.folds)
  return (ctrl)
}

# ------------------------
# Start of seed looping
# ------------------------

for (i in 1:10) {

seed <- seed.table$seed[i]

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

# Prepare cross validation settings
# Create multi-folds
set.seed(seed)
# So cross validation is splitting the whole records into k parts, then set one of the part
# as the testing part, and the rest of them are used for training
# Times is just the number of times it is repeated, because different starting points matter
ctrl <- prepare_ctrl(data.label, multifolds_k, multifolds_times)

# Grab features
rpart.train.seed <- train
rpart.train.seed$ToPromote <- NULL # Remove away the label
rpart.train.seed$PerformanceRating <- NULL

rpart.test.seed <- test
rpart.test.seed$ToPromote <- NULL # Remove away the label
rpart.test.seed$PerformanceRating <- NULL


# Run CV and check out results
rpart.cv.seed <- rpart.cv(seed, rpart.train.seed, data.label, ctrl)

# Save it to the table
seed.table$CV_Accuracy[i] <- max(rpart.cv.seed$results$Accuracy)

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.seed.preds <- predict(rpart.cv.seed$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.seed.preds, test$ToPromote)

# Save it to the table
seed.table$Accuracy[i] <- conf_matrix$overall["Accuracy"]
}

# -------------------------------
# 4.2 Various different seeds
# ------------------------------

# Check the highest accuracy based on different random starting points
# Highest CV_accuracy: 0.7915700 with seed of 20095
# Highest accuracy: 0.8084513 with seed of 20095
seed.table

# Save it to the folder
write.csv(seed.table, file = "seed_table.csv", row.names = FALSE)
