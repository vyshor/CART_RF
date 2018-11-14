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

# Setting the default parameters
training_percent = 0.8
number_of_threads = 4
multifolds_k = 7
multifolds_times = 10
seed = 20095 # Note this is not 37596 anymore, different from the one used in CART.R
train <- NULL
test <- NULL

seed.table <- data.table(c(37596, 12345, 95824, 56932, 10514, 20095, 23420, 20515, 31925, 29392))
names(seed.table)[1] <- 'seed'
seed.table$CV_Accuracy <- 0
seed.table$Accuracy <- 0

# Read this after visualise_data.R

# Conclusion from visualise_data
#
# What things we can try in attempt to increase accuracy:
# 1. Recode the JobRoles into PositionalLevels with low, medium and high
# 2. Remove EmployeeSource
# 3. Recode StockOptionLevel to StockOption with low (0,1) and high (2,3)
# 4. Recode StockOptionLevel to StockOption with none (0), low(1) and high (2,3)
# 5. Recode StockOptionLevel to StockOption with no (0) and yes(1,2,3)
# 6. Recode Attrition to yes (voluntary, termination) and no (current employee)
# 7. Remove YearsWithCurrManager
# 8. Remove TotalWorkingYears
# 9. Remove WorkLifeBalance
#
# Method
# We going to do each item independently first
# And check if the accuracy increased
# We would be using the CV k=7
# Then at the end, we consider mixing a few if those increase the accuracy

# Setting up the CART functions (without all those help comments)
prepare_train_test <- function(training_percent, edited_data, seed) {
  
  training_count = round(training_percent * nrow(edited_data))
  testing_count = nrow(edited_data) - training_count
  set.seed(seed)
  edited_data <- edited_data[sample(.N, .N)]
  train <<- edited_data[1:training_count]
  test <<- edited_data[(training_count+1):(training_count+testing_count)]
}

rpart.cv <- function(seed, training, labels, ctrl) {

  cl <- parallel::makeCluster(number_of_threads)
  registerDoSNOW(cl)
  set.seed(seed)
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  stopCluster(cl)
  
  return (rpart.cv)
}

prepare_ctrl <- function(data.label, multifolds_k, multifolds_times) {
  cv.folds <- createMultiFolds(data.label, k = multifolds_k, times = multifolds_times)
  ctrl <- trainControl(method = "repeatedcv", number = multifolds_k, repeats = multifolds_times,
                       index = cv.folds)
  return (ctrl)
}

rpart_run <- function(preshuffled_data.3, multifolds_k, multifolds_times){
  edited_data <- preshuffle_data.3
  
  prepare_train_test(training_percent, edited_data, seed)
  data.label <- train$ToPromote
  
  rpart.train.refined <- train
  rpart.train.refined$ToPromote <- NULL # Remove away the label
  rpart.train.refined$PerformanceRating <- NULL
  
  rpart.test.refined <- test
  rpart.test.refined$ToPromote <- NULL # Remove away the label
  rpart.test.refined$PerformanceRating <- NULL
  
  ctrl <- prepare_ctrl(data.label, multifolds_k, multifolds_times)
  
  rpart.refined.cv <- rpart.cv(seed, rpart.train.refined, data.label, ctrl)
  return (rpart.refined.cv)
}

rpart_run_seeds <- function(preshuffled_data.3, multifolds_k, multifolds_times, seed.table) {
  for (i in 1:10) {
    
    seed <- seed.table$seed[i]
    edited_data <- preshuffle_data.3

    prepare_train_test(training_percent, edited_data, seed)
    
    data.label <- train$ToPromote
    
    set.seed(seed)
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

return (seed.table)
}

# --------------------
# 5.3.1
# ---------------------------------------------------------
# 1. Recode the JobRoles into PositionalLevels with low, medium and high
# ---------------------------------------------------------
#
# Based on findings in Visualise StockOptionLevel with JobRole
# in visualise_data.R
# Attrition rate and LowStockOption is relatively highest for Human Resources, Sales Representative, and Laboratory Technician
# So we might want to recode these 3 to lowest positions based on their nature and attrition rate
# And then we choose those jobRoles that require one of the highest job position to be high
# In this case, we assume it to be Manufacturing Director, Research Director, and Healthcare Representative
# The rest then fall under medium
#
# High: Manufacturing Director, Research Director, Healthcare Representative
# Medium: Sales Executive, Research Scientist, Manager
# Low: Human Resources, Sales Representative, Laboratory Technician
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


# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7870013
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.780099
# Worse than initial benchmark accuracy of 0.8084513


# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.8212960 with seed of 95824 (accuracy: 0.8082368)
# Refined accuracy: 0.8217503 with seed of 12345  (cv_accuracy: 0.8024507)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Seems like the recoding of JobRole indeed increases the accuracy
write.csv(seed.table, file = "refine_data_1.csv", row.names = FALSE)

# --------------------
# 5.3.2
# ---------------------------------------------------------
# 2. Remove EmployeeSource
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Clear EmployeeSource
preshuffle_data.3$EmployeeSource <- NULL

# Check its not there
summary(preshuffle_data.3)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.752013
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7456027
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7788597 with seed of 20515
# Refined accuracy: 0.7923638 with seed of 20515
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Seems like the removal of EmployeeSource is not necessary
write.csv(seed.table, file = "refine_data_2.csv", row.names = FALSE)

# --------------------
# 5.3.3
# ---------------------------------------------------------
# 3. Recode StockOptionLevel to StockOption with low (0,1) and high (2,3)
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Convert to characters
preshuffle_data.3$StockOptionLevel <- as.character(preshuffle_data.3$StockOptionLevel)

# Recode them
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '0'| preshuffle_data.3$StockOptionLevel == '1')] <- 'Low' 
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '2'| preshuffle_data.3$StockOptionLevel == '3')] <- 'High' 

# Then factor the feature again
preshuffle_data.3$StockOptionLevel <- as.factor(preshuffle_data.3$StockOptionLevel)

# Check if it is factored properly
summary(preshuffle_data.3$StockOptionLevel)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7905941
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7893608
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7933185 with seed of 29392
# Refined accuracy: 0.7893608 with seed of 29392
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Seems like the recoding of StockOptionLevel into low and high, does not increases the accuracy
write.csv(seed.table, file = "refine_data_3.csv", row.names = FALSE)

# --------------------
# 5.3.4
# ---------------------------------------------------------
# 4. Recode StockOptionLevel to StockOption with none (0), low(1) and high (2,3)
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

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

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7925411
# (Based on CV accuracy)
# Better than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7674818
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7963372 with seed of 20095 (accuracy: 0.7919348)
# Refined accuracy: 0.8030888 with seed of 20515  (cv_accuracy: 0.790696)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Recoding StockOptionLevel to 3 levels, only leads to a slight increase in CV accuracy
# and a slight fall in accuracy
write.csv(seed.table, file = "refine_data_4.csv", row.names = FALSE)

# --------------------
# 5.3.5
# ---------------------------------------------------------
# 5. Recode StockOptionLevel to StockOption with no (0) and yes(1,2,3)
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Convert to characters
preshuffle_data.3$StockOptionLevel <- as.character(preshuffle_data.3$StockOptionLevel)

# Recode them
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '0')] <- 'No' 
preshuffle_data.3$StockOptionLevel[which(preshuffle_data.3$StockOptionLevel == '1' | preshuffle_data.3$StockOptionLevel == '2'| preshuffle_data.3$StockOptionLevel == '3')] <- 'Yes' 

# Then factor the feature again
preshuffle_data.3$StockOptionLevel <- as.factor(preshuffle_data.3$StockOptionLevel)

# Check if it is factored properly
summary(preshuffle_data.3$StockOptionLevel)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7907179
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7833548
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7934684 with seed of 29392 (accuracy: 0.7833548)
# Refined accuracy: 0.7837838 with seed of 12345  (cv_accuracy: 0.7553923)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Recoding StockOptionLevel to 2 levels (yes/no), only leads to a slight increase in CV accuracy
# and a significant fall in accuracy
write.csv(seed.table, file = "refine_data_5.csv", row.names = FALSE)

# --------------------
# 5.3.6
# ---------------------------------------------------------
# 6. Recode Attrition to yes (voluntary, termination) and no (current employee)
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# See the datatype for Attrition
summary(preshuffle_data.3$Attrition)

# Convert to characters
preshuffle_data.3$Attrition <- as.character(preshuffle_data.3$Attrition)

# Recode them
preshuffle_data.3$Attrition[which(preshuffle_data.3$Attrition == 'Current employee')] <- 'No' 
preshuffle_data.3$Attrition[which(preshuffle_data.3$Attrition == 'Voluntary Resignation' | preshuffle_data.3$Attrition == 'Termination')] <- 'Yes' 

# Then factor the feature again
preshuffle_data.3$Attrition <- as.factor(preshuffle_data.3$Attrition)

# Check if it is factored properly
summary(preshuffle_data.3$Attrition)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7809464
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7518233
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7944338 with seed of 20515 (accuracy: 0.7951523)
# Refined accuracy: 0.8084513 with seed of 20095  (cv_accuracy: 0.79157)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Same accuracy as the original model, which means that the CART that we built initial already
# considers Voluntary Resignation and Termination as same
# Hence, my grouping of it together only increase CV_accuracy, but not accuracy of model
write.csv(seed.table, file = "refine_data_6.csv", row.names = FALSE)


# --------------------
# 5.3.7
# ---------------------------------------------------------
# 7. Remove YearsWithCurrManager
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Clear EmployeeSource
preshuffle_data.3$YearsWithCurrManager <- NULL

# Check its not there
summary(preshuffle_data.3)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7409295
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7483912
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7873808 with seed of 37596 (accuracy: 0.7659803)
# Refined accuracy: 0.7962248 with seed of 10514  (cv_accuracy: 0.7698227)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Definitely removal of YearsWithCurrManager is not needed
write.csv(seed.table, file = "refine_data_7.csv", row.names = FALSE)

# --------------------
# 5.3.8
# ---------------------------------------------------------
# 8. Remove TotalWorkingYears
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Clear EmployeeSource
preshuffle_data.3$TotalWorkingYears <- NULL

# Check its not there
summary(preshuffle_data.3)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7647296
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7355212
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7875478 with seed of 20515 (accuracy: 0.7820678)
# Refined accuracy: 0.7947233 with seed of 20095  (cv_accuracy: 0.7837508)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# TotalWorkingYears is relevant in predicting, and shouldn't be removed
write.csv(seed.table, file = "refine_data_8.csv", row.names = FALSE)

# --------------------
# 5.3.9
# ---------------------------------------------------------
# 9. Remove WorkLifeBalance
# ---------------------------------------------------------

# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Clear EmployeeSource
preshuffle_data.3$TotalWorkingYears <- NULL

# Check its not there
summary(preshuffle_data.3)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7647296
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7355212
# Worse than initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.7875478 with seed of 20515 (accuracy: 0.7820678)
# Refined accuracy: 0.7947233 with seed of 20095  (cv_accuracy: 0.7837508)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Comments:
# Seems like the WorkLifeBalance is relevant in predicting ToPromote
write.csv(seed.table, file = "refine_data_9.csv", row.names = FALSE)

# -------------------------------------------------------------------
# Conclusion
# -------------------------------------------------------------------
# Based on what we did
# 1. Recode the JobRoles into PositionalLevels with low, medium and high
# 2. Remove EmployeeSource
# 3. Recode StockOptionLevel to StockOption with low (0,1) and high (2,3)
# 4. Recode StockOptionLevel to StockOption with none (0), low(1) and high (2,3)
# 5. Recode StockOptionLevel to StockOption with no (0) and yes(1,2,3)
# 6. Recode Attrition to yes (voluntary, termination) and no (current employee)
# 7. Remove YearsWithCurrManager
# 8. Remove TotalWorkingYears
# 9. Remove WorkLifeBalance
#
# 1 increased in the accuracy the most
# 4 increase the CV_accuracy the most among other ways of recoding StockOptionLevel, but sacrifice abit of actual accuracy
# 6 increase the CV_accuracy, and leads to the same actual accuracy
# The rest (especially those removal ones) lead to a fall in accuracy, mostly by 2-4%
#
# --------------------
# 5.4
# -------------------------------------------------------------------
# Combination of 1, 4, 6
# -------------------------------------------------------------------

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


# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7792625
# (Based on CV accuracy)
# Worse than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7870013
# Worse than initial benchmark accuracy of 0.8084513

# The importance of each variable for the tree
rpart.refined.cv$finalModel$variable.importance

# The whole tree
prp(rpart.refined.cv$finalModel, type = 0, extra = 2, under = TRUE, nn=TRUE)

# printcp(rpart.refined.cv$finalModel, digits=3)

rpart.refined.cv$finalModel$cptable

# Pick the 2nd complexity parameter, so that you can see which top spltting nodes
cp.opt <- rpart.refined.cv$finalModel$cptable[2,"CP"]
cp.opt

rpart.refined.cv.pruned <- prune(rpart.refined.cv$finalModel, cp=cp.opt)

# So then you prune the tree, only showing the most relevant by varying the cp value
prp(rpart.refined.cv.pruned, type = 0, extra = 2, under = TRUE, nn=TRUE)
print(rpart.refined.cv.pruned)

# Importance of variables for the pruned tree
rpart.refined.cv.pruned$variable.importance

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.pruned.preds <- predict(rpart.refined.cv.pruned, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.pruned.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.6447876
# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.8206418 with seed of 95824 (accuracy: 0.8056628)
# Refined accuracy: 0.8217503 with seed of 12345  (cv_accuracy: 0.8019842)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# Accuracies based on recoding JobRole only
# CV_accuracy: 0.8212960 with seed of 95824 (accuracy: 0.8082368)
# Accuracy: 0.8217503 with seed of 12345  (cv_accuracy: 0.8024507)
# --------------------------------------------------
# Comments:
# It appeared that just recoding JobRole only is a good choice,
# But in fact, there is actually no difference between the actual accuracy between
# the combined feature model and the isolated JobRole feature model, other than a slight decrease in CV_accuracy

write.csv(seed.table, file = "refine_data_combined.csv", row.names = FALSE)

# --------------------------------------------------
#
# On top of the combination of 1, 4, 6
# I also want to test out if it would be better to recode JobRole and remove original JobRole
# Or should I leave the original JobRole in there too, because how we recoded JobRole was analysis of multiple factors
# at once, and not solely on JobRole
# --------------------
# 5.4.1
# ---------------------------------------------------
# Recoding JobRole, and keeping original JobRole
# ---------------------------------------------------
#
# All work would be done in preshuffle_data.3
preshuffle_data.3 <- preshuffle_data.2

# Convert back to characters
preshuffle_data.3$JobRole2 <- as.character(preshuffle_data.3$JobRole)

# Recode them
preshuffle_data.3$JobRole2[which(preshuffle_data.3$JobRole2 == 'Manufacturing Director'| preshuffle_data.3$JobRole2 == 'Research Director' | preshuffle_data.3$JobRole2 == 'Healthcare Representative' )] <- 'High' 
preshuffle_data.3$JobRole2[which(preshuffle_data.3$JobRole2 == 'Sales Executive'| preshuffle_data.3$JobRole2 == 'Research Scientist' | preshuffle_data.3$JobRole2 == 'Manager' )] <- 'Medium' 
preshuffle_data.3$JobRole2[which(preshuffle_data.3$JobRole2 == 'Human Resources'| preshuffle_data.3$JobRole2 == 'Sales Representative' | preshuffle_data.3$JobRole2 == 'Laboratory Technician' )] <- 'Low' 

# Then factor the feature again
preshuffle_data.3$JobRole2 <- as.factor(preshuffle_data.3$JobRole2)

# Check if it is factored properly
summary(preshuffle_data.3$JobRole2)
# And our original JobRole
summary(preshuffle_data.3$JobRole)

# -----------------
# Run of CART
# -----------------

rpart.refined.cv <- rpart_run(preshuffled_data.3, multifolds_k, multifolds_times)
max(rpart.refined.cv$results$Accuracy)
# Accuracy: 0.7932485
# (Based on CV accuracy)
# Better than initial benchmark CV_accuracy of 0.7915700

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.refined.preds <- predict(rpart.refined.cv$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.refined.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.8084513
# Same as initial benchmark accuracy of 0.8084513

# --------------------------
# Run with different seeds
# --------------------------

seed.table <- rpart_run_seeds(preshuffled_data.3, multifolds_k, multifolds_times, seed.table)

seed.table
seed.table[CV_Accuracy == max(CV_Accuracy)]
seed.table[Accuracy == max(Accuracy)]
# Check the highest accuracy based on different random starting points
# Refined CV_accuracy: 0.794482 with seed of 20515 (accuracy: 0.7951523)
# Refined accuracy: 0.8084513 with seed of 20095  (cv_accuracy: 0.79157)
# --------------------------------------------------
# Benchmark CV_accuracy: 0.7915700 with seed of 20095
# Benchmark accuracy: 0.8084513 with seed of 20095
# --------------------------------------------------
# (If Recode JobRole only)
# CV_accuracy: 0.8212960 with seed of 95824 (accuracy: 0.8082368)
# Accuracy: 0.8217503 with seed of 12345  (cv_accuracy: 0.8024507)
# --------------------------------------------------
# Comments:
# In this case, recoding JobRole and removing the original JobRole is the right move, as it
# recoded the data into better category and the model is able to predict better
write.csv(seed.table, file = "refine_data_combined_2.csv", row.names = FALSE)

