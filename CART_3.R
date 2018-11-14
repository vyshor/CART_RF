library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(doSNOW)
library(plyr)
library(ggplot2)
library(cluster)

# Keep a copy of pre-shuffled dataset
preshuffle_data <- edited_data
train <- NULL
test <- NULL

# -------------------------------
# 3.1 Training : Testing split
# ------------------------------

# Split into training and testing dataset
# With 8 : 2 ratio
# Standardise at 80% train, 20% test
training_percent = 0.8


# -------------------------------
# 3.2 Use of multi-threads
# ------------------------------
# Number of multithreads for parallelism to save time
number_of_threads = 4

# The seed used so that you can ensure reproduce the same result everytime
seed = 37596

# Explanation of multifolds can be found below at the setting multi-parts
# k stands for the number of sample split
# Times stands for the number of times you repeat this bagging process
multifolds_k = 3
multifolds_times = 10


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

prepare_train_test(training_percent, edited_data, seed)

# Label is the result/outcome that you are trying to predict
data.label <- train$PerformanceRating

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

# -------------------------------
# 3.3 Cross-validation
# ------------------------------

# Create multi-folds
set.seed(seed)
# So cross validation is splitting the whole records into k parts, then set one of the part
# as the testing part, and the rest of them are used for training
# Times is just the number of times it is repeated, because different starting points
prepare_ctrl <- function(data.label, multifolds_k, multifolds_times) {

  cv.folds <- createMultiFolds(data.label, k = multifolds_k, times = multifolds_times)

  ctrl <- trainControl(method = "repeatedcv", number = multifolds_k, repeats = multifolds_times,
                       index = cv.folds)
  return (ctrl)
}

ctrl <- prepare_ctrl(data.label, multifolds_k, multifolds_times)

# -------------------------------
# 3.4 Run #1
# ------------------------------

# --------------------------------------
# Run 1, with performance rating as the label, taking '3' as bad, and '4' as good
# and including PercentSalaryHike as one of the condition
# --------------------------------------

# Grab features
rpart.train.1 <- train
rpart.train.1$PerformanceRating <- NULL # Remove away the label

rpart.test.1 <- test
rpart.test.1$PerformanceRating <- NULL # Remove away the label

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(seed, rpart.train.1, data.label, ctrl)
rpart.1.cv.1

max(rpart.1.cv.1$results$Accuracy)
# Best: 0.867968
# (Based on CV accuracy)

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.1.preds <- predict(rpart.1.cv.1$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.1.preds, test$PerformanceRating)

conf_matrix$overall["Accuracy"]
# Best: 0.8637924
# Problem lies that they can actually get the higher accuracy by using PercentSalaryHike only
# other features are not used in predicting

# -------------------------------------
# Analysis on using performance rating as label
# -------------------------------------

# -------------------------------
# 3.5 Analysis of PerformanceRating as label
# ------------------------------

# Look at the frequency of the labels
count(preshuffle_data$PerformanceRating)

# Then look at the percentage of which how many of the labels are '3'
count(preshuffle_data$PerformanceRating)[2][1,]/preshuffle_data[,.N]
# 84.48% are '3'
# which means that we have a very skewed label
# which explains why the CART will just tend to assume all are '3'


# -------------------------------
# 3.6 Development of new label
# ------------------------------

# --------------------------------------
# Using a better label
# --------------------------------------

# There is another label we can use
# which is the PercentSalaryHike
# which presumably is the percentage amount of raise in salary
# probably due to promotion in position

summary(preshuffle_data$PercentSalaryHike)
# The min is 11, max is 25, median is 14
# The key thing here is that the minimum is not 0, which means that
# presumably everyone got promoted somehow or so,
# but "how much" they are promoted will then depends on the percent of increase in salary
count(preshuffle_data$PercentSalaryHike)
# The max frequency being 3359

# So then we try to graph out the distribution of it
hist(preshuffle_data$PercentSalaryHike, ylim=c(0,7000), breaks = seq(8, 28, by=2), xlab="Percent", main = "Distribution of PercentSalaryHike", labels=T, col ="light blue")

# So, in mind of trying to creating a balanced label set,
# it be better to set a criteria around the median to split
# the label into should_promote and those that shouldn't

# 14 in this case is ideal, and matches with the distribution
# As the graph reflects the larger proportion of the population (those below 14%) having
# mediocre growth in salary and then as percent goes above 14%, there is a sharp drop in numbers
# the frequency starts to decrease gradually
# which reflects strongly the distinction between the 'better' promotions due to performance
# and the 'worse' promotions which is purely due to seniority and normal career progression

# Create a copy of the preshuffle_data
# preshuffle_data.2 now contains the new label of ToPromote instead of PercentSalaryHike
preshuffle_data.2 <- preshuffle_data
# Set all to be NO first
preshuffle_data.2$ToPromote <- 'NO' 
# Then set for those above 14, ToPromote to be YES
preshuffle_data.2$ToPromote[which(preshuffle_data.2$PercentSalaryHike > 14)] <- 'YES'

# Change into categorical format
preshuffle_data.2$ToPromote <- factor(preshuffle_data.2$ToPromote, levels=c('YES', 'NO'), ordered=TRUE)

# Then we check the proportion again
count(preshuffle_data.2$ToPromote)

# And check the percentage of proportion
count(preshuffle_data.2$ToPromote)[2][1,]/preshuffle_data.2[,.N]
# 44.29% of whole dataset are to promote, which is kind of near 50%, but slightly more
# towards being able to not promote

# Then also remove percentSalaryHike in the preshuffle matrix
preshuffle_data.2$PercentSalaryHike <- NULL

# -------------------------------
# 3.7 Run #2
# ------------------------------

# --------------------------------------
# Run 2, with ToPromote as the label
# --------------------------------------

# Remember to set your working data, reset to original, to preshuffle period
edited_data <- preshuffle_data.2

prepare_train_test(training_percent, edited_data, seed)
data.label <- train$ToPromote

# Grab features
rpart.train.2 <- train
rpart.train.2$ToPromote <- NULL # Remove away the label

rpart.test.2 <- test
rpart.test.2$ToPromote <- NULL # Remove away the label

# Prepare cross validation settings
ctrl <- prepare_ctrl(data.label, multifolds_k, multifolds_times)

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(seed, rpart.train.2, data.label, ctrl)
rpart.2.cv.1

max(rpart.2.cv.1$results$Accuracy)
# Best: 0.8035499
# (Based on CV accuracy)

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)
# The tree is too big to be plotted

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.2.preds <- predict(rpart.2.cv.1$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.2.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Best: 0.8206778
# Although the accuracy is lower, but this label makes more sense
# A better starting benchmark

# -------------------------------
# 3.8 Run #3
# ------------------------------

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
rpart.train.3 <- train
rpart.train.3$ToPromote <- NULL # Remove away the label
rpart.train.3$PerformanceRating <- NULL

rpart.test.3 <- test
rpart.test.3$ToPromote <- NULL # Remove away the label
rpart.test.3$PerformanceRating <- NULL

# Prepare cross validation settings
ctrl <- prepare_ctrl(data.label, multifolds_k, multifolds_times)

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(seed, rpart.train.3, data.label, ctrl)
rpart.3.cv.1

max(rpart.3.cv.1$results$Accuracy)
# Best: 0.7752615
# (Based on CV accuracy)

# View the importance of variables
rpart.3.cv.1$finalModel$variable.importance

# Plot 
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)
# Too bad the graph is too big
rpart.plot(rpart.3.cv.1$finalModel, compress=TRUE)
# Another way of representation, but graph is too big

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test, type = "class")
conf_matrix <- confusionMatrix(rpart.3.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Best: 0.7696268
# Make sense that the accuracy is worse off, because it is highly correlated
# Therefore, makes it a strong determining factor
# Although we have a worse accuracy, but I believe it is better to measure without the use of PerformanceRating
# because based on practical aspect, you most likely won't have the information on PerformanceRating anyway

# ------------------------------------------------
# End of finding benchmark accuracy for our model
# Accuracy is 0.7696268 with CV-k = 3, and seed = 37596
# Optimal accuracy from multi-runs is 0.8084513 with CV-k = 7, and seed = 20095
# (Refer to CARTmultiCV.R and CARTmultiseed.R for optimized parameter picking)
# ------------------------------------------------

# -------------------------------
# 5.1 Features that provide most prediction capabilities
# ------------------------------

# Then now we have to look back at the trees constructed and find if we can find way to improve it
# We can then start by visualising the tree, and look at the trees/nodes which are the least effective in sorting data
# Then we have to rely on a measurement called cp. complexity parameter of each tree

# Text format
print(rpart.3.cv.1$finalModel)
# The main point is to see the first few nodes to get the essential criterias that split them

# So then you prune the tree, only showing the most relevant by varying the cp value
prp(prune(rpart.3.cv.1$finalModel, cp=0.01), type = 0, extra = 2, under = TRUE, nn=TRUE)
print(prune(rpart.3.cv.1$finalModel, cp=0.01))
# Done by taking a random cp value such that not much splitting nodes are seen

# ----------------------------------------------------
# Pruning by only keeping the top few splitting nodes
# --------------------------------------------------
# printcp(rpart.3.cv.1$finalModel, digits=3)

rpart.3.cv.1$finalModel$cptable

# Pick the 2nd complexity parameter, so that you can see which top spltting nodes
cp.opt <- rpart.3.cv.1$finalModel$cptable[2,"CP"]
cp.opt

# So then you prune the tree, only showing the most relevant by varying the cp value
rpart.3.cv.1.pruned <- prune(rpart.3.cv.1$finalModel, cp=cp.opt)
prp(rpart.3.cv.1.pruned, type = 0, extra = 2, under = TRUE, nn=TRUE)
print(rpart.3.cv.1.pruned)

rpart.3.cv.1.pruned$variable.importance

# Checking our accuracy for our test data set we obtained from splitting the dataset
rpart.3.pruned.preds <- predict(rpart.3.cv.1.pruned, test, type = "class")
conf_matrix <- confusionMatrix(rpart.3.pruned.preds, test$ToPromote)
conf_matrix

conf_matrix$overall["Accuracy"]
# Accuracy: 0.7505363

# --------------------------------------
# Additional way to visualise the data, but not as effective
# ---------------------------------------
# Different ways to perceive the data
# printcp(rpart.3.cv.1$finalModel) # Too many irrelevant data at the start
# plot(rpart.3.cv.1$finalModel)
# plot(rpart.3.cv.1, uniform=TRUE) # This graph does not serve much purpose
# post(rpart.3.cv.1$finalModel, file="tree1.ps") # The tree too complicated to see in photoshop
# prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE, snip=TRUE) # Snipping functionality
# ---------------------------------------

# ---------------------------------------
# Explanation of complexity parameter
# ---------------------------------------
# Complexity parameter is the parameter we control to determine the size of the tree
# So if cost of adding another tree is more than cp, the tree building does not continue
# So cp of each tree is like the splitting ability of the tree
# Hence, the higher the cp the better, showing better accuracy in splitting the data
# So that is why we prune by setting cp of certain value, and those that are less than the
# cp is removed, and those that are left are the better features in splitting the data
# Then we try to look if those features make sense, or how we improve on the features
# to increase the accuracy
# ---------------------------------------


