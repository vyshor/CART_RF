library(data.table)

# Load raw data
raw_data <- fread("IBM HR Data.csv", header = TRUE)

# Have a look at what columns
head(raw_data)

# ---------------------------------------------------------------------------
# Start of data cleaning
# ---------------------------------------------------------------------------

# Create a copy of the raw_data to process the data, also to see the difference after processing
edited_data <- raw_data

# -------------------------------
# 2.1 Datatypes of imported data
# ------------------------------

# Look at the R datatypes of each column
str(edited_data)

# Change those strings into factor type
# Factors are like creating categorisation boxes to put in for the data

# Example to see how it is done
str(raw_data$Attrition)
edited_data$Attrition <- as.factor(edited_data$Attrition)
str(edited_data$Attrition)
levels(edited_data$Attrition)

# Converting factors for other variables that are categorical
# How to know which variables are categorical depends on the source of the data, and what values are there
# Categorical depends on the interpretation of the data

edited_data$Attrition <- as.factor(edited_data$Attrition)
edited_data$BusinessTravel <- as.factor(edited_data$BusinessTravel)
edited_data$Department <- as.factor(edited_data$Department)
edited_data$Education <- as.factor(edited_data$Education)
edited_data$EducationField <- as.factor(edited_data$EducationField)
edited_data$EnvironmentSatisfaction <- as.factor(edited_data$EnvironmentSatisfaction)
edited_data$Gender <- as.factor(edited_data$Gender)
edited_data$JobInvolvement <- as.factor(edited_data$JobInvolvement)
edited_data$JobLevel <- as.factor(edited_data$JobLevel)
edited_data$JobRole <- as.factor(edited_data$JobRole)
edited_data$JobSatisfaction <- as.factor(edited_data$JobSatisfaction)
edited_data$MaritalStatus <- as.factor(edited_data$MaritalStatus)
edited_data$Over18 <- as.factor(edited_data$Over18)
edited_data$OverTime <- as.factor(edited_data$OverTime)
edited_data$PerformanceRating <- as.factor(edited_data$PerformanceRating)
edited_data$RelationshipSatisfaction <- as.factor(edited_data$RelationshipSatisfaction)
edited_data$StockOptionLevel <- as.factor(edited_data$StockOptionLevel)
edited_data$WorkLifeBalance <- as.factor(edited_data$WorkLifeBalance)
edited_data$EmployeeSource <- as.factor(edited_data$'Employee Source')
edited_data$'Employee Source' <- NULL # Just renaming to exclude and spaces in heading

# Check at the data types again
str(edited_data)

# Then going through each variable and see it is relevant in predicting performance
# Application ID definitely is not relevant in this case
edited_data$`Application ID` <- NULL

# Then we going to take a look at the datatypes for variables that are not factor
# and see the right datatypes are chosen
# For example, DistanceFromHome should be integer rather than characters
edited_data$DistanceFromHome <- as.integer(edited_data$DistanceFromHome)

# Do not worry about any NA values, because going to correct it later

# Checking if the conversion is did correctly
str(edited_data$DistanceFromHome)

# For rest of the variables
edited_data$EmployeeCount <- as.integer(edited_data$EmployeeCount)
edited_data$EmployeeNumber <- as.integer(edited_data$EmployeeNumber)
edited_data$HourlyRate <- as.integer(edited_data$HourlyRate)
edited_data$MonthlyIncome <- as.integer(edited_data$MonthlyIncome)
edited_data$PercentSalaryHike <- as.integer(edited_data$PercentSalaryHike)

# Check the variables types again
str(edited_data)

# And there is another interesting variable
summary(edited_data$EmployeeCount)
# Look at EmployeeCount, all the values is either missing or 1
# which means that this variable serve no purpose in predicting too, hence can be removed
edited_data$EmployeeCount <- NULL

# Likewise for Over18
summary(edited_data$Over18)
# All the values is either missing or Y
# which means that this variable serve no purpose in predicting too, hence can be removed
edited_data$Over18 <- NULL

# Likewise for standard hours
summary(edited_data$StandardHours)
# Find the number of entries that are less than 80
edited_data[StandardHours<80,.N]
# Only 2 entries, lets take a look at the two entries
edited_data[StandardHours<80,]
# Standard hours are 4, and 3...
# But considering these two are outliners, not sure because of error data entry
# or because of any reasons, but it is neligible considering to the bigger number
# of dataset, so then we can safely assume that the standardhours is 80 for
# the whole dataset, and thus serve no predicting value.
edited_data$StandardHours <- NULL

# -------------------------------
# 2.2 Missing data
# ------------------------------

# Now we look at the NA values for each variable and see if you can correct them or not
summary(edited_data)

# But first, we compare the number of rows 
# if we just omit all the entries with any missing values and the original number of rows
nrow(raw_data) - nrow(na.omit(edited_data))
(nrow(raw_data) - nrow(na.omit(edited_data))) / nrow(raw_data)
# 162 which is 0.69% of all the data we have
# So technically we can just omit all the entries, since it is just a really small proportion
# However, na.omit only remove values of data type numerical, and missing
# Hence it does not remove values with data type of factor, hence we have to look at variables that is converted
edited_data <- na.omit(edited_data)
summary(edited_data)

summary(edited_data$Attrition)
# For Attrition, there are 10 values that are missing
# Lets see these values
edited_data[which(Attrition == '')]

# Because the number of entries with missing values is very insignificant
# And there is no easy way to fill it with data with certainty because it is categorical, rather than continuous
# A quick solution is to remove these values
# However, if this variable is not used in the model, then the entries should not be removed, because
# these entries are still valid

edited_data <- edited_data[which(Attrition != '')]

# Checked if those data are removed
summary(edited_data$Attrition)

# Now do for all other categorical data
summary(edited_data)

# Need to becareful of those variables that are factor, but with 7 or more levels
# because only max 7 levels can be seen in summary of whole table
# Hence, there might be other invalid entries that cannot be seen in summary(edited_data), and will be categoried
# in (Others)

# -------------------------------
# 2.3 Datatypes of imported data
# ------------------------------

# For instance
summary(edited_data$EducationField)
summary(edited_data$EmployeeSource)


edited_data <- edited_data[which(BusinessTravel != '')]
edited_data <- edited_data[which(Department != '' & Department != '1296')]
edited_data <- edited_data[which(EducationField != '' & EducationField != 'Test')]
edited_data <- edited_data[which(Gender != '')]
edited_data <- edited_data[which(JobRole != '')]
edited_data <- edited_data[which(JobSatisfaction != '')]
edited_data <- edited_data[which(MaritalStatus != '')]
edited_data <- edited_data[which(OverTime != '')]
edited_data <- edited_data[which(EmployeeSource != '' & EmployeeSource != 'Test')]

# Check if everything is done properly
summary(edited_data)

# Now in terms of formatting, need to remove all the factor boxes with 0 entries
edited_data <- droplevels.data.frame(edited_data)

# Check if everything is done properly
summary(edited_data)

# -------------------------------
# 2.4 Summary of data cleaning
# ------------------------------

# Now check the difference in number of entries from the original matrix and the cleaned one
nrow(raw_data) - nrow(na.omit(edited_data))
(nrow(raw_data) - nrow(na.omit(edited_data))) / nrow(raw_data)

# 223 entries, with 0.95%, still quite insignificant

# ----------------------------------------------
# End of data cleaning
# ----------------------------------------------

# Save a copy of clean_data to csv
write.csv(edited_data, file = "clean_data.csv", row.names = FALSE)

