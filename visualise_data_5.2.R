library(data.table)
library(plyr)
library(ggplot2)

# --------------------------
# Requirement:
# Run clean_data.R 
# Run CART.R
# There is preshuffle_data.2 in your object variables
# There is rpart.3.cv.1 in your object variables
# --------------------------


# So from the tree, we observed there are some features
# that are good in splitting the data
# So then we try to visualise the data in a graph to see
# if we can gain any additional information from them
# or anything we can do with them
# and if those data make sense

# Features:
# JobRole
# EmployeeSource
# StockOptionLevel
# YearsWithCurrManager
# TotalWorkingYears
# WorkLifeBalance

# ----------------------------------------------
# 5.2 Visualise JobRole 
# -----------------------------------------------
ggplot(preshuffle_data.2, aes(x = JobRole, fill = ToPromote)) +
  geom_bar() +
  xlab("JobRole") +
  ylab("Total Count") +
  labs(fill = "YES") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Visualise in terms of table
visualise.JobRole <- preshuffle_data.2[, .N, by = JobRole][order(JobRole)]
visualise.JobRole$PercentYES <- preshuffle_data.2[ToPromote=="YES", .N, by = JobRole][order(JobRole)][,N]/preshuffle_data.2[, .N, by = JobRole][order(JobRole)][,N]
visualise.JobRole

# Not really any obvious trend
# Highest PercentYES: Manufacturing Director with 0.5193440
# Lowest PercentYES: Manager 0.3833124
# Possible reasons are that people who are managers might stuck in the position for a long time, because 
# a promotion would often need academic qualification which they lack
# Whereas as compared to Manufacturing Director, he/she might already have the qualifications
# and alot control over the company, hence it might be easier for them to promoted

# Another key thing to remember is that this ToPromote criteria is derived from PercentSalaryHike
# Hence, a possible explanation is the 80 - 20 parety, where the top 20% of the company
# holds the 80% of the aggregrate income of all the employees, hence rich get richer, and poor get poorer,
# hence leading to much higher increase in salary when they are at higher position

# Hence, we might want to recode the JobRoles into PositionalLevels with low, medium and high

# Then as we refer back to the pruned tree
prp(prune(rpart.3.cv.1$finalModel, cp=0.01), type = 0, extra = 2, under = TRUE, nn=TRUE)
print(prune(rpart.3.cv.1$finalModel, cp=0.01))
# From the node 1, where JobRole = HlR, HmR, MnD, RsS, SlR,
# if the job roles belong to the category, they would be pushed towardsd a YES for to Promoted
# Healthcare Representative,Human Resources,Manufacturing Director,Research Scientist,Sales Representative

# Whereas if they are Laboratory Technician,Manager,Research Director,Sales Executive,
# they would be pushed towards NO

# As you can see, they get these splits based on the PercentYES, those that are pushed towards YES,
# are those jobRoles with PercentYES above 0.45
# We might want to try consider recoding these JobRoles to reflect the reality, and check the accuracy again

# ----------------------------------------------
# 5.2.2 Visualise EmployeeSource 
# -----------------------------------------------
ggplot(preshuffle_data.2, aes(x = EmployeeSource, fill = ToPromote)) +
  geom_bar() +
  xlab("EmployeeSource") +
  ylab("Total Count") +
  labs(fill = "YES") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Visualise in terms of table
visualise.EmployeeSource <- preshuffle_data.2[, .N, by = EmployeeSource][order(EmployeeSource)]
visualise.EmployeeSource$PercentYES <- preshuffle_data.2[ToPromote=="YES", .N, by = EmployeeSource][order(EmployeeSource)][,N]/preshuffle_data.2[, .N, by = EmployeeSource][order(EmployeeSource)][,N]
visualise.EmployeeSource

# The PercentYes Is fair kept between 0.41 and 0.5,
# and it is consistent through different EmployeeSource except for referral
# with a low PercentYes of 0.1923, but might be due to the low number of counts of Referral EmployeeSource anyway

# Then as we refer back to the pruned tree
prp(prune(rpart.3.cv.1$finalModel, cp=0.01), type = 0, extra = 2, under = TRUE, nn=TRUE)
print(prune(rpart.3.cv.1$finalModel, cp=0.01))
# Employee Source seems to have very low accuracy in splitting the data

#  with logical sense that
# employeeSource should not have much relation on the performance of one in work, and the ability to be Promoted
# Might consider removing this feature itself

# ----------------------------------------------
# 5.2.3 Visualise StockOptionLevel 
# -----------------------------------------------

# Checking whether if StockOptionLevel is categorical or continuous
typeof(preshuffle_data.2$StockOptionLevel)
# In this case, continous
summary(preshuffle_data.2$StockOptionLevel)

# Visualise the data again
ggplot(preshuffle_data.2, aes(x = StockOptionLevel, fill = ToPromote)) +
  geom_bar() +
  xlab("StockOptionLevel") +
  ylab("Total Count") +
  labs(fill = "YES") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Visualise in terms of table
visualise.StockOptionLevel <- preshuffle_data.2[, .N, by = StockOptionLevel][order(StockOptionLevel)]
visualise.StockOptionLevel$PercentYES <- preshuffle_data.2[ToPromote=="YES", .N, by = StockOptionLevel][order(StockOptionLevel)][,N]/preshuffle_data.2[, .N, by = StockOptionLevel][order(StockOptionLevel)][,N]
visualise.StockOptionLevel

# First off, 
# the interpretation of StockOptionLevel is not provided on the website,
# but based on experience, it is more for tech companies and startups
# where employees get to choose if they want stocks are their pay, rather than just a paycheck
# so that employees would be more motivated to work for the company
# That is the whole point of the StockOptionLevel

# And I assume that the 'level' of stockOption is how much proportion of the income
# is replaced with StockOption instead, as such, 3 being the highest, and 0 being just pure paycheck salary
# However, basic interpretation of StockOptionLevel as a motivation factor contradicts the result shown
# because PercentYES peaks at StockOptionLevel being 1, and yet, dropped as it goes to 2 and 3...
# This might be a sign that the presence of StockOption itself is useful, because it motivates people,
# but the proportion of StockOption is not that important
# Hence, it might be better to recode it to binary, with or without stockOption

# Another idea might be that we can try to visualise the StockOptionLevel with the jobRole,
# as I believe that people who are of higher management role will tend to have StockOptionLevel
# at a higher level
# So lets try visualise it

# ----------------------------------------------
# 5.2.3.1 Visualise StockOptionLevel with JobRole
# -----------------------------------------------

ggplot(preshuffle_data.2, aes(x = StockOptionLevel, fill = ToPromote)) +
  geom_bar() +
  facet_wrap(~JobRole) + 
  ggtitle("JobRole") +
  xlab("StockOptionLevel") +
  ylab("Total Count") +
  labs(fill = "ToPromote")

# This is kind of interesting
# First off, based on the table previously, the number of people who opted for StockOptionLevel 0 and 1
# and way larger than those of 2 and 3,
# So definitely, natural to see most people regardless of JobRole opt for StockOptionLevel 0 and 1

# But in terms of proportion,
# it is obvious that laboratory technician prefers StockOptionLevel 0 and 1 as compared to 2 and 3
# Likewise for sales executive and research scientist,
# Whereas the rest of them, seems to follow a very decreasing slope
# The possible explanations might be that for laboratory technician,
# they might understand that their contribution towards the company
# might not be so relevant in affecting the company's performance
# in earning more profits and hence getting a higher evaluation
# likewise for research scientist, (because experimental findings are not guaranteed to be useful)
# As for sales executive, they might know that job switching is very common,
# hence one tend to not stay in the company for too long,
# thus they might opt for lower stockOptionLevel
# This might be confirmed or refuted by observing the AttritionRate for sales executive

preshuffle_data.2[JobRole == 'Sales Executive', .N, by=Attrition]
# This might not be very useful, as it does not compare with other JobRoles on the AttritionRate

visualise.AttritionByJobRole <- preshuffle_data.2[, .N, by = JobRole][order(JobRole)]
visualise.AttritionByJobRole$AttritionNum <- preshuffle_data.2[Attrition != 'Current employee', .N, by=JobRole][order(JobRole)][,N]
visualise.AttritionByJobRole$AttritionRate <- visualise.AttritionByJobRole[,AttritionNum]/visualise.AttritionByJobRole[,N]
visualise.AttritionByJobRole[order(-AttritionRate)]

# In this case, SalesExecutive is ranked 4th in AttritionRate at 0.16624 out of 9
# Perhaps my hypothesis on job switching for sales executive might not be accurate enough
# However, for laboratory technician, it might be that reason

# Yet, for sales representative with the highest attritionRate, the different between stockoptionlevels 0, 1 and 2,3
# are not that obvious

# Okay maybe lets try construct a table to see the percentage more accurately
visualise.StockOptionLevelByJobRole <- preshuffle_data.2[, .N, by = JobRole][order(JobRole)]
visualise.StockOptionLevelByJobRole[,'0'] <- ''
for (i in 0:3) {
visualise.StockOptionLevelByJobRole[,toString(i)] <- preshuffle_data.2[StockOptionLevel == i, .N, by = JobRole][order(JobRole)][,N]
}
for (i in 0:3) {
visualise.StockOptionLevelByJobRole[,paste('P',toString(i), sep='')] <- (preshuffle_data.2[StockOptionLevel == i, .N, by = JobRole][order(JobRole)][,N]/visualise.StockOptionLevelByJobRole[,N])
}
visualise.StockOptionLevelByJobRole[, LowStock := P0 + P1]
visualise.StockOptionLevelByJobRole[order(-LowStock)]

# Now this values might actually contradict the graph
# Because the graph is scaled down, so it is actually hard to see the percentage
# In this case, we know that
# Managers prefer lowStockOption (0 or 1) the most at 0.9159348
# Followed by Sales Represnetative at 0.8792049
# Then Human Resources at 0.8627685
# 4th is Laboratory Techincian at 0.8442187

# So our prior claim on Laboratory Technician, Sales Executive, and Research Scientist is invalid
# Only perhaps Laboratory Techinician make sense

# Then as I see those jobRoles with highest LowStock option, I found that it is kind of familiar
# to the AttritionRate by JobRole
visualise.AttritionByJobRole[order(-AttritionRate)]

# By comparing both tables,
# Attrition rate and LowStockOption is relatively highest for Human Resources, Sales Representative, and Laboratory Technician

# And something common about all three is that all 3 JobRoles have very low career progression in nature
# And is highly replaceable in the context of workplace
# Perhaps that explains the high attritionRate, and lowStockOption
# This might actually give us on some insights in picking what categories to re-encode JobRoles

# ----------------------------------------------
# 5.2.4 Visualise YearsWithCurrManager
# -----------------------------------------------

# First look at the minimum and maximum
# since it is a continuous variable type
summary(preshuffle_data.2$YearsWithCurrManager)

ggplot(preshuffle_data.2, aes(x = YearsWithCurrManager, fill = ToPromote)) +
  geom_histogram(binwidth = 1) +
  xlab("YearsWithCurrManager") +
  ylab("Total Count")

# The graph might indicates that as year goes, your promotion rate tends to drop
# which might be expected, because when an employee reach certain high position, they might be stuck in the position
# despite their seniority
# But lets take a closer look in numbers, so that we don't make same mistake

visualise.YearsWithCurrManager <- preshuffle_data.2[, .N, by = YearsWithCurrManager][order(YearsWithCurrManager)]
visualise.YearsWithCurrManager$PromoteNum <- preshuffle_data.2[ToPromote == "YES", .N, by = YearsWithCurrManager][order(YearsWithCurrManager)][,N]
# Interesting error
# Supplied 17 items to be assigned to 18 items of column 'PromoteNum' (recycled leaving remainder of 1 items).
# Might be causedd by the column for 17 years, where there might be no one chosen to be promoted
# Double check it
preshuffle_data.2[ToPromote == "YES", .N, by = YearsWithCurrManager][order(YearsWithCurrManager)]
# Oh, it is not for 17 years, but for 16 years
# Hence then we need to fix the visualise table
visualise.YearsWithCurrManager[YearsWithCurrManager == 16,]$PromoteNum <- 0
visualise.YearsWithCurrManager[YearsWithCurrManager == 17,]$PromoteNum <- 16
visualise.YearsWithCurrManager
# Double Check again
preshuffle_data.2[ToPromote == "YES", .N, by = YearsWithCurrManager][order(YearsWithCurrManager)]
# Yes, it matches

visualise.YearsWithCurrManager$PromotePercent <- visualise.YearsWithCurrManager$PromoteNum/visualise.YearsWithCurrManager[,N]
visualise.YearsWithCurrManager

visualise.YearsWithCurrManager[order(-PromotePercent)]
# In this case, there really isn't that much of a trend observable...
# because years with double digit and single digits are spread evenly throughout the range of PromotePercent
# Perhaps one less significant sign is that single digits tend to cluster towards higher PromotePercent
# while double digits tend to cluster towards lower PromotePercent

# If judging by the variable on its own, the best can be done is
# either recode it to senior and junior (because of the clustering)
# or just remove the variable
# or keep it intact (because nothing can be improved on)

# Another way we can do is perhaps try to visualise it together with
# another variable which is JobRole, or Department, or even TotalWorkingYears (to gauge seniority).
# or JobLevel (to gauge seniority vs position rank)

# ----------------------------------------------
# 5.2.4.1 Visualise YearsWithCurrManager with JobRole
# -----------------------------------------------

ggplot(preshuffle_data.2, aes(x = YearsWithCurrManager, fill = ToPromote)) +
  geom_bar() +
  facet_wrap(~JobRole) + 
  ggtitle("JobRole") +
  xlab("YearsWithCurrManager") +
  ylab("Total Count") +
  labs(fill = "ToPromote")

# Not much things can be inferred from the graph
# because the promotionRate is around 50% for most part at each year,
# the only sign is that Laboratry Technician, Manager, and Human Resources,
# there are very little employees with high number of YearWithCurrManager
# which is likely to explained by two of the following reasons
# Either the job progression is very slow for that jobRole
# or the attrition rate for the JobRole is high
# But as we have seen previously, JobRole for those 3 are very high, so it is most likely due to attrition
# Not much relevant information on improving is obtained here

# ----------------------------------------------
# 5.2.4.2 Visualise YearsWithCurrManager with Department
# -----------------------------------------------

ggplot(preshuffle_data.2, aes(x = YearsWithCurrManager, fill = ToPromote)) +
  geom_bar() +
  facet_wrap(~Department) + 
  ggtitle("Department") +
  xlab("YearsWithCurrManager") +
  ylab("Total Count") +
  labs(fill = "ToPromote")

# Likewise for this, the distribution of ToPromote is quite even at every YearsWithCurrManager
# Perhaps the only sign is that there is very little employees in HumanResources Department
# above 10 years, but this is likely due to the low number in HumanResources department as compared to the whole dataset
# Not much relevant information on improving is obtained here

# ----------------------------------------------
# 5.2.4.3 Analysis YearsWithCurrManager with TotalWorkingYears
# -----------------------------------------------

cor(preshuffle_data.2$YearsWithCurrManager, preshuffle_data.2$TotalWorkingYears)
# 0.4584932
# This is interesting, you would expect that people with high YearsWithCurrManager
# would have worked a long time, to have high TotalWorkingYears
# Yet, the correlation is less than 0.5

# It would actually means that seniority (based on TotalWorkingYears) should not be judged by the number
# of yearsWithCurrentManager

# ----------------------------------------------
# 5.2.4.4 Visualise YearsWithCurrManager with JobLevel
# -----------------------------------------------

ggplot(preshuffle_data.2, aes(x = YearsWithCurrManager, fill = ToPromote)) +
  geom_bar() +
  facet_wrap(~JobLevel) + 
  ggtitle("JobLevel") +
  xlab("YearsWithCurrManager") +
  ylab("Total Count") +
  labs(fill = "ToPromote")

# I assume here that JobLevel means the positionRank of the employee
# In this case here, the pattern is kind of similar across all 5 job levels
# Hence, not much information can be obtained here in improving YearsWithCurrManager

# ----------------------------------------------
# 5.2.5 Visualise TotalWorkingYears 
# -----------------------------------------------

# First look at the minimum and maximum
# since it is a continuous variable type
summary(preshuffle_data.2$TotalWorkingYears)

ggplot(preshuffle_data.2, aes(x = TotalWorkingYears, fill = ToPromote)) +
  geom_histogram(binwidth = 4) +
  xlab("TotalWorkingYears") +
  ylab("Total Count")

# The distribution of ToPromote looks kind of similar at each NumOfWorkingYear
# Lets look at table
visualise.TotalWorkingYears <- preshuffle_data.2[, .N, by = TotalWorkingYears][order(TotalWorkingYears)]
visualise.TotalWorkingYears$PromoteNum <- preshuffle_data.2[ToPromote == "YES", .N, by = TotalWorkingYears][order(TotalWorkingYears)][,N]
# Same warning message
# Supplied 38 items to be assigned to 40 items of column 'PromoteNum' (recycled leaving remainder of 2 items).
# Double check it
preshuffle_data.2[ToPromote == "YES", .N, by = TotalWorkingYears][order(TotalWorkingYears)]
visualise.TotalWorkingYears
# Oh, there is no one promoted at 38, and 40 years
visualise.TotalWorkingYears[TotalWorkingYears == 38,]$PromoteNum <- 0
visualise.TotalWorkingYears[TotalWorkingYears == 40,]$PromoteNum <- 0
visualise.TotalWorkingYears
# Double Check again
preshuffle_data.2[ToPromote == "YES", .N, by = YearsWithCurrManager][order(YearsWithCurrManager)]
# Yes, it matches

visualise.TotalWorkingYears$PromotePercent <- visualise.TotalWorkingYears$PromoteNum/visualise.TotalWorkingYears[,N]
visualise.TotalWorkingYears

visualise.TotalWorkingYears[order(-PromotePercent)]

# There might be signs that for people who are working longer, is more likely to be promoted
# Because you can tend to see double digits cluster at the top of PromotePercent
# But if observed careful, there might be no trend or sufficient confirmation that
# higher seniority means higher PromotionChances

# So PromotionChances might actually be indepedent of seniority
# Sowe might actually want to consider remove the TotalWorkingYears
# or leave it there

# ----------------------------------------------
# 5.2.6 Visualise WorkLifeBalance 
# -----------------------------------------------

summary(preshuffle_data.2$WorkLifeBalance)
# categorical
ggplot(preshuffle_data.2, aes(x = WorkLifeBalance, fill = ToPromote)) +
  geom_bar() +
  xlab("WorkLifeBalance") +
  ylab("Total Count") +
  labs(fill = "YES")

# From the grpah, the percentage looks kind of around 50% at each level
# Then I checked back to the prune tree
prp(prune(rpart.3.cv.1$finalModel, cp=0.01), type = 0, extra = 2, under = TRUE, nn=TRUE)
print(prune(rpart.3.cv.1$finalModel, cp=0.01))

# In this case, WorkLifeBalance is splitted whether if WorkLifeBalance = 1,4   or 2,3
# Which does not make sense

# Lets try visualise in table
visualise.WorkLifeBalance <- preshuffle_data.2[, .N, by = WorkLifeBalance][order(WorkLifeBalance)]
visualise.WorkLifeBalance$PercentYES <- preshuffle_data.2[ToPromote=="YES", .N, by = WorkLifeBalance][order(WorkLifeBalance)][,N]/preshuffle_data.2[, .N, by = WorkLifeBalance][order(WorkLifeBalance)][,N]
visualise.WorkLifeBalance

# Can see that the tree is built based on the highest PercentYES which is when WorkLifeBalance 1,3
# However this not make sense
# We might want to try visualise this feature with JobRole, because having workLifeBalance might relate to JobRole

# ----------------------------------------------
# 5.2.6.1 Visualise WorkLifeBalance with JobRole
# -----------------------------------------------

ggplot(preshuffle_data.2, aes(x = WorkLifeBalance, fill = ToPromote)) +
  geom_bar() +
  facet_wrap(~JobRole) + 
  ggtitle("JobRole") +
  xlab("WorkLifeBalance") +
  ylab("Total Count") +
  labs(fill = "ToPromote")

# The distribution of ToPromote seems same across JobRoles,
# and distribution across WorkLifeBalance is same for differnt JobRoles
# in this case, WorkLifeBalance might be unrelated with JobRoles

# Perhap WorkLifeBalance should be removed on the whole, as it does not make sense
