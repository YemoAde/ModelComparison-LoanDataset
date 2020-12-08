library(dplyr)
library(ggplot2)
library(visdat)
library(reshape2)
library(ggcorrplot)


credit <- read.csv("Credit_Bureau.csv", header = T)
demo <- read.csv("demogs.csv", header = T)
#=========================#
# Remove Duplicates       #
#=========================#

demo <- demo %>% group_by(Application.ID) %>% filter(n() == 1)
credit <- credit %>% group_by(Application.ID) %>% filter(n() == 1)

# Merge Data
merge_data <- merge(demo, credit, by=c("Application.ID", "Performance.Tag"))

## Search Missing Data
merge_data <- merge_data %>% filter(!is.na(Performance.Tag))
merge_data <- na.omit(merge_data)
table(merge_data$Performance.Tag)
merge_data %>% group_by(Performance.Tag) %>% summarise(n()/nrow(merge_data))

#=========================#
# Age                     #
#=========================#

merge_data$Age %>% boxplot()
merge_data[(which(merge_data$Age < 18)), ]$Age <- 18


# Create Bins
age_bin <- function(age=3){
  if(age > 17 && age < 21)
    return ("18-20")
  else if(age > 20 && age < 26)
    return ("21-25")
  else if(age > 25 && age < 31)
    return ("26-30")
  else if(age > 30 && age < 36)
    return ("31-35")
  else if(age > 35 && age < 41)
    return ("36-40")
  else if(age > 40 && age < 46)
    return ("41-45")
  else if(age > 45 && age < 51)
    return ("46-50")
  else if(age > 50 && age < 56)
    return ("51-55")
  else if(age > 55 && age < 61)
    return ("56-60")
  else if(age > 60 && age < 66)
    return ("61-65")
}
merge_data$Age_Bin <- merge_data$Age %>% sapply(age_bin) %>% as.factor()
summary(merge_data$Age_Bin)
attributes(merge_data$Age_Bin)


#=========================#
# Martial Status          #
#=========================#

#Check for NA variable
merge_data$Marital.Status..at.the.time.of.application. %>% is.na() %>% sum()

#Check and Remove for Empty String
merge_data <- merge_data %>% filter(Marital.Status..at.the.time.of.application. != "")
merge_data$Marital.Status..at.the.time.of.application. <- as.factor(merge_data$Marital.Status..at.the.time.of.application.)
attributes(merge_data$Marital.Status..at.the.time.of.application.)

#=========================#
# Profession              #
#=========================#

#Check for NA variable
merge_data$Profession %>% is.na() %>% sum()
merge_data %>% count(Profession)

# Remove empty
merge_data <- merge_data %>% filter(Profession != "")
merge_data$Profession <- as.factor(merge_data$Profession)
merge_data %>% count(Profession)

#=========================#
# Income                  #
#=========================#

#Check for NA variable
merge_data$Income %>% is.na() %>% sum()
merge_data <- merge_data %>% filter(Income >= 0)
merge_data$Income %>% boxplot()


#=========================#
# Outstanding Balance     #
#=========================#

# Check for NA variable
merge_data$Outstanding.Balance %>% is.na() %>% sum()
merge_data <- merge_data %>% filter(!is.na(Outstanding.Balance))
summary(merge_data$Outstanding.Balance)
merge_data$Outstanding.Balance %>% boxplot()

#================================#
# Avg CC Util                    #
#================================#

#remove NAs
merge_data <- merge_data %>% filter(!is.na(Avgas.CC.Utilization.in.last.12.months))
summary(merge_data$No.of.times.30.DPD.or.worse.in.last.12.months)

#correlation
merge_data_correlation <- cor(merge_data %>% select_if(is.numeric), method = "pearson", use = "complete.obs")
merge_data_correlation
ggcorrplot(merge_data_correlation, lab = T, hc.order = T, type = "lower")

#================================#
# Others                         #
#================================#

merge_data$Education <- merge_data$Education %>% as.factor()
merge_data$Gender <- merge_data$Gender %>% as.factor()

#================================#
# EDA Distrib./Visualization     #
#================================#

## Income
merge_data$Income %>% boxplot()

# Profession vs Income
ggplot(merge_data, aes(Profession, Income, fill=Profession)) + geom_boxplot()
labs(x="Age Buckets", y="Frequency in 1000s", fill="Performance Tag", title="Age Bucket wise Performance Tag Frequency")

# Income vs Credit Usage
ggplot(merge_data, aes(Income, Avgas.CC.Utilization.in.last.12.months)) + geom_smooth()

# Income vs Average Debt
ggplot(merge_data %>% group_by(Income) %>% summarize(meanDebt = mean(Outstanding.Balance)), aes(Income, meanDebt)) + geom_smooth()

# Income Distribution
ggplot(merge_data, aes(Income)) + geom_histogram(binwidth = 10)

# Income vs Performance
ggplot(merge_data, aes(x = as.factor(Performance.Tag), y = Income, fill = as.factor(Performance.Tag))) + geom_boxplot()


### Age
# Age Distribution on data
ggplot(merge_data, aes(Age_Bin, fill = Age_Bin)) + geom_bar()

# Age vs Income
ggplot(merge_data, aes(x = Age_Bin, y = Income, fill = Age_Bin)) + geom_boxplot()
ggplot(merge_data, aes(x = Age, y = Income)) + geom_smooth()

# Age vs Performance
ggplot(merge_data, aes(x = Age_Bin, fill = as.factor(Performance.Tag))) + 
  geom_bar() + 
  scale_y_continuous(
    name = "Number of Applications in thousands",
    labels = function(x) x / 1000
  )


## Average Use of CC
# Avgas.CC.Utilization.in.last.12.months vs Performance
ggplot(merge_data, aes(x = as.factor(Performance.Tag), y = Avgas.CC.Utilization.in.last.12.months, fill = as.factor(Performance.Tag))) + geom_boxplot()


#Effect of Past Late Payments vs Performance
ggplot(merge_data, aes(Performance.Tag, No.of.times.30.DPD.or.worse.in.last.12.months, fill = as.factor(Performance.Tag))) + geom_boxplot()
ggplot(merge_data, aes(Performance.Tag, No.of.times.90.DPD.or.worse.in.last.12.months, fill = as.factor(Performance.Tag))) + geom_boxplot()

#Effect of Credit check
ggplot(merge_data, aes(as.factor(Performance.Tag), No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., fill = as.factor(Performance.Tag))) + geom_boxplot()

# Performance vs Outstanding Balance
merge_data$Outstanding.Balance %>% boxplot()
ggplot(merge_data, aes(x = as.factor(Performance.Tag), y = Outstanding.Balance/1000, fill = as.factor(Performance.Tag))) + geom_boxplot()

# Marital Status
ggplot(merge_data, aes(x = Marital.Status..at.the.time.of.application., fill = as.factor(Performance.Tag))) + 
  geom_bar(stat="count") + scale_y_log10()

# Number of Dependents
ggplot(merge_data, aes(x = Marital.Status..at.the.time.of.application., y = No.of.dependents)) + 
  geom_boxplot()


ggplot(merge_data, aes(x = Age_Bin, y = log(Outstanding.Balance))) + geom_boxplot()
ggplot(merge_data, aes(x = Marital.Status..at.the.time.of.application., y = Income, fill = Marital.Status..at.the.time.of.application.)) + geom_boxplot()


# Correlation Matrix
ggcorrplot(cor(merge_data[,c("Performance.Tag", "Income","Age", "Avgas.CC.Utilization.in.last.12.months","Outstanding.Balance", "No.of.times.30.DPD.or.worse.in.last.12.months", "Presence.of.open.home.loan", "Presence.of.open.auto.loan", "Total.No.of.Trades")]), lab = T)

# Correlation Matrix of Financial Information
ggcorrplot(cor(merge_data[, c(2, 7 ,seq(13,29,1))]), lab = T)


#=====================================#
# Data Visual. and EDA
#=====================================#

#=====================================#
# Split Data Set into Train and Test  #
#=====================================#

set.seed(129)
sample_size <- floor(0.75 * nrow(merge_data))
train_ind <- sample(seq_len(nrow(merge_data)), size = sample_size)

data_train <- merge_data[train_ind,]
data_test <- merge_data[-train_ind,]



#==================================#
# Logistic Regression              #
#==================================#

formula <- (Performance.Tag ~ Income_Bin + Avg_CC_Utilization_12_months + Outstanding_Balance + No.of.times.90.DPD.or.worse.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +  Marital.Status..at.the.time.of.application. + Age_Bin)
lrm_model <- glm(formula, data = data_train, family = "binomial")
summary(lrm_model)

data_train$predictions <- predict(lrm_model, type = "response")
data_train$predictions <- ifelse(data_train$predictions > mean(data_train$Performance.Tag), 1, 0)
mean(data_train$Performance.Tag == data_train$predictions)

data_test$predictions <- predict(lrm_model, newdata = data_test, type = "response")
data_test$predictions <- ifelse(data_test$predictions > mean(data_test$Performance.Tag), 1, 0)
mean(data_test$Performance.Tag == data_test$predictions)


#==================================#
#  Stepwise Regression model
#==================================#

# Specify a null model with no predictors
null_model <- glm(Performance.Tag ~ 1, data = data_train, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(Performance.Tag ~ ., data = data_train, family = "binomial")

step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(step_model)
step_prob <- predict(step_model, type="response", probability =TRUE)
mean(data_train$Performance.Tag == step_prob)

library(pROC)
ROC <- roc(data_train$Performance.Tag, step_prob)
plot(ROC, col = "red")
auc(ROC)


#==================================#
# Classification Tree              #
#==================================#
new_formula <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                     No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                     Total.No.of.Trades + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     No.of.months.in.current.company + Outstanding.Balance + No.of.times.90.DPD.or.worse.in.last.12.months + 
                     No.of.months.in.current.residence, family = "binomial", data = data_train)
library(rpart)
tree_model <- rpart(Performance.Tag ~ Age_Bin + No.of.times.30.DPD.or.worse.in.last.12.months + Income + Outstanding.Balance + Avgas.CC.Utilization.in.last.12.months , data = data_test, method = "class", control= rpart.control(cp =0, maxdepth=10))
summary(tree_model)

library(rpart.plot)
rpart.plot(tree_model)



## Find the correlation
merge_data_mini <- merge_data[,c(2,3,7,9,5,18,19,27,28)]
correl <- cor(merge_data[,c(2,3,7,18,19,27,28)], method = "pearson", use = "complete.obs")
ggcorrplot(correl, hc.order = T, type = "lower")


