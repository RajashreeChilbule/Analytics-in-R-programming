#HR_Attrition_Prediction OR Churn Rate Prediction
#Read the dataset:
#----Importing the dataset----
data_set = read.csv("C:/Users/Raj/Documents/R/Matrix Data structure/data/HR_Data.csv")
View(data_set)

#----Data Cleaning----
## Checking if there are any missing values in data and checking overall summary
summary(data_set)

#----------Exploring the Data---------
#Statistical Overview
#Check the no. of rows & columns in the dataset
dim(data_set)

#Check the datatype of all the features. 
str(data_set)

#Using transform method to change the in-built data type of variables:
#:left,salary,Work_accident & promotion_last_5years
dataset <- transform(data_set,
                     left=as.factor(left),
                     role=as.factor(role),
                     salary=as.factor(salary),
                     Work_accident=as.factor(Work_accident),
                     promotion_last_5years=as.factor(promotion_last_5years)
)
str(dataset)

#Encoding categorical var with more than 2 levels:
# ----------------------- role --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$role)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select role, COUNT (*) as obs, avg(left) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here out of 10 Types only 3 have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 3-1 = 2.
dataset_1 <- dataset
dataset_1$Role1 <- ifelse(dataset_1$role=='sales',1,0)
dataset_1$Role2 <- ifelse(dataset_1$role=='technical',1,0)
View(dataset_1)
# ----------------------- salary --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$salary)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select salary, COUNT (*) as obs, avg(left) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here out of 3 Types only 2 have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 2-1 = 1.
dataset_1$Salary1 <- ifelse(dataset_1$salary=='low',1,0)

#Feature Engg.: Since we have created dummy var for few char var,
#so lets remove those char var whose dummy var we have created.
dataset_1 <- dataset_1[,-c(9,10)]
View(dataset_1)

#Split the dataset in Train/Test data----
#Prep Training and Test data.----
View(dataset_1)
trainDataIndex <- sample(1:nrow(dataset_1),0.7*nrow(dataset_1), replace = F)
trainData <-dataset_1[trainDataIndex, ]
testData <- dataset_1[-trainDataIndex, ]
View(trainData)
View(testData)

#Build the regression model----
logit <- glm(left~., data = trainData, family = 'binomial')
summary(logit)

logit_2 <- glm(left ~ satisfaction_level+last_evaluation+Role2+
               number_project+average_montly_hours+exp_in_company+
               Work_accident+promotion_last_5years+Salary1,
             data = trainData, family = 'binomial')
summary(logit_2)

#----------Predict--------------------
testData$Pred_Attrition <- predict(logit_2,testData,type =c("response"))
View(testData)
table(testData$left)/nrow(testData)

#from above cmd, we know that in the test dataset, 
#count of "0" in column "left" is 76% and count of "1" is 24%.
#Hence, we try to see the data in column "Pred_Attrition", using quantile fxn.
#And notice the values at quantile level 76%, i.e., between 75% to 80%
quantile(testData$Pred_Attrition, probs = seq(0,1,0.05))
table(testData$left)
#Since, the value at quantile level at 76% is 0.35,
#Hence, we consider all the values greater than this value as 1 and others as 0 in our Predicted values column.
testData$Pred_Attrition <- ifelse(testData$Pred_Attrition > 0.35,1,0)
View(testData)

#Checking Accuracy of Model using: Confusion Matrix----
table(testData$Pred_Attrition)/nrow(testData)
table_mat<-table(testData$Pred_Attrition,testData$left)
table_mat

##From the above table cmd, we have got confusion matrix, stating that:
#No. of "0" present in training dataset is equal to no. of "0" predicted in test dataset , for 3057 enteries.
#No. of "1" present in training dataset is equal to no. of "1" predicted in test dataset for 536 enteries.
#Hence, Accuracy will be = (3057+536)/(3057+536+534+373) = 80%
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test