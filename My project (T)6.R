
# Multiple Linear Regression 

# Importing the dataset 
dataset = read.csv("C:/Users/Raj/Documents/R/Matrix Data structure/data/Luxury_Cars.csv")
View(dataset)

#Is there any missing value in the dataset
summary(dataset)
sum(is.na(dataset))

#Lets check the data type of each field:
str(dataset)
#We observe that fields: "Make, Model, Type, Origin, DriveTrain" are having 
#character data type, so we need to convert them to factors. 
#Also field: "Cylinders" is wrongly assigned numerical data type, so we need to 
#convert it to factor type.
#Also field: "Model" may not be significant to predict the mileage of the cars.
#Hence,we can remove it.
table(dataset$Model)

#First removing "Model" column
dataset <- dataset[,-2]
View(dataset)
str(dataset)
#We can use transform method to change the in-built data type of above char var.
dataset <- transform(dataset,
                  Make=as.factor(Make),
                  Type=as.factor(Type),
                  Origin=as.factor(Origin),
                  DriveTrain=as.factor(DriveTrain),
                  Cylinders=as.factor(Cylinders)
)

str(dataset)

#Encoding categorical data 

# -----------------------  Make --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$Make)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select Make, COUNT (*) as obs, avg(MPG_Mileage) from dataset GROUP BY 1")
#Since here none of the levels in "Make" have large/significant no. of obs,
#so lets ignore this var.
dataset <- dataset[,-1]
View(dataset)

# ----------------------- Type --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$Type)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select Type, COUNT (*) as obs, avg(MPG_Mileage) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here out of 6 Types only 3 have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 3-1 = 2.
dataset_1 <- dataset
dataset_1$Type1 <- ifelse(dataset_1$Type=='Sedan',1,0)
dataset_1$Type2 <- ifelse(dataset_1$Type=='SUV',1,0)
View(dataset_1)
# ----------------------- Origin --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$Origin)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select Origin, COUNT (*) as obs, avg(MPG_Mileage) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here all 3 Origins have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 3-1 = 2.
dataset_1$Origin1 <- ifelse(dataset_1$Origin=='Asia',1,0)
dataset_1$Origin2 <- ifelse(dataset_1$Origin=='USA',1,0)

# ----------------------- DriveTrain  --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$DriveTrain)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select DriveTrain, COUNT (*) as obs, avg(MPG_Mileage) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here all 3 DriveTrains have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 3-1 = 2.
dataset_1$DriveTrain1 <- ifelse(dataset_1$DriveTrain=='Front',1,0)
dataset_1$DriveTrain2 <- ifelse(dataset_1$DriveTrain=='Rear',1,0) 

# ----------------------- Cylinders  --------------------------------
# To check Plan Subscription ratio in dataset
table(dataset$Cylinders)/nrow(dataset)
# Avg Premium by state# To check Plan Subscription ratio in dataset
library(sqldf)
sqldf("select Cylinders, COUNT (*) as obs, avg(MPG_Mileage) from dataset GROUP BY 1")
#Create dummy variables for Type:
#Since here out of 7 Cylinder types only 3 have large/significant no.of obs,
#Hence, the no. of dummy var here, will be 3-1 = 2.
dataset_1$Cylinders1 <- ifelse(dataset_1$Cylinders=='6',1,0)
dataset_1$Cylinders2 <- ifelse(dataset_1$Cylinders=='4',1,0)

View(dataset_1)
#Feature Engg.: Since we have created dummy var for few char var,
#so lets remove those char var whose dummy var we have created.
dataset_1 <- dataset_1[,-c(1,2,3,5)]
View(dataset_1)
# Splitting the dataset into the Training set and Test set 
trainDataIndex <- sample(1:nrow(dataset_1),0.7*nrow(dataset_1), replace = F)
trainData <-dataset_1[trainDataIndex, ]
testData <- dataset_1[-trainDataIndex, ]
View(trainData)
View(testData)

# Fitting Multiple Regression to the Training set 
regressor = lm(MPG_Mileage ~ .,data = trainData) 
summary(regressor)

regressor1 = lm(MPG_Mileage ~ Type2+#Type1+
                 #Origin1+
                 Origin2+
                 DriveTrain1+#DriveTrain2+
                 #Engine_Size+
                 Wheelbase_inch+
                 Cylinders2+
                 Cylinders1+Horsepower+Weight_LBS#+Length_inch
               ,data = trainData) 
summary(regressor1)

# Predicting the Test set results 
y_pred = predict(regressor1, newdata = testData)
testData$Pred_MPG = y_pred

#Accuracy of the Model:
#MAPE(MeanAbsolutePercentageError): 
#Lower its value better is the accuracy of the model.

#MAPE Calculation:
mape <- mean(abs((testData$Pred_MPG - testData$MPG_Mileage))/testData$MPG_Mileage)
mape

# Mape using mape function
#install.packages("Metrics")
#Regression Eqn:
#Mileage = 41.85 -1.9*(Type2) -1.2*(Origin2) +2*(DriveTrain1) +0.10*(Wheelbase)
# -2.58(cylinders2) -3.02(cylinder1) -0.02(horsepower) -0.005(weight)


#Since the error term is around 0.08, 
#it means there is only 8% error in our model's prediction.

