#Importing the Framingham.csv file
framingham <- read.csv("framingham.csv")

str(framingham)

#Use library "caTools" for splitting the dataset into training and testing set
library(caTools)

#The sample.split() function is used to distribute data 
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
framTrain <- subset(framingham, split == T)
framTest <- subset(framingham, split == F)

#glm() is used to create a logistics regression model
fLog <- glm(TenYearCHD ~ ., data = framTrain, family = binomial)

#Using predict()
predictTest <- predict(fLog, type = "response", newdata= framTest)
#Choosing the threshold value 0.5
table(framTest$TenYearCHD, predictTest > 0.5)

#Calculating the current model accuracy (cma) and baseline model accuracy (bma)
(1525+19)/(1525 + 14 + 262 + 19)
(1525 + 14)/(1525 + 14 + 262 + 19)

library(ROCR)
ROCRpred <- prediction(predictTest, framTest$TenYearCHD)

#Finding the AUC value of the model
as.numeric(performance(ROCRpred, "auc")@y.values)
