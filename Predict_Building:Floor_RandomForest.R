# Title: Random Forest Model
#Version: 1.0
#Author: Anita Rijal
#Date: March 27, 2020


library(caret)
library(dplyr)

train_clean <- readRDS('./train_clean')
validation_clean <- readRDS('./validation_clean')


set.seed(108)
inTrain <- createDataPartition(train_clean$locationID, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset <- train_clean[inTrain, ]
Testset <- train_clean[-inTrain, ]

fitting <- trainControl(method = "cv",
                        number = 10,
                        search = "grid",
                        savePredictions = "final")

#Creating df with just predictors and target value
Trainset<- Trainset %>% 
  select(starts_with("WAP"), locationID)

Testset<- Testset %>% 
  select(starts_with("WAP"), locationID)

#RANDOM FOREST CLASSIFIER MODEL
set.seed(108)
RFmodel <- train(factor(locationID) ~., 
                 data = Trainset,
                 trControl = fitting,
                 tuneLength = 2)
RFmodel
summary(RFmodel)

#predicting test set outcomes using training set
testresults <- predict(RFmodel, Testset)

#using validation data set to predict
modelresults <- predict(RFmodel,validation_clean)

confusionMatrix(testresults, Testset$locationID)
confusionMatrix(modelresults, validation_clean$locationID)
confusionMatrix
