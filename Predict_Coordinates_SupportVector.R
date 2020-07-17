#Title:       SVM-MODELING Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        April 15, 2020

library(dplyr)
library(tidyverse)
library(caret)

train_clean <- readRDS('./train_clean')
validation_clean <- readRDS('./validation_clean')

set.seed(108)
inTrain_svm <- createDataPartition(train_clean$location, 
                                   times = 1,
                                   p = 0.7,
                                   list = FALSE)

Trainset_svm <- train_clean[inTrain_svm, ]

Testset_svm <- train_clean[-inTrain_svm, ]

#SVM model
fitting_svm <- trainControl(method = "repeatedcv", 
                            number = 10, 
                            repeats = 3)
set.seed(108)
svm_Linear <- train(location ~., 
                    data = Trainset_svm, 
                    method = "svmLinear",
                    trControl=fitting_svm,
                    tuneLength = 10)

svm_Linear
summary(svm_Linear)

test_svm <- predict(svm_Linear, Testset_svm)
svm_results <- predict(svm_Linear, pca_val_svm)

confusionMatrix(test_svm, Testset_svm$location)
confusionMatrix(svm_results, pca_val_svm$location)
str(Testset_svm$location)
