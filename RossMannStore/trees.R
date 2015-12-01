library(caret)
library(plyr)
library(dplyr)
library(xgboost)
library(tidyr)
library(ranger)
library(lubridate)
library(stringr)
library(car)
library(doParallel)

source('E:/KaggleProject/RossMannStore/myFuns.R')
setwd("E:/KaggleProject/RossMannStore")

load("E:/KaggleProject/RossMannStore/data.RData")
#trainIni$revenue[trainIni$revenue > 1.0e+7] = 1.0e+7

train <- data[split == 1, ]
test <- data[split == 0, ]

trainIndex <- which(train$Date < "2015-05-01")
# data$Year <- NULL
train$Date <- NULL
test$Date <- NULL


trainY <- Sales$Sales

#build linear models
registerDoParallel(4)



# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "LGOCV", 
                       # repeats = 5,
                       number = 1,
                       returnData = FALSE,
                       savePredictions = T,
                       summaryFunction = logRMSPE,#RMSPE
                       index = list(trainSet = trainIndex))

set.seed(849)
xgbLog <- train(train, log(trainY), 
                 method = "xgbTree", 
                 objective = "reg:linear",
                 trControl = trCtrl,
                 metric = "RMSPE", 
                 maximize = F,
                 tuneGrid = expand.grid(nrounds = c(300,600,900,1200,1500,1600,1700,1800,1900,2000), #
                                        max_depth = c(11),#8
                                        eta = c(0.05), 
                                        gamma = 0, 
                                        colsample_bytree = c(0.85), #0.6
                                        min_child_weight = c(20)),
                 subsample = 0.65, #0.85
                 # early.stop.round = 100,
                 stratified = TRUE,
                 verbose = 1)

xgbLogImp <- varImp(xgbLog)

xgbLogPred <- predict(xgbLog, test)
submission$Sales[testIDOpen$Open == 1] <- exp(xgbLogPred)




trCtrl <- trainControl(method = "LGOCV", 
                       # repeats = 5,
                       number = 1,
                       returnData = FALSE,
                       savePredictions = T,
                       summaryFunction = RMSPE,#RMSPE
                       index = list(trainSet = trainIndex))


set.seed(849)
xgb <- train(train, trainY, 
             method = "xgbTree", 
             objective = "reg:linear",
             trControl = trCtrl,
             metric = "RMSPE", 
             maximize = F,
             tuneGrid = expand.grid(nrounds = c(900,1200,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,2500), #
                                    max_depth = c(8,9,10),#8
                                    eta = c(0.08), 
                                    gamma = 0, 
                                    colsample_bytree = c(0.75), #0.6
                                    min_child_weight = c(15)),
             subsample = 0.65, 
             # early.stop.round = 100,
             stratified = TRUE,
             verbose = 1)

xgbImp <- varImp(xgb)


write.csv(submission, "submission.csv", row.names=FALSE)
stopCluster(cl)
