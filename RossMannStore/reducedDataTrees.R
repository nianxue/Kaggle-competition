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
train <- filter(train, Year >= 2014)
test <- data[split == 0, ]

trainIndex <- which(train$Date < "2015-05-01")
trainY <- train %>% left_join(Sales) %>% select(Sales)
# data$Year <- NULL
train$Date <- NULL
test$Date <- NULL




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
xgbLogReduce <- train(train, log(trainY$Sales), 
                      method = "xgbTree", 
                      objective = "reg:linear",
                      trControl = trCtrl,
                      metric = "RMSPE", 
                      maximize = F,
                      tuneGrid = expand.grid(nrounds = c(1500,1600,1700,1800,1900,2000,2100,2200,2300), #
                                             max_depth = c(11),#8
                                             eta = c(0.03), 
                                             gamma = 0, 
                                             colsample_bytree = c(0.85), #0.6
                                             min_child_weight = c(25)),
                      subsample = 0.85, #0.85
                      # early.stop.round = 100,
                      stratified = TRUE)

xgbLogReduceImp <- varImp(xgbLogReduce)
xgbLogReducePred <- predict(xgbLogReduce, test)
submission$Sales[testIDOpen$Open == 1] <- exp(xgbLogReducePred)

write.csv(submission, "submission.csv", row.names=FALSE)
stopCluster(cl)
