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

data$SchoolHoliday <- NULL
data$Date <- NULL
data$Promo2DaysLeft <- NULL
data$DaysOfPromo2 <- NULL
data$beforeCloseDay <- NULL
data$afterCloseDay <- NULL

train <- data[split == 1, ]
train <- filter(train, Year == 2013)
test <- data[split == 0, ]

train$Year <- NULL
test$Year <- NULL

trainY <- train$Customers
train$Customers <- NULL
test$Customers <- NULL

#build linear models
registerDoParallel(4)



# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "CV", 
                       # repeats = 5,
                       number = 5,
                       returnData = FALSE,
                       savePredictions = T,
                       summaryFunction = logRMSPE
                       )

set.seed(849)
xgbLogCustomer <- train(train, log(trainY), 
                        method = "xgbTree", 
                        objective = "reg:linear",
                        trControl = trCtrl,
                        metric = "RMSPE", 
                        maximize = F,
                        tuneGrid = expand.grid(nrounds = c(2500), #
                                               max_depth = c(10),#8
                                               eta = c(0.05), 
                                               gamma = 0, 
                                               colsample_bytree = c(0.75), #
                                               min_child_weight = c(10)), #
                        subsample = 0.7, #0.85
                        # early.stop.round = 100,
                        stratified = TRUE,
                        verbose = 1)

data1415 <- filter(data, Year > 2013) %>% select(-Year, -Customers)
CustomersTrain1415 <- predict(xgbLogCustomer, data1415)

write.csv(CustomersTrain1415, "CustomersTrain1415.csv", row.names=FALSE)

