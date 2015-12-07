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
CustomersTrain1415 <- read.csv("E:/KaggleProject/RossMannStore/CustomersTrain1415.csv", stringsAsFactors=FALSE)
data$split <- split
#trainIni$revenue[trainIni$revenue > 1.0e+7] = 1.0e+7

data2013 <- data %>% right_join(Sales) %>% filter(Year == 2013) %>% 
    group_by(Store, Promo, DayOfWeek) %>%
    summarise(customerMean3 = mean(Customers),
              SalesMean3 = mean(Sales), SalesMax3 = max(Sales),
              SalesMin3 = min(Sales), SalesSd3 = sd(Sales),
              SalePerCostomerMean3 = mean(Sales/Customers), SalePerCostomerMax3 = max(Sales/Customers),
              SalePerCostomerMin3 = min(Sales/Customers))
data <- left_join(data, data2013)


data2013 <- data %>% right_join(Sales) %>% filter(Year == 2013) %>% 
    group_by(Store, Promo) %>%
    summarise(customerMean2 = mean(Customers),
              SalesMean2 = mean(Sales), SalesMax2 = max(Sales),
              SalesMin2 = min(Sales), SalesSd2 = sd(Sales),
              SalePerCostomerMean2 = mean(Sales/Customers), SalePerCostomerMax2 = max(Sales/Customers),
              SalePerCostomerMin2 = min(Sales/Customers))
data <- left_join(data, data2013)


data2013 <- data %>% right_join(Sales) %>% filter(Year == 2013) %>% 
    group_by(Store, DayOfWeek) %>%
    summarise(customerMean22 = mean(Customers),
              SalesMean22 = mean(Sales), SalesMax22 = max(Sales),
              SalesMin22 = min(Sales), SalesSd22 = sd(Sales),
              SalePerCostomerMean22 = mean(Sales/Customers), SalePerCostomerMax22 = max(Sales/Customers),
              SalePerCostomerMin22 = min(Sales/Customers))
data <- left_join(data, data2013)


data2013 <- data %>% right_join(Sales) %>% filter(Year == 2013) %>% 
    group_by(Store) %>%
    summarise(customerMean1 = mean(Customers),
              SalesMean1 = mean(Sales), SalesMax1 = max(Sales),
              SalesMin1 = min(Sales), SalesSd1 = sd(Sales),
              SalePerCostomerMean1 = mean(Sales/Customers), SalePerCostomerMax1 = max(Sales/Customers),
              SalePerCostomerMin1 = min(Sales/Customers))
data <- left_join(data, data2013)




data <- filter(data, Year >= 2014) %>% select(-Year)
data$Customers <- CustomersTrain1415$x

train <- data[data$split == 1, ]
test <- data[data$split == 0, ]

train$split <- NULL
test$split <- NULL


trainIndex <- which(train$Date < "2015-05-01")
trainY <- train %>% left_join(Sales) %>% select(Sales)
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
xgbLogReduceSales <- train(train, log(trainY$Sales), 
                          method = "xgbTree", 
                          objective = "reg:linear",
                          trControl = trCtrl,
                          metric = "RMSPE", 
                          maximize = F,
                          tuneGrid = expand.grid(nrounds = c(1000,1100,1200,1300,1400,1500,
                                                             1600,1700,1800,1900,2000), #
                                                 max_depth = c(10),#8
                                                 eta = c(0.04), 
                                                 gamma = 0, 
                                                 colsample_bytree = c(0.6), #0.6
                                                 min_child_weight = c(20,25)), #
                          subsample = 0.65, #0.85
                          # early.stop.round = 100,
                          stratified = TRUE)


xgbLogReduceSalesImp <- varImp(xgbLogReduceSales)

Pred <- predict(xgbLogReduceSales, test)
submission$Sales[testIDOpen$Open == 1] <- exp(Pred)

write.csv(submission, "submission.csv", row.names=FALSE)
stopCluster(cl)
