library(xgboost)
library(caret)
library(plyr)
library(dplyr)


setwd("E:/KaggleProject/Otto")
source('E:/KaggleProject/Otto/caretMultiLogloss.R')
trainIni <- read.csv("E:/KaggleProject/Otto/train.csv/train.csv")
testIni <- read.csv("E:/KaggleProject/Otto/test.csv/test.csv")

################################prepare the data
target <- trainIni$target
trainIni$target <- NULL

trainIni$split <- 1
testIni$split <- 0

data <- bind_rows(trainIni, testIni)


split <- data$split
data$id <- NULL
data$split <- NULL

data[,] <- lapply(data, as.numeric)
################################get train and test
train <- data[split == 1,]
test <- data[split == 0,]


label <- gsub("Class_", "", x = target)
label <- as.numeric(label) - 1

############################tune xgb parameters
dtrain <- xgb.DMatrix(data = as.matrix(train), label = label)
dtest <- xgb.DMatrix(data = as.matrix(test))

###############################################in xgb models##################


##################first xgb
set.seed(8763)
xgbTune3 <- xgb.cv(data = dtrain,
                   max_depth = 3,         #6
                   eta = 0.15, 
                   nround = 900, 
                   min_child_weight = 1,  #7
                   #                  gamma = 0.0001,
                   #max_delta_step = 1,
                   objective = "multi:softprob",
                   num_class = 9,
                   eval_metric = "mlogloss",
                   nfold = 10,
                   stratified = TRUE,
                   prediction = TRUE,
                   subsample = 0.8, #subsample ratio of the training instance 0.5
                   colsample_bytree = 0.6,
                   verbose = 1)

xgb3Cv <- as.data.frame(xgbTune3$pred, )
names(xgb3Cv) <- paste0("xgb3","class",1:9)

set.seed(8763)
xgb3 <- xgboost(data = dtrain,
               max_depth = 3,         #6
               eta = 0.15, 
               nround = 900, 
               min_child_weight = 1,  #7
               #gamma = 0.0001,
               #max_delta_step = 1,
               objective = "multi:softprob",
               num_class = 9,
               eval_metric = "mlogloss",
               #prediction = TRUE,
               subsample = 0.8, #subsample ratio of the training instance 0.5
               colsample_bytree = 0.6,
               verbose = 1)

xgb3Pred = predict(xgb3,dtest)
xgb3Pred = matrix(xgb3Pred,9,length(xgb3Pred)/9)
xgb3Pred = as.data.frame(t(xgb3Pred))
names(xgb3Pred) <- paste0("xgb3","class",1:9)
xgb.save(xgb3, 'xgb3')

##################second xgb
set.seed(8763)
xgbTune6 <- xgb.cv(data = dtrain,
                   max_depth = 6,         #6
                   eta = 0.3, 
                   nround = 160, 
                   min_child_weight = 3,  #7
                   #                  gamma = 0.0001,
                   #max_delta_step = 1,
                   objective = "multi:softprob",
                   num_class = 9,
                   eval_metric = "mlogloss",
                   nfold = 10,
                   stratified = TRUE,
                   prediction = TRUE,
                   subsample = 0.8, #subsample ratio of the training instance 0.5
                   colsample_bytree = 0.6,
                   verbose = 1)

xgb6Cv <- as.data.frame(xgbTune6$pred, )
names(xgb6Cv) <- paste0("xgb6","class",1:9)


set.seed(8763)
xgb6 <- xgboost(data = dtrain,
              max_depth = 6,         #6
              eta = 0.3, 
              nround = 160, 
              min_child_weight = 3,  #7
              objective = "multi:softprob",
              num_class = 9,
              eval_metric = "mlogloss",
              # prediction = TRUE,
              subsample = 0.8, #subsample ratio of the training instance 0.5
              colsample_bytree = 0.5,
              verbose = 1)

xgb6Pred = predict(xgb6,dtest)
xgb6Pred = matrix(xgb6Pred,9,length(xgb6Pred)/9)
xgb6Pred = as.data.frame(t(xgb6Pred))
names(xgb6Pred) <- paste0("xgb6","class",1:9)
xgb.save(xgb6, 'xgb6')

###########################third xgb
set.seed(8763)
xgbTune8 <- xgb.cv(data = dtrain,
                    max_depth = 8,         #6
                    eta = 0.03, 
                    nround = 1600, 
                    min_child_weight = 10,  #7
                    #                  gamma = 0.0001,
                    #max_delta_step = 1,
                    objective = "multi:softprob",
                    num_class = 9,
                    eval_metric = "mlogloss",
                    nfold = 10,
                    stratified = TRUE,
                    prediction = TRUE,
                    subsample = 0.8, #subsample ratio of the training instance 0.5
                    colsample_bytree = 0.6,
                    verbose = 1)

xgb8Cv <- as.data.frame(xgbTune8$pred, )
names(xgb8Cv) <- paste0("xgb8","class",1:9)


set.seed(8763)
xgb8 <- xgboost(data = dtrain,
                max_depth = 8,         #6
                eta = 0.03, 
                nround = 1600, 
                min_child_weight = 10,  #7
                objective = "multi:softprob",
                num_class = 9,
                eval_metric = "mlogloss",
                subsample = 0.8, #subsample ratio of the training instance
                colsample_bytree = 0.5,
                verbose = 1)

xgb8Pred = predict(xgb8,dtest)
xgb8Pred = matrix(xgb8Pred,9,length(xgb8Pred)/9)
xgb8Pred = as.data.frame(t(xgb8Pred))
names(xgb8Pred) <- paste0("xgb8","class",1:9)
xgb.save(xgb8, 'xgb8')


