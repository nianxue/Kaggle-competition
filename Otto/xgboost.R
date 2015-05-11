library(xgboost)
library(caret)
library(plyr)
library(dplyr)


setwd("E:/KaggleProject/Otto")
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

set.seed(8763)
xgbTune <- xgb.cv(data = dtrain,
                 max_depth = 6,         #6
                 eta = 0.3, 
                 nround = 2000, 
                 min_child_weight = 10,  #7
                 #gamma = 0.05,
                 #max_delta_step = 1,
                 objective = "multi:softprob",
                 num_class = 9,
                 eval_metric = "mlogloss",
                 nfold = 5,
                 stratified = TRUE,
                 #prediction = TRUE,
                 subsample = 0.8, #subsample ratio of the training instance 0.5
                 colsample_bytree = 0.5,
                 verbose = 1)


##########Train xgb model

set.seed(8763)
xgb <- xgboost(data = dtrainTwoway,
              max_depth = 6,         #6
              eta = 0.2, 
              nround = 200, 
              min_child_weight = 7,  #7
              objective = "multi:softprob",
              num_class = 9,
              eval_metric = "mlogloss",
              # prediction = TRUE,
              subsample = 0.8, #subsample ratio of the training instance 0.5
              colsample_bytree = 0.5,
              verbose = 1)


xgbs <- vector(mode = "list", length = 3)
xgbPred <- vector(mode = "list", length = 3)


xgb <- xgboost(data = dtrain,
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

pred = predict(xgb,dtest)

# Get the feature real names
names <- attr(train,"dimnames")[[2]]

# Compute feature importance matrix
varImp <- xgb.importance(names, model = xgb)
top50 <- varImp$Feature[1:50]




# Nice graph
xgb.plot.importance(importance_matrix[1:50,])


xgb.plot.tree(feature_names = names, model = xgb, n_first_tree = 3)


############################################ Make prediction
pred = predict(xgb,dtest)
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
