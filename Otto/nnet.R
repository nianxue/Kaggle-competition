library(xgboost)
library(caret)
library(plyr)
library(dplyr)
library(glmnet)
library(nnet)
library(doParallel)


source('E:/KaggleProject/Otto/caretMultiLogloss.R')
setwd("E:/KaggleProject/Otto")
trainIni <- read.csv("E:/KaggleProject/Otto/train.csv/train.csv")
testIni <- read.csv("E:/KaggleProject/Otto/test.csv/test.csv")
submission <- read.csv("E:/KaggleProject/Otto/sampleSubmission.csv")

################################prepare the data
target <- trainIni$target
trainIni$target <- NULL

trainIni$split <- 1
testIni$split <- 0

data <- bind_rows(trainIni, testIni)
split <- data$split
data$id <- NULL
data$split <- NULL


##################get numerical data set
data.num <- data
data.num[,] <- lapply(data.num, log1p)

train.num <- data.num[split == 1,]
test.num <- data.num[split == 0,]

############################tune earth model
#parallel calculation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "cv", 
                       number = 10, 
                       seeds = NULL,
                       classProbs = T,
                       summaryFunction = caretMultiLogloss,
                       returnData = FALSE,
                       savePredictions = T)


set.seed(983)
nnet50 <- train(x = train.num, 
                y = target, 
                method = "nnet", 
                metric = "Logloss", 
                trControl = trCtrl, 
                maximize = FALSE,
                entropy = TRUE,
                maxit = 250,
                MaxNWts = 55 * (ncol(train.num) + 1) + 55 + 1,
                tuneGrid = expand.grid(size = c(50), 
                                       decay = c(5)))

nnet50Cv <- arrange(nnet50$pred, rowIndex)
nnet50Cv <- nnet50Cv %>% select(starts_with("class"))
names(nnet50Cv) <- paste0("nnet50","class",1:9)

nnet50Pred <- predict(nnet50, test.num, type = "prob")
names(nnet50Pred) <- paste0("nnet50","class",1:9)

set.seed(983)
nnet150 <- train(x = train.num, 
                 y = target, 
                 method = "nnet", 
                 metric = "Logloss", 
                 trControl = trCtrl, 
                 maximize = FALSE,
                 entropy = TRUE,
                 maxit = 250,
                 MaxNWts = 160 * (ncol(train.num) + 1) + 160 + 1,
                 tuneGrid = expand.grid(size = c(150), 
                                        decay = c(5)))

nnet150Cv <- arrange(nnet150$pred, rowIndex)
nnet150Cv <- nnet150Cv %>% select(starts_with("class"))
names(nnet150Cv) <- paste0("nnet150","class",1:9)

nnet150Pred <- predict(nnet150, test.num, type = "prob")
names(nnet150Pred) <- paste0("nnet150","class",1:9)


set.seed(983)
nnet250 <- train(x = train.num, 
                 y = target, 
                 method = "nnet", 
                 metric = "Logloss", 
                 trControl = trCtrl, 
                 maximize = FALSE,
                 entropy = TRUE,
                 maxit = 200,
                 MaxNWts = 300 * (ncol(train.num) + 1) + 300 + 1,
                 tuneGrid = expand.grid(size = c(250), 
                                        decay = c(4)))

nnet250Cv <- arrange(nnet250$pred, rowIndex)
nnet250Cv <- nnet250Cv %>% select(starts_with("class"))
names(nnet250Cv) <- paste0("nnet250","class",1:9)

nnet250Pred <- predict(nnet250, test.num, type = "prob")
names(nnet250Pred) <- paste0("nnet250","class",1:9)



stopCluster(cl)

