library(xgboost)
library(caret)
library(plyr)
library(dplyr)
library(glmnet)
library(nnet)
library(doParallel)
library(gbm)

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

######################get factor data set
data.factor <- data
data.factor[,] <- lapply(data.factor, function(x) {tab <- table(x)
                                                   smalls <- as.integer(names(tab)[tab < 2500])
                                                   ifelse(x %in% smalls, 999, x)})
data.factor[,] <- lapply(data.factor, as.factor)

train.fac <- data.factor[split == 1,]
test.fac <- data.factor[split == 0,]

train.fac <- model.matrix(~.-1, train.fac)
test.fac <- model.matrix(~.-1, test.fac)

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
glmnet.num <- train(x = train.num, 
                   y = target, 
                   method = "glmnet", 
                   family = "multinomial",
                   metric = "Logloss", 
                   trControl = trCtrl, 
    #                tuneLength = 9,
                   maximize = FALSE,
                   tuneGrid = expand.grid(alpha = 0.8875,
                                          lambda = 0.0001277322))

predGlmnet.num <- predict(glmnet.num, test.num, type = "prob")



set.seed(983)
glmnet.fac <- train(x = train.fac, 
                    y = target, 
                    method = "glmnet", 
                    family = "multinomial",
                    metric = "Logloss", 
                    trControl = trCtrl, 
                    #                 tuneLength = 9,
                    maximize = FALSE,
                    tuneGrid = expand.grid(alpha = 0.8875,
                                           lambda = c(0.0002)))

predGlmnet.fac <- predict(glmnet.fac, test.fac, type = "prob")



submission[,2:10] <- glmnetPred
write.csv(submission,file='submission.csv', quote=FALSE,row.names=FALSE)



stopCluster(cl)

############################################ Make prediction

write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
