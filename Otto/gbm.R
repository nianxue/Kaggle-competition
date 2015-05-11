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

data[,] <- lapply(data, log1p)
################################get train and test
train <- data[split == 1,]
test <- data[split == 0,]


############################tune earth model
#parallel calculation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "cv", 
                       number = 3, 
                       seeds = NULL,
                       classProbs = T,
                       summaryFunction = caretMultiLogloss,
                       returnData = FALSE,
                       savePredictions = F)




set.seed(983)
gbm <- train(target~.,          
             train, 
             method = "gbm", 
             metric = "Logloss", 
             trControl = trCtrl, 
             bag.fraction = 0.5,
             #             tuneLength = 9,
             maximize = FALSE,
             tuneGrid = expand.grid(n.trees = c(300,400,500, 600,800,1000), 
                                    interaction.depth = c(7,9), 
                                    shrinkage = c(0.001, 0.01),
                                    n.minobsinnode = c(10)))


glmnetPred <- predict(glmnet.num, train)
submission[,2:10] <- glmnetPred
write.csv(submission,file='submission.csv', quote=FALSE,row.names=FALSE)


stopCluster(cl)

############################################ Make prediction

write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
