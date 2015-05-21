library(randomForest)
library(caret)
library(plyr)
library(dplyr)
library(doParallel)
library(glmnet)
library(extraTrees)

source('E:/KaggleProject/Otto/caretMultiLogloss.R')
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

################################get train and test
train <- data[split == 1,]
# train$target <- target
test <- data[split == 0,]


cl <- makeCluster(4)
registerDoParallel(cl)


# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "adaptive_cv", 
                       number = 10, 
                       classProbs = TRUE,
                       summaryFunction = caretMultiLogloss,
                       seeds = NULL,
                       returnData = FALSE,
                       savePredictions = TRUE)


set.seed(6421)
rf <- train(target~.,
           train, 
           method = "rf", 
           metric = "Logloss", 
           trControl = trCtrl,
#            tuneLength = 2
            tuneGrid = expand.grid(mtry = c(25)),
            ntree = 300,
           nodesize = 10,
           maximize = FALSE,
           importance = TRUE,
           do.trace = TRUE)

rfCv <- arrange(rf$pred, rowIndex)
rfCv <- rfCv %>% select(starts_with("class"))
names(rfCv) <- paste0("rf","class",1:9)


rfPred <- predict(rf, test, type = "prob")
names(rfPred) <- paste0("rf","class",1:9)


stopCluster(cl)
