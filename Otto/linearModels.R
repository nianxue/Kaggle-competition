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

glmnet.numCv <- arrange(glmnet.num$pred, rowIndex)
glmnet.numCv <- glmnet.numCv %>% select(starts_with("class"))
names(glmnet.numCv) <- paste0("glmnet.num","class",1:9)

glmnet.numPred <- predict(glmnet.num, test.num, type = "prob")
names(glmnet.numPred) <- paste0("glmnet.num","class",1:9)




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

glmnet.facCv <- arrange(glmnet.fac$pred, rowIndex)
glmnet.facCv <- glmnet.facCv %>% select(starts_with("class"))
names(glmnet.facCv) <- paste0("glmnet.fac","class",1:9)

glmnet.facPred <- predict(glmnet.fac, test.fac, type = "prob")
names(glmnet.facPred) <- paste0("glmnet.fac","class",1:9)


stopCluster(cl)

