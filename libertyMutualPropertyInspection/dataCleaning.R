library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(doParallel)
library(xgboost)
library(randomForest)
library(glmnet)
library(caret)
library(extraTrees)
library(Rtsne)
library(gbm)
library(earth)

setwd("E:/KaggleProject/libertyMutualPropertyInspection")
source('E:/KaggleProject/libertyMutualPropertyInspection/caretGini.R')

submission <- read.csv("sample_submission.csv/sample_submission.csv")
trainIni <- read.csv("train.csv/train.csv")
testIni <- read.csv("test.csv/test.csv")


Hazard <- trainIni$Hazard
trainIni$Hazard <- NULL

HazardFactor5 <- ifelse(between(Hazard, 2, 3), '23', Hazard)
HazardFactor5 <- ifelse(between(Hazard, 4, 6), '456', HazardFactor5)
HazardFactor5 <- ifelse(between(Hazard, 7, 10), '789', HazardFactor5)
HazardFactor5 <- ifelse(Hazard >= 11, 999, HazardFactor5)
HazardFactor5 <- factor(HazardFactor5)
levels(HazardFactor5) <- paste0("value", levels(HazardFactor5))

HazardFactor2 <- ifelse(Hazard > 2, 999, 1)
HazardFactor2 <- factor(HazardFactor2)
levels(HazardFactor2) <- paste0("value", levels(HazardFactor2))



trainIni$split <- 1
testIni$split <- 0

data <- rbind(trainIni, testIni)
split <- data$split

data$split <- NULL
data$Id <- NULL

factorVars <- sapply(data, is.factor)
factorVars <- names(data)[factorVars]

numericVars <- setdiff(names(data), factorVars)


########################change numerical features to factors
data$facT1_V1 <- as.factor(data$T1_V1)

data$facT1_V2 <- ifelse(data$T1_V2 >= 23, 23, data$T1_V2)
data$facT1_V2 <- as.factor(data$facT1_V2)

data$facT1_V3 <- ifelse(data$T1_V3 >= 7, 7, data$T1_V3)
data$facT1_V3 <- as.factor(data$facT1_V3)

data$facT1_V10 <- as.factor(data$T1_V10)
data$facT1_V13 <- as.factor(data$T1_V13)

data$facT1_V14 <- ifelse(data$T1_V14 == 0 | data$T1_V14 == 4, 4, data$T1_V14)
data$facT1_V14 <- as.factor(data$facT1_V14)

data$facT2_V1 <- ifelse(data$T2_V1 < 27, 27, data$T2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 28, 39), 39, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 40, 45), 45, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 46, 49), 49, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 50, 54), 54, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 55, 65), 65, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 66, 75), 75, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 76, 85), 85, data$facT2_V1)
data$facT2_V1 <- ifelse(between(data$facT2_V1, 86, 100), 100, data$facT2_V1)
data$facT2_V1 <- as.factor(data$facT2_V1)

data$facT2_V2 <- ifelse(data$T2_V2 < 6, 6, data$T2_V2)
data$facT2_V2 <- ifelse(between(data$facT2_V2, 21, 39), 39, data$facT2_V2)
data$facT2_V2 <- as.factor(data$facT2_V2)

data$facT2_V4 <- ifelse(data$T2_V4 < 2, 2, data$T2_V4)
data$facT2_V4 <- ifelse(between(data$facT2_V4, 19, 22), 22, data$facT2_V4)
data$facT2_V4 <- as.factor(data$facT2_V4)


data$facT2_V6 <- ifelse(data$T2_V6 >= 4, 4, data$T2_V6)
data$facT2_V6 <- as.factor(data$facT2_V6)

data$facT2_V7 <- as.factor(data$T2_V7)

data$facT2_V8 <- ifelse(data$T2_V8 >= 2, 2, data$T2_V8)
data$facT2_V8 <- as.factor(data$facT2_V8)

data$facT2_V9 <- as.factor(data$T2_V9)

data$facT2_V10 <- as.factor(data$T2_V10)

data$facT2_V14 <- ifelse(data$T2_V14 >= 6, 6, data$T2_V14)
data$facT2_V14 <- as.factor(data$facT2_V14)

data$facT2_V15 <- as.factor(data$T2_V15)


#####################create rowwise features
data$rowMax <- apply(data[,numericVars], 1, max)
data$rowMean <- apply(data[,numericVars], 1, mean)
data$rowMaxIndex <- apply(data[,numericVars], 1, which.max)

#level 1 cv split
set.seed(39282)
temp <- createFolds(y = Hazard, k = 10)
index <- lapply(temp, function(x) setdiff(1:length(Hazard), x))

#create feature by reducing data dimmations
temp <- model.matrix(~.-1, data = data)
cs <- preProcess(temp, method = c("center", "scale"))
temp <- predict(cs, temp)
tsne <- Rtsne(temp, dims = 2)

tsneData <- as.data.frame(tsne$Y)
# ggplot(tsneData[split == 1, ], aes(x = V1, y = V2)) + 
#     geom_point(aes(col = HazardFactor5),alpha = 0.3)

cl <- makeCluster(4)
registerDoParallel(cl)
clusterExport(cl, varlist=c("NormalizedWeightedGini", "WeightedGini"))
# stopCluster(cl)




