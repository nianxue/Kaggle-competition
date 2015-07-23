library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(doParallel)
library(xgboost)
library(randomForest)
library(glmnet)
library(caret)


setwd("E:/KaggleProject/libertyMutualPropertyInspection")
source('E:/KaggleProject/libertyMutualPropertyInspection/caretGini.R')

submission <- read_csv("sample_submission.csv/sample_submission.csv")
trainIni <- read_csv("train.csv/train.csv")
testIni <- read_csv("test.csv/test.csv")

# submission <- read_csv("sample_submission.csv")
# trainIni <- read_csv("train.csv")
# testIni <- read_csv("test.csv")

Hazard <- trainIni$Hazard
trainIni$Hazard <- NULL

HazardFactor <- ifelse(Hazard >= 16, 16, Hazard)
HazardFactor <- ifelse(Hazard >= 11 & Hazard < 16, 11, HazardFactor)
HazardFactor <- factor(HazardFactor)
levels(HazardFactor) <- paste0("value", levels(HazardFactor))

trainIni$split <- 1
testIni$split <- 0

data <- bind_rows(trainIni, testIni)
split <- data$split

data$split <- NULL
data$Id <- NULL

#change character variable to factors
characterVars <- sapply(data, is.character)
data[,characterVars] <- lapply(data[,characterVars], as.factor)



