library(xgboost)
library(plyr)
library(dplyr)
library(ranger)
library(caret)
library(doParallel)
library(stringr)

tc <- read.csv("E:/KaggleProject/SantanderCustomerSatisfaction/test.csv")
setwd("E:/KaggleProject/SantanderCustomerSatisfaction")
load("E:/KaggleProject/SantanderCustomerSatisfaction/data.RData")
source('E:/KaggleProject/PrudentialLifeAssessment/myFuns.R')

FactorResponse <- ifelse(target$TARGET == 1, "level1", "level0")
FactorResponse <- factor(FactorResponse, levels = c("level1", "level0"))

smallVars <- intersect(names(train), iniVars)

cl <- makeCluster(4)
registerDoParallel(cl)
# stopCluster(cl)

trCtrlNum <- trainControl(method = "cv",
                           number = 15,
                           returnData = FALSE,
                           savePredictions = T)

set.seed(75960)
xgbPoisson <- train(train[,smallVars], target$TARGET, 
                   method = "xgbTree", 
                   objective = "count:poisson",
                   trControl = trCtrlNum,
                   metric = "RMSE", 
                   maximize = FALSE,
                   tuneGrid = expand.grid(nrounds = 2000,
                                          max_depth = c(4), #
                                          eta = c(0.01), # bag
                                          gamma = c(0),
                                          colsample_bytree = c(0.5), #0.55
                                          min_child_weight = c(3)),#,7,9
                   subsample = 0.7, #0.7
                   alpha = 0,
                   lambda = 1,
                   # colsample_bylevel = 0.55,
                   stratified = TRUE,
                   verbose = 1)

set.seed(5960)
xgbLinear <- train(train[,smallVars], target$TARGET, 
                    method = "xgbTree", 
                    objective = "reg:linear",
                    trControl = trCtrlNum,
                    metric = "RMSE", 
                    maximize = FALSE,
                    tuneGrid = expand.grid(nrounds = 800,
                                           max_depth = c(6), #
                                           eta = c(0.01), # bag
                                           gamma = c(0),
                                           colsample_bytree = c(0.5), #0.55
                                           min_child_weight = c(100)),#,7,9
                    subsample = 0.7, #0.7
                    alpha = 0,
                    lambda = 1,
                    # colsample_bylevel = 0.55,
                    stratified = TRUE,
                    verbose = 1)



xgbPoissonCV <- xgbPoisson$pred %>% arrange(rowIndex)
xgbLinearCV <- xgbLinear$pred %>% arrange(rowIndex)

train$xgbPoissonPred <- xgbPoissonCV[["pred"]]
train$xgbLinearPred <- xgbLinearCV[["pred"]]

test$xgbPoissonPred <- predict(xgbPoisson, test[, smallVars])
test$xgbLinearPred <- predict(xgbLinear, test[, smallVars])


##########xgb with poisson and linear features
trCtrl <- trainControl(method = "cv",
                       number = 5,
                       summaryFunction = twoClassSummary,
                       returnData = FALSE,
                       classProbs = TRUE,
                       savePredictions = T)

set.seed(83750)
xgbProb <- train(train, FactorResponse, 
                 method = "xgbTree", 
                 objective = "binary:logistic",
                 trControl = trCtrl,
                 metric = "ROC", 
                 maximize = T,
                 tuneGrid = expand.grid(nrounds = seq(1200,1800, 100), #1800,
                                        max_depth = c(4,5), #
                                        eta = c(0.004), # bag
                                        gamma = c(0),
                                        colsample_bytree = c(0.35), #0.55
                                        min_child_weight = c(15)),#,7,9
                 subsample = 0.5, #0.7
                 alpha = 0,
                 lambda = 1,
                 # colsample_bylevel = 0.55,
                 stratified = TRUE,
                 verbose = 1)


print(xgbProb,showSD = T)
xgbImp <- varImp(xgbProb)$importance

xgb.dump(xgbProb$finalModel,fname = "xgb.dump", fmap='xgb.fmap', with.stats = TRUE)


trCtrlBag <- trainControl(method = "none",
                          number = 7,
                          summaryFunction = twoClassSummary,
                          returnData = FALSE,
                          classProbs = TRUE,
                          savePredictions = T)


set.seed(642891)
xgbBagFit <- train(train, FactorResponse, 
                   method = "bag", 
                   metric = "ROC", 
                   maximize = TRUE,
                   trControl = trCtrlBag,
                   tuneGrid = expand.grid(vars = c(90)),
                   B = 51, 
                   bagControl = bagControl(fit = xgbBag$fit,
                                           predict = xgbBag$pred,
                                           aggregate = xgbBag$aggregate),
                   objective = "binary:logistic",
                   nrounds = 1800, 
                   max_depth = 4,
                   eta = 0.004,
                   subsample = 0.5, 
                   colsample_bytree = c(0.35),
                   min_child_weight = 11,
                   stratified = TRUE,
                   bag.fraction = 1)

submission$TARGET <- predict(xgbBagFit, test, "prob")[[1]]
write.csv(submission, "submission.csv", row.names=FALSE)

