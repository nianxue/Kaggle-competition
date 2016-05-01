library(xgboost)
library(plyr)
library(dplyr)
library(ranger)
library(caret)
library(doParallel)
library(stringr)


setwd("E:/KaggleProject/SantanderCustomerSatisfaction")
load("E:/KaggleProject/SantanderCustomerSatisfaction/data.RData")
source('E:/KaggleProject/PrudentialLifeAssessment/myFuns.R')

FactorResponse <- ifelse(target$TARGET == 1, "level1", "level0")
FactorResponse <- factor(FactorResponse, levels = c("level1", "level0"))

cl <- makeCluster(4)
registerDoParallel(cl)
# stopCluster(cl)

trCtrl <- trainControl(method = "cv",
                       number = 5,
                       summaryFunction = twoClassSummary,
                       returnData = FALSE,
                       classProbs = TRUE,
                       savePredictions = T)


set.seed(75960)
xgbProb <- train(train, FactorResponse, 
                method = "xgbTree", 
                objective = "binary:logistic",
                trControl = trCtrl,
                metric = "ROC", 
                maximize = T,
                tuneGrid = expand.grid(nrounds = seq(1000,2500,100),
                                       max_depth = c(5), #
                                       eta = c(0.004), # bag
                                       gamma = c(0),
                                       colsample_bytree = c(0.55), #0.55
                                       min_child_weight = c(7)),#,7,9
                subsample = 0.6, #0.7
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
                   tuneGrid = expand.grid(vars = c(100)),
                   B = 35, 
                   bagControl = bagControl(fit = xgbBag$fit,
                                           predict = xgbBag$pred,
                                           aggregate = xgbBag$aggregate),
                   objective = "binary:logistic",
                   nrounds = 2000, 
                   max_depth = 5,
                   eta = 0.004,
                   subsample = 0.8, 
                   colsample_bytree = c(0.55),
                   min_child_weight = 7,
                   stratified = TRUE,
                   bag.fraction = 0.8)


set.seed(642891)
xgbBagFit1 <- train(train, FactorResponse, 
                   method = "bag", 
                   metric = "ROC", 
                   maximize = TRUE,
                   trControl = trCtrlBag,
                   tuneGrid = expand.grid(vars = c(110)),
                   B = 35, 
                   bagControl = bagControl(fit = xgbBag$fit,
                                           predict = xgbBag$pred,
                                           aggregate = xgbBag$aggregate),
                   objective = "binary:logistic",
                   nrounds = 2000, 
                   max_depth = 5,
                   eta = 0.004,
                   subsample = 0.6, 
                   colsample_bytree = c(0.55),
                   min_child_weight = 7,
                   stratified = TRUE,
                   bag.fraction = 1)

submission$TARGET <- predict(xgbBagFit1, test, "prob")[[1]]
write.csv(submission, "submission.csv", row.names=FALSE)

