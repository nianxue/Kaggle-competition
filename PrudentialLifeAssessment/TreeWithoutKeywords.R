library(xgboost)
library(plyr)
library(dplyr)
library(ranger)
library(extraTrees)
library(caret)
library(doParallel)
library(stringr)
library(Hmisc)
library(Metrics)


load("E:/KaggleProject/PrudentialLifeAssessment/treeData.RData")
setwd("E:/KaggleProject/PrudentialLifeAssessment")

Response <- Response$Response[split == 1]

cl <- makeCluster(4)
registerDoParallel(cl)
# stopCluster(cl)

Vars <- !str_detect(names(train), "Medical_Keyword")
Vars <- c(names(train)[Vars], 
          "Medical_Keyword_Sum", "Medical_Keyword_3", "Medical_Keyword_15", "Medical_Keyword_38")

trCtrl <- trainControl(method = "cv",
                       # repeats = 5,
                       number = 5,
                       returnData = FALSE,
                       savePredictions = T)

set.seed(849)
xgbAllLinear <- train(train[,Vars], Response, 
                      method = "xgbTree", 
                      objective = "reg:linear",
                      trControl = trCtrl,
                      metric = "RMSE", 
                      tuneGrid = expand.grid(nrounds = c(1800,1900,2000,2100,2200,2300,2400), #
                                             max_depth = c(7,8,9),
                                             eta = c(0.0075), 
                                             gamma = 0, 
                                             colsample_bytree = c(0.6), #0.6
                                             min_child_weight = c(15)),
                      subsample = 0.75, #0.85
                      # early.stop.round = 100,
                      stratified = TRUE,
                      verbose = 1)


set.seed(849)
xgbAllPoisson <- train(train[,Vars], Response, 
                       method = "xgbTree", 
                       objective = "count:poisson",
                       trControl = trCtrl,
                       metric = "RMSE", 
                       maximize = F,
                       tuneGrid = expand.grid(nrounds = c(3500,3600,3800,3900,4000,4100), #
                                              max_depth = c(5,6), #6 1.8191
                                              eta = c(0.01), 
                                              gamma = 0, 
                                              colsample_bytree = c(0.6), #0.6
                                              min_child_weight = c(15)),
                       subsample = 0.6, #0.85
                       # early.stop.round = 100,
                       stratified = TRUE,
                       verbose = 1)



set.seed(849)
RFAll <- train(train, Response, 
               method = "ranger", 
               trControl = trCtrl,
               metric = "RMSE", 
               maximize = F,
               tuneGrid = expand.grid(mtry = c(50,75,90)),#25
               num.trees = 500,
               min.node.size = 7) #5/1.863


xgbAllImpLinear <- varImp(xgbAllLinear)$importance
xgbAllImpPoisson <- varImp(xgbAllPoisson)$importance

cvResult <- xgbAllLinearBag$pred
optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = cvResult)

###CV score
cvPred <- as.numeric(Hmisc::cut2(cvResult$pred, optCuts$par))
ScoreQuadraticWeightedKappa(cvPred, cvResult$obs, 1, 8)

pred <- predict(xgbAllLinear, test)
pred <- as.numeric(Hmisc::cut2(pred, optCuts$par))

submission$Response <- pred
write.csv(submission, "submission.csv", row.names=FALSE)
