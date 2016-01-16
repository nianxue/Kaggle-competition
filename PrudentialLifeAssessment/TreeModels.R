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

trCtrl <- trainControl(method = "cv",
                       # repeats = 5,
                       number = 5,
                       returnData = FALSE,
                       savePredictions = T)

set.seed(849)
xgbAllLinearL <- train(train, Response, 
                      method = "xgbTree", 
                      objective = "reg:linear",
                      trControl = trCtrl,
                      metric = "RMSE", 
                      tuneGrid = expand.grid(nrounds = c(3500), #
                                             max_depth = c(10),
                                             eta = c(0.0045), 
                                             gamma = 0, 
                                             colsample_bytree = c(0.6), #0.6
                                             min_child_weight = c(15)),
                      subsample = 0.75, #0.85
                      # early.stop.round = 100,
                      stratified = TRUE,
                      verbose = 1)


set.seed(849)
xgbAllLinear <- train(train, Response, 
                        method = "xgbTree", 
                        objective = "reg:linear",
                        trControl = trCtrl,
                        metric = "RMSE", 
                        tuneGrid = expand.grid(nrounds = c(2200), #
                                               max_depth = c(10),
                                               eta = c(0.0075), 
                                               gamma = 0, 
                                               colsample_bytree = c(0.6), #0.6
                                               min_child_weight = c(15)),
                        subsample = 0.75, #0.85
                        # early.stop.round = 100,
                        stratified = TRUE,
                        verbose = 1)

set.seed(849)
xgbAllPoissonL <- train(train, Response, 
                       method = "xgbTree", 
                       objective = "count:poisson",
                       trControl = trCtrl,
                       metric = "RMSE", 
                       maximize = F,
                       tuneGrid = expand.grid(nrounds = c(5300), #
                                              max_depth = c(7), #6 1.8191
                                              eta = c(0.0075), 
                                              gamma = 0, 
                                              colsample_bytree = c(0.5), #0.6
                                              min_child_weight = c(15)),
                       subsample = 0.6, #0.85
                       # early.stop.round = 100,
                       stratified = TRUE,
                       verbose = 1)


set.seed(849)
xgbAllPoisson <- train(train, Response, 
                       method = "xgbTree", 
                       objective = "count:poisson",
                       trControl = trCtrl,
                       metric = "RMSE", 
                       maximize = F,
                       tuneGrid = expand.grid(nrounds = c(4100), #
                                              max_depth = c(7), #6 1.8191
                                              eta = c(0.01), 
                                              gamma = 0, 
                                              colsample_bytree = c(0.5), #0.6
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
               tuneGrid = expand.grid(mtry = c(50)),#25
               num.trees = 500,
               replace = F,
               min.node.size = 7) #5/1.863



trCtrlBag <- trainControl(method = "cv",
                           number = 4,
                           returnData = FALSE,
                           savePredictions = T)


set.seed(6421)
xgbAllLinearBag <- train(x = train, 
                        y = Response, 
                        method = "bag", 
                        metric = "RMSE", 
                        maximize = F,
                        trControl = trCtrlBag,
                        tuneGrid = expand.grid(vars = c(100)),
                        B = 25, 
                        bagControl = bagControl(fit = xgbBag$fit,
                                                predict = xgbBag$pred,
                                                aggregate = xgbBag$aggregate,
                                                oob = T),
                        objective = "reg:linear",
                        nrounds = 2200, 
                        max_depth = 10,
                        eta = 0.0075,
                        subsample = 0.75, 
                        colsample_bytree = 0.6,
                        min_child_weight = 15,
                        stratified = TRUE,
                        bag.fraction = 0.75)


xgbAllImpLinear <- varImp(xgbAllLinear)$importance
xgbAllImpPoisson <- varImp(xgbAllPoisson)$importance

cvResult <- data.frame(obs = xgbAllLinearL$pred$obs,
                       xgb1 = xgbAllLinearL$pred$pred, 
                       xgb2 = xgbAllLinear$pred$pred, 
                       xgb3 = xgbAllPoisson$pred$pred, 
                       xgb4 = xgbAllPoissonL$pred$pred)

cvResult$pred <- apply(cvResult[,2:5], 1, mean)



cut <- as.vector(table(Response))
cut <- cumsum(cut)
cvResult <- arrange(cvResult, pred)
cut <- cvResult$pred[cut[1:7]]

# optCuts = optim(seq(1.5, 7.5, by = 1), SQWKfun, pred = cvResult)

optCuts = optim(cut, SQWKfun, pred = cvResult)

###CV score
cvPred <- as.numeric(Hmisc::cut2(cvResult$pred, optCuts$par))
ScoreQuadraticWeightedKappa(cvPred, cvResult$obs, 1, 8)

pred <- data.frame(xgb1 = predict(xgbAllLinear, test),
                   xgb2 = predict(xgbAllLinearL, test),
                   xgb3 = predict(xgbAllPoisson, test),
                   xgb4 = predict(xgbAllPoissonL, test))
pred$pred <- apply(pred, 1, mean)
pred <- as.numeric(Hmisc::cut2(pred$pred, optCuts$par))

submission$Response <- pred
write.csv(submission, "submission.csv", row.names=FALSE)
