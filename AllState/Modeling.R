library(xgboost)
library(plyr)
library(dplyr)
library(ranger)
# library(extraTrees)
library(caret)
# library(doParallel)
library(Metrics)

source('E:/KaggleProject/AllState/myFuns.R')
load("E:/KaggleProject/AllState/data.RData")

# cl <- makeCluster(3)
# registerDoParallel(cl)
# clusterExport(cl, "fairobj")
# stopCluster(cl)


trCtrl <- trainControl(method = "cv",
                       # repeats = 5,
                       number = 3,
                       summaryFunction = CaretMAE,
                       returnData = FALSE,
                       savePredictions = T)

dtrain <- xgb.DMatrix(data = as.matrix(train_Count), label = log_target200)
dtest <- xgb.DMatrix(data = as.matrix(test_Count))

xgb_params  <- list(max_depth = 12,
                    eta = 0.025, 
                    min_child_weight = 120,
                    gamma = 0.075,
                    objective = fairobj,
                    eval_metric = xg_eval_mae,
                    subsample = 0.85, 
                    colsample_bytree = 0.35)

set.seed(7652)
xgb_fair_cv <- xgb.cv(data = dtrain,
                      params = xgb_params,
                      nround = 50, 
                      nfold = 3,
                      stratified = TRUE,
                      prediction = TRUE,
                      maximize = FALSE,
                      early.stop.round = 100,
                      print.every.n = 30,
                      verbose = 1)

best_nrounds = xgb_fair$best_iteration
cv_mean = xgb_fair$evaluation_log$test_error_mean[best_nrounds]
cv_std = xgb_fair$evaluation_log$test_error_std[best_nrounds]
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

xgb_fair = xgb.train(xgb_params, dtrain, nrounds=best_nrounds)
out <- exp(predict(gbdt,dtest))


set.seed(8969)
xgb_Count <- train(train_Count, log_target200, 
                   method = "xgbTree", 
                   objective = "reg:linear",
                   trControl = trCtrl,
                   metric = "MAE", 
                   maximize = FALSE,
                   tuneGrid = expand.grid(nrounds = 16000, #1100
                                          max_depth = c(12), # 
                                          eta = c(0.002), #0.025
                                          gamma = c(1), #
                                          colsample_bytree = c(0.35),
                                          min_child_weight = c(135),
                                          subsample = c(0.85)), 
                   # colsample_bylevel = 0.75,
                   alpha = 0,
                   lambda = 1,
                   # early.stop.round = 100,
                   stratified = TRUE,
                   verbose = 1)


set.seed(8969)
xgb_Count_poisson <- train(train_Count, log_target200, 
                           method = "xgbTree", 
                           objective = "count:poisson",
                           trControl = trCtrl,
                           metric = "MAE", 
                           maximize = FALSE,
                           tuneGrid = expand.grid(nrounds = 14000, #3100
                                                  max_depth = c(12), # 
                                                  eta = c(0.002), 
                                                  gamma = c(0.075), #
                                                  colsample_bytree = c(0.35),
                                                  min_child_weight = c(120),
                                                  subsample = 0.95), 
                           alpha = 0,
                           lambda = 1,
                           # early.stop.round = 100,
                           stratified = TRUE,
                           verbose = 1)





print(xgb_Count, showSD = T)
pred_train <- predict(xgb_Count_poisson, train_Count)
mae(target, exp(pred_train) - 200)

submission$loss <- exp(predict(xgb_Count, test_Count)) - 200
submission <- write_csv(submission, "submission.csv")

set.seed(8969)
xgb_Count <- train(train_Count, log_target200, 
                   method = "xgbTree", 
                   objective = "reg:linear",
                   trControl = trCtrl,
                   metric = "MAE", 
                   maximize = FALSE,
                   tuneGrid = expand.grid(nrounds = seq(800, 1500, 100), #1100
                                          max_depth = c(10,11,12), # 
                                          eta = c(0.025), #0.025
                                          gamma = c(1), #
                                          colsample_bytree = c(0.35),
                                          min_child_weight = c(75,111,135),
                                          subsample = c(0.85)), 
                   # colsample_bylevel = 0.75,
                   alpha = 0,
                   lambda = 1,
                   # early.stop.round = 100,
                   stratified = TRUE,
                   verbose = 1)

varImportance <- varImp(xgb_Count)

getInteractionMap(names(train_Count))
xgb.dump(model = xgb_Count$finalModel, fname = "xgb.dump", fmap = "xgb.fmap", with.stats = TRUE)
  

