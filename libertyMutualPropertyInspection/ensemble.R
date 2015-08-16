#try bag with meta rf
#try xgb with different min_child_weight

############################################ ensemble models

cvPred <-  xgbFac2CV %>% bind_cols(xgbFac5CV)
cvPred$rf <- rfCV
cvPred$xgb <- xgbCV
cvPred$xgbPoiss <- xgbPoissCV
# cvPred$gbmGauss <- gbmGaussCV
# cvPred$gbmLapla <- gbmLaplaCV
# cvPred$gbmPoiss <- gbmPoissCV
cvPred$glmnetPoiss <- glmnetPoissCV
cvPred$glmnetGauss <- glmnetGaussCV
cvPred$xgbLinear <- xgbLinearCV
cvPred$earth <- earthCV
# cvPred$bagXgbCV <- bagXgbCV
# cvPred$bagRFCV <- bagRFCV


testPred <-  xgbFac2Test %>% bind_cols(xgbFac5Test)
testPred$rf <- rfTest
testPred$xgb <- xgbTest
testPred$xgbPoiss <- xgbPoissTest
# testPred$gbmGauss <- gbmGaussTest
# testPred$gbmLapla <- gbmLaplaTest
# testPred$gbmPoiss <- gbmPoissTest
testPred$glmnetPoiss <- glmnetPoissTest
testPred$glmnetGauss <- glmnetGaussTest
testPred$xgbLinear <- xgbLinearTest
testPred$earth <- earthTest
# testPred$bagXgbTest <- bagXgbTest
# testPred$bagRFTest <- bagRFTest
############################tune xgb parameters

trCtrl <- trainControl(method = "cv", 
                       number = 5, 
                       returnResamp = "all",
                       returnData = F,
                       savePredictions = T,
                       summaryFunction = caretGini)


set.seed(849)
xgbEnsemble <- train(cvPred, Hazard, 
                      method = "xgbTree", 
                      objective = "count:poisson", #reg:linear
                      trControl = trCtrl,
                      metric = "Gini", 
                      maximize = TRUE,
                      tuneGrid = expand.grid(nrounds = c(600,700,800,900,1000,1100,1200,1300), #3000
                                             max_depth = c(2,3,4),
                                             eta = c(0.002)),
                      subsample = 0.75, 
                      colsample_bytree = 0.45,
                      min_child_weight = 35,
                      # early.stop.round = 100,
                      stratified = TRUE,
                      verbose = 1)

set.seed(6421)
rfEnsemble <- train(cvPred, Hazard, 
                    method = "rf", 
                    metric = "Gini", 
                    maximize = TRUE,
                    trControl = trCtrl,
                    tuneGrid = expand.grid(mtry = c(1)),
                    ntree = 500,
                    nodesize = 70, #30
                    importance = TRUE,
                    replace = F,
                    do.trace = TRUE)

xgbEnsembleImp <- varImp(xgbEnsemble)$importance
xgbEnsembleImp$name <- row.names(xgbEnsembleImp)
temp <- xgbEnsemble$pred
plot(temp$obs, temp$pred)

prediction <- predict(bagXgb, testPred)
submission$Hazard <- prediction
write.csv(submission,file='submission.csv', quote=FALSE,row.names=FALSE)

submission$Hazard <- gbmLaplaTest
