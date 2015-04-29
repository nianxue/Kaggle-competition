
library(gam)
predictions <- vector(mode = "list")
cvPredictions <- vector(mode = "list")
source('E:/KaggleProject/RestaurantRevenuePrediction/bagFuns.R')

########Svm model
source.with.encoding('E:/KaggleProject/RestaurantRevenuePrediction/svmModel.R', 
                     encoding='UTF-8')
predictions$svm <- predSVM


########glmnet models
source.with.encoding('E:/KaggleProject/RestaurantRevenuePrediction/linearModels.R', 
                     encoding='UTF-8')
predictions$glmnet.num <- predGlmnet.num
predictions$glmnet.factor <- predGlmnet.factor


########earth model
source.with.encoding('E:/KaggleProject/RestaurantRevenuePrediction/earthModel.R', 
                     encoding='UTF-8')
predictions$earth <- predEarth


########tree models
source.with.encoding('E:/KaggleProject/RestaurantRevenuePrediction/treeModels.R', 
                     encoding='UTF-8')
predictions$RfFull700 <- predRfFull700
predictions$RfFull300 <- predRfFull300
predictions$RfVars300 <- predRfVars300
predictions$RfVars700 <- predRfVars700
predictions$ExtraFull200 <- predExtraFull200
predictions$ExtraFull700 <- predExtraFull700
predictions$ExtraVars200 <- predExtraVars200
predictions$ExtraVars700 <- predExtraVars700
predictions$Cubist <- predCubist
predictions$BagGbm <- predBagGbm


set.seed(983)
stack <- train(revenue ~., 
               data = cvPredictions, 
               method = "glmnet", 
               metric = "RMSE", 
               trControl = trCtrl, 
               tuneLength = 5)
temp <- svm$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$svm <- exp(temp$pred)

temp <- glmnet.num$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$glmnet.num <- exp(temp$pred)

temp <- glmnet.factor$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$glmnet.factor <- exp(temp$pred)

temp <- earth$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$earth <- exp(temp$pred)

temp <- rfFull700$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$RfFull700 <- temp$pred

temp <- rfFull300$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$RfFull300 <- temp$pred

temp <- rfVars300$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$RfVars300 <- temp$pred

temp <- rfVars700$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$RfVars700 <- temp$pred

temp <- extraFull200$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$ExtraFull200 <- temp$pred

temp <- extraFull700$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$ExtraFull700 <- temp$pred

temp <- extraVars200$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$ExtraVars200 <- temp$pred

temp <- extraVars700$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$ExtraVars700 <- temp$pred

temp <- M5$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$M5 <- temp$pred

temp <- cubist$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$Cubist <- temp$pred

temp <- bagGbm$pred %>% group_by(rowIndex) %>% summarise(pred = median(pred))
cvPredictions$BagGbm <- temp$pred



predictions <- as.data.frame(predictions)
cvPredictions <- as.data.frame(cvPredictions)


#cvPredictions$revenue <- revenue

cl <- makeCluster(detectCores())
registerDoParallel(cl)

# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "repeatedcv", 
                       number = 30, 
                       repeats = 5, 
                       seeds = NULL,
                       returnData = FALSE,
                       savePredictions = T)





set.seed(721)
RFE.glmnet <- rfe(x = cvPredictions[,1:15],
                   cvPredictions$revenue,
                   sizes = varSeq,
                   rfeControl = rfeCtrl,
                   metric = "RMSE",
                   ## Now arguments to train() are used.
                   method = "glmnet",
                   tuneLength = 3,
                   trControl = trCtrl)

set.seed(721)
rf <- train(x = cvPredictions[,1:15],
              cvPredictions$revenue,
              metric = "RMSE",
              ## Now arguments to train() are used.
              method = "rf",
              tuneGrid = expand.grid(mtry = c(3:11)),
              trControl = trCtrl,
              importance = TRUE)

set.seed(721)
gbm <- train(x = cvPredictions[,1:15],
            cvPredictions$revenue,
            metric = "RMSE",
            ## Now arguments to train() are used.
            method = "gbm",
            tuneLength = 5,
            trControl = trCtrl)



set.seed(983)
gamEnsemble<- gam(revenue~.-1,
                  data = cvPredictions,
                  family = gaussian)

RMSE(predSVM, revenue)    


predEnsemble <- predict(gamEnsemble, predictions)
submission$Prediction <- predictions$BagGbm
write.csv(submission, "submissionBagGbm.csv", row.names=FALSE)
