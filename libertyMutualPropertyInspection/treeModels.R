## try:
# deep net
# mf

load("E:/KaggleProject/libertyMutualPropertyInspection/cleanData.RData")
cl <- makeCluster(4)
registerDoParallel(cl)
clusterExport(cl, varlist=c("NormalizedWeightedGini", "WeightedGini"))

################generate count features##################
temp <- data
dot <- list(~n())
for(name in factorVars){
    temp <- temp %>% 
        group_by_(name) %>% mutate_(.dots = setNames(dot, paste0(name, "_cnt")))
}

################add 2 tsne features
# temp <- bind_cols(temp, tsneData)

temp <- model.matrix(~.-1, data = temp)

train <- temp[split == 1,]
test <- temp[split == 0,]

############################################### xgb models##################

trCtrl <- trainControl(method = "cv", 
                       number = 10, 
                       returnResamp = "all",
                       returnData = F,
                       savePredictions = T,
                       summaryFunction = caretGini,
                       index = index)

# trCtrl <- trainControl(method = "cv", 
#                        number = 5, 
#                        returnResamp = "all",
#                        returnData = F,
#                        savePredictions = T,
#                        summaryFunction = caretGini)

###########################train glmnet model
prediction <- predict(earth, test)
submission$Hazard <- prediction
write.csv(submission,file='submission.csv', quote=FALSE,row.names=FALSE)

set.seed(849)
xgbPoiss <- train(train, Hazard, 
                 method = "xgbTree", 
                 objective = "count:poisson",
                 trControl = trCtrl,
                 metric = "Gini", 
                 maximize = TRUE,
                 tuneGrid = expand.grid(nrounds = c(2400), #3000
                                        max_depth = c(7),
                                        eta = c(0.005)),
                 subsample = 0.8, 
                 colsample_bytree = 0.5,
                 min_child_weight = 55,
                 # early.stop.round = 100,
                 stratified = TRUE,
                 verbose = 1)

xgbPoissCV <- arrange(xgbPoiss$pred, rowIndex)$pred
xgbPoissTest <- predict(xgbPoiss, test)


# importancexgbPoiss <- varImp(xgbPoiss)$importance
# importancexgbPoiss$name <- row.names(importancexgbPoiss)
# temp1 <- xgbPoiss$pred
# plot(temp$obs, temp$pred)


set.seed(849)
xgb <- train(train, Hazard, 
             method = "xgbTree", 
             objective = "reg:linear",
             trControl = trCtrl,
             metric = "Gini", 
             maximize = TRUE,
             tuneGrid = expand.grid(nrounds = c(700), #3000
                                    max_depth = c(9),
                                    eta = c(0.01)),
             subsample = 0.85, 
             colsample_bytree = 0.75,
             min_child_weight = 55,
             # gamma = 0.0075,
             # early.stop.round = 100,
             stratified = TRUE,
             verbose = 1)



xgbCV <- arrange(xgb$pred, rowIndex)$pred
xgbTest <- predict(xgb, test)


set.seed(6421)
rf <- train(train, Hazard, 
            method = "rf", 
            metric = "Gini", 
            maximize = TRUE,
            trControl = trCtrl,
            tuneGrid = expand.grid(mtry = c(40)),
            ntree = 400,
            nodesize = 35, #30
            importance = TRUE,
            replace = F,
            do.trace = TRUE)

rfCV <- arrange(rf$pred, rowIndex)$pred
rfTest <- predict(rf, test)


set.seed(6421)
gbmGauss <- train(train, Hazard, 
                method = "gbm", 
                metric = "Gini", 
                maximize = TRUE,
                trControl = trCtrl,
                tuneGrid = expand.grid(n.trees = c(300), 
                                       interaction.depth = c(9), 
                                       shrinkage = c(0.1),
                                       n.minobsinnode = c(100)),
                distribution = "gaussian",
                bag.fraction = 0.85)

gbmGaussCV <- arrange(gbmGauss$pred, rowIndex)$pred
gbmGaussTest <- predict(gbmGauss, test)


set.seed(6421)
gbmLapla <- train(train, Hazard, 
                  method = "gbm", 
                  metric = "Gini", 
                  maximize = TRUE,
                  trControl = trCtrl,
                  tuneGrid = expand.grid(n.trees = c(350), 
                                         interaction.depth = c(9), 
                                         shrinkage = c(0.1), 
                                         n.minobsinnode = c(100)),
                  distribution = "laplace",
                  bag.fraction = 0.85)

gbmLaplaCV <- arrange(gbmLapla$pred, rowIndex)$pred
gbmLaplaTest <- predict(gbmLapla, test)


set.seed(6421)
gbmPoiss <- train(train, Hazard, 
                  method = "gbm", 
                  metric = "Gini", 
                  maximize = TRUE,
                  trControl = trCtrl,
                  tuneGrid = expand.grid(n.trees = c(350), 
                                         interaction.depth = c(9), 
                                         shrinkage = c(0.1), 
                                         n.minobsinnode = c(100)),
                  distribution = "poisson",
                  bag.fraction = 0.85)

gbmPoissCV <- arrange(gbmPoiss$pred, rowIndex)$pred
gbmPoissTest <- predict(gbmPoiss, test)
