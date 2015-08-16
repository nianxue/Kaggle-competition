#treat it as a classfication problem and get the probability
#nb
#svm

temp <- data
dot <- list(~n())
for(name in factorVars){
    temp <- temp %>% 
        group_by_(name) %>% mutate_(.dots = setNames(dot, paste0(name, "_cnt")))
}

temp <- model.matrix(~.-1, data = temp)
train <- temp[split == 1, ]
test <- temp[split == 0, ]

############################get features for train data set
trCtrlFac <- trainControl(method = "cv", 
                          number = 10, 
                          returnData = F,
                          savePredictions = TRUE,
                          classProbs = TRUE,
                          index = index,
                          summaryFunction = caretMultiLogloss)

# trCtrlFac <- trainControl(method = "cv", 
#                           number = 5, 
#                           returnData = F,
#                           savePredictions = TRUE,
#                           classProbs = TRUE,
#                           index = index,
#                           summaryFunction = caretMultiLogloss)

set.seed(849)
xgbFacFit5 <- train(train, HazardFactor5, 
                    method = "xgbTree", 
                    trControl = trCtrlFac,
                    objective = "multi:softprob",
                    num_class = 5,
                    metric = "Logloss", 
                    maximize = F,
                    tuneGrid = expand.grid(nrounds = c(2200),
                                           max_depth = c(7),
                                           eta = c(0.005)),
                    subsample = 0.85, 
                    colsample_bytree = 0.55,
                    min_child_weight = 30,
                    # early.stop.round = 100,
                    stratified = TRUE,
                    verbose = 1)


set.seed(849)
xgbFacFit2 <- train(train, HazardFactor2, 
                   method = "xgbTree", 
                   trControl = trCtrlFac,
                   objective = "binary:logistic",
                   metric = "Logloss", 
                   maximize = F,
                   tuneGrid = expand.grid(nrounds = c(2100),
                                          max_depth = c(7),
                                          eta = c(0.005)),
                   subsample = 0.85, 
                   colsample_bytree = 0.55,
                   min_child_weight = 30,
                   # early.stop.round = 100,
                   stratified = TRUE,
                   verbose = 1)


xgbFac2CV <- arrange(xgbFacFit2$pred, rowIndex)[3:4]
colnames(xgbFac2CV) <- paste0("Fac2", colnames(xgbFac2CV))
xgbFac2CV$Fac2max <- apply(xgbFac2CV, 1, max)
xgbFac2CV$Fac2diff <- apply(xgbFac2CV, 1, function(x) max(x) - min(x))

xgbFac2Test <- predict(xgbFacFit2, test, type = "prob")
colnames(xgbFac2Test) <- paste0("Fac2", colnames(xgbFac2Test))
xgbFac2Test$Fac2max <- apply(xgbFac2Test, 1, max)
xgbFac2Test$Fac2diff <- apply(xgbFac2Test, 1, function(x) max(x) - min(x))


xgbFac5CV <- arrange(xgbFacFit5$pred, rowIndex)[3:7]
colnames(xgbFac5CV) <- paste0("Fac5", colnames(xgbFac5CV))
xgbFac5CV$Fac5max <- apply(xgbFac5CV, 1, max)
xgbFac5CV$Fac5diff <- apply(xgbFac5CV, 1, function(x) max(x) - min(x))

xgbFac5Test <- predict(xgbFacFit5, test, type = "prob")
colnames(xgbFac5Test) <- paste0("Fac5", colnames(xgbFac5Test))
xgbFac5Test$Fac5max <- apply(xgbFac5Test, 1, max)
xgbFac5Test$Fac5diff <- apply(xgbFac5Test, 1, function(x) max(x) - min(x))

