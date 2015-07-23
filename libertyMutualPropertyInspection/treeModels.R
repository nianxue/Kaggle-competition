## try:
# factorise Hazard, 
# change factor features to numerical,
# deep net
# mf
# feature engineering

load("E:/KaggleProject/libertyMutualPropertyInspection/cleanData.RData")

################################get train and test
data <- model.matrix(~.-1, data = data)
train <- data[split == 1,]
test <- data[split == 0,]

cl <- makeCluster(2)
registerDoParallel(cl)
clusterExport(cl, varlist=c("NormalizedWeightedGini", "WeightedGini"))

############################################### xgb models##################
trCtrlFac <- trainControl(method = "cv", 
                          number = 3, 
                          returnResamp = "all",
                          returnData = F,
                          savePredictions = TRUE,
                          classProbs = TRUE,
                          summaryFunction = caretGini)


trCtrl <- trainControl(method = "cv", 
                       number = 3, 
                       returnResamp = "all",
                       returnData = F,
                       savePredictions = T,
                       summaryFunction = caretGini)

set.seed(849)
xgbFactor <- train(train, Hazard, 
                   method = "xgbTree", 
                   trControl = trCtrl,
                   metric = "Gini", 
                   maximize = TRUE,
                   objective = "multi:softprob",
                   tuneGrid = expand.grid(nrounds = c(2300),
                                          max_depth = c(7),
                                          eta = c(0.005)),
                   subsample = 0.75, 
                   colsample_bytree = 0.4,
                   min_child_weight = 55,
                   # early.stop.round = 100,
                   stratified = TRUE,
                   verbose = 1)

set.seed(849)
xgb <- train(train, Hazard, 
             method = "xgbTree", 
             trControl = trCtrl,
             metric = "Gini", 
             maximize = TRUE,
             tuneGrid = expand.grid(nrounds = c(2300),
                                    max_depth = c(7),
                                    eta = c(0.005)),
             subsample = 0.75, 
             colsample_bytree = 0.4,
             min_child_weight = 55,
             # early.stop.round = 100,
             stratified = TRUE,
             verbose = 1)

xgbVarImp <- varImp(xgb)


set.seed(849)
xgbLinear <- train(train, Hazard, 
                   method = "xgbLinear", 
                   trControl = trCtrl,
                   metric = "Gini", 
                   maximize = TRUE,
                   objective = "reg:linear",
                   tuneGrid = expand.grid(nrounds = c(22),
                                          lambda = 0.1,
                                          alpha = c(0.0015)),
                   stratified = TRUE,
                   verbose = 1)

set.seed(6421)
rf <- train(train, Hazard, 
            method = "rf", 
            metric = "Gini", 
            maximize = TRUE,
            trControl = trCtrl,
            #            tuneLength = 2
            tuneGrid = expand.grid(mtry = c(25)),
            ntree = 300,
            nodesize = 10,
            importance = TRUE,
            do.trace = TRUE)


prediction <- predict(xgb, test)
submission$Hazard <- prediction
write.csv(submission,file='submission.csv', quote=FALSE,row.names=FALSE)
