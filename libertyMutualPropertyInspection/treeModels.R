
#load("E:/KaggleProject/libertyMutualPropertyInspection/cleanData.RData")
load("C:/NIAN/Correlation/libertyMutualPropertyInspection/libertyMutualPropertyInspection/clearnData.RData")
data <- model.matrix(~.-1, data = data)

################################get train and test
train <- data[split == 1,]
test <- data[split == 0,]

cl <- makeCluster(2)
registerDoParallel(cl)
clusterExport(cl, varlist=c("NormalizedWeightedGini", "WeightedGini"))

############################################### xgb models##################

trCtrl <- trainControl(method = "cv", 
                       number = 3, 
                       returnResamp = "all",
                       returnData = F,
                       savePredictions = T,
                       summaryFunction = caretGini)


set.seed(849)
xgb <- train(train, Hazard, 
             method = "xgbTree", 
             trControl = trCtrl,
             metric = "Gini", 
             maximize = TRUE,
             objective = "reg:linear",
             tuneGrid = expand.grid(nrounds = c(2000,2200,2400,2600),
                                    max_depth = c(7,8,9,10),
                                    eta = c(0.001,0.0025,0.005)),
             subsample = 0.75, #subsample ratio of the training instance 0.5
             colsample_bytree = 0.4,
             min_child_weight = 55,
             # early.stop.round = 100,
             stratified = TRUE,
             verbose = 1)


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


