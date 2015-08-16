## try:
# deep net
# mf

load("E:/KaggleProject/libertyMutualPropertyInspection/cleanData.RData")
cl <- makeCluster(4)
registerDoParallel(cl)
clusterExport(cl, varlist=c("NormalizedWeightedGini", "WeightedGini"))
source('E:/KaggleProject/bagFuns.R')
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
                       number = 5, 
                       returnResamp = "all",
                       returnData = F,
                       savePredictions = T,
                       summaryFunction = caretGini)
###########################train glmnet model
# 0.3847319
set.seed(6421)
bagXgb <- train(x = cvPred,
                y = Hazard, 
                method = "bag", 
                metric = "Gini", 
                maximize = TRUE,
                trControl = trCtrl,
                tuneGrid = expand.grid(vars = c(18)),
                B = 100, 
                bagControl = bagControl(fit = xgbBag$fit,
                                        predict = xgbBag$pred,
                                        aggregate = xgbBag$aggregate),
                objective = "count:poisson",
                nrounds = 1300, 
                max_depth = 4,
                eta = 0.001,
                subsample = 0.75, 
                colsample_bytree = 0.45,
                min_child_weight = 40,
                stratified = TRUE,
                bag.fraction = 0.9)

bagXgbCV <- arrange(bagXgb$pred, rowIndex)$pred
bagXgbTest <- predict(bagXgb, test)

#0.3464705
set.seed(6421)
bagRF <- train(x = cvPred,
               y = Hazard, 
               method = "bag", 
               metric = "Gini", 
               maximize = TRUE,
               trControl = trCtrl,
               tuneGrid = expand.grid(vars = c(200)),
               B = 20,
               bagControl = bagControl(fit = rfBag$fit,
                                       predict = rfBag$pred,
                                       aggregate = rfBag$aggregate),
               mtry = 40,
               ntree = 400,
               nodesize = 35,
               bag.fraction = 0.8)


bagRFCV <- arrange(bagRF$pred, rowIndex)$pred
bagRFTest <- predict(bagRF, test)
