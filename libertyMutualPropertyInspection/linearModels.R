## try:
# deep net
# mf

load("E:/KaggleProject/libertyMutualPropertyInspection/cleanData.RData")

################################train factor vars with glmnet models
temp <- model.matrix(~.-1, data = data)

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
#                        number = 3, 
#                        returnResamp = "all",
#                        returnData = F,
#                        savePredictions = T,
#                        summaryFunction = caretGini)

###########################train glmnet model

set.seed(983)
glmnetPoiss <- train(x = train, 
                    y = Hazard, 
                    method = "glmnet", 
                    family = "poisson", #
                    metric = "Gini", 
                    maximize = TRUE,
                    trControl = trCtrl, 
                    # penalty.factor = p.factor,
                    # tuneLength = 9,
                    preProcess = c("center", "scale"),
                     tuneGrid = expand.grid(alpha = c(1),
                                            lambda = c(0.005))
                    )

glmnetPoissCV <- arrange(glmnetPoiss$pred, rowIndex)$pred
glmnetPoissTest <- predict(glmnetPoiss, test)


set.seed(983)
glmnetGauss <- train(x = train[,], 
                    y = Hazard, 
                    method = "glmnet", 
                    family = "gaussian", #
                    metric = "Gini", 
                    maximize = TRUE,
                    trControl = trCtrl, 
                    # penalty.factor = p.factor,
                    # tuneLength = 9,
                    preProcess = c("center", "scale"),
                    tuneGrid = expand.grid(alpha = c(1),
                                           lambda = c(0.01))
                    )



importance <- varImp(glmnetGauss)$importance
importance$name <- row.names(importance)
# temp1 <- xgbPoiss$pred
# plot(temp$obs, temp$pred)

glmnetGaussCV <- arrange(glmnetGauss$pred, rowIndex)$pred
glmnetGaussTest <- predict(glmnetGauss, test)


set.seed(849)
xgbLinear <- train(train, Hazard, 
                   method = "xgbLinear", 
                   trControl = trCtrl,
                   metric = "Gini", 
                   maximize = TRUE,
                   objective = "reg:linear",
                   tuneGrid = expand.grid(nrounds = c(17),
                                          lambda = c(0.005),
                                          alpha = c(0.8)),
                   stratified = TRUE,
                   verbose = 1)

xgbLinearCV <- arrange(xgbLinear$pred, rowIndex)$pred
xgbLinearTest <- predict(xgbLinear, test)


set.seed(983)
earth <- train(x = train, 
                y = Hazard, 
                method = "earth", 
                metric = "Gini", 
                maximize = TRUE,
                trControl = trCtrl, 
                tuneGrid = expand.grid(nprune = c(37), 
                                       degree = c(2))
              )


earthCV <- arrange(earth$pred, rowIndex)$pred
earthTest <- predict(earth, test)


# prediction <- predict(xgb, test)
# submission$Hazard <- prediction
# write.csv(submission,file='submission.csv', quote=FALSE,row.names=FALSE)
