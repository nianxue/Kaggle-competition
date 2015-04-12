load("E:/KaggleProject/RestaurantRevenuePrediction/cleanData.RData")

data$duration <- log(data$duration)
data <- model.matrix(~.- Id - 1, data = data)

revenue[revenue > 8.0e+6] = 8.0e+6
logRevenue <- log(revenue)

train <- data[split == 1, ]
test <- data[split != 1, ]


##build linear models
# lm1 <- glm(logRevenue ~ P285 + CityAnkara + duration, family = "gaussian", data = as.data.frame(train))
# lm1.pred <- predict(lm1, newdata = as.data.frame(test))


#build linear models
cl <- makeCluster(detectCores())
registerDoParallel(cl)


### set rfe parameters, 25 times of 25folds cross validation is employed because 
### the data set is small.



# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "repeatedcv", 
                       number = 25, 
                       repeats = 10, 
                       returnData = FALSE,
                       savePredictions = FALSE)


set.seed(4632)
earth <- train(x = train,
               y = logRevenue, 
               method = "earth", 
               metric = "RMSE", 
               trControl = trCtrl,
               tuneGrid = expand.grid(degree = 1, 
                                       nprune = seq(2,6,1)), 
               nk = 21,
               minspan = 20,
               endspan = 40              
               )
pred <- predict(earth, test)
submission$Prediction <- exp(pred)
write.csv(submission, "submission.csv", row.names=FALSE)

stopCluster(cl)
