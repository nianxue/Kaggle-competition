load("E:/KaggleProject/RestaurantRevenuePrediction/cleanData.RData")

data[,2:23] <- as_data_frame(lapply(data[,2:23], as.factor))
data$duration <- log(data$duration)
data <- model.matrix(~.- Id - 1, data = data)

revenue[revenue > 8.0e+6] = 8.0e+6
logRevenue <- log(revenue)

correlation <- cor(data[split == 1, ], method = "spearman")
highCor <- findCorrelation(correlation, cutoff = 0.5)
data <- data[, -highCor]

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
                       repeats = 5, 
                       returnData = FALSE,
                       savePredictions = FALSE)

#set penalty factor for glmnet model
p.fac = rep(1, dim(train)[2])
p.fac[43] = 0.01

set.seed(4632)
glmnet <- train(x = train,
                y = logRevenue, 
                method = "glmnet", 
                family="gaussian",
                metric = "RMSE", 
                trControl = trCtrl,
                tuneGrid = expand.grid(alpha = c(0.1, 0.2, 0.3, 0.35, 0.4, 0.5, 0.6, 0.7), 
                                       lambda = c(0.05,0.1,0.15,0.2,0.5,10)),
#                dfmax = 5,
#                  pmax = 8,
                 penalty.factor = p.fac
                )
glmnetPred <- predict(glmnet, test)
submission$Prediction <- exp(glmnetPred)
write.csv(submission, "submission.csv", row.names=FALSE)

0.1000  3.0000000  2851666  0.2319482  1115468  0.2203007  0.9
0.2125  3.0000000  2750951  0.2355501  1142775  0.2187419  0.7
0.1000  3.0000000  2702419  0.1894374  1091733  0.2164081  0.5
0.1000  3.0000000  2506590  0.1747579  1107517  0.1924532  0.3
0.1000  3.0000000  2229493  0.2527332  1162107  0.2526489
2207934
revenue[revenue>=8.0e+06] = 8.0e+06
1964180
stopCluster(cl)
