library(plyr)
library(dplyr)
library(caret)
library(lubridate)
library(car)
library(doParallel)
library(glmnet)
library(earth)
library(randomForest)
library(extraTrees)
library(Cubist)
library(RWeka)
library(gbm)



setwd("E:/KaggleProject/RestaurantRevenuePrediction")
trainIni <- read.csv("E:/KaggleProject/RestaurantRevenuePrediction/train.csv/train.csv")
testIni <- read.csv("E:/KaggleProject/RestaurantRevenuePrediction/test.csv/test.csv")
submission <- read.csv("E:/KaggleProject/RestaurantRevenuePrediction/sampleSubmission.csv")


trainIni$revenue[trainIni$revenue > 1.0e+7] = 1.0e+7


revenue <- trainIni$revenue
trainIni$revenue <- NULL

trainIni$split <- 1
testIni$split <- 0

data <- bind_rows(trainIni, testIni)
split <- data$split

data$Open.Date <- mdy(data$Open.Date)
data$duration <- (mdy("3/1/2015") - data$Open.Date) / dyears(1)

data$City <- recode(data$City, "'Ä°stanbul' = 'Astanbul'; 'Ankara' = 'Ankara'; 'Ä°zmir' = 'Azmir';else = 'otherCity'")
data$Type <- recode(data$Type, "'IL' = 'IL'; else = 'FC'")


data$Open.Date <- NULL
data$split <- NULL


data <- model.matrix(~.- Id - 1, data = data)
data <- as.data.frame(data)

train <- data[split == 1, ]
train$large <- trainPred

correlation <- cor(train, method = "spearman")
highCorVars <- findCorrelation(correlation,cutoff = .95)
train <- train[,-highCorVars]


test <- data[split != 1, ]
test$large <- testPred
test <- test[,-highCorVars]



#build linear models
cl <- makeCluster(detectCores())
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)



# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "repeatedcv", 
                       number = 30, 
                       repeats = 5, 
                       seeds = NULL,
                       returnData = FALSE,
                       savePredictions = T)


set.seed(6421)
bagGbm <- train(x = train,
                y = revenue, 
                method = "bag", 
                metric = "RMSE", 
                trControl = trCtrl,
                tuneGrid = expand.grid(vars = c(11)),
                B = 60,
                bagControl = bagControl(fit = gbmBag$fit,
                                        predict = gbmBag$pred,
                                        aggregate = gbmBag$aggregate),
                n.trees = 70,
                interaction.depth = 5,
                shrinkage = 0.08,
                distribution = "gaussian",
                bag.fraction = 0.8,
                n.minobsinnode = 4)

predBagGbm <- predict(bagGbm, test)


set.seed(6421)
bagRF <- train(x = train,
               y = revenue, 
               method = "bag", 
               metric = "RMSE", 
               trControl = trCtrl,
               tuneGrid = expand.grid(vars = c(13)),
               B = 40,
               bagControl = bagControl(fit = rfBag$fit,
                                       predict = rfBag$pred,
                                       aggregate = rfBag$aggregate),
               mtry = 5,
               ntree = 300,
               nodesize = 3,
               bag.fraction = 0.8)

predBagRf <- predict(bagRF, test)



submission$Prediction <- predBagGbm
write.csv(submission, "submission.csv", row.names=FALSE)
stopCluster(cl)

