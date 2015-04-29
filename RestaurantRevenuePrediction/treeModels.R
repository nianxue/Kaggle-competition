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

#remove outliers
outliers <- trainIni$revenue > 1.0e+7
trainIni <- trainIni[!outliers,]

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

train <- data[split == 1, ]
train <- as.data.frame(train)
test <- data[split != 1, ]
test <- as.data.frame(test)


treeVars = c("P28","CityAstanbul","duration","P1","P20","CityAnkara","P23","P29",
            "P17","P21","P22","P26","P16","P36","P18","P19","P32","City.GroupOther",
            "CityotherCity","P6","P37")

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
rfFull700 <- train(x = train,
                   y = revenue, 
                   method = "parRF", 
                   metric = "RMSE", 
                   trControl = trCtrl,
                   tuneGrid = expand.grid(mtry = c(13)),
                   ntree = 700,
                   nodesize = 2,
                   importance = TRUE
                   )

predRfFull700 <- predict(rfFull700, test)


set.seed(6421)
rfFull300 <- train(x = train,
            y = revenue, 
            method = "parRF", 
            metric = "RMSE", 
            trControl = trCtrl,
            tuneGrid = expand.grid(mtry = c(11)),
            ntree = 300,
            nodesize = 9,
            importance = TRUE)


predRfFull300 <- predict(rfFull300, test)


set.seed(6421)
rfVars300 <- train(x = train[,treeVars],
                   y = revenue, 
                   method = "parRF", 
                   metric = "RMSE", 
                   trControl = trCtrl,
                   tuneGrid = expand.grid(mtry = c(9)),
                   ntree = 300,
                   nodesize = 9,
                   importance = TRUE)

predRfVars300 <- predict(rfVars300, test[,treeVars])



set.seed(6421)
rfVars700 <- train(x = train[,treeVars],
                   y = revenue, 
                   method = "parRF", 
                   metric = "RMSE", 
                   trControl = trCtrl,
                   tuneGrid = expand.grid(mtry = c(9)),
                   ntree = 700,
                   nodesize = 2,
                   importance = TRUE)

predRfVars700 <- predict(rfVars700, test[,treeVars])




set.seed(6421)
extraFull200 <- train(x = train,
                    y = revenue, 
                    method = "extraTrees", 
                    metric = "RMSE", 
                    trControl = trCtrl,
                    tuneGrid = expand.grid(mtry = 7,
                                           numRandomCuts = 14),
                    ntree = 200,
                    nodesize = 9)

predExtraFull200 <- predict(extraFull200, test)



set.seed(6421)
extraFull700 <- train(x = train,
                      y = revenue, 
                      method = "extraTrees", 
                      metric = "RMSE", 
                      trControl = trCtrl,
                      tuneGrid = expand.grid(mtry = 9,
                                             numRandomCuts = 11),
                      ntree = 700,
                      nodesize = 2)

predExtraFull700 <- predict(extraFull700, test)



set.seed(6421)
extraVars200 <- train(x = train[,treeVars],
                        y = revenue, 
                        method = "extraTrees", 
                        metric = "RMSE", 
                        trControl = trCtrl,
                        tuneGrid = expand.grid(mtry = 13,
                                               numRandomCuts = 9),
                        ntree = 200,
                        nodesize = 9)
predExtraVars200 <- predict(extraVars200, test[,treeVars])



set.seed(6421)
extraVars700 <- train(x = train[,treeVars],
                        y = revenue, 
                        method = "extraTrees", 
                        metric = "RMSE", 
                        trControl = trCtrl,
                        tuneGrid = expand.grid(mtry = c(9),
                                               numRandomCuts = c(13)),
                        ntree = 700,
                        nodesize = 2)
predExtraVars700 <- predict(extraVars700, test[,treeVars])



set.seed(6421)
M5 <- train(x = train[,treeVars],
          y = revenue, 
          method = "M5", 
          metric = "RMSE", 
          tuneGrid = expand.grid(pruned = c("No"), 
                                 smoothed = c("Yes"), 
                                 rules = c("No")),
          control = Weka_control(M = 10),
          trControl = trCtrl)
predM5 <- predict(M5, test[,treeVars])



set.seed(6421)
cubist <- train(x = train[,treeVars],
                y = revenue, 
                method = "cubist", 
                metric = "RMSE", 
                tuneGrid = expand.grid(committees = c(12),
                                       neighbors = c(9)),
                trControl = trCtrl)
predCubist <- predict(cubist, test[,treeVars])


set.seed(6421)
bagGbm <- train(x = train[,treeVars],
                y = revenue, 
                method = "bag", 
                metric = "RMSE", 
                trControl = trCtrl,
                tuneGrid = expand.grid(vars = c(14)),
                B = 70,
                bagControl = bagControl(fit = gbmBag$fit,
                                        predict = gbmBag$pred,
                                        aggregate = gbmBag$aggregate),
                n.trees = 70,
                interaction.depth = 6,
                shrinkage = 0.07,
                distribution = "gaussian",
                bag.fraction = 0.8,
                n.minobsinnode = 5)
predBagGbm <- predict(bagGbm, test[,treeVars])

stopCluster(cl)

