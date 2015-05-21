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
library(pROC)


setwd("E:/KaggleProject/RestaurantRevenuePrediction")
trainIni <- read.csv("E:/KaggleProject/RestaurantRevenuePrediction/train.csv/train.csv")
testIni <- read.csv("E:/KaggleProject/RestaurantRevenuePrediction/test.csv/test.csv")
submission <- read.csv("E:/KaggleProject/RestaurantRevenuePrediction/sampleSubmission.csv")

#code outliers
large <- rep("normal", times = nrow(trainIni))
large[trainIni$revenue > 4.5e+6] <- "large"  # 7e+6
large <- factor(large, levels = c("large", "normal"))

# small <- rep("normal", times = nrow(trainIni))
# small[trainIni$revenue > 6e+6] <- "small"  # 7e+6
# small <- factor(small, levels = c("small", "normal"))

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


cl <- makeCluster(detectCores())
registerDoParallel(cl)



# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "repeatedcv", 
                       number = 30, 
                       repeats = 15,
                       seeds = NULL,
                       classProbs = T,
                       summaryFunction = twoClassSummary,
                       returnData = FALSE,
                       savePredictions = T)

set.seed(4632)
classLarge <- train(x = train，
                   y = large, 
                   method = "rf", 
                   importance = T,
                   metric = "ROC",
                   trControl = trCtrl,
                    tuneGrid = expand.grid(mtry = c(15))
    #                tuneLength = 5  
                   )

trainPred <- classLarge$pred
trainPred <- trainPred %>% group_by(rowIndex) %>% summarise(large = median(large))
trainPred <- trainPred$large

testPred <- predict(classLarge, test, type = "prob")$large


# Roc <- roc(response = classLarge$pred$obs,
#            predictor = classLarge$pred$large,
#            levels = rev(levels(classLarge$pred$obs)))
# plot(Roc, legacy.axes = TRUE)
# 
# plot(trainPred, revenue)
# # 
# nian <- varImp(rfFull300)$importance
# nian$var <- rownames(nian)
# nian <- arrange(nian, desc(Overall))
# var <- nian$var[1:23]

