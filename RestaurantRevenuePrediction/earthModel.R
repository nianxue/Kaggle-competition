library(plyr)
library(dplyr)
library(caret)
library(lubridate)
library(car)
library(doParallel)
library(glmnet)
library(earth)
library(randomForest)

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

data$duration <- log(data$duration)
logRevenue <- log(revenue)

#parallel calculation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# set repeatedcv for measure model performace
trCtrl <- trainControl(method = "repeatedcv", 
                       number = 30, 
                       repeats = 5, 
                       seeds = NULL,
                       returnData = FALSE,
                       savePredictions = T)



##########build earth model with all variabls
data.full <- model.matrix(~.- Id - 1, data = data)
train <- data.full[split == 1, ]
train <- as.data.frame(train)
test <- data.full[split != 1, ]
test <- as.data.frame(test)

set.seed(983)
earth <- train(x = train, 
               y = logRevenue, 
               method = "earth", 
               metric = "RMSE", 
               trControl = trCtrl, 
               tuneGrid = expand.grid(degree = 1,
                                      nprune = c(4)))


predEarth <- predict(earth, test)
predEarth <- exp(predEarth)
stopCluster(cl)
