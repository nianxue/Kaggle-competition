library(h2o)
library(plyr)
library(dplyr)
library(caret)


localH2O <- h2o.init(nthread=-1,Xmx="11g")

source('E:/KaggleProject/Otto/caretMultiLogloss.R')
setwd("E:/KaggleProject/Otto")
train <- read.csv("E:/KaggleProject/Otto/train.csv/train.csv")
test <- read.csv("E:/KaggleProject/Otto/test.csv/test.csv")
submission <- read.csv("E:/KaggleProject/Otto/sampleSubmission.csv")
submission[,2:10] <- 0


for(i in 2:94){
    train[,i] <- as.numeric(train[,i])
    train[,i] <- sqrt(train[,i]+(3/8))
}


for(i in 2:94){
    test[,i] <- as.numeric(test[,i])
    test[,i] <- sqrt(test[,i]+(3/8))
}


predictors <- 2:(ncol(train)-1)
response <- ncol(train)


########################my cross validation
h2oCv <- vector(mode = "list", length = 3)

K = 10
for(i in 1:3){
    print(i)
    temp <- data.frame()
    folds <- createFolds(y = train$target,k = K)
    
    for(j in 1:K){
        print(j)
        train.hex <- as.h2o(localH2O,train[-folds[[j]],])
        test.hex <- as.h2o(localH2O,train[folds[[j]],2:94])
        
        cvModels <- h2o.deeplearning(x=predictors,
                                     y=response,
                                     training_frame=train.hex,
                                     activation="RectifierWithDropout",
                                     hidden=c(1024,512,256),
                                     hidden_dropout_ratio=c(0.5,0.5,0.5),
                                     input_dropout_ratio=0.05,
                                     classification_stop = -1,
                                     epochs=100,
                                     l1=1e-5,
                                     l2=1e-5,
                                     rho=0.99,
                                     epsilon=1e-8,
                                     train_samples_per_iteration =2000,
                                     max_w2=10,
                                     seed=1)
        cvPred <- as.data.frame(h2o.predict(cvModels,test.hex))
        cvPred$rowIndex <- folds[[j]]
        
        temp <- bind_rows(temp, cvPred)
    }
    
    h2oCv[[i]] <- temp
}


h2oCv1024 <- arrange(h2oCv[[1]], rowIndex)
h2oCv1024[,2:10] <- 0
h2oCv1024[,2:10] <- (arrange(h2oCv[[1]], rowIndex)[,2:10] + 
                    arrange(h2oCv[[2]], rowIndex)[,2:10] +
                    arrange(h2oCv[[3]], rowIndex)[,2:10])/3

h2oCv1024 <- h2oCv1024 %>% select(starts_with("class"))
names(h2oCv1024) <- paste0("h2o1024","class",1:9)

#######################build h2o nn models
train.hex <- as.h2o(localH2O,train)
test.hex <- as.h2o(localH2O,test[,2:94])

h2oPred <- vector(mode = "list", 3)

for(i in 1:3){
    print(i)
    model <- h2o.deeplearning(x=predictors,
                              y=response,
                              training_frame=train.hex,
                              activation="RectifierWithDropout",
                              hidden=c(1024,512,256),
                              hidden_dropout_ratio=c(0.5,0.5,0.5),
                              input_dropout_ratio=0.05,
                              classification_stop = -1,
                              loss = "CrossEntropy",
                              epochs=100,
                              l1=1e-5,
                              l2=1e-5,
                              rho=0.99,
                              epsilon=1e-8,
                              train_samples_per_iteration =2000,
                              max_w2=10,
                              seed=1)
    
    h2oPred[[i]] <- as.data.frame(h2o.predict(model,test.hex))
}

h2oPred1024 <- h2oPred[[1]]
h2oPred1024[,2:10] <- 0
h2oPred1024[,2:10] <- (h2oPred[[1]][,2:10] + h2oPred[[2]][,2:10] + h2oPred[[3]][,2:10])/3

h2oPred1024 <- h2oPred1024 %>% select(starts_with("class"))
names(h2oPred1024) <- paste0("h2o1024","class",1:9)

