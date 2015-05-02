source(file = "base_funs.R")


############################### get the initial train data and feature engineer######################
ctrData <- read.csv.ffdf(file = "./train/train.csv", header=TRUE, 
                         colClasses = c("factor", rep("integer", 4),
                                        rep("factor", 9), rep("integer", 10)))
iniTrain <- ctrData[,]
iniTrain$id <- 1:nrow(iniTrain)
save(list = "iniTrain", file = "iniTrain.RData")


#feature engineering for initial train data
tidyTrainData <- featureEngineering(data = iniTrain)
rm(iniTrain)
gc()
save(list = "tidyTrainData", file = "tidyTrainData.RData")


############################### get the initial test data and feature engineer##############
data <- read.csv(file = "./test/test.csv", colClasses = c("id" = "character"))
save(list = "data", file = "iniTestData.RData")

#feature engineering for initial test data
tidyTestData <- featureEngineering(data = data)
save(list = "tidyTestData", file = "tidyTestData.RData")


#################################split data and train models######################
days <- yday(tidyTrainData$hour)

stack1 <- filter(tidyTrainData, days == 300)
save(list = "stack1", file = "stack1.RData")


stack2 <- filter(tidyTrainData, days == 301)
save(list = "stack2", file = "stack2.RData")


stack3 <- filter(tidyTrainData, days == 302)
save(list = "stack3", file = "stack3.RData")


for(i in 0:6){
    training <- filter(tidyTrainData, days == 294 + i)
    save(list = "training", file = paste("training", i, ".RData", sep = ""))
}

rm(tidyTrainData)
gc()

##set formula
Vars <- names(stack1[,4:35])
formula1 <- paste("click ~ ", paste(Vars, collapse = " + "), sep = "")
formula1 <- as.formula(formula1)

#get stack data for xgb
stack1$id <- stack1$hour <- NULL
stack1.x <- sparse.model.matrix(formula1, data = stack1)[, -1]
stackLabel <- stack1$click
dstack1 <- xgb.DMatrix(data = stack1.x, 
                        label = stackLabel)
rm(stack1)
rm(stack1.x)
gc()
xgb.DMatrix.save(dstack1, "dstack1")


stack2$id <- stack2$hour <- NULL
stack2.x <- sparse.model.matrix(formula1, data = stack2)[, -1]
stackLabel <- stack2$click
dstack2 <- xgb.DMatrix(data = stack2.x, 
                       label = stackLabel)
rm(stack2)
rm(stack2.x)
gc()
xgb.DMatrix.save(dstack2, "dstack2")


stack3$id <- stack3$hour <- NULL
stack3.x <- sparse.model.matrix(formula1, data = stack3)[, -1]
stackLabel <- stack3$click
dstack3 <- xgb.DMatrix(data = stack3.x, 
                       label = stackLabel)
rm(stack3)
rm(stack3.x)
gc()
xgb.DMatrix.save(dstack3, "dstack3")


for(i in 0:6){
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/training", i, ".RData", sep = "")
    load(filePath)
    training$id <- training$hour <-  NULL
    
    ##########################train xgb model###################
    #get training data for xgb    
    trainingLabel = training$click
    
    training.x <- sparse.model.matrix(formula, data = training)[, -1]
    dtraining <- xgb.DMatrix(data = training.x, 
                             label = trainingLabel)
    
    
    rm(training)
    rm(training.x)
    gc()
    xgb.DMatrix.save(dtraining, paste("dtraining", i, ".RData", sep = ""))
    
    
    ############################### train linear gbm model #################################
    watchlist <- list(train = dtraining)
    
    params.linear <- list(booster = "gblinear", 
                          objective = "binary:logistic",
                          #subsample = 0.1, #subsample ratio of the training instance
                          #colsample_bytree = 0.2,
                          alpha = 1, 
                          lambda = 5,
                          #eval.metric = c("logloss"),
                          silence = 1)
    set.seed(8763)
    bst_linear <- xgb.train(data=dtraining, 
                     nround = 2, 
                     params = params.linear,
                     watchlist=watchlist,
                     feval = xgb.logLoss)    
    
    
    # save model to binary local file
    xgb.save(bst_linear, paste("bst_linear", i, sep = ""))
}

#dstack1 <- xgb.DMatrix("dstack1")
load("E:/KaggleProject/Click-ThroughRate/stack1.RData")
load("E:/KaggleProject/Click-ThroughRate/stack2.RData")
load("E:/KaggleProject/Click-ThroughRate/stack3.RData")

###############predict the stack data sets###################
for(i in 0:6){
    bst_linear <- xgb.load(paste("bst_linear", i, sep = ""))
    stack1[,paste("linear_pred", i, sep = "")] <- predict(bst_linear, dstack1)
    stack2[,paste("linear_pred", i, sep = "")] <- predict(bst_linear, dstack2)
    stack3[,paste("linear_pred", i, sep = "")] <- predict(bst_linear, dstack3)
}


#after get features from linear model rebuild xgb matrix
##set formula
Vars <- names(stack1[,4:42])
formula2 <- paste("click ~ ", paste(Vars, collapse = " + "), sep = "")
formula2 <- as.formula(formula2)

stack1$id <- stack1$hour <- NULL
stack1.x <- sparse.model.matrix(formula2, data = stack1)[, -1]
stackLabel <- stack1$click
dstack11 <- xgb.DMatrix(data = stack1.x, 
                       label = stackLabel)
rm(stack1)
rm(stack1.x)
gc()
xgb.DMatrix.save(dstack11, "dstack11")


stack2$id <- stack2$hour <- NULL
stack2.x <- sparse.model.matrix(formula2, data = stack2)[, -1]
stackLabel <- stack2$click
dstack22 <- xgb.DMatrix(data = stack2.x, 
                       label = stackLabel)
rm(stack2)
rm(stack2.x)
gc()
xgb.DMatrix.save(dstack22, "dstack22")


stack3$id <- stack3$hour <- NULL
stack3.x <- sparse.model.matrix(formula2, data = stack3)[, -1]
stackLabel <- stack3$click
dstack3 <- xgb.DMatrix(data = stack3.x, 
                       label = stackLabel)
rm(stack3)
rm(stack3.x)
gc()
xgb.DMatrix.save(dstack3, "dstack3")







# save model to binary local file
xgb.save(bst, "xgboost_3_.2_70")

#predict on my local test data set
click <- predict(bst, dlocalTest)
xgb.logLoss(click, dlocalTest)

#Save a xgboost model to text file.
xgb.dump(bst, 'xgb.model.dump')
featureImp <- xgb.importance(predictors, 'xgb.model.dump')


load("E:/KaggleProject/Click-ThroughRate/tidyTestData.RData")
summitionId <- tidyTestData$id
tidyTestData$id <- tidyTestData$hour <- NULL
test <- sparse.model.matrix(~ .-1, data = tidyTestData)
rm(tidyTestData)
test <- xgb.DMatrix(data = test)

# load binary model to R
bst <- xgb.load("xgboost_5_.1_100")
click <- predict(bst, test)

summition <- data.frame(id = summitionId, click = click)
summition$click <- click
write.csv(summition, file = "summition.csv", quote = F, row.names = F)
