source("base_funs.R")
############################ model training####################################
load("E:/KaggleProject/Click-ThroughRate/factor_test.RData")
summitionId <- factor_test$id
factor_test$id <- NULL
test <- sparse.model.matrix(~ .-1, data = factor_test)
rm(factor_test)

############train glmnet models with all features
glmnetModels <- vector("list", 10)
glmnetPredictions <- vector("list", 10)

for(i in 1:10){
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/factor_training", i, ".RData", sep = "")
    load(filePath)
    
    print(i)
    
    train.y <- factor(factor_training$click)
    train.x <- sparse.model.matrix(click ~ .-1, data = factor_training[,2:24])
    rm(factor_training)
    

    
    glmnetModels[[i]] <- glmnet(x = train.x, 
                              y = train.y,
                              family = "binomial",
                              alpha = 0.1,
                              lambda = 0.02,
                              standardize = FALSE)

    glmnetPredictions[[i]] = predict(glmnetModels[[i]],
                                   s = 0.02, 
                                   newx = test,
                                   type = "response")
    glmnetPredictions[[i]] <- as.numeric(glmnetPredictions[[i]])
}

names(glmnetPredictions)[1:10] <- paste("pred", 1:10, sep = "")



############train glmnet models with Cs features
Vars <- c("banner_pos_factor", "C1_factor", "C14_factor", "C15_factor", "C16_factor", 
          "C17_factor", "C18_factor", "C19_factor", "C20_factor", "C21_factor" )

load("E:/KaggleProject/Click-ThroughRate/factor_test.RData")
summitionId <- factor_test$id
factor_test$id <- NULL
test <- sparse.model.matrix(~ .-1, data = factor_test[,Vars])
rm(factor_test)

glmnetModels_Cs <- vector("list", 10)
glmnetPredictions_Cs <- vector("list", 10)
Vars <- c("click", "banner_pos_factor", "C1_factor", "C14_factor", "C15_factor", "C16_factor", 
          "C17_factor", "C18_factor", "C19_factor", "C20_factor", "C21_factor" ) 

for(i in 1:10){
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/factor_training", i, ".RData", sep = "")
    load(filePath)
    
    print(i)
    
    train.y <- factor(factor_training$click)
    train.x <- sparse.model.matrix(click ~ .-1, data = factor_training[,Vars])
    rm(factor_training)
    

    glmnetModels_Cs[[i]] <- glmnet(x = train.x, 
                                y = train.y,
                                family = "binomial",
                                alpha = 0.1,
                                lambda = 0.01,
                                standardize = FALSE)
    
    glmnetPredictions_Cs[[i]] = predict(glmnetModels_Cs[[i]],
                                     s = 0.01, 
                                     newx = test,
                                     type = "response")
    glmnetPredictions_Cs[[i]] <- as.numeric(glmnetPredictions_Cs[[i]])
}

names(glmnetPredictions_Cs)[1:10] <- paste("pred", 1:10, sep = "")



############train glmnet models with site/app/device features
load("E:/KaggleProject/Click-ThroughRate/factor_test.RData")
summitionId <- factor_test$id
factor_test$id <- NULL
test <- sparse.model.matrix(~ .-1, data = factor_test[,1:13])
rm(factor_test)

glmnetModels_noCs <- vector("list", 10)
glmnetPredictions_noCs <- vector("list", 10)

for(i in 1:10){
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/factor_training", i, ".RData", sep = "")
    load(filePath)
    
    print(i)
    
    train.y <- factor(factor_training$click)
    train.x <- sparse.model.matrix(click ~ .-1, data = factor_training[,2:15])
    rm(factor_training)
    
    
    
    glmnetModels_noCs[[i]] <- glmnet(x = train.x, 
                                   y = train.y,
                                   family = "binomial",
                                   alpha = 0.1,
                                   lambda = 0.01,
                                   standardize = FALSE)
    
    glmnetPredictions_noCs[[i]] = predict(glmnetModels_noCs[[i]],
                                        s = 0.01, 
                                        newx = test,
                                        type = "response")
    glmnetPredictions_noCs[[i]] <- as.numeric(glmnetPredictions_noCs[[i]])
}

names(glmnetPredictions_noCs)[1:10] <- paste("pred", 1:10, sep = "")




################predict on the last 5 factor training data
################and using them as features to gbm models 
glmnetPred <- vector("list", 10)

for(i in 1:10){
    print(i)
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/factor_training", i, ".RData", sep = "")
    load(filePath)
    
    j = 11 - i
    
    len <- nrow(factor_training)
    glmnetPred[[i]] <- data.frame(full = numeric(length = len),
                                  Cs = numeric(length = len),
                                  noCs = numeric(length = len))
    
    test.x <- sparse.model.matrix(click ~ .-1, data = factor_training[,2:24])   
    pred <-  predict(glmnetModels[[j]],
                     s = 0.02,
                     newx = test.x,
                     type = "response")
    glmnetPred[[i]]$full <- as.numeric(pred)
    
    Vars <- c("click", "banner_pos_factor", "C1_factor", "C14_factor", "C15_factor", "C16_factor", 
              "C17_factor", "C18_factor", "C19_factor", "C20_factor", "C21_factor" )
    
    test.x <- sparse.model.matrix(click ~ .-1, data = factor_training[,Vars])
    pred <- predict(glmnetModels_Cs[[j]],
                    s = 0.01,
                    newx = test.x,
                    type = "response")
    glmnetPred[[i]]$Cs <- as.numeric(pred)
    
    
    test.x <- sparse.model.matrix(click ~ .-1, data = factor_training[,2:15])
    pred <- predict(glmnetModels_noCs[[j]],
                    s = 0.01,
                    newx = test.x,
                    type = "response")
    glmnetPred[[i]]$noCs <- as.numeric(pred)
}
save(list = "glmnetPred", file = paste("glmnetPred.RData"))
                                          
                                          
### add glmnet predict result as a feature for gbm model
for(i in 1:10){
    print(i)
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/numerical_training_new", i, ".RData", sep = "")
    load(filePath)
    
    numerical_training_new <- bind_cols(numerical_training_new, glmnetPred[[i]])
    
    save(list = "numerical_training_new", file = filePath)
}


#################### train gbm models ###############################
load("E:/KaggleProject/Click-ThroughRate/numerical_test.RData")
summitionId <- numerical_test$id
numerical_test$id <- NULL

test <- as.matrix(numerical_test)
test <- xgb.DMatrix(data = test)
rm(numerical_test)

for(i in 2:10){
    print(i)
    filePath <- paste("E:/KaggleProject/Click-ThroughRate/numerical_training_new", i, ".RData", sep = "")
    load(filePath)
    
    label <- numerical_training_new$click
    numerical_training_new <- as.matrix(numerical_training_new[,3:91])
    predictors <- attr(numerical_training_new, "dimnames")[[2]]
    training <- xgb.DMatrix(data = numerical_training_new, label = label)
    rm(numerical_training_new)
    gc()
    
    watchlist <- list(train = training)
    
    params <- list(max.depth = 9, 
                   eta = 0.05, 
                   objective = "binary:logistic",
                   #                subsample = 0.7, #subsample ratio of the training instance
                   #                colsample_bytree = 0.4,
                   #eval.metric = c("logLoss"),
                   silence = 1)
    
    
    
    set.seed(8763)
    bst <- xgb.train(data = training, 
                     nround = 100, 
                     params = params,
                     watchlist=watchlist,
                     feval = xgb.logLoss)
    
    
    xgb.save(bst, paste0("xgb", i, ".model"))
    rm(training)
    rm(watchlist)
    gc()
            
}


gbmPred <- vector("list", 10)
for(i in 1:10){
    print(i)
    bst <- xgb.load(paste0("xgb", i, ".model"))
    gbmPred[[i]] <- predict(bst, test)
                    
}
save(list = "gbmPred", file = "gbmPred.RData")


########################## prediction ###########################


#Save a xgboost model to text file.
xgb.dump(bst, 'xgb.model.dump')
featureImp <- xgb.importance(predictors, 'xgb.model.dump')
featureImp <- arrange(featureImp, Weight)
fix(featureImp)


# load binary model to R
bst <- xgb.load("xgboost_5_.1_100")
click <- predict(bst, test)


summition <- data.frame(id = summitionId, click = click, stringsAsFactors = F)
write.csv(summition, file = "summition.csv", quote = F, row.names = F)








