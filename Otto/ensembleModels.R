
############################################ ensemble models

cvPred <- xgb8Cv %>% 
    bind_cols(xgb6Cv) %>% 
    bind_cols(xgb3Cv) %>% 
    bind_cols(nnet250Cv) %>%
    bind_cols(nnet150Cv) %>%
    bind_cols(nnet50Cv) %>%
    bind_cols(glmnet.facCv) %>%
    bind_cols(glmnet.numCv) %>%
    bind_cols(rfCv) %>%
    bind_cols(h2oCv1024) %>%
    bind_cols(h2oCv512)


testPred <- xgb8Pred %>% 
    bind_cols(xgb6Pred) %>% 
    bind_cols(xgb3Pred) %>% 
    bind_cols(nnet250Pred) %>%
    bind_cols(nnet150Pred) %>%
    bind_cols(nnet50Pred) %>%
    bind_cols(glmnet.facPred) %>%
    bind_cols(glmnet.numPred) %>%
    bind_cols(rfPred) %>%
    bind_cols(h2oPred1024) %>%
    bind_cols(h2oPred512)


label <- gsub("Class_", "", x = target)
label <- as.numeric(label) - 1


dtrain <- xgb.DMatrix(data = as.matrix(cvPred), label = label)
dtest <- xgb.DMatrix(data = as.matrix(testPred))

############################tune xgb parameters


pred <- submission
pred[,2:10] <- 0

for(i in 1:5){
    set.seed(100*i + 973)
    xgb <- xgboost(data = dtrain,
                   max_depth = 4,         #6
                   eta = 0.03, 
                   nround = 410, 
                   min_child_weight = 3,  #7
                   objective = "multi:softprob",
                   num_class = 9,
                   eval_metric = "mlogloss",
                   subsample = 0.8, #subsample ratio of the training instance
                   colsample_bytree = 0.5,
                   verbose = 1)
    
    xgbPred <- predict(xgb, dtest)
    xgbPred = matrix(xgbPred,9,length(xgbPred)/9)
    xgbPred = as.data.frame(t(xgbPred))
    
    pred[,2:10] <- pred[,2:10] + xgbPred
}

pred[,2:10] <- pred[,2:10]/5
write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)

