library(xgboost)
library(caret)
library(plyr)
library(dplyr)


setwd("E:/KaggleProject/Otto")
trainIni <- read.csv("E:/KaggleProject/Otto/train.csv/train.csv")
testIni <- read.csv("E:/KaggleProject/Otto/test.csv/test.csv")

################################prepare the data
target <- trainIni$target
trainIni$target <- NULL

trainIni$split <- 1
testIni$split <- 0

data <- bind_rows(trainIni, testIni)


split <- data$split
data$id <- NULL
data$split <- NULL
################generate tow way count features##################

twoWayVars <- c("feat_11", "feat_34", "feat_60", "feat_14", "feat_40", "feat_15",
                "feat_26", "feat_90", "feat_25", "feat_67", "feat_86", "feat_42",
                "feat_62", "feat_36", "feat_75")

twoWayInter <- ""
for(i in 1:(length(twoWayVars) - 1)){
    for(j in (i+1):length(twoWayVars)){
        temp <- paste0(c(twoWayVars[i], twoWayVars[j]), collapse = ":")
        twoWayInter <- paste(twoWayInter, temp, sep = "+")
    }
}


formula <- paste("~.-1", twoWayInter)
formula <- as.formula(formula)

data <- model.matrix(formula, data = data)


# 
# #get two way counts
# dot <- list(~n())


# for(name in c_Vars){
#     train.count <- train.count %>% 
#         group_by_(name) %>% mutate_(.dots = setNames(dot, paste0(name, "_cnt")))
# }


# for(i in 1:(length(twoWayVars) - 1)){
#     for(j in (i+1):length(twoWayVars)){
#         groupName <- list(twoWayVars[i], twoWayVars[j])
#         
#         data <- data %>% 
#             group_by_(.dots = groupName) %>% 
#             mutate_(.dots = setNames(dot, paste(twoWayVars[i], twoWayVars[j], sep = ".")))
#     }
# }




data[,] <- lapply(data, as.numeric)
################################get train and test
train <- data[split == 1,]
test <- data[split == 0,]


##################################build xgb models

label <- gsub("Class_", "", x = target)
label <- as.numeric(label) - 1


############################tune xgb parameters
dtrainTwoway <- xgb.DMatrix(data = as.matrix(train), label = label)
dtestTwoway <- xgb.DMatrix(data = as.matrix(test))


xgbTuneTwoway <- xgb.cv(data = dtrainTwoway,
                         max_depth = 8,         #6
                         eta = 0.03, 
                         nround = 2000, 
                         min_child_weight = 10,  #7
                         #                   gamma = 0.05,
                         #                   max_delta_step = 1,
                         objective = "multi:softprob",
                         num_class = 9,
                         eval_metric = "mlogloss",
                         nfold = 5,
                         stratified = TRUE,
                         #                   prediction = TRUE,
                         subsample = 0.8, #subsample ratio of the training instance 0.5
                         colsample_bytree = 0.5,
                         verbose = 1)


set.seed(8763)
xgb <- xgboost(data = dtrainTwoway,
              max_depth = 6,         #6
              eta = 0.2, 
              nround = 200, 
              min_child_weight = 7,  #7
              objective = "multi:softprob",
              num_class = 9,
              eval_metric = "mlogloss",
              # prediction = TRUE,
              subsample = 0.8, #subsample ratio of the training instance 0.5
              colsample_bytree = 0.5,
              verbose = 1)


##########Train xgb model
dtrain <- xgb.DMatrix(data = as.matrix(train[,1:93]), label = label)
dtest <- xgb.DMatrix(data = as.matrix(test[,1:93]))


xgb.ini <- xgboost(data = dtrain,
                  max_depth = 8,         #6
                  eta = 0.03, 
                  nround = 1550, 
                  min_child_weight = 10,  #7
                  objective = "multi:softprob",
                  num_class = 9,
                  eval_metric = "mlogloss",
                  subsample = 0.8, #subsample ratio of the training instance
                  colsample_bytree = 0.5,
                  verbose = 1)


pred = predict(xgb,dtest)

# Get the feature real names
names <- names(train)

# Compute feature importance matrix
varImp <- xgb.importance(names, model = xgb)
top50 <- varImp$Feature[1:50]




# Nice graph
xgb.plot.importance(importance_matrix[1:50,])


xgb.plot.tree(feature_names = names, model = xgb, n_first_tree = 3)


############################################ Make prediction
pred = predict(xgb,dtest)
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
