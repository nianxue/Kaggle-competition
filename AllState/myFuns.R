CaretMAE <- function(data, lev = NULL, model = NULL) 
{
  require(Metrics)
  pred <- data[, "pred"]
  obs <- data[, "obs"]
  
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  
  out <- mae(exp(obs) - 200, exp(pred) - 200)
  names(out) <- c("MAE")
  out
}

#Steps to create interaction
# getInteractionMap(names(train))
# xgb.dump(model = xgbProb$finalModel, fname = "xgb.dump", fmap = "xgb.fmap", with.stats = TRUE)

getInteractionMap <- function(Vars) {
  
  featureMapString = ""
  
  for (i in 1:length(Vars)) {
    
    #Add new line
    featureMapString = paste(featureMapString, i-1, "\t", Vars[i], "\t", "q", "\n", sep="")
    
  }
  #Write file
  fileConn = file("xgb.fmap")
  writeLines(featureMapString, fileConn)
  close(fileConn)
}



##########train only contains character or factor variables, 
##########test includes all variables
##########parameters need to tune: minCnt, fillNa, times
getTargetMean <- function(train, test, target, 
                          minCnt = 5, fillNa = 9999, 
                          K = 2, times = 3, seeds = 9)
{
  library(stringr)
  library(dplyr)
  library(lazyeval)
  
  N = nrow(train)
  
  #create folds for "CV"
  split <- vector("list", times)
  for(i in 1:times)
  {
    set.seed(seeds + 99 * i)
    split[[i]] <- createFolds(y = target, k = K)
  }
  
  Vars <- names(train)
  train$target <- target
  dot = list(~mean(target), ~n())
  
  for(var in Vars)
  {
    trainProb <- vector("list", times)
    probVar <- paste0(var, "_Prob")
    
    ############### get target mean for the test data ###############
    temp <- train %>% group_by_(var) %>%
      summarise_(.dots = setNames(dot, paste0(var, c("_Prob", "_cnt")))) %>%
      ungroup()
    
    temp[temp[[3]] <= minCnt, probVar] <- NA
    temp[[3]] <- NULL
    test <- left_join(test, temp)
    
    ############## get target mean for the train data ##################
    for(i in 1:times)
    {
      innerSplit <- split[[i]]
      trainProb[[i]] <- rep(NA, N)
      
      for(fold in 1:K)
      {
        temp <- train[-innerSplit[[fold]], ] %>% group_by_(var) %>%
          summarise_(.dots = setNames(dot, paste0(var, c("_Prob", "_cnt")))) %>%
          ungroup()
        
        temp[temp[[3]] <= minCnt * (K - 1) / K, probVar] <- NA
        temp[[3]] <- NULL
        
        temp <- left_join(train[innerSplit[[fold]], var], temp, by = var)
        trainProb[[i]][innerSplit[[fold]]] <- temp[[probVar]]
      }
    }
    
    names(trainProb) <- paste0(var, "_Prob", 1:times)
    trainProb <- as_data_frame(trainProb)
    
    if(fillNa != 9999)
    {
      train[is.na(train)] <- fillNa
    }
    
    train[, probVar] <- apply(trainProb, 1, mean, na.rm = T)
  }
  
  test[is.na(test)] <- fillNa
  train[is.na(train)] <- fillNa
  
  list(train = select(train, matches("Prob$")), test = test) 
}


xgbBag <- list(fit = function(x, y,  ...)
{
  library(xgboost)
  
  if(is.factor(y))
  {
    lev <- levels(y)
    
    if(length(lev) == 2)
      label <- ifelse(y == lev[1], 1, 0)
    else
      label <- as.numeric(y) - 1
  }
  else{
    
    label <- y
  }
  
  dtrain <- xgb.DMatrix(data = as.matrix(x), label = label)
  out <- xgboost(data = dtrain,...)
  
  if(is.factor(y))
  {
    out$obsLevels = lev
  }
  
  out
},


pred = function(object, x)
{
  
  dtest <- xgb.DMatrix(data = as.matrix(x))
  out <- predict(object, dtest)
  
  if(length(object$obsLevels) == 2) {
    out <- cbind(out, 1 - out)
    colnames(out) <- object$obsLevels
    out <- as.data.frame(out)
  }
  
  if(length(object$obsLevels) > 2){
    out <- matrix(out, ncol = length(object$obsLevels), byrow = TRUE)
    colnames(out) <- object$obsLevels
    out <- as.data.frame(out)
  }
  
  out
},

#xgbBagFit$finalModel$control$aggregate <- xgbBag$aggregate
aggregate = function(x, type = "class")
{
  library(psych)
  if(is.matrix(x[[1]]) | is.data.frame(x[[1]]))
  {
    pooled <- x[[1]] & NA
    
    classes <- colnames(pooled)
    for(i in 1:ncol(pooled))
    {
      tmp <- lapply(x, function(y, col) y[,col], col = i)
      tmp <- do.call("rbind", tmp)
      pooled[,i] <- apply(tmp, 2, median)
    }
    if(type == "class")
    {
      out <- factor(classes[apply(pooled, 1, which.max)],
                    levels = classes)
    } else out <- as.data.frame(pooled)
  } else {
    x <- matrix(unlist(x), ncol = length(x))
    out <- apply(x, 1, median)
  }
  out
})


