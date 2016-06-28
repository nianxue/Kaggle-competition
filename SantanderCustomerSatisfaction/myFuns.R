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



getInteractionVars <- function(TwoWays, data){
  
  for(i in 1:length(TwoWays))
  {

    var1 <- TwoWays[[i]][1]
    var2 <- TwoWays[[i]][2]

    if(n_distinct(data[[var1]]) > 100 & n_distinct(data[[var2]]) > 100)
    {

      data[,paste0(var1, "_", var2, "Sum")] <- data[[var1]] + data[[var2]]
      data[,paste0(var1, "_", var2, "Sub")] <- data[[var1]] - data[[var2]]
      data[,paste0(var1, "_", var2, "Mult")] <- data[[var1]] * data[[var2]]
      data[,paste0(var1, "_", var2, "Div")] <- data[[var1]] / data[[var2]]
          
    } else if(n_distinct(data[[var1]]) <= 100 & n_distinct(data[[var2]]) <= 100){
      
      # data[,paste0(var1, "_", var2, "Comb")] <- as.factor(str_c(data[[var1]], data[[var2]]))

      #get two way counts
      dot <- list(~n())
      groupName <- list(var1, var2)
      
      data <- data %>% 
          group_by_(.dots = groupName) %>% 
          mutate_(.dots = setNames(dot, paste0(var1, var2, "_cnt")))
      
    }
    else if(n_distinct(data[[var1]]) <= 100){
      dot <- interp(~ var2 - mean(var2), var2 = as.name(var2))
      data <- data %>% 
        group_by_(.dots = var1) %>% 
        mutate_(.dots = setNames(list(dot), paste0(var1, var2, "_Group")))
      
    }
    else{
        
      dot <- interp(~ var1 - mean(var1), var1 = as.name(var1))
      data <- data %>% 
          group_by_(.dots = var2) %>% 
          mutate_(.dots = setNames(list(dot), paste0(var2, var1, "_Group")))
    }
  }

  data
}

 
CaretLogloss <- function(data, lev = NULL, model = NULL) 
{
  require(pROC)
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  rocObject <- try(pROC::roc(data$obs, data[, lev[1]]), silent = TRUE)
  rocAUC <- if (class(rocObject)[1] == "try-error") 
              NA
            else 
              rocObject$auc
  
  #caculate logLoss
  epsilon <- .000000000000001
  yhat <- pmin(pmax(data[, lev[1]], epsilon), 1-epsilon)
  y <- as.numeric(as.character(data[, "obs"]) == lev[1])
  
  logloss <- -mean(y * log(yhat) + (1 - y) * log(1 - yhat))
  
  out <- c(logloss,
           rocAUC)
  
  names(out) <- c("logLoss", "ROC")
  out
}



# dot <- list(~n())
# for(name in c_Vars){
#     train.count <- train.count %>% 
#         group_by_(name) %>% mutate_(.dots = setNames(dot, paste0(name, "_cnt")))
# }
# 
# 
# #get two way counts
# for(i in 1:(length(imp_vars) - 1)){
#     for(j in (i+1):length(imp_vars)){
#         groupName <- list(imp_vars[i], imp_vars[j])
#         
#         train.count <- train.count %>% 
#             group_by_(.dots = groupName) %>% 
#             mutate_(.dots = setNames(dot, paste0(imp_vars[i], imp_vars[j], "_cnt")))
#     }
# }



getResponseFeatures <- function(trainLevel1, Response, trainLevel2, vars)
{
  trainLevel1$Response <- Response
  dot = list(~mean(Response), ~n())
  
  for(var in vars)
  {
    temp <- trainLevel1 %>% group_by_(var) %>%
      summarise_(.dots = setNames(dot, paste0(var, c("_ResponseProb", "_cnt")))) %>%
      filter_(.dots = paste0(var, "_cnt >= 50")) %>%
      select_(.dots = paste0("-", var, "_cnt"))
    
    ###It has to be summrase because the following join. Too much duplicate if mutate above.
    trainLevel2 <- left_join(trainLevel2, temp)
  }
  
  trainLevel2
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


