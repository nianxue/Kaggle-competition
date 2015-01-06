require(compiler)
enableJIT(3)
library(doParallel)
library(caret)
#library(ff)
library(lubridate)
#library(dplyr) 
#library(car) 
#library(Hmisc)
library(glmnet)
library(xgboost)

logLoss <- function(obs, pred){
    #caculate logLoss
    epsilon <- .000000000000001
    yhat <- pmin(pmax(pred, epsilon), 1-epsilon)
    y <- as.numeric(as.character(obs))
    
    logloss <- -mean(y * log(yhat) + (1 - y) * log(1 - yhat))
    logloss
}

mySummary <- function(data, lev = NULL, model = NULL) 
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
    y <- as.numeric(as.character(data[, "obs"]))
    
    logloss <- -mean(y * log(yhat) + (1 - y) * log(1 - yhat))
    
    out <- c(logloss,
             rocAUC, 
             sensitivity(data[, "pred"], data[, "obs"], lev[1]), 
             specificity(data[, "pred"], data[, "obs"], lev[2]))
    
    names(out) <- c("logLoss", "ROC", "Sens", "Spec")
    out
}

renameFactors <- function(x){
    if(is.factor(x)){
        x <- gsub("\\[ *", "from", x)
        x <- gsub(", *", "to", x)
        x <- gsub(")", "", x)
        x <- gsub("\\]", "", x)
        x <- gsub(" *-", "negative", x)
        factor(x)
    }
}