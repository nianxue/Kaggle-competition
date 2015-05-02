require("compiler")
enableJIT(3)
setCompilerOptions(suppressUndefined = T)


path.wd <- getwd()

library(data.table)
library(caret)
library(doParallel)

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


summaryWithGini <- function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    
    obs <- data[, "obs"]
    pred <- data[, "pred"]
   
    isNA <- is.na(pred)
    pred <- pred[!isNA]
    obs <- obs[!isNA]
    
    if (!is.factor(obs) & is.numeric(obs)) {
        if (length(obs) + length(pred) == 0) {
            out <- rep(NA, 2)
        }
        else {
            gini <- WeightedGini(obs, weights = 1, pred)/WeightedGini(obs, weights = 1, obs)
            
            
            mse <- mean((pred - obs)^2)
            out <- c(gini, sqrt(mse))
        }
        names(out) <- c("Gini", "RMSE")
    }
    
    if (any(is.nan(out))) 
        out[is.nan(out)] <- NA
    out
}



WeightedGini <- function(solution, weights, submission){
    df = data.frame(solution = solution, weights = weights, submission = submission)
    df <- df[order(df$submission, decreasing = TRUE),]
    df$random = cumsum((df$weights/sum(df$weights)))
    totalPositive <- sum(df$solution * df$weights)
    df$cumPosFound <- cumsum(df$solution * df$weights)
    df$Lorentz <- df$cumPosFound / totalPositive
    n <- nrow(df)
    gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
    return(gini)
}


NormalizedWeightedGini <- function(solution, weights, submission) {
    WeightedGini(solution, weights, submission) / WeightedGini(solution, weights, solution)
}
