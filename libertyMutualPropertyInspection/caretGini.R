require("compiler")
enableJIT(3)
setCompilerOptions(suppressUndefined = T)


path.wd <- getwd()

library(data.table)
library(caret)
library(doParallel)


caretGini <- function (data, lev = NULL, model = NULL) 
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
            #gini <- WeightedGini(obs, weights = 1, pred)/WeightedGini(obs, weights = 1, obs)
            gini <- NormalizedWeightedGini(obs, weights = 1, pred)/NormalizedWeightedGini(obs, weights = 1, obs)
            
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
    
    df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
    return(sum(df$Gini))
    
    #     n <- nrow(df)
    #     gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
    #     return(gini)
}


NormalizedWeightedGini <- function(solution, weights, submission) {
    WeightedGini(solution, weights, submission) / WeightedGini(solution, weights, solution)
}
