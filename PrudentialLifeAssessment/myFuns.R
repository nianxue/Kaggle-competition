logSum <- function(x)
{
    sum(log1p(x), na.rm = T)
}

myHarmonic <- function(x)
{
    harmonic.mean(x + 1, na.rm = T)
}


## Create Evaluation Function
SQWKfun = function(x = seq(1.5, 7.5, by = 1), pred) {
    preds = pred$pred
    obs = pred$obs
    cuts = c(x[1], x[2], x[3], x[4], x[5], x[6], x[7])
    preds = as.numeric(Hmisc::cut2(preds, cuts))
    err = Metrics::ScoreQuadraticWeightedKappa(preds, obs, 1, 8)
    return(-err)
}



xgbBag <- list(fit = function(x, y,  ...)
{
    library(xgboost)
    dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
    xgboost(data = dtrain,...)
},

####Need to modify n.trees in this function
pred = function(object, x)
{
    dtest <- xgb.DMatrix(data = as.matrix(x))
    out <- predict(object, dtest)
    out
},

aggregate = function(x, type = "class")
{
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


