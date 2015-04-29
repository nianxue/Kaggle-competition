gbmBag <- list(fit = function(x, y,  ...)
{
    library(gbm)
    data <- as.data.frame(x)
    data$y <- y
    gbm(y~., data = data, ...)
},

####Need to modify n.trees in this function
pred = function(object, x)
{
    if(!is.data.frame(x)) x <- as.data.frame(x)
    out <- predict(object, x, n.trees = 70)
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




rfBag <- list(fit = function(x, y,  ...)
{
    library(randomForest)
    data <- as.data.frame(x)
    data$y <- y
    randomForest(y~., data = data, ...)
},

pred = function(object, x)
{
    if(!is.data.frame(x)) x <- as.data.frame(x)
    out <- predict(object, x)
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