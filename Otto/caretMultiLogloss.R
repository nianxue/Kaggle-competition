caretMultiLogloss <- function(data, lev = NULL, model = NULL) 
{
  library(reshape2)
  N <- nrow(data)
  
  data <- melt(data, id.vars = c("pred", "obs", "rowIndex"))  
  data$y <- as.numeric(data$obs == data$variable)
  
  #caculate logLoss
  epsilon <- 1e-15
  yhat <- pmin(pmax(data$value, epsilon), 1-epsilon)
  y <- data$y
  
  Logloss <- sum(y * log(yhat) )
  Logloss <- -Logloss / N 
  
  names(Logloss) <- c("Logloss")
  Logloss
}