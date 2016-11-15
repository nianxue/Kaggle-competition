library(readr)
library(plyr)
library(dplyr)

train <- read_csv("train.csv")
test <- read_csv("test.csv")
submission <- read_csv("sample_submission.csv")

train$split <- 1
test$split <- 0
loss <- train$loss

target <- if_else(loss >= 40000, 40000, loss)
# cat_target10000 <- as.factor(if_else(loss >= 10000, "large", "normal"))
# cat_target20000 <- as.factor(if_else(loss >= 20000, "large", "normal"))


log_target200 <- log(target + 200)
train$loss <- NULL

###create count features
data <- bind_rows(train, test)
dot <- list(~n())
c_Vars <- names(data)[sapply(data, is.character)]


for(var in c_Vars){
  
  ####create count features
    data <- data %>%
      group_by_(var) %>%
      mutate_(.dots = setNames(dot, paste0(var, "_cnt"))) %>%
      ungroup()
    
    ####create categorical features to numerical based on the alphabetial order
    # data[[paste0(var, "_AlphaOrder")]] <- as.integer(as.factor(data[[var]]))
}


###rank cat features by loss and create rank summary features
tempData <- data
tempTrain <- train
tempTrain$loss <- loss
dot = list(~median(loss))

for(var in c_Vars){
  temp <- tempTrain %>%
    group_by_(var) %>%
    summarise_(.dots = setNames(dot, paste0(var, "_TargetMedian"))) 
  
  temp[[paste0(var, "_Rank")]] <- row_number(temp[[paste0(var, "_TargetMedian")]])
  tempData <- tempData %>% left_join(temp)
}

temp <- select(tempData, matches("rank"))
data$RankRowSum <- apply(temp, 1, sum, na.rm = T)
data$Rank1s <- apply(temp, 1, function(x) sum(x==1, na.rm = T))
data$Rank2s <- apply(temp, 1, function(x) sum(x==2, na.rm = T))
data$Rank3s <- apply(temp, 1, function(x) sum(x==3, na.rm = T))
data$Rank4s <- apply(temp, 1, function(x) sum(x==4, na.rm = T))
data$Rankgreaterthan4 <- apply(temp, 1, function(x) sum(x>=5, na.rm = T))

train_Count <- filter(data, split == 1) %>% select(matches("^cont|cnt|rank"))
test_Count <- filter(data, split == 0) %>% select(matches("^cont|cnt|rank"))




##########train only contains character or factor variables, 
##########test includes all variables
##########parameters need to tune: minCnt, fillNa, times


#get variables that have more than 10 categories
temp <- data %>% select(matches("^cat.+[0-9]$"))
temp <- sapply(temp, n_distinct)
HighOrdinalVars <- names(temp[temp >= 10])

temp <- getTargetMean(train[, HighOrdinalVars], test, log_target200, minCnt = 10,
                      fillNa = 99999, K = 2, times = 3, seeds = 9)

train_Count <- bind_cols(train_Count, temp$train)
test_Count <- bind_cols(test_Count, select(temp$test, matches("Prob")))
