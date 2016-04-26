library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(caret)


setwd("E:/KaggleProject/SantanderCustomerSatisfaction")
source('E:/KaggleProject/SantanderCustomerSatisfaction/myFuns.R')
train <- read.csv("E:/KaggleProject/SantanderCustomerSatisfaction/train.csv")
test <- read.csv("E:/KaggleProject/SantanderCustomerSatisfaction/test.csv")
submission <- read.csv("E:/KaggleProject/SantanderCustomerSatisfaction/submission.csv")

target <- select(train, ID, TARGET)

train$TARGET <- NULL
train$split <- 1
test$split <- 0

data <- bind_rows(train, test)

ID_Split <- select(data, ID, split)
data <- select(data, -ID, -split)

iniVars <- names(data)
##################create features for each group############
data$total_cnt0 <- apply(data, 1, function(x) {sum(abs(x) <= 0.0001)})
data$total_max <- apply(data, 1, max)

temp <- select(data[,iniVars], matches("^imp_op_var[0-9]{1,}"))
data$imp_op_var_comer_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x))})
# data$imp_op_var_comer_max <- apply(temp, 1, max)

temp <- select(data[,iniVars], matches("^ind_var[0-9]{1,}"))
data$ind_var_all_sum <- apply(temp, 1, sum)

temp <- select(data[,iniVars], matches("^num_var[0-9]{1,}$"))
data$num_var_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x))})
data$num_var_cnt3 <- apply(temp, 1, function(x) {sum(x == 3)})
# data$num_var_max <- apply(temp, 1, max)

temp <- select(data[,iniVars], matches("^num_var[0-9]{1,}_0$"))
data$num_var_0_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x))})
data$num_var_0_cnt3 <- apply(temp, 1, function(x) {sum(x == 3)})

temp <- select(data[,iniVars], matches("^num_var[0-9]{1,}_[a-z]+"))
data$num_varOther_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x))})
data$num_varOther_cnt0 <- apply(temp, 1, function(x) {sum(x == 0)})
# data$num_varOther_max <- apply(temp, 1, max)
# data$num_varOther_cnt15g <- apply(temp, 1, function(x) {sum(x > 15)})

temp <- select(data[,iniVars], matches("^num_op_var[0-9]{1,}"))
data$num_op_var_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x))})

temp <- select(data[,iniVars], matches("^num_meses_var[0-9]{1,}"))
data$num_meses_var_cnt1 <- apply(temp, 1, function(x) {sum(x == 1)})
data$num_meses_var_cnt2 <- apply(temp, 1, function(x) {sum(x == 2)})
data$num_meses_var_cnt3 <- apply(temp, 1, function(x) {sum(x == 3)})

temp <- select(data[,iniVars], matches("^saldo_var[0-9]{1,}$"))
# data$saldo_var_max <- apply(temp, 1, max)
data$saldo_var_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x[x>=0]))})
# data$saldo_var_which_max <- apply(temp, 1, which.max)

temp <- select(data[,iniVars], matches("^imp"))
# data$imp_var_max <- apply(temp, 1, max)
data$imp_var_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x))})

temp <- select(data[,iniVars], matches("^saldo_medio_var[0-9]{1,}"))
data$saldo_medio_var_cnt_0 <- apply(temp, 1, function(x) {sum(abs(x) < 0.0001)})
data$saldo_medio_var_cnt_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x[x>=0]))})
# data$saldo_medio_var_max <- apply(temp, 1, max)

temp <- select(data[,iniVars], matches("_ult1$"))
data$ult1_cnt_1 <- apply(temp, 1, function(x) {sum(abs(x - 1) < 0.0001)})
data$ult1_cnt_0 <- apply(temp, 1, function(x) {sum(abs(x) < 0.0001)})
data$ult1_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x[x >= 0]))})
# data$ult1_max <- apply(temp, 1, max)
# data$ult1_which_max <- apply(temp, 1, which.max)

temp <- select(data[,iniVars], matches("_ult3$"))
data$ult3_cnt_1 <- apply(temp, 1, function(x) {sum(abs(x - 1) < 0.0001)})
data$ult3_cnt_0 <- apply(temp, 1, function(x) {sum(abs(x) < 0.0001)})
data$ult3_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x[x >= 0]))})
# data$ult3_max <- apply(temp, 1, max)
# data$ult3_which_max <- apply(temp, 1, which.max)

temp <- select(data[,iniVars], matches("_hace2$"))
data$hace2_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x[x >= 0]))})

temp <- select(data[,iniVars], matches("_hace3$"))
# data$hace3_max <- apply(temp, 1, max)
data$hace3_sumLog1p <- apply(temp, 1, function(x) {sum(log1p(x[x >= 0]))})

data$saldo_var30_saldo_var8Sub <- data$saldo_var30 - data$saldo_var8
data$saldo_var30_var38Sum <- data$saldo_var30 + data$var38
data$saldo_var30_saldo_var8Sum <- data$saldo_var30 + data$saldo_var8

newVars <- setdiff(names(data), iniVars)
temp <- data[,newVars]

#######################remove features###################
data <- data[,iniVars]

near0Vars <- nearZeroVar(data, 1000, 0.1)
data <- data[, -near0Vars]

correlation <- cor(data, method = "spearman")
highCor <- findCorrelation(correlation, cutoff = 0.99)
data <- data[, -highCor]

remove <- c("ind_var5_0", "ind_var5", "ind_var7_recib_ult1", "num_var8", "num_var8_0", "ind_var13_0", "ind_var13_corto_0"
            , "saldo_var13_largo", "ind_v14", "num_sal_var16_ult1", "saldo_medio_var17_ult3", "num_var20", "ind_var24_0"
            , "ind_var25", "ind_var26", "ind_var30", "ind_var31", "ind_var32", "ind_var37", "ind_var39_0", "imp_op_var40_ult1"
            , "ind_var44", "num_var45_ult3", "ind_var32_cte", "num_var20_0", "ind_var31_0", "ind_var14_0", "delta_num_aport_var13_1y3")

data <- data[, setdiff(names(data), remove)]
data <- bind_cols(data, temp)


train <- filter(data, ID_Split$split == 1)
test <- filter(data, ID_Split$split == 0)

remove(temp)
gc()