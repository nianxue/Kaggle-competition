library(stringr)
library(tidyr)
library(plyr)
library(dplyr)
library(psych)

source('E:/KaggleProject/PrudentialLifeAssessment/myFuns.R')
setwd("E:/KaggleProject/PrudentialLifeAssessment")

train <- read.csv("E:/KaggleProject/PrudentialLifeAssessment/train.csv", stringsAsFactors=FALSE)
test <- read.csv("E:/KaggleProject/PrudentialLifeAssessment/test.csv", stringsAsFactors=FALSE)
submission <- read.csv("E:/KaggleProject/PrudentialLifeAssessment/submission.csv", stringsAsFactors=FALSE)


test$Response <- -1
train$split <- 1
test$split <- 0
data <- bind_rows(train, test)


####feature engineering
Vars <- str_subset(names(data), "Employment_Info")
data$Employment_Info_logSum <- data %>% select(one_of(Vars)) %>% apply(1, logSum)
data$Employment_Info_harmonic <- data %>% select(one_of(Vars)) %>% apply(1, myHarmonic)

Vars <- str_subset(names(data), "InsuredInfo")
data$InsuredInfo_Sum <- data %>% select(one_of(Vars)) %>% apply(1, sum, na.rm = T)
data$InsuredInfo_logSum <- data %>% select(one_of(Vars)) %>% apply(1, logSum)
data$InsuredInfo_1s <- data %>% select(one_of(Vars)) %>% apply(1, function(x) sum(x == 1, na.rm = T))
data$InsuredInfo_2s <- data %>% select(one_of(Vars)) %>% apply(1, function(x) sum(x == 2, na.rm = T))
data$InsuredInfo_3s <- data %>% select(one_of(Vars)) %>% apply(1, function(x) sum(x == 3, na.rm = T))


Vars <- str_subset(names(data), "Insurance_History")
data$Insurance_History_Sum <- data %>% select(one_of(Vars)) %>% apply(1, sum, na.rm = T)
data$Insurance_History_1s <- data %>% select(one_of(Vars)) %>% apply(1, function(x) sum(x == 1, na.rm = T))
data$Insurance_History_2s <- data %>% select(one_of(Vars)) %>% apply(1, function(x) sum(x == 2, na.rm = T))
data$Insurance_History_3s <- data %>% select(one_of(Vars)) %>% apply(1, function(x) sum(x == 3, na.rm = T))


Vars <- str_subset(names(data), "Family_Hist_[2-5]")
data$Family_Hist_Sum <- data %>% select(one_of(Vars)) %>% apply(1, sum, na.rm = T)
temp <- data %>% select(one_of(Vars))
temp$Family_Hist_2 <- ifelse(is.na(temp$Family_Hist_2), "", "A")
temp$Family_Hist_3 <- ifelse(is.na(temp$Family_Hist_3), "", "B")
temp$Family_Hist_4 <- ifelse(is.na(temp$Family_Hist_4), "", "C")
temp$Family_Hist_5 <- ifelse(is.na(temp$Family_Hist_5), "", "D")
data$Family_Hist_Comb <- str_c(temp[[1]],temp[[2]],temp[[3]],temp[[4]])


temp <- data %>% select(contains("Medical_History"))
Vars <- sapply(temp, function(x) max(x, na.rm = T) < 4 )
temp <- temp[Vars]       
data$Medical_History_Sum <- temp %>% apply(1, sum, na.rm = T)
data$Medical_History_1s <- temp %>% apply(1, function(x) sum(x == 1, na.rm = T))
data$Medical_History_2s <- temp %>% apply(1, function(x) sum(x == 2, na.rm = T))
data$Medical_History_3s <- temp %>% apply(1, function(x) sum(x == 3, na.rm = T))

temp <- data %>% select(contains("Medical_Keyword"))
data$Medical_Keyword_Sum <- temp %>% apply(1, sum, na.rm = T)

data <- separate(data, Product_Info_2, c("Product_Info_2_type", "Product_Info_2_No"), 1, F)
data$Product_Info_2_No <- as.integer(data$Product_Info_2_No)

## fill out NAs
data$NumOfNAs <- apply(data, 1, function(x) sum(is.na(x)))

data$Employment_Info_1[is.na(data$Employment_Info_1)] <- mean(data$Employment_Info_1, na.rm = T)

data$Employment_Info_4na <- ifelse(is.na(data$Employment_Info_4), 1, 0)
data$Employment_Info_4[is.na(data$Employment_Info_4)] <- -1

data$Employment_Info_6na <- ifelse(is.na(data$Employment_Info_6), 1, 0)
data$Employment_Info_6[is.na(data$Employment_Info_6)] <- mean(data$Employment_Info_6, na.rm = T)

data$Insurance_History_5na <- ifelse(is.na(data$Insurance_History_5), 1, 0)
data$Insurance_History_5[is.na(data$Insurance_History_5)] <- 0

data$Family_Hist_2na <- ifelse(is.na(data$Family_Hist_2), 1, 0)
data$Family_Hist_2[is.na(data$Family_Hist_2)] <- 0

data$Family_Hist_3na <- ifelse(is.na(data$Family_Hist_3), 1, 0)
data$Family_Hist_3[is.na(data$Family_Hist_3)] <- 0

data$Family_Hist_4na <- ifelse(is.na(data$Family_Hist_4), 1, 0)
data$Family_Hist_4[is.na(data$Family_Hist_4)] <- 0

data$Family_Hist_5na <- ifelse(is.na(data$Family_Hist_5), 1, 0)
data$Family_Hist_5[is.na(data$Family_Hist_5)] <- 0

data$Medical_History_1na <- ifelse(is.na(data$Medical_History_1), 1, 0)
data$Medical_History_1[is.na(data$Medical_History_1)] <- median(data$Medical_History_1, na.rm = T)

data$Medical_History_15na <- ifelse(is.na(data$Medical_History_15), 1, 0)
data$Medical_History_15[is.na(data$Medical_History_15)] <- median(data$Medical_History_15, na.rm = T)

#The following 3 variables may not needed
data$Medical_History_10[is.na(data$Medical_History_10)] <- -1
data$Medical_History_24[is.na(data$Medical_History_24)] <- -1
data$Medical_History_32[is.na(data$Medical_History_32)] <- -1

Response <- data %>% select(Id, split, Response)
split <- data$split
data <- data %>% select(-Id, -split, -Response)

factorVars <- sapply(data, n_distinct) <= 20
data[,factorVars] <- lapply(data[,factorVars], as.factor)


Ins_Age^2 + Ins_Age^3 + Product_Info_4^(1/2) + BMI^2 +Employment_Info_1^(1/2)
+ Insurance_History_5^(1/5) + Medical_History_15^(1/3) + Medical_History_32^(1/5)

data <- model.matrix(~.-1, data)



train <-  data %>% filter(split == 1)
test <- data %>% filter(split == 0)

