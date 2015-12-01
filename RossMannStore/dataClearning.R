library(caret)
library(plyr)
library(dplyr)
library(xgboost)
library(tidyr)
library(ranger)
library(lubridate)
library(stringr)
library(car)
library(doParallel)

source('E:/KaggleProject/RossMannStore/myFuns.R')
setwd("E:/KaggleProject/RossMannStore")

train <- read.csv("E:/KaggleProject/RossMannStore/train.csv/train.csv", stringsAsFactors=FALSE)
test <- read.csv("E:/KaggleProject/RossMannStore/test.csv/test.csv", stringsAsFactors=FALSE)
store <- read.csv("E:/KaggleProject/RossMannStore/store.csv/store.csv", stringsAsFactors=FALSE)
submission <- read.csv("E:/KaggleProject/RossMannStore/submission.csv", stringsAsFactors=FALSE)

test[is.na(test)] <- 1
testIDOpen <- select(test, Id, Open)
test$Id <- NULL
test$Customers <- -1
test$Sales <- -1
test$split <- 0


train <- select(train, Store:Date, Open:SchoolHoliday, Customers, Sales)
train$split <- 1

data <- bind_rows(train, test)

data$Date <- ymd(data$Date)
data$Year <- year(data$Date)              #need to remove later
data$Month <- month(data$Date)
data$Week <- week(data$Date)
data$Mday <- mday(data$Date)
data$Yday <- yday(data$Date)

##find the date which right before and after a close day
temp <- data %>% filter(Open == 0) %>%
    mutate(before = Date - days(1), beforeCloseDay = 1) %>%
    select(Store, before, beforeCloseDay)
data <- left_join(data, temp, by = c("Store" = "Store", "Date" = "before"))
data$beforeCloseDay[is.na(data$beforeCloseDay)] = 0

temp <- data %>% filter(Open == 0) %>%
    mutate(after = Date + days(1), afterCloseDay = 1) %>%
    select(Store, after, afterCloseDay)
data <- left_join(data, temp, by = c("Store" = "Store", "Date" = "after"))
data$afterCloseDay[is.na(data$afterCloseDay)] = 0

data <- filter(data, Open == 1) %>% filter(Sales >= 200 | Sales == -1)

Sales <- data %>% filter(split == 1) %>% select(Store, Date, Sales)

#create costumer related features
temp <- data %>% filter(split == 1) %>% 
    select(Store, Year, Month, Date, Promo, DayOfWeek)


##impute store data
store$Promo2SinceYear <- str_c(store$Promo2SinceYear, "0101")
store$Promo2Since <- ymd(store$Promo2SinceYear) + weeks(store$Promo2SinceWeek)
store$Promo2SinceYear <- NULL
store$Promo2SinceWeek <- NULL

store$CompetitionOpenSince <- str_c(store$CompetitionOpenSinceYear, "-", store$CompetitionOpenSinceMonth, "-01")
store$CompetitionOpenSince <- ymd(store$CompetitionOpenSince)
store$CompetitionOpenSinceYear <- NULL
store$CompetitionOpenSinceMonth <- NULL
store$CompetitionOpenSince[store$CompetitionOpenSince < ymd("1990-06-01")] = ymd("1990-01-01")
store$CompetitionOpenSince[is.na(store$CompetitionOpenSince)] = ymd("2011-01-01")

store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 99999

data <- inner_join(data, store)
data$DaysOfCompetion <- (data$Date - data$CompetitionOpenSince) / ddays(1)
data$DaysOfCompetion[data$DaysOfCompetion < 0] <- 0
data$CompetitionOpenSince <- NULL


data$DaysOfPromo2 <- (data$Date - data$Promo2Since) / ddays(1)
data$DaysOfPromo2[data$DaysOfPromo2 < 0] <- 0
data$DaysOfPromo2[is.na(data$DaysOfPromo2)] <- 0

data$PromoInterval <- recode(data$PromoInterval, 
                             "'Feb,May,Aug,Nov' = '2,5,8,11';
                             'Jan,Apr,Jul,Oct' = '1,4,7,10';
                             'Mar,Jun,Sept,Dec' = '3,6,9,12'")

data$StoreType <- recode(data$StoreType,"'a' = '1'; 'b' = '2'; 'c' = '3'; 'd' = '4'")
data$Assortment <- recode(data$Assortment,"'a' = '1'; 'b' = '2'; 'c' = '3'")

temp <- getPromo2Features(data)
data <- left_join(data, temp)

data$Promo2Since <- NULL
data$Promo2DaysLeft[is.na(data$Promo2DaysLeft)] <- -1

data$PromoInterval <- recode(data$PromoInterval,"'' = '0'; '2,5,8,11' = '2';
                             '1,4,7,10' = '1'; '3,6,9,12' = '3'")

split <- data$split
data$split <- NULL
data$Open <- NULL
data$Sales <- NULL
data$StateHoliday <- NULL
data$Customers <- NULL
