getPromo2Features <- function(data)
{
    temp <- data %>% filter(Promo2 == 1, Date >= Promo2Since) %>% 
        select(Store, Year, Yday, Date, Month, PromoInterval, Promo2Since) %>%
        separate(PromoInterval, c("PromoInterval1","PromoInterval2","PromoInterval3","PromoInterval4")
                 , ",", convert = T)
    
    #find the right interval for promothion 2 due day
    rowIndicator <- (temp$Month >= temp$PromoInterval1) & (temp$Month < temp$PromoInterval4)
    dueIndex <- ceiling((temp$Month[rowIndicator] - temp$PromoInterval1[rowIndicator] + 1) / 3) + 1
     
    temp$Promo2DueMonth <- temp$PromoInterval1
    temp$Promo2DueMonth[rowIndicator] <- temp$PromoInterval1[rowIndicator] + (dueIndex - 1) * 3
    
    overYearRows <- temp$Month >= temp$PromoInterval4
    temp$Promo2DueYear <- temp$Year
    temp$Promo2DueYear[overYearRows] <- temp$Year[overYearRows] + 1
    
    temp$Promo2DueDay <- str_c(temp$Promo2DueYear, "-", temp$Promo2DueMonth, "-01")
    temp$Promo2DaysLeft <- (ymd(temp$Promo2DueDay) - temp$Date) / ddays(1)
    temp <- select(temp, Store, Date, Promo2DaysLeft)
    temp
}

RMSPE <- function (data, lev = NULL, model = NULL) 
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
            out <- RMSE(1, pred/obs)
        }
        names(out) <- c("RMSPE")
    }
    
    if (any(is.nan(out))) 
        out[is.nan(out)] <- NA
    out
}

logRMSPE <- function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    
    obs <- exp(data[, "obs"])
    pred <- exp(data[, "pred"])
    
    isNA <- is.na(pred)
    pred <- pred[!isNA]
    obs <- obs[!isNA]
    
    if (!is.factor(obs) & is.numeric(obs)) {
        if (length(obs) + length(pred) == 0) {
            out <- rep(NA, 2)
        }
        else {
            out <- RMSE(1, pred/obs)
        }
        names(out) <- c("RMSPE")
    }
    
    if (any(is.nan(out))) 
        out[is.nan(out)] <- NA
    out
}