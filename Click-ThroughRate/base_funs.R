require(compiler)
enableJIT(3)
setCompilerOptions(suppressUndefined = T)


library(caret)
library(lubridate)
library(dplyr) 
library(stringr) 
library(glmnet)
library(xgboost)
library(data.table)


xgb.logLoss <- function(pred, dtrain){
    #caculate logLoss
    epsilon <- .000000000000001
    obs <- getinfo(dtrain, "label")
    
    yhat <- pmin(pmax(pred, epsilon), 1-epsilon)
    y <- obs
    
    logloss <- -mean(y * log(yhat) + (1 - y) * log(1 - yhat))
    return(list(metric = "logLoss", value = logloss))
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


################################ feature engineering #############################
featureEngineering <- function(data = NULL){
    
    ##########recode the site_id levels
    #keep the top site unchanged and code others as "other_site"
    site_id_table <- sort(table(data$site_id), decreasing = TRUE)
    top_site_id <- names(site_id_table)[c(1:100, 104, 120, 135, 138, 140)]
    
    data$site_id <- as.character(data$site_id)
    data$site_id[!data$site_id %in% top_site_id] = "other_site"
    data$site_id <- as.factor(data$site_id)
    
    
    #########recode the site_domain levels
    #keep the top site domain unchanged and code others as "other_domain"
    site_domain_table <- sort(table(data$site_domain), decreasing = TRUE)
    top_site_domain <- names(site_domain_table)[c(1:56, 77, 79)]
    
    data$site_domain <- as.character(data$site_domain)
    data$site_domain[!data$site_domain %in% top_site_domain] = "other_site_domain"
    data$site_domain <- as.factor(data$site_domain)
    
    
    #########recode the site_category levels
    #keep the top 4 site domain unchanged and code others as "other_site_category"
    site_category_table <- sort(table(data$site_category), decreasing = TRUE)
    top4_site_category <- names(site_category_table)[1:4]
    
    data$site_category <- as.character(data$site_category)
    data$site_category[!data$site_category %in% top4_site_category] = "other_site_category"
    data$site_category <- as.factor(data$site_category)
    
    
    #########recode the app_id levels
    #keep the top app_id unchanged and code others as "other_app"
    app_id_table <- sort(table(data$app_id), decreasing = TRUE)
    top_app_id <- names(app_id_table)[c(1:60, 91, 93)]
    
    data$app_id <- as.character(data$app_id)
    data$app_id[!data$app_id %in% top_app_id] = "other_app_id"
    data$app_id <- as.factor(data$app_id)
    
    
    #########recode the app_domain levels
    #keep the top 10 app_domain unchanged and code others as "other_domain"
    app_domain_table <- sort(table(data$app_domain), decreasing = TRUE)
    top10_app_domain <- names(app_domain_table)[1:10]
    
    data$app_domain <- as.character(data$app_domain)
    data$app_domain[!data$app_domain %in% top10_app_domain] = "other_domain"
    data$app_domain <- as.factor(data$app_domain)
    
    
    #########recode the app_category levels
    #keep the top 4 site domain unchanged and code others as "other_app_category"
    app_category_table <- sort(table(data$app_category), decreasing = TRUE)
    top4_app_category <- names(app_category_table)[1:4]
    
    data$app_category <- as.character(data$app_category)
    data$app_category[!data$app_category %in% top4_app_category] = "other_app_category"
    data$app_category <- as.factor(data$app_category)
    
    
    #########recode the device_id levels
    #keep the top 1 device_id unchanged and code others as "other_device_id"
    device_id_table <- sort(table(data$device_id), decreasing = TRUE)
    top1_device_id <- names(device_id_table)[1]
    
    data$device_id <- as.character(data$device_id)
    data$device_id[!data$device_id %in% top1_device_id] = "other_device_id"
    data$device_id <- as.factor(data$device_id)
    
    
    
    #########recode the device_ip levels
    #keep important device_ip unchanged and code others as "other_device_ip"
    device_ip_table <- sort(table(data$device_ip), decreasing = TRUE)
    top_device_ip <- names(device_ip_table)[c(1:68, 134, 151, 167)]
    
    data$device_ip <- as.character(data$device_ip)
    data$device_ip[!data$device_ip %in% top_device_ip] = "other_device_ip"
    data$device_ip <- as.factor(data$device_ip)
    
    
    #########recode the device_model levels
    #keep the top device_model unchanged and code others as "other_device_model"
    device_model_table <- sort(table(data$device_model), decreasing = TRUE)
    top_device_model <- names(device_model_table)[c(1:136, 145, 159, 161, 162, 164, 172, 177,
                                                    181, 183, 187, 190, 194, 196, 202, 208, 217, 218, 219, 220,
                                                    225, 240, 245, 253, 254, 258, 267, 276, 288)]
    
    data$device_model <- as.character(data$device_model)
    data$device_model[!data$device_model %in% top_device_model] = "other_device_model"
    data$device_model <- as.factor(data$device_model)
    
    
    #########recode the device_type levels
    data$device_type[data$device_type == 2] <- 1
    data$device_type <- as.factor(data$device_type)
    
    
    #########recode the device_conn_type levels
    data$device_conn_type <- as.factor(data$device_conn_type)
    
    
    ########recode the hour variable
    data$hour <- ymd_h(data$hour)
    data$hours <- as.factor(hour(data$hour))

    
    #########recode the banner_pos levels
    data$banner_pos_factor <- data$banner_pos
    data$banner_pos_factor[data$banner_pos_factor >= 2] <- 1
    data$banner_pos_factor <- as.factor(data$banner_pos_factor)
    
    
    #########recode the C1 levels
    data$C1_factor <- data$C1
    data$C1_factor[data$C1_factor == 1001] <- 1002
    data$C1_factor[data$C1_factor == 1007] <- 1012
    data$C1_factor[data$C1_factor == 1008] <- 1012
    data$C1_factor <- as.factor(data$C1_factor)
    

    #########recode the C14 levels
    data$C14_factor <- data$C14
    C14_factor_table <- sort(table(data$C14_factor), decreasing = TRUE)
    top_C14_factor <- names(C14_factor_table)[1:150]
    
    data$C14_factor <- as.character(data$C14_factor)
    data$C14_factor[!data$C14_factor %in% top_C14_factor] = "other_C14_factor"
    data$C14_factor <- as.factor(data$C14_factor)
    
    
    #########recode the C15 levels
    data$C15_factor <- data$C15   
    data$C15_factor[data$C15_factor == 120] <- 216
    data$C15_factor[data$C15_factor >= 320] <- 320
    data$C15_factor <- as.factor(data$C15_factor)
    
 
    #########recode the C16 levels
    data$C16_factor <- data$C16   
    data$C16_factor[data$C16_factor == 20] <- 90
    data$C16_factor[data$C16_factor == 320] <- 90
    data$C16_factor[data$C16_factor >= 768] <- 90
    data$C16_factor <- as.factor(data$C16_factor)

    
    #########recode the C17 levels
    data$C17_factor <- data$C17
    C17_factor_table <- sort(table(data$C17_factor), decreasing = TRUE)
    top_C17_factor <- names(C17_factor_table)[1:100]
    
    data$C17_factor <- as.character(data$C17_factor)
    data$C17_factor[!data$C17_factor %in% top_C17_factor] = "other_C17_factor"
    data$C17_factor <- as.factor(data$C17_factor)
    
  
    #########recode the C18 levels
    data$C18_factor <- data$C18   
    data$C18_factor <- as.factor(data$C18_factor)
    
    
    #########recode the C19 levels
    data$C19_factor <- data$C19
    C19_factor_table <- sort(table(data$C19_factor), decreasing = TRUE)
    top_C19_factor <- names(C19_factor_table)[1:35]
    
    data$C19_factor <- as.character(data$C19_factor)
    data$C19_factor[!data$C19_factor %in% top_C19_factor] = "other_C19_factor"
    data$C19_factor <- as.factor(data$C19_factor)   
    
 
    #########recode the C20 levels
    data$C20_factor <- data$C20
    C20_factor_table <- sort(table(data$C20_factor), decreasing = TRUE)
    top_C20_factor <- names(C20_factor_table)[1:30]
    
    data$C20_factor <- as.character(data$C20_factor)
    data$C20_factor[!data$C20_factor %in% top_C20_factor] = "other_C20_factor"
    data$C20_factor <- as.factor(data$C20_factor) 
  
    
    #########recode the C21 levels
    data$C21_factor <- data$C21
    C21_factor_table <- sort(table(data$C21_factor), decreasing = TRUE)
    top_C21_factor <- names(C21_factor_table)[1:35]
    
    data$C21_factor <- as.character(data$C21_factor)
    data$C21_factor[!data$C21_factor %in% top_C21_factor] = "other_C20_factor"
    data$C21_factor <- as.factor(data$C21_factor)
    
    data
}





#xgb.importance
xgb.importance <- function(feature_names = NULL, filename_dump = NULL){  
    if (!class(feature_names) %in% c("character", "NULL")) {       
        stop("feature_names: Has to be a vector of character or NULL if the model dump already contains feature name. Look at this function documentation to see where to get feature names.")
    }
    if (class(filename_dump) != "character" & file.exists(filename_dump)) {
        stop("filename_dump: Has to be a path to the model dump file.")
    }
    text <- readLines(filename_dump)
    if(text[2] == "bias:"){
        result <- linearDump(feature_names, text)
    }  else {
        result <- treeDump(feature_names, text)
    }
    result
}

treeDump <- function(feature_names, text){
    featureVec <- c()
    gainVec <- c()
    for(line in text){
        p <- str_extract(line, "\\[f.*<")
        if (!is.na(p)) {
            featureVec <- substr(p, 3, nchar(p)-1) %>% c(featureVec)
            gainVec <- str_extract(line, "gain.*,") %>%  substr(x = ., 6, nchar(.)-1) %>% as.numeric %>% c(gainVec)
        }
    }
    if(!is.null(feature_names)) {
        featureVec %<>% as.numeric %>% {c =.+1; feature_names[c]} #+1 because in R indexing start with 1 instead of 0.
    }
    #1. Reduce, 2. %, 3. reorder - bigger top, 4. remove temp col
    data.table(Feature = featureVec, Weight = gainVec)[,list(sum(Weight), .N), by = Feature][, Gain:= V1/sum(V1)][,Weight:= N/sum(N)][order(-rank(Gain))][,-c(2,3), with = F]
}

linearDump <- function(feature_names, text){
    which(text == "weight:") %>% {a=.+1;text[a:length(text)]} %>% as.numeric %>% data.table(Feature = feature_names, Weight = .)
}