
############################### get the initial train data ##############################
ctrData <- read.csv.ffdf(file = "./train/train.csv", header=TRUE, 
                         colClasses = c("factor", rep("integer", 4),
                                        rep("factor", 9), rep("integer", 10)))
iniTrain <- ctrData[,]
iniTrain$id <- 1:nrow(iniTrain)
save(list = "iniTrain", file = "iniTrain.RData")


################################ feature engineering #############################

##########recode the site_id levels
#keep the top 90 site unchanged and code others as "other_site"???????????????? or 50
site_id_table <- sort(table(data$site_id), decreasing = TRUE)
top150_site_id <- names(site_id_table)[1:150]

data$site_id <- as.character(data$site_id)
data$site_id[!data$site_id %in% top150_site_id] = "other_site"
data$site_id <- as.factor(data$site_id)


#########recode the site_domain levels
#keep the top 60 site domain unchanged and code others as "other_domain"?????????????or40
site_domain_table <- sort(table(data$site_domain), decreasing = TRUE)
top100_site_domain <- names(site_domain_table)[1:100]

data$site_domain <- as.character(data$site_domain)
data$site_domain[!data$site_domain %in% top100_site_domain] = "other_domain"
data$site_domain <- as.factor(data$site_domain)


#########recode the site_category levels
#keep the top 4 site domain unchanged and code others as "other_site_category"
site_category_table <- sort(table(data$site_category), decreasing = TRUE)
top4_site_category <- names(site_category_table)[1:4]

data$site_category <- as.character(data$site_category)
data$site_category[!data$site_category %in% top4_site_category] = "other_site_category"
data$site_category <- as.factor(data$site_category)


#########recode the app_id levels
#keep the top 60 app_id unchanged and code others as "other_app"?????????????or40
app_id_table <- sort(table(data$app_id), decreasing = TRUE)
top100_app_id <- names(app_id_table)[1:100]

data$app_id <- as.character(data$app_id)
data$app_id[!data$app_id %in% top100_app_id] = "other_app"
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
#keep the top 200 device_ip unchanged and code others as "other_device_ip"
device_ip_table <- sort(table(data$device_ip), decreasing = TRUE)
top200_device_ip <- names(device_ip_table)[1:200]

data$device_ip <- as.character(data$device_ip)
data$device_ip[!data$device_ip %in% top200_device_ip] = "other_device_ip"
data$device_ip <- as.factor(data$device_ip)


#########recode the device_model levels
#keep the top 400 device_model unchanged and code others as "other_device_model"
device_model_table <- sort(table(data$device_model), decreasing = TRUE)
top300_device_model <- names(device_model_table)[1:300]

data$device_model <- as.character(data$device_model)
data$device_model[!data$device_model %in% top300_device_model] = "other_device_model"
data$device_model <- as.factor(data$device_model)


#########recode the device_type levels
data$device_type[data$device_type == 2] <- 1
data$device_type <- as.factor(as.character(data$device_type))


#########recode the device_conn_type levels
data$device_conn_type <- as.factor(as.character(data$device_conn_type))











#########recode the C1 levels
data$C1[data$C1 == 1008] <- 1007
data$C1 <- as.factor(as.character(data$C1))



#########recode the C1 levels
data$C1[data$C1 == 1008] <- 1007
data$C1 <- as.factor(as.character(data$C1))


#########recode the C15 levels
data$C15[data$C15 == 120] <- 216
data$C15[data$C15 >= 480] <- 480
data$C15 <- as.factor(as.character(data$C15))


#########recode the C16 levels
data$C16[data$C16 == 20] <- 36
data$C16[data$C16 >= 320] <- 480
data$C16 <- as.factor(as.character(data$C16))



#########recode the C17 levels
#keep the top 400 device_model unchanged and code others as "other_device_model"
C17_table <- sort(table(data$C17), decreasing = TRUE)
top300_C17 <- names(C17_table)[1:300]

data$device_model <- as.character(data$device_model)
data$device_model[!data$device_model %in% top300_device_model] = "other_device_model"
data$device_model <- as.factor(data$device_model)

#########recode C14, C17, C19, C20   
#we put c14, C17 into several bins according to the histogram
#hist(data$C14, breaks = 400)
data$C14 <- cut2(data$C14, c(5000, 15000, 16000, 19000, 21000,22000))

#hist(data$C17, breaks = 200)
data$C17 <- cut2(data$C17, c(1600, 1750, 2200, 2400, 2460, 2600))


data$C19 <- cut2(data$C19, c(36, 40, 100, 164, 250, 350, 750, 1250))
data$C20 <- cut2(data$C20, c(1, 100070, 100076, 100080, 100084, 100106, 100144, 100156))
data$C21 <- cut2(data$C21, c(24, 33, 40, 47, 50, 60, 70, 77, 80, 220))


#########recode the following vars according to the return value of table func
data$device_type <- recode(data$device_type,"'2'= '1'")
data$banner_pos <- recode(data$banner_pos, "c(3,4,5,7) = '2'")
data$device_conn_type <- recode(data$device_conn_type, "'5' = '3'")

data$C1 <- recode(data$C1, "'1001' = '1002'; c(1007, 1008) = '1010'")
data$C15 <- recode(data$C15, "c(120, 480, 728, 768, 1024) = '216'")
data$C16 <- recode(data$C16, "c(20, 90) = '36'; c(320, 768, 1024) = '480'")
data$click <- relevel(data$click, ref = "1")

########recode the hour variable
data$hour <- ymd_h(data$hour)
data$weekDays <- as.factor(weekdays(data$hour))
data$hours <- as.factor(hour(data$hour))
data$hour <- NULL

save(list = "data", file = "tidyTrainData.RData")




############################### same procedure to tidy the test data ####################

############################### get the initial test data 
data <- read.csv(file = "./test/test.csv", colClasses = c("id" = "character"))
save(list = "data", file = "iniTestData.RData")


################################ feature engineering #############################
#change all non factor vars to factor vars but "id" and "hour"
factorVars <- sapply(data, is.factor)
tmp <- setdiff(names(data)[!factorVars], c("id", "hour", "C14", "C17", "C19", "C20", "C21"))
data[, tmp] <- lapply(data[, tmp], as.factor)

##########recode the site_id levels
#keep the top 90 site unchanged and code others as "other_site"???????????????? or 50
site_id_table <- sort(table(data$site_id), decreasing = TRUE)
top90_site_id <- names(site_id_table)[1:90]

data$site_id <- as.character(data$site_id)
data$site_id[!data$site_id %in% top90_site_id] = "other_site"
data$site_id <- as.factor(data$site_id)


#########recode the site_domain levels
#keep the top 60 site domain unchanged and code others as "other_domain"?????????????or40
site_domain_table <- sort(table(data$site_domain), decreasing = TRUE)
top60_site_domain <- names(site_domain_table)[1:60]

data$site_domain <- as.character(data$site_domain)
data$site_domain[!data$site_domain %in% top60_site_domain] = "other_domain"
data$site_domain <- as.factor(data$site_domain)


#########recode the site_category levels
#keep the top 4 site domain unchanged and code others as "other_site_category"
site_category_table <- sort(table(data$site_category), decreasing = TRUE)
top4_site_category <- names(site_category_table)[1:4]

data$site_category <- as.character(data$site_category)
data$site_category[!data$site_category %in% top4_site_category] = "other_site_category"
data$site_category <- as.factor(data$site_category)


#########recode the app_id levels
#keep the top 60 app_id unchanged and code others as "other_app"?????????????or40
app_id_table <- sort(table(data$app_id), decreasing = TRUE)
top60_app_id <- names(app_id_table)[1:60]

data$app_id <- as.character(data$app_id)
data$app_id[!data$app_id %in% top60_app_id] = "other_app"
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


#########recode the device_model levels
#keep the top 60 device_model unchanged and code others as "other_device_model"
device_model_table <- sort(table(data$device_model), decreasing = TRUE)
top60_device_model <- names(device_model_table)[1:60]

data$device_model <- as.character(data$device_model)
data$device_model[!data$device_model %in% top60_device_model] = "other_device_model"
data$device_model <- as.factor(data$device_model)


#########remove device_ip 
data$device_ip <- NULL


#########recode C14, C17, C19, C20   
#we put c14, C17 into several bins according to the histogram
#hist(data$C14, breaks = 400)
data$C14 <- cut2(data$C14, c(5000, 15000, 16000, 19000, 21000,22000))

#hist(data$C17, breaks = 200)
data$C17 <- cut2(data$C17, c(1600, 1750, 2200, 2400, 2460, 2600))


data$C19 <- cut2(data$C19, c(36, 40, 100, 164, 250, 350, 750, 1250))
data$C20 <- cut2(data$C20, c(1, 100070, 100076, 100080, 100084, 100106, 100144, 100156))
data$C21 <- cut2(data$C21, c(24, 33, 40, 47, 50, 60, 70, 77, 80, 220))


#########recode the following vars according to the return value of table func
data$device_type <- recode(data$device_type,"'2'= '1'")
data$banner_pos <- recode(data$banner_pos, "c(3,4,5,7) = '2'")
data$device_conn_type <- recode(data$device_conn_type, "'5' = '3'")

data$C1 <- recode(data$C1, "'1001' = '1002'; c(1007, 1008) = '1010'")
data$C15 <- recode(data$C15, "c(120, 480, 728, 768, 1024) = '216'")
data$C16 <- recode(data$C16, "c(20, 90) = '36'; c(320, 768, 1024) = '480'")

########recode the hour variable
data$hour <- ymd_h(data$hour)
data$weekDays <- as.factor(weekdays(data$hour))
data$hours <- as.factor(hour(data$hour))
data$hour <- NULL

save(list = "data", file = "tidyTestData.RData")
