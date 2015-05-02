source(file = "base_funs.R")


############################### get the initial train data and feature engineer######################
ctrData <- read.csv.ffdf(file = "./train/train.csv", header=TRUE, 
                         colClasses = c("factor", rep("integer", 4),
                                        rep("factor", 9), rep("integer", 10)))
iniTrain <- ctrData[,]
iniTrain$id <- 1:nrow(iniTrain)

save(list = "iniTrain", file = "iniTrain.RData")


############################### get the initial test data and feature engineer##############
iniTest <- read.csv(file = "./test/test.csv", colClasses = c("id" = "character"))
save(list = "iniTest", file = "iniTest.RData")


##############################feature engineer#########################################
#feature engineering for initial test data
tidyTestData <- featureEngineering(data = iniTest)
save(list = "tidyTestData", file = "tidyTestData.RData")


#feature engineering for initial train data
tidyTrainData <- featureEngineering(data = iniTrain)
save(list = "tidyTrainData", file = "tidyTrainData.RData")


##########create one way count features

######site counts
iniTrain <- iniTrain %>% group_by(site_id) %>% mutate(site_id_cnt = n())
iniTrain <- iniTrain %>% group_by(site_domain) %>% mutate(site_domain_cnt = n())
iniTrain <- iniTrain %>% group_by(site_category) %>% mutate(site_category_cnt = n())
iniTrain <- iniTrain %>% group_by(site_id,site_domain) %>% mutate(site_id_domain = n())
iniTrain <- iniTrain %>% group_by(site_id,site_category) %>% mutate(site_id_category = n())
iniTrain <- iniTrain %>% group_by(site_domain,site_category) %>% mutate(site_domain_category = n())
iniTrain <- iniTrain %>% group_by(site_id,site_domain,site_category) %>% mutate(site_id_domain_category = n())

temp <- iniTrain %>% group_by(site_id) %>% summarise(site_id_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(site_domain) %>% summarise(site_domain_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(site_category) %>% summarise(site_category_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(site_id,site_domain) %>% summarise(site_id_domain = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(site_id,site_category) %>% summarise(site_id_category = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(site_domain,site_category) %>% summarise(site_domain_category = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(site_id,site_domain,site_category) %>% summarise(site_id_domain_category = n())
iniTest <- left_join(iniTest, temp)


######app counts
iniTrain <- iniTrain %>% group_by(app_id) %>% mutate(app_id_cnt = n())
iniTrain <- iniTrain %>% group_by(app_domain) %>% mutate(app_domain_cnt = n())
iniTrain <- iniTrain %>% group_by(app_category) %>% mutate(app_category_cnt = n())
iniTrain <- iniTrain %>% group_by(app_id,app_domain) %>% mutate(app_id_domain_cnt = n())
iniTrain <- iniTrain %>% group_by(app_id,app_category) %>% mutate(app_id_category_cnt = n())
iniTrain <- iniTrain %>% group_by(app_domain,app_category) %>% mutate(app_domain_category_cnt = n())
iniTrain <- iniTrain %>% group_by(app_id,app_domain,app_category) %>% mutate(app_id_domain_category_cnt = n())


temp <- iniTrain %>% group_by(app_id) %>% summarise(app_id_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(app_domain) %>% summarise(app_domain_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(app_category) %>% summarise(app_category_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(app_id,app_domain) %>% summarise(app_id_domain_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(app_id,app_category) %>% summarise(app_id_category_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(app_domain,app_category) %>% summarise(app_domain_category_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(app_id,app_domain,app_category) %>% summarise(app_id_domain_category_cnt = n())
iniTest <- left_join(iniTest, temp)



######device counts
iniTrain <- iniTrain %>% group_by(device_id) %>% mutate(device_id_cnt = n())
iniTrain <- iniTrain %>% group_by(device_ip) %>% mutate(device_ip_cnt = n())
iniTrain <- iniTrain %>% group_by(device_model) %>% mutate(device_model_cnt = n())
iniTrain <- iniTrain %>% group_by(device_type, device_model) %>% mutate(device_type_model_cnt = n())
iniTrain <- iniTrain %>% group_by(device_conn_type, device_model) %>% mutate(device_conn_model_cnt = n())


temp <- iniTrain %>% group_by(device_id) %>% summarise(device_id_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(device_ip) %>% summarise(device_ip_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(device_model) %>% summarise(device_model_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(device_type, device_model) %>% summarise(device_type_model_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(device_conn_type, device_model) %>% summarise(device_conn_model_cnt = n())
iniTest <- left_join(iniTest, temp)



######Cs counts
iniTrain <- iniTrain %>% group_by(C14) %>% mutate(C14_cnt = n())
iniTrain <- iniTrain %>% group_by(C17) %>% mutate(C17_cnt = n())
iniTrain <- iniTrain %>% group_by(C19) %>% mutate(C19_cnt = n())
iniTrain <- iniTrain %>% group_by(C20) %>% mutate(C20_cnt = n())
iniTrain <- iniTrain %>% group_by(C21) %>% mutate(C21_cnt = n())

temp <- iniTrain %>% group_by(C14) %>% summarise(C14_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C17) %>% summarise(C17_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C19) %>% summarise(C19_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C20) %>% summarise(C20_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C21) %>% summarise(C21_cnt = n())
iniTest <- left_join(iniTest, temp)

######banner_pos and other counts
iniTrain <- iniTrain %>% group_by(banner_pos, site_id) %>% mutate(banner_pos_site_id_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, app_id) %>% mutate(banner_pos_app_id_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, device_id) %>% mutate(banner_pos_device_id_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, device_ip) %>% mutate(banner_pos_device_ip_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, C14) %>% mutate(banner_pos_C14_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, C21) %>% mutate(banner_pos_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, C1) %>% mutate(banner_pos_C1_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, C16) %>% mutate(banner_pos_C16_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, C17) %>% mutate(banner_pos_C17_cnt = n())
iniTrain <- iniTrain %>% group_by(banner_pos, C18) %>% mutate(banner_pos_C18_cnt = n())


temp <- iniTrain %>% group_by(banner_pos, site_id) %>% summarise(banner_pos_site_id_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, app_id) %>% summarise(banner_pos_app_id_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, device_id) %>% summarise(banner_pos_device_id_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, device_ip) %>% summarise(banner_pos_device_ip_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, C14) %>% summarise(banner_pos_C14_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, C21) %>% summarise(banner_pos_C21_cnt = n())
iniTest <- left_join(iniTest, temp)

temp <- iniTrain %>% group_by(banner_pos, C1) %>% summarise(banner_pos_C1_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, C16) %>% summarise(banner_pos_C16_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, C17) %>% summarise(banner_pos_C17_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(banner_pos, C18) %>% summarise(banner_pos_C18_cnt = n())
iniTest <- left_join(iniTest, temp)


###### counts of two Cs
iniTrain <- iniTrain %>% group_by(C1, C18) %>% mutate(C1_C18_cnt = n())
iniTrain <- iniTrain %>% group_by(C16, C18) %>% mutate(C16_C18_cnt = n())
iniTrain <- iniTrain %>% group_by(C16, C21) %>% mutate(C16_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C1, C19) %>% mutate(C1_C19_cnt = n())
iniTrain <- iniTrain %>% group_by(C18, C19) %>% mutate(C18_C19_cnt = n())
iniTrain <- iniTrain %>% group_by(C16, C19) %>% mutate(C16_C19_cnt = n())
iniTrain <- iniTrain %>% group_by(C1, C16) %>% mutate(C1_C16_cnt = n())
iniTrain <- iniTrain %>% group_by(C19, C21) %>% mutate(C19_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C15, C19) %>% mutate(C15_C19_cnt = n())
iniTrain <- iniTrain %>% group_by(C15, C21) %>% mutate(C15_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C20, C21) %>% mutate(C20_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C18, C20) %>% mutate(C18_C20_cnt = n())
iniTrain <- iniTrain %>% group_by(C15, C18) %>% mutate(C15_C18_cnt = n())
iniTrain <- iniTrain %>% group_by(C18, C21) %>% mutate(C18_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C1, C21) %>% mutate(C1_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C14, C21) %>% mutate(C14_C21_cnt = n())
iniTrain <- iniTrain %>% group_by(C15, C17) %>% mutate(C15_C17_cnt = n())
iniTrain <- iniTrain %>% group_by(C14, C16) %>% mutate(C14_C16_cnt = n())


temp <- iniTrain %>% group_by(C1, C18) %>% summarise(C1_C18_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C16, C18) %>% summarise(C16_C18_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C16, C21) %>% summarise(C16_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C1, C19) %>% summarise(C1_C19_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C18, C19) %>% summarise(C18_C19_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C16, C19) %>% summarise(C16_C19_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C1, C16) %>% summarise(C1_C16_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C19, C21) %>% summarise(C19_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C15, C19) %>% summarise(C15_C19_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C15, C21) %>% summarise(C15_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C20, C21) %>% summarise(C20_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C18, C20) %>% summarise(C18_C20_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C15, C18) %>% summarise(C15_C18_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C18, C21) %>% summarise(C18_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C1, C21) %>% summarise(C1_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C14, C21) %>% summarise(C14_C21_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C15, C17) %>% summarise(C15_C17_cnt = n())
iniTest <- left_join(iniTest, temp)
temp <- iniTrain %>% group_by(C14, C16) %>% summarise(C14_C16_cnt = n())
iniTest <- left_join(iniTest, temp)



####################################get smoothed likelihood features ###############################
iniTest <- iniTest[, 1:23]

noise = 0.3
pAve <- mean(iniTrain$click)
adj <- 20
len <- nrow(iniTrain)

iniTrain <- iniTrain %>% group_by(C1) %>% mutate(temp = (sum(click) - click),
                                                 C1_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C1_exp <- iniTrain$C1_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C14) %>% mutate(temp = (sum(click) - click),
                                                 C14_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C14_exp <- iniTrain$C14_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C15) %>% mutate(temp = (sum(click) - click),
                                                  C15_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C15_exp <- iniTrain$C15_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C16) %>% mutate(temp = (sum(click) - click),
                                                  C16_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C16_exp <- iniTrain$C16_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C17) %>% mutate(temp = (sum(click) - click),
                                                  C17_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C17_exp <- iniTrain$C17_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C18) %>% mutate(temp = (sum(click) - click),
                                                  C18_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C18_exp <- iniTrain$C18_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C19) %>% mutate(temp = (sum(click) - click),
                                                  C19_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C19_exp <- iniTrain$C19_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C20) %>% mutate(temp = (sum(click) - click),
                                                  C20_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C20_exp <- iniTrain$C20_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(C21) %>% mutate(temp = (sum(click) - click),
                                                  C21_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$C21_exp <- iniTrain$C21_exp * (1 + (runif(len) - 0.5) * noise)


iniTrain <- iniTrain %>% group_by(banner_pos) %>% mutate(temp = (sum(click) - click),
                                                         banner_pos_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$banner_pos_exp <- iniTrain$banner_pos_exp * (1 + (runif(len) - 0.5) * noise)


iniTrain <- iniTrain %>% group_by(site_id) %>% mutate(temp = (sum(click) - click),
                                                      site_id_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$site_id_exp <- iniTrain$site_id_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(site_domain) %>% mutate(temp = (sum(click) - click),
                                                      site_domain_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$site_domain_exp <- iniTrain$site_domain_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(site_category) %>% mutate(temp = (sum(click) - click),
                                                            site_category_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$site_category_exp <- iniTrain$site_category_exp * (1 + (runif(len) - 0.5) * noise)



iniTrain <- iniTrain %>% group_by(app_id) %>% mutate(temp = (sum(click) - click),
                                                     app_id_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$app_id_exp <- iniTrain$app_id_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(app_domain) %>% mutate(temp = (sum(click) - click),
                                                     app_domain_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$app_domain_exp <- iniTrain$app_domain_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(app_category) %>% mutate(temp = (sum(click) - click),
                                                           app_category_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$app_category_exp <- iniTrain$app_category_exp * (1 + (runif(len) - 0.5) * noise)




iniTrain <- iniTrain %>% group_by(device_id) %>% mutate(temp = (sum(click) - click),
                                                        device_id_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$device_id_exp <- iniTrain$device_id_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(device_ip) %>% mutate(temp = (sum(click) - click),
                                                        device_ip_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$device_ip_exp <- iniTrain$device_ip_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(device_model) %>% mutate(temp = (sum(click) - click),
                                                           device_model_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$device_model_exp <- iniTrain$device_model_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(device_type) %>% mutate(temp = (sum(click) - click),
                                                          device_type_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$device_type_exp <- iniTrain$device_type_exp * (1 + (runif(len) - 0.5) * noise)

iniTrain <- iniTrain %>% group_by(device_conn_type) %>% mutate(temp = (sum(click) - click),
                                                               device_conn_type_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$device_conn_type_exp <- iniTrain$device_conn_type_exp * (1 + (runif(len) - 0.5) * noise)


iniTrain <- iniTrain %>% group_by(hours) %>% mutate(temp = (sum(click) - click),
                                                    hours_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$hours_exp <- iniTrain$hours_exp * (1 + (runif(len) - 0.5) * noise)




iniTrain <- iniTrain %>% group_by(banner_pos, C1) %>% mutate(temp = (sum(click) - click),
                                                             banner_pos_C1_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$banner_pos_C1_exp <- iniTrain$banner_pos_C1_exp * (1 + (runif(len) - 0.5) * noise)


iniTrain <- iniTrain %>% group_by(banner_pos, C14) %>% mutate(temp = (sum(click) - click),
                                                              banner_pos_C14_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$banner_pos_C14_exp <- iniTrain$banner_pos_C14_exp * (1 + (runif(len) - 0.5) * noise)


iniTrain <- iniTrain %>% group_by(banner_pos, C21) %>% mutate(temp = (sum(click) - click),
                                                              banner_pos_C21_exp = (temp + adj * pAve)/(n() - 1 + adj))
iniTrain$banner_pos_C21_exp <- iniTrain$banner_pos_C21_exp * (1 + (runif(len) - 0.5) * noise)




#############get expect for test data######################
tmp <- iniTrain %>% group_by(C1) %>% summarise(C1_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C14) %>% summarise(C14_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C15) %>% summarise(C15_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C16) %>% summarise(C16_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C17) %>% summarise(C17_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C18) %>% summarise(C18_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C19) %>% summarise(C19_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C20) %>% summarise(C20_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(C21) %>% summarise(C21_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)


tmp <- iniTrain %>% group_by(banner_pos) %>% summarise(banner_pos_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(site_id) %>% summarise(site_id_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(site_domain) %>% summarise(site_domain_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(site_category) %>% summarise(site_category_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)


tmp <- iniTrain %>% group_by(app_id) %>% summarise(app_id_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(app_domain) %>% summarise(app_domain_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(app_category) %>% summarise(app_category_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)


tmp <- iniTrain %>% group_by(device_id) %>% summarise(device_id_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(device_ip) %>% summarise(device_ip_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(device_model) %>% summarise(device_model_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(device_type) %>% summarise(device_type_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(device_conn_type) %>% summarise(device_conn_type_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)

tmp <- iniTrain %>% group_by(hours) %>% summarise(hours_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)


tmp <- iniTrain %>% group_by(banner_pos, C14) %>% summarise(banner_pos_C14_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(banner_pos, C21) %>% summarise(banner_pos_C21_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)
tmp <- iniTrain %>% group_by(banner_pos, C1) %>% summarise(banner_pos_C1_exp = (sum(click) + adj * pAve)/(n() + adj))
iniTest <- left_join(iniTest, tmp)


iniTest[is.na(iniTest)] <- pAve
save(list = "iniTest", file = "iniTest.RData")

##################get factor/numerical data for train and test respectively#############
##for test data
numericalVars <- names(iniTest)[c(1,3,4,14:75)]
numerical_test <- iniTest[, numericalVars]
save(list = "numerical_test", file = "numerical_test.RData")

factorVars <- names(tidyTestData)[c(1,5:15,24:34)]
factor_test <- tidyTestData[, factorVars]
save(list = "factor_test", file = "factor_test.RData")

##for train data
factorVars <- names(tidyTrainData)[c(1:2,6:16,25:35)]
factor_train <- tidyTrainData[, factorVars]
save(list = "factor_train", file = "factor_train.RData")

numericalVars <- names(iniTrain)[c(1:2,4:5,15:76)]
numerical_train <- iniTrain[, numericalVars]
save(list = "numerical_train", file = "numerical_train.RData")

#################################split train data into 10 folds######################
set.seed(87334)
folds <- createFolds(y = factor_train$click, k = 10)


for(i in 1:10){
    factor_training <- factor_train[folds[[i]],]
    save(list = "factor_training", file = paste("factor_training", i, ".RData", sep = ""))
}

for(i in 1:10){
    numerical_training <- numerical_train[folds[[i]],]
    save(list = "numerical_training", file = paste("numerical_training", i, ".RData", sep = ""))
}