library(reshape)
setwd("/home/venom/Documents/dataset")
cdr_input <- read.csv('sms-call-internet-mi-2013-12-01.txt',sep="\t",header=F)
colnames(cdr_input) <- c("square_id","time_interval","country_code","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity")
library(dplyr)
cdr_input_bkp <- cdr_input
cdr_input_cln <- cdr_input%>% group_by(country_code) %>%mutate(sms_in_activity = ifelse(is.na(sms_in_activity),as.integer(mean(sms_in_activity, na.rm = TRUE)), sms_in_activity))
cdr_input_cln  <- cdr_input_cln %>% mutate(sms_in_activity = ifelse(is.na(sms_in_activity),0, sms_in_activity))
cdr_input_cln <- cdr_input_cln%>% group_by(country_code) %>%mutate(sms_out_activity = ifelse(is.na(sms_out_activity),as.integer(mean(sms_out_activity, na.rm = TRUE)), sms_out_activity))
cdr_input_cln  <- cdr_input_cln %>% mutate(sms_out_activity = ifelse(is.na(sms_out_activity),0, sms_out_activity))
cdr_input_cln <- cdr_input_cln%>% group_by(country_code) %>%mutate(call_in_activity = ifelse(is.na(call_in_activity),as.integer(mean(call_in_activity, na.rm = TRUE)), call_in_activity))
cdr_input_cln  <- cdr_input_cln %>% mutate(call_in_activity = ifelse(is.na(call_in_activity),0, call_in_activity))
cdr_input_cln <- cdr_input_cln%>% group_by(country_code) %>%mutate(call_out_activity = ifelse(is.na(call_out_activity),as.integer(mean(call_out_activity, na.rm = TRUE)), call_out_activity))
cdr_input_cln  <- cdr_input_cln %>% mutate(call_out_activity = ifelse(is.na(call_out_activity),0, call_out_activity))
cdr_input_subset_df <- subset(cdr_input_cln,cdr_input_cln$square_id<=500)
head(cdr_input_subset_df)
factorColumns <- c("square_id","country_code")
cdr_input_subset_df[factorColumns] <- lapply(cdr_input_subset_df[factorColumns],as.factor)
head(cdr_input_subset_df)
val <- cdr_input_subset_df$time_interval/1000
cdr_input_subset_df$outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
head(cdr_input_subset_df)
cdr_input_subset_df$activity_start_time <- cdr_input_subset_df$outputTime 
cdr_input_subset_df$activity_date <- as.Date(cdr_input_subset_df$activity_start_time)
cdr_input_subset_df$activity_time <- format(cdr_input_subset_df$activity_start_time,"%H")
cdr_input_subset_df$total_activity <- rowSums(cdr_input_subset_df[, c(4,5,6,7,8)],na.rm=T)
cdrActivityDF <- subset(cdr_input_subset_df,select=c("activity_time","total_activity"))
wcss <- vector()
for(i in 1:20) wcss[i] = sum(kmeans(cdrActivityDF,i)$withinss)
clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))
library(ggplot2)
ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")+theme_bw()
cdrActivityDF <- subset(cdrActivityDF,select=c("activity_time","total_activity"))
cdrClusterModel <- kmeans(cdrActivityDF,10,nstart=10)
cdrActivityDF$cluster <- as.factor(cdrClusterModel$cluster)
cdrActivityDF$activity_time <- as.factor(cdrActivityDF$activity_time)
cdrActivityDF.melt <- melt(cdrActivityDF)
cdrActivityClusterPlot <- ggplot(cdrActivityDF.melt, aes(activity_time,cluster)) + geom_tile(aes(fill = value))+ scale_fill_gradient(low = "#FA8167",high = "#FF2D00")+theme_bw()
print(cdrActivityClusterPlot)
library(tidyr)
set.seed(42) 
library(caret)
cdr_input_subset_df_cln_P <- subset(cdr_input_subset_df,select=c("activity_time","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity","total_activity"))
cdr_input_subset_df_cln_P <- cdr_input_subset_df_cln_P%>% drop_na(internet_traffic_activity)%>% drop_na(call_out_activity)%>% drop_na(sms_out_activity)
model_1 <- train(total_activity~ ., cdr_input_subset_df_cln_P,method = "lm",trControl = trainControl(method = "cv", number = 10,repeats = 5, verboseIter = TRUE))
print(model_1)
cdr_input_subset_df_cln_P$total_avtivity_p1 <- predict(model_1,cdr_input_subset_df_cln_P)
ggplot(cdr_input_subset_df_cln_P, aes(x = activity_time)) + 
  geom_point(aes(y = cdr_input_subset_df_cln_P$total_avtivity_p1), colour = "#FF0202") +
  ylab(label="total_activity") + 
  xlab("Activity Time")
library(arm)
library(abind)
model_2 <- train(total_activity ~ ., 
                 cdr_input_subset_df_cln_P,method = 'bayesglm',
                 trControl = trainControl(
                   method = "cv", number = 10,repeats = 5, verboseIter = TRUE
                 )
)
print(model_2)
cdr_input_subset_df_cln_P$total_activity_p2 <- predict(model_2,cdr_input_subset_df_cln_P)
ggplot(cdr_input_subset_df_cln_P, aes(x = activity_time)) + 
  geom_point(aes(y = cdr_input_subset_df_cln_P$total_activity_p2), colour = "#4271AE") +
  ylab(label="Total Activity") + 
  xlab("Activity Time")
