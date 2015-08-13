#R code for W-241 experiment

#Loading libraries
library (doBy)
library (plyr)
library (dplyr)

#Working directory
setwd('/Users/Vamsi/Documents/W241_UCB/Code')

#commands to convert to convert logfile to csv

#First -> download the log file
#rhc scp myflaskapp download /Users/Vamsi/Documents/W241_UCB/Code app-root/logs/cuing_webapp.log

#Second -> remove the old log entries
#'tail -n +1168 cuing_webapp.log >data.log'

#Third -> python script to convert log file to csv
#'python log2csv.py'

#Loading data
data_experiment <- read.csv(file = "data.csv",header=TRUE)

#Creating a data frame for exposures
data_exposure <- data_experiment[data_experiment$event=="exposure",]

#Converting Unix time into regular time
data_exposure$time_val <- as.POSIXct(data_exposure$time,origin="1970-01-01") 

#Creating a new data frame for exposures per User ID
data_exposure_sub <-data_exposure %>%
group_by(userid) %>%
arrange(time) %>%
summarise(time_exposed=first(time_val),num_of_exposures = n(),treatment=first(count_visibility)) %>%
ungroup

#Printing out the number of exposures and the number of unique User IDs
cat('We have', nrow(data_exposure), 'exposures.', '\n')
cat('We have', with(data_exposure, length(unique(userid))), 'unique subjects.', '\n\n')


#Creating a data frame for Votes
data_vote <- data_experiment[data_experiment$event=="vote",]

#Converting Unix time into regular time
data_vote$time_val <- as.POSIXct(data_vote$time,origin="1970-01-01") 

#Creating a new data frame for exposures per User ID
data_vote_sub <-data_vote %>%
  group_by(userid) %>%
  arrange(time) %>%
  summarise(time_voted=first(time_val),num_of_votes = n(),age=first(age),education=first(education),
            email = first(email),gender=first(gender),v1_choice=first(v1_choice),v2_choice=first(v2_choice),
            v3_choice=first(v3_choice)) %>%
  ungroup

#Printing out the number of votes and the number of unique User IDs
cat('We have', nrow(data_vote), 'Votes', '\n')
cat('We have', with(data_vote, length(unique(userid))), 'unique subjects.', '\n\n')

#Merging the data exposure and data vote frames
merged_data <-left_join(data_exposure_sub,data_vote_sub,by="userid") 

#Filtering out NA's
merged_datac <- merged_data[complete.cases(merged_data),]

#Hack : rebuhr@gmail.com voted twice, but his second attempt was the right vote
merged_datac$v1_choice[merged_datac$email=="rebuhr@gmail.com"] = 4.0
merged_datac$v2_choice[merged_datac$email=="rebuhr@gmail.com"] = 3.6   
merged_datac$v3_choice[merged_datac$email=="rebuhr@gmail.com"] = 3.7
  
#calculating estimate ATE per video

video1_control <- mean(merged_datac$v1_choice[merged_datac$treatment=="hidden"])
video1_treatment <- mean(merged_datac$v1_choice[merged_datac$treatment=="visible"])
  
video2_control <- mean(merged_datac$v2_choice[merged_datac$treatment=="hidden"])
video2_treatment <- mean(merged_datac$v2_choice[merged_datac$treatment=="visible"])

video3_control <- mean(merged_datac$v3_choice[merged_datac$treatment=="hidden"])
video3_treatment <- mean(merged_datac$v3_choice[merged_datac$treatment=="visible"])
  
#ATE per video
ate_v1 <- video1_treatment - video1_control
ate_v2 <- video2_treatment - video2_control
ate_v3 <- video3_treatment - video3_control

#Overall ATE
ate <- mean(c(ate_v1,ate_v2,ate_v3))

#Computing p-value:

##########################################################################################
#1. Randomization Inference

#Creating a treatment dummy variable "treatd"
merged_datac$treatd <- ifelse(merged_datac$treatment=="visible",1,0)
len <- length(merged_datac$treatd)

randomize <- function (len) {
  random <- sample(c(rep(0,floor(len/2)),rep(1,len-floor(len/2))))
  return (random)
}

est_ate <- function (data,treat) {
  video1_control <- mean(data$v1_choice[treat==0])
  video1_treatment <- mean(data$v1_choice[treat==1])
  
  video2_control <- mean(data$v2_choice[treat==0])
  video2_treatment <- mean(data$v2_choice[treat==1])
  
  video3_control <- mean(data$v3_choice[treat==0])
  video3_treatment <- mean(data$v3_choice[treat==1])
  
  #ATE per video
  ate_v1 <- video1_treatment - video1_control
  ate_v2 <- video2_treatment - video2_control
  ate_v3 <- video3_treatment - video3_treatment
  
  #Overall ATE
  est_ate <- mean(c(ate_v1,ate_v2,ate_v3))
  
  return (est_ate)
}

#Running est_ate a few times for fun
est_ate(merged_datac,randomize(len))

distribution_under_sharp_null <- replicate(10000,est_ate(merged_datac,randomize(len)))
plot(density(distribution_under_sharp_null))

# 2-tailed p-value
p_value <- mean(abs(ate) < abs(distribution_under_sharp_null))
cat('p-value as determined by randonmization inference is ', p_value, '\n')
abline(v=ate)

##########################################################################################

#2. Linear Regression (TBD)

merged_datac$overall_avg_rating <- (merged_datac$v1_choice + merged_datac$v2_choice + merged_datac$v3_choice)/3

v1_views <- 1664885
v2_views <- 120937
v3_views <- 10392

merged_datac$v1_views <- ifelse(merged_datac$treatd==1,v1_views,0) #todo
merged_datac$v2_views <- ifelse(merged_datac$treatd==1,v2_views,0) #todo
merged_datac$v3_views <- ifelse(merged_datac$treatd==1,v3_views,0) #todo 

lm <- lm(merged_datac$overall_avg_rating~merged_datac$treatd)
summary(lm)

lm <- lm(merged_datac$v1_choice~merged_datac$treatd)
summary(lm)

lm <- lm(merged_datac$v2_choice~merged_datac$treatd)
summary(lm)

lm <- lm(merged_datac$v3_choice~merged_datac$treatd)
summary(lm)

