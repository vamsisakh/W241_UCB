#R code for W-241 experiment

#Loading libraries
library (doBy)
library (plyr)
library (dplyr)
library (tidyr)
library(stargazer)
library(gridExtra)

#Working directory
setwd('/Users/Vamsi/Documents/W241_UCB/Code')

###################################################################################################
#commands to convert to convert logfile to csv

#First -> download the log file
#rhc scp myflaskapp download /Users/Vamsi/Documents/W241_UCB/Code app-root/logs/cuing_webapp.log

#Second -> remove the old log entries
#'tail -n +1168 cuing_webapp.log >data.log'

#Third -> python script to convert log file to csv
#'python log2csv.py'
###################################################################################################

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

#2. Linear Regression

#Loading data from AMT
data_mturk <- read.csv("MTurk Results.csv",header=TRUE)

#Two subjects incorrectly entered the userid,just missed one alphabet, 
# Correct it so as to merge with main data-frame

levels(data_mturk$Answer.surveycode)[levels(data_mturk$Answer.surveycode) == "e90df31-315c-4d81-b0c8-35c8e6d2eed2"] = "de90df31-315c-4d81-b0c8-35c8e6d2eed2" 
levels(data_mturk$Answer.surveycode)[levels(data_mturk$Answer.surveycode) == "a1f6cc1-0796-4f2e-8ab1-f6e63dc8652b"] <- "a1f6cc21-0796-4f2e-8ab1-f6e63dc8652b"

#Checking from logs how many users fell below the minimum time threshold
v1_time <- 2*60 + 56
v2_time <- 2*60 + 49
v3_time <- 3*60 + 16

min_time_threshold <- v1_time + v2_time + v3_time

mturkID_time <- data.frame(data_mturk$Answer.surveycode,data_mturk$WorkTimeInSeconds)
colnames(mturkID_time) <- c("userid","Time_Taken_Turk")

#Merging (by userid) mturk data with the existing data frame 
merged_w_turk<-left_join(merged_datac,mturkID_time,by="userid") 

#time from exposure to vote (computed from the logs)
merged_w_turk$time_taken_log <- merged_datac$time_voted - merged_datac$time_exposed 

#whether the subject was a Mturker or not
merged_w_turk$MTurk_Subject <- ifelse(is.na(merged_w_turk$Time_Taken_Turk),0,1)

#whether a Mturker was in treatment or control
merged_w_turk$MTurk_Subject_Treat <- ifelse(((merged_w_turk$MTurk_Subject==1) & (merged_w_turk$treatd==1)),1,0)

#whether the time spent from exposure to vote was less than the minimum time required to watch the videos
merged_w_turk$above_threshold <- ifelse((merged_w_turk$time_taken_log<min_time_threshold),0,1)

#dummy for gender
merged_w_turk$female <- ifelse(merged_datac$gender=="Female",1,0)

#dummies for education
educd <- as.data.frame(model.matrix(age~education,merged_w_turk))
merged_w_turk$educ_HS <- educd$educationHS
merged_w_turk$educ_COL <- educd$educationCol
merged_w_turk$educ_GS <- educd$educationGS

#dummies for age
merged_w_turk$age_21_30 <- ifelse(merged_w_turk$age=='21-30',1,0)
merged_w_turk$age_31_40 <- ifelse(merged_w_turk$age=='31-40',1,0)
merged_w_turk$age_41_50 <- ifelse(merged_w_turk$age=='41-50',1,0)
merged_w_turk$age_51_60 <- ifelse(merged_w_turk$age=='51-60',1,0)
merged_w_turk$age_61_70 <- ifelse(merged_w_turk$age=='61-70',1,0)
merged_w_turk$age_71_80 <- ifelse(merged_w_turk$age=='71-80',1,0)
merged_w_turk$age_81_plus <- ifelse(merged_w_turk$age=='81_plus',1,0)

#Converting data_frame from wide to long format so as to be able to run regression
merged_w_turk_long<-gather(merged_w_turk, condition, rating, v1_choice:v3_choice)

#Creating dummies for videos 1,2 and 3
merged_w_turk_long$isVideo1 <- ifelse(merged_w_turk_long$condition=='v1_choice',1,0)
merged_w_turk_long$isVideo2 <- ifelse(merged_w_turk_long$condition=='v2_choice',1,0)
merged_w_turk_long$isVideo3 <- ifelse(merged_w_turk_long$condition=='v3_choice',1,0)

#Creating interaction terms for videos 1, 2 and 3 with treatment 
merged_w_turk_long$isVideo1_treat <-  ifelse((merged_w_turk_long$isVideo1==1) & (merged_w_turk_long$treatd==1),1,0)
merged_w_turk_long$isVideo2_treat <-  ifelse((merged_w_turk_long$isVideo2==1) & (merged_w_turk_long$treatd==1),1,0)
merged_w_turk_long$isVideo3_treat <-  ifelse((merged_w_turk_long$isVideo3==1) & (merged_w_turk_long$treatd==1),1,0)

#Creating interaction terms for videos 1, 2 and 3 with treatment for non-mturk subjects
merged_w_turk_long$isVideo1_treat_nonTurk <-  ifelse((merged_w_turk_long$isVideo1==1) & (merged_w_turk_long$treatd==1) & (merged_w_turk_long$MTurk_Subject==0),1,0)
merged_w_turk_long$isVideo2_treat_nonTurk <-  ifelse((merged_w_turk_long$isVideo2==1) & (merged_w_turk_long$treatd==1) & (merged_w_turk_long$MTurk_Subject==0),1,0)
merged_w_turk_long$isVideo3_treat_nonTurk <-  ifelse((merged_w_turk_long$isVideo3==1) & (merged_w_turk_long$treatd==1) & (merged_w_turk_long$MTurk_Subject==0),1,0)

#Creating interaction terms for videos 1, 2 and 3 with treatment for mturk subjects
merged_w_turk_long$isVideo1_treat_mTurk <-  ifelse((merged_w_turk_long$isVideo1==1) & (merged_w_turk_long$MTurk_Subject_Treat==1),1,0)
merged_w_turk_long$isVideo2_treat_mTurk <-  ifelse((merged_w_turk_long$isVideo2==1) & (merged_w_turk_long$MTurk_Subject_Treat==1),1,0)
merged_w_turk_long$isVideo3_treat_mTurk <-  ifelse((merged_w_turk_long$isVideo3==1) & (merged_w_turk_long$MTurk_Subject_Treat==1),1,0)

#Regression 1 : Rating = Intercept + B1*treatment + B2*isVideo1 + B3*isVideo2 + B4*isVideo1*Treatment + B5*isVideo2*Treatment
linear.1 <- lm(rating~treatd+isVideo1+isVideo2+isVideo1_treat+isVideo2_treat,data=merged_w_turk_long)
summary(linear.1)

#Regression 2: 
linear.2 <- lm(rating~treatd+isVideo1+isVideo2+isVideo1_treat_nonTurk+isVideo2_treat_nonTurk+isVideo1_treat_mTurk+isVideo2_treat_mTurk,data=merged_w_turk_long)
summary(linear.2)

#Regression 3: Regression 1 + Covariates
linear.3 <- lm(rating~treatd+isVideo1+isVideo2+isVideo1_treat+isVideo2_treat+female+educ_HS+educ_COL+age_21_30+age_31_40+age_41_50+age_51_60+age_61_70,data=merged_w_turk_long)
summary(linear.3)

#Regression 4: Regression 2 + Covariates
linear.4 <- lm(rating~treatd+isVideo1+isVideo2+isVideo1_treat_nonTurk+isVideo2_treat_nonTurk+isVideo1_treat_mTurk+isVideo2_treat_mTurk+female+educ_HS+educ_COL+age_21_30+age_31_40+age_41_50+age_51_60+age_61_70,data=merged_w_turk_long)
summary(linear.4)

#Function to compute Clustered Standard errors
cl <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

#Clustering on UserID's
cl1 <- cl(linear.1,factor(as.character(merged_w_turk_long$userid)))
cl1

cl2 <- cl(linear.2,factor(as.character(merged_w_turk_long$userid)))
cl2

cl3 <- cl(linear.3,factor(as.character(merged_w_turk_long$userid)))
cl3

cl4 <- cl(linear.4,factor(as.character(merged_w_turk_long$userid)))
cl4

d <- merged_w_turk

##Pulling out Demographic Characteristics from the data-frame
freq<-d %>% 
  group_by(MTurk_Subject) %>%
  summarise(women = (sum(female==1)),
            men = (sum(female==0)),
            HS = (sum(educ_HS==1)),
            COL = (sum(educ_COL==1)),
            GS  = sum(educ_GS==1),
            age_21_30  = sum(age_21_30==1),
            age_31_40  = sum(age_31_40==1),
            age_41_50  = sum(age_41_50==1),
            age_51_60  = sum(age_51_60==1),
            age_61_70  = sum(age_61_70==1),
            age_71_80  = sum(age_71_80==1),
            age_81_plus  = sum(age_81_plus==1)
            ) 
ungroup

#outputting freq data-frame to PDF
pdf("data_output2.pdf", height=3, width=15)
grid.table(freq,rows=NULL)
dev.off()

#Histograms and means of vote distributions

hist(merged_w_turk$v1_choice[merged_w_turk$treatd==1])
hist(merged_w_turk$v1_choice[merged_w_turk$treatd==0])

hist(merged_w_turk$v2_choice[merged_w_turk$treatd==1])
hist(merged_w_turk$v2_choice[merged_w_turk$treatd==0])

hist(merged_w_turk$v3_choice[merged_w_turk$treatd==1])
hist(merged_w_turk$v3_choice[merged_w_turk$treatd==0])

mean(merged_w_turk$v1_choice[merged_w_turk$treatd==1])
mean(merged_w_turk$v1_choice[merged_w_turk$treatd==0])

mean(merged_w_turk$v2_choice[merged_w_turk$treatd==1])
mean(merged_w_turk$v2_choice[merged_w_turk$treatd==0])

mean(merged_w_turk$v3_choice[merged_w_turk$treatd==1])
mean(merged_w_turk$v3_choice[merged_w_turk$treatd==0])

########################################################################################
#Covariate balance check for gender (TO-DO)

#female_control <- ifelse((merged_datac$female==1) & (merged_datac$treatd==0),1,0)
#female_treatment <- ifelse((merged_datac$female==1) & (merged_datac$treatd==1),1,0)
#t.test(female_control,female_treatment)
########################################################################################

#latex for regression tables
#pandoc t.txt -V geometry:margin=0in -V fontsize=10pt -o t2.pdf
stargazer(cl1,cl2,cl3,cl4)

#outputting data-frame to PDF
pdf("data_output0.pdf", height=3, width=22)
dim(merged_w_turk)
grid.table(merged_w_turk[1:10,1:14],rows=NULL)
dev.off()

#outputting data-frame to PDF
pdf("data_output1.pdf", height=3, width=22)
dim(merged_w_turk)
grid.table(merged_w_turk[1:10,15:30],rows=NULL)
dev.off()
