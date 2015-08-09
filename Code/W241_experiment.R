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






