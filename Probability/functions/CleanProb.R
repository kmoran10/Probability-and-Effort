#### To get from raw data to only:  total_correct_LP, total_inactive_LP, mean_timetill_NP, and NP_responsestate


## This function cleans a single raw Coubourn data file. Whatever file it is cleaning is the one specified in the argument (or can be used on a list (more frequent use case) with lapply on a list of CSVs)

CleanProb <- function(data) {
  
  library(tidyverse)

## first: total_correct_LP, total_inactive_LP, mean_timetill_NP

data2 <- data %>%
  select(Subject, Time.Start, Time.Finish, Duration, Enter.State, A1, A2, A3) #select only useful cols

colnames(data2)[6:8] <- c("Number.A1", "Number.A2", "Number.A3") #rename cols to reflect count data

data2 <- data2[!apply(is.na(data2) | data2 == "", 1, all),] #removes all NAs and empty cells

# group number a1 a2 and a3, remove first row, group the other cols, remove bottom row, combine those, fixes mismatch problem.

data3 <- data2 %>%
  mutate(correct.LP= ifelse(Enter.State==3,1,0)) %>% #create new col with "correct LPs"/active lever presses 
  mutate(timeafter.LP = ifelse(Number.A3>0 & Enter.State==8, Duration/10,"")) %>% #create new column with the time it took (in secs) to NP afer correct LP
  mutate(inactive.LP = ifelse(Number.A1>0, Number.A1, 0)) #create new column with LPs on inactive lever

data3$timeafter.LP <- as.numeric(data3$timeafter.LP) #change time after LP to NP to numeric (was char)

results1 <- data3 %>%
  group_by(Subject) %>%
  summarise(total_correct_LP = sum (correct.LP), total_inactive_LP = sum(inactive.LP), mean_timetill_NP = mean(timeafter.LP, na.rm = TRUE))

## Now: NP_responsestate

npdata <- data %>%
  select(Subject.1, Time, Current.State, Transition.State, A1.., A2.., A3..) #select important cols

colnames(npdata)[1:7] <- c("Subject", "Time", "Current.State", "Transition.State", "A1", "A2", "A3") #change col names

npdata2 <- npdata[!apply(is.na(npdata) | npdata == "", 1, all),] #remove all NA and empty cells

npdata3 <- npdata2 %>%
  mutate(NP_respstate = ifelse(Current.State==2, A3, 0)) #create col with NP during response state S2 

results2 <- npdata3 %>%
  group_by(Subject) %>%
  summarise(NP_responsestate = sum(NP_respstate)) #total NP per animal that NP in responsestate S2 (within 20s of correct LP)

allresults <- merge(results1, results2) 

allresults$mean_timetill_NP <- round(allresults$mean_timetill_NP, 2) #rounds mean_timetill_NP to 2 decimals

allresults$Subject <- gsub("_", "", allresults$Subject) #removes _ from subject name

return(allresults)
}
