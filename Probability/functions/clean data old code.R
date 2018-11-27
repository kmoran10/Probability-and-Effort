###Can I change this ???

##### *2 transform functions. 

##### Rewrites all files in WD folder to *2 version--total_correct, total_incorrect, time_np, and NP_responsestate

##### Will CHANGE ALL files in WD folder to new file type, so MAKE SURE WD is a folder only containing copies of the original CSVs, because these are permanently changed. 

library(dplyr) # call libray
library(tidyverse)

clean_prob <- function(data) {

allfiles <- dir(pattern = "*_csv.csv") #pulls all CSVs in folder, make sure they are all the ones you want to transform

  
  data2<- data %>%  # select the columns with the data we need and save them in a new data frame for first set of results
    select(Subject,Time.Start,Time.Finish, Duration,Enter.State, A1, A2,A3) 
  
  colnames(data2)[6:8] <-c("Number.A1", "Number.A2", "Number.A3")
  
  data2 <- data2[!apply(is.na(data2) | data2 == "", 1, all),] # remove all empty and NA cells
  
  mydata <- data2[-nrow(data2),]
  
  mydata2 <- mydata%>% 
    mutate(correct.LP= ifelse(Enter.State==3,1,0))%>% # create new column with the corect LP
    mutate(timeafterLP = ifelse(Number.A3>0 & Enter.State==8,Duration/10,""))%>% # crate new column with the time it took to NP in food cup after  correct LP
    mutate(inactiveLP = ifelse(Number.A1>0,Number.A1,0)) # create new column with the LP on the inactive lever
  
  mydata2$timeafterLP<-as.numeric(mydata2$timeafterLP) # treat the column with the time as numeric and not character so the mean can be calculated later
  
  result1 <-mydata2%>%
    group_by(Subject)%>% # group by subject
    summarise(total_correct = sum(correct.LP),total_inactiveLP = sum(inactiveLP),time_np = mean(timeafterLP, na.rm=TRUE))#summarise the mean of total correct LP, time to NP, and LP in inactive lever
  
  dataextra <- data %>%  # data for nosepokes in response state
    select(Subject.1,Time, Current.State,Transition.State,A1..,A2..,A3..)
  colnames(dataextra)[1:7] <-c("Subject", "Time", "Current.State", "Transition.State", "A1","A2", "A3") # change names
  
  dataextra2 <- dataextra[!apply(is.na(dataextra) | dataextra == "", 1, all),] # remove NA and empty cells
  
  mydataextra2 <- dataextra2%>%
    mutate(NP_respstate= ifelse(Current.State==2,A3,0)) # get column with NP during response state S2
  
  results2 <-mydataextra2%>%
    group_by(Subject)%>% # group by subject
    summarise(NP_responsestate = sum(NP_respstate))
  
  allresults <-merge(result1, results2) # combine all results without subject duplicate
  
  allresults$Subject <- gsub("_", "", allresults$Subject) # Remove the _ form animal name
  return(allresults)
}

#### MAKE SURE IN THE RIGHT WD WITH FILES YOU WANT TRANSFORMED, no tunring back after this, as it overwrites the files we're working with. # There's probably some way to work with them, make a new file that is transformed, and keep original in folder also, but idk how

for(file in allfiles) {
  data <- read.csv(file,
                   header= TRUE,
                   sep = ",")
  write.csv((allresults2(data)),file.path("Probability/clean_data/RETRAIN_2"))
}

##### Renames all files in folder to *_2.csv

tfiles <- dir(pattern = "*.csv")
sapply(tfiles, FUN=function(eachPath){
  file.rename(from=eachPath,to=sub(pattern="_csv",replacement="_2",eachPath))
})

