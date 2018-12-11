# Function to clean CSV file and get columns needed for data analysis. Data come from csv files



cleandata1 <- function(mydata){
  library(tidyverse)
  data2<- mydata %>%  # select the columns with the data we need and save them in a new data frame for first set of results
    select(Subject,Time.Start,Time.Finish, Duration,Enter.State, A1, A2,A3)
  colnames(data2)[6:8] <-c("Number.A1", "Number.A2", "Number.A3")
  data2 <- data2[!apply(is.na(data2) | data2 == "", 1, all),] # remove all empty and NA cells
  data2.2 <-data2%>% # create a new data frame with only Number A1, Number A2 and Number A3
    select(Number.A1,Number.A2,Number.A3)
  data2.2 <- data2.2[-1,] # delete the first row
  data2.3<-data2%>% # create a new data frame with the other columns
    select(Subject,Time.Start,Time.Finish, Duration,Enter.State)
  data2.3 <- data2.3[-nrow(data2.3),] # delete last row, so ther is an equal number of rows and this data frame can be merged with the one wih A1 A2 and A3
  cleandata0.1 <-cbind(data2.3, data2.2) # create a new data frame combining the previous 2
  cleandata0.1$Subject <-gsub("_", "",cleandata0.1$Subject) # removes _ from name
  return(cleandata0.1)
}

# Function to add columns with correct LP, Time to NP after correct LP, and LP in inactive lever. Data come from file after using function cleandata1

newcolumns <- function (cleandata0.1){
  library(tidyverse)
  mydata2 <- cleandata0.1%>%
    mutate(correct.LP= ifelse(Enter.State==3,1,0))%>% # create new column with the corect LP (food sate)
    mutate(timeafterLP = ifelse(Number.A3>0 & Enter.State==8,Duration/10,""))%>% # crate new column with the time it took to NP in food cup after correct LP
    mutate(inactiveLP = ifelse(Number.A1>0,Number.A1,0)) # create new column with the LP on the inactive lever
  mydata2$timeafterLP<-as.numeric(mydata2$timeafterLP) # treat the column with the time as numeric and not character so the mean can be calculated later
  mydata2.2 <- mydata2[!apply(is.na(mydata2) | mydata2 == "", 1, all),]
  return(mydata2.2)
}

# Function to get Totals  of LP in active, LP inactice, mean to NP. Data come from newcolumns.

summary1 <- function (mydata2.2){
  library(tidyverse)
  results1 <-mydata2.2%>%
    group_by(Subject)%>% # group by subject
    summarise(total_active_LP = sum(Number.A2), total_correct = sum(correct.LP),total_inactiveLP = sum(inactiveLP), total_np = sum(Number.A3),time_np = mean(timeafterLP, na.rm=TRUE))#summarise of total correct LP, time to NP, and LP in inactive lever, and total active LP
  return(results1)
}

# Function to clean CSV file and get columns needed for data analysis of NP in response state. Data come from csv files

cleandata2 <- function(mydata){
  library(tidyverse)
  dataextra <- mydata %>%  # data for nosepokes in response state
    select(Subject.1,Time, Current.State,Transition.State,A1..,A2..,A3..)
  colnames(dataextra)[1:7] <-c("Subject", "Time", "Current.State", "Transition.State", "A1","A2", "A3") # change names
  dataextra2 <- dataextra[!apply(is.na(dataextra) | dataextra == "", 1, all),] # remove NA and empty cells
  dataextra2$Subject <-gsub("_", "",dataextra2$Subject) # removes _ from name
  return(dataextra2)
}

# Function to get a new column with NP during response state. Data come from file after using the function cleandata2.

newcolumns2<- function(dataextra2){
  library(tidyverse)
  dataextra3 <- dataextra2 %>%  # data for nosepokes in response state
    mutate(NP_respstate= ifelse(Current.State==2,A3,0)) %>%# get column with NP during response state S2
    mutate(unrewarded_active_LP= ifelse(Current.State==2 & Transition.State==0 & A2==1,1,0))
  dataextra4<-dataextra3[!apply(is.na(dataextra3) | dataextra3 == "", 1, all),]
  return(dataextra4)
}

# Function to get total NP in response state. Data come from file after using the function newcolumns 2.

summary2<- function (dataextra4){
  library(tidyverse)
  results2 <-dataextra4%>%
    group_by(Subject)%>% # group by subject
    summarise(total_unrewarded_correct_LP = sum(unrewarded_active_LP), NP_responsestate = sum(NP_respstate))
  return(results2)
}


#  all together

 mydata <- read.csv("raw_data/TEST/101618_12p_LF281-283-284-286-289_csv.csv")
 

 cl1 <- cleandata1(mydata)
 cl2 <- newcolumns(cl1)
 cl3 <- summary1(cl2)
 cl4 <- cleandata2(mydata)
 cl5 <- newcolumns2(cl4)
 cl6 <- summary2(cl5)
 clA <- cl3 %>% 
  bind_rows
 clB <- cl6 %>% 
  bind_rows
 clC <- cbind(clA, clB[,2:3])
 cleaned <- asngrp(clC)
