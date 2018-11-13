
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

##### RETRAIN

## Make sure you're in the RETRAIN_2 folder as WD

files_rt <- dir(pattern = "*RETRAIN_2.csv")
files_rt

data_rt <- files_rt %>%
  map(read.csv) %>%    # read in all the files individually
  reduce(cbind2)        # reduce with cbind into one dataframe
data_rt

correct_LP_rt<- data_rt[,c(2,3,9,15,21,27,33,39,45,51,57,63,69)] #keeps Subject and only correct LP columns for every retrain day

total_inactiveLP_rt<- data_rt[,c(2,4,10,16,22,28,34,40,46,52,58,64,70)] #subject and inactive LP

time_to_NP_rt<- data_rt[,c(2,5,11,17,23,29,35,41,47,53,59,65,71)] #subject and time to NP after cLP
  
NP_responsestate_rt<- data_rt[,c(2,6,12,18,24,30,36,42,48,54,60,66,72)] 



##### TESTS

## 12p

data_12p <- dir(pattern = ".*12p.*\\.csv") %>%
  lapply(read_csv) %>%
  bind_rows

## 25p

data_25p <- dir(pattern = ".*25p.*\\.csv") %>%
  lapply(read_csv) %>%
  bind_rows

## 50p

data_50p <- dir(pattern = ".*50p.*\\.csv") %>%
  lapply(read_csv) %>%
  bind_rows

## 100p

data_100p <- dir(pattern = ".*100p.*\\.csv") %>%
  lapply(read_csv) %>%
  bind_rows



##### LeverTrain (easiest to put last 5 days in a folder and run this code)

files_LT <- dir(pattern = "*LEVERTRAIN_2.csv")
files_LT

data_LT <- files_LT %>%
  map(read.csv) %>%    # read in all the files individually
  reduce(cbind2)        # reduce with cbind into one dataframe
data_LT

correct_LP_LT<- data_LT[,c(2,3,9,15,21,27)] #keeps Subject and only correct LP columns for every retrain day

total_inactiveLP_LT<- data_LT[,c(2,4,10,16,22,28)] #subject and inactive LP

time_to_NP_LT<- data_LT[,c(2,5,11,17,23,29)] #subject and time to NP after cLP

NP_responsestate_LT<- data_LT[,c(2,6,12,18,24,30)] 

cLPLT2 <- as.data.frame(t(correct_LP_LT))
  
