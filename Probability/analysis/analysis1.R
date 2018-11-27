#### analysis1

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/CleanProb.R")
source("functions/cbindPad.R")


#### Retrain analysis


rtfiles <- list.files("raw_data/RETRAIN", pattern="*.csv", full.names = TRUE) #creates a list of all files in folder RETRAIN type .csv
rtdf <- lapply(rtfiles, read.csv) #reads all CSVs in rtfiles and makes them a list
rtres <- lapply(rtdf, CleanProb) #applies CleanProb fxn to all files in RETRAIN list
names(rtres) <- substr(rtfiles, 1, 31)


data_rt <- rtres %>%
  reduce(cbindPad) #puts together all data frames in the list of clean data so that I can pull the columns on interest for each type of analysis (below)

cor1 <- data_rt[,c(1, seq(2, by = 5, length = 19))] #keeps Subject and only correct LP columns for every retrain day for first set of animals

cor2 <- data_rt[,c(96, seq(97, by = 5, length = 12))] #same as above, group 2

total_correct_RT <- plyr::rbind.fill(cor1, cor2)


# MUST DO MORE HERE, going to compare test data first though!


## 12% tests

p12files <- list.files("raw_data/TEST", pattern=".*12p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 12% test type .csv
p12df <- lapply(p12files, read.csv) #reads all CSVs in filenames and makes them a list
p12res <- lapply(p12df, CleanProb) #applies CleanProb fxn to all files in RETRAIN list
names(p12res) <- substr(p12files, 1, 31)


data_p12 <- p12res %>%
  bind_rows %>%
  mutate(Group = ifelse(grepl("0",data_p12$Subject), "Ctrl", "Expt")) %>%
  mutate(Group = ifelse(grepl("2",data_p12$Subject), "Ctrl", "Expt")) %>%
  mutate(Group = ifelse(grepl("4",data_p12$Subject), "Ctrl", "Expt")) %>%
  mutate(Group = ifelse(grepl("6",data_p12$Subject), "Ctrl", "Expt")) %>%
  mutate(Group = ifelse(grepl("8",data_p12$Subject), "Ctrl", "Expt"))

data_p12 %>%
  group_by(Group) %>%
  summarise(mean_correct_LP = mean(total_correct_LP))

data_p12 %>%
  group_by(Group) %>%
  summarise(mean_inactive_LP = mean(total_inactive_LP))

data_p12 %>%
  group_by(Group) %>%
  summarise(mean_NP_responsestate = mean(NP_responsestate))
