#### analysis1

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/CleanProb.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")
source("functions/returnmulti.R")


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


#writing csv
sapply(names(rtres),
       function(x)write.table(rtres[[x]], file=paste(x, "csv", sep=".")))

# MUST DO MORE HERE, going to compare test data first though!




anova(lm(data_p12$total_correct_LP ~ data_p12$Group)) #p=.48
anova(lm(data_p12$total_inactive_LP ~ data_p12$Group)) #p=.48
anova(lm(data_p12$mean_timetill_NP ~ data_p12$Group)) #p=.93
anova(lm(data_p12$NP_responsestate ~ data_p12$Group)) #p=.81


