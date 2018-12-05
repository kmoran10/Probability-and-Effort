## 25% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/CleanProb.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p25files <- list.files("raw_data/TEST", pattern=".*25p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 25% test type .csv
p25df <- lapply(p25files, read.csv) #reads all CSVs in filenames and makes them a list
p25res <- lapply(p25df, CleanProb) #applies CleanProb fxn to all files in RETRAIN list
names(p25res) <- substr(p25files, 1, 31)


data_p25 <- p25res %>% 
  bind_rows

data_p25 <- asngrp(data_p25)

data_p25 %>%
  group_by(Group) %>%
  summarise(mean_correct_LP = mean(total_correct_LP))

data_p25 %>%
  group_by(Group) %>%
  summarise(mean_inactive_LP = mean(total_inactive_LP))

data_p25 %>%
  group_by(Group) %>%
  summarise(mean_NP_responsestate = mean(NP_responsestate))


lapply(data_p25[,2:5], function(x) t.test(x ~ data_p25$Group, var.equal = TRUE))


anova(lm(data_p25$total_correct_LP ~ data_p25$Group)) #p=.18
anova(lm(data_p25$total_inactive_LP ~ data_p25$Group)) #p=.37
anova(lm(data_p25$mean_timetill_NP ~ data_p25$Group)) #p=.89
anova(lm(data_p25$NP_responsestate ~ data_p25$Group)) #p=.14
