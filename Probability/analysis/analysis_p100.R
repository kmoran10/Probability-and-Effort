## 100% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/CleanProb.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p100files <- list.files("raw_data/TEST", pattern=".*100p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 100% test type .csv
p100df <- lapply(p100files, read.csv) #reads all CSVs in filenames and makes them a list
p100res <- lapply(p100df, CleanProb) #applies CleanProb fxn to all files in RETRAIN list
names(p100res) <- substr(p100files, 1, 31)


data_p100 <- p100res %>% 
  bind_rows

data_p100 <- asngrp(data_p100)

data_p100 %>%
  group_by(Group) %>%
  summarise(mean_correct_LP = mean(total_correct_LP))

data_p100 %>%
  group_by(Group) %>%
  summarise(mean_inactive_LP = mean(total_inactive_LP))

data_p100 %>%
  group_by(Group) %>%
  summarise(mean_NP_responsestate = mean(NP_responsestate))


lapply(data_p100[,2:5], function(x) t.test(x ~ data_p100$Group, var.equal = TRUE))


anova(lm(data_p100$total_correct_LP ~ data_p100$Group)) #p=.33
anova(lm(data_p100$total_inactive_LP ~ data_p100$Group)) #p=.37
anova(lm(data_p100$mean_timetill_NP ~ data_p100$Group)) #p=.94
anova(lm(data_p100$NP_responsestate ~ data_p100$Group)) #p=.33
