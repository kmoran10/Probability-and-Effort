## 12% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/CleanProb.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p12files <- list.files("raw_data/TEST", pattern=".*12p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 12% test type .csv
p12df <- lapply(p12files, read.csv) #reads all CSVs in filenames and makes them a list
p12res <- lapply(p12df, CleanProb) #applies CleanProb fxn to all files in RETRAIN list
names(p12res) <- substr(p12files, 1, 31)


data_p12 <- p12res %>% 
  bind_rows

data_p12 <- asngrp(data_p12)

data_p12 %>%
  group_by(Group) %>%
  summarise(mean_correct_LP = mean(total_correct_LP))

data_p12 %>%
  group_by(Group) %>%
  summarise(mean_inactive_LP = mean(total_inactive_LP))

data_p12 %>%
  group_by(Group) %>%
  summarise(mean_NP_responsestate = mean(NP_responsestate))



lapply(data_p12[,2:5], function(x) t.test(x ~ data_p12$Group, var.equal = TRUE))
apply(data_p12[,2:5], 2, shapiro.test)
lapply(data_p12[,2:5], function(x) wilcox.test(x ~ data_p12$Group))

by(data_p12$total_correct_LP, data_p12$Group, pastecs::stat.desc)


anova(lm(data_p12$total_correct_LP ~ data_p12$Group)) #p=.48
anova(lm(data_p12$total_inactive_LP ~ data_p12$Group)) #p=.48
anova(lm(data_p12$mean_timetill_NP ~ data_p12$Group)) #p=.93
anova(lm(data_p12$NP_responsestate ~ data_p12$Group)) #p=.81

