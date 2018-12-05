## 50% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/CleanProb.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p50files <- list.files("raw_data/TEST", pattern=".*50p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 50% test type .csv
p50df <- lapply(p50files, read.csv) #reads all CSVs in filenames and makes them a list
p50res <- lapply(p50df, CleanProb) #applies CleanProb fxn to all files in RETRAIN list
names(p50res) <- substr(p50files, 1, 31)


data_p50 <- p50res %>% 
  bind_rows

data_p50 <- asngrp(data_p50)


p50cl1 <- lapply(p50df, cleandata1)
p50cl2 <- lapply(p50cl1, newcolumns)
p50cl3 <- lapply(p50cl2, summary1)
names(p50cl3) <- substr(p50files, 1, 31)

p50clean1 <- p50cl3 %>% 
  bind_rows

p50clean1 <- asngrp(p50clean1)

data_p50$total_active_LP <- as.numeric(p50clean1$total_active_LP)


lapply(data_p50[,2:5], function(x) t.test(x ~ data_p50$Group, var.equal = TRUE))


anova(lm(data_p50$total_correct_LP ~ data_p50$Group)) #p=.51
anova(lm(data_p50$total_inactive_LP ~ data_p50$Group)) #p=.32
anova(lm(data_p50$mean_timetill_NP ~ data_p50$Group)) #p=.44
anova(lm(data_p50$NP_responsestate ~ data_p50$Group)) #p=.52
