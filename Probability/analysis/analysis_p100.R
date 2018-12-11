## 100% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/All functions_L.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p100files <- list.files("raw_data/TEST", pattern=".*100p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 100% test type .csv
p100df <- lapply(p100files, read.csv) #reads all CSVs in filenames and makes them a list




p100cl1 <- lapply(p100df, cleandata1)
p100cl2 <- lapply(p100cl1, newcolumns)
p100cl3 <- lapply(p100cl2, summary1)
names(p100cl3) <- substr(p100files, 1, 31)
p100cl4 <- lapply(p100df, cleandata2)
p100cl5 <- lapply(p100cl4, newcolumns2)
p100cl6 <- lapply(p100cl5, summary2)
names(p100cl6) <- substr(p100files, 1, 31)

p100clA <- p100cl3 %>% 
  bind_rows

p100clB <- p100cl6 %>% 
  bind_rows

p100clean <- cbind(p100clA, p100clB[,2:3])

data_p100 <- asngrp(p100clean)




lapply(data_p100[,2:7], function(x) t.test(x ~ data_p100$Group, var.equal = TRUE))
apply(data_p100[,2:7], 2, shapiro.test)
lapply(data_p100[,2:7], function(x) wilcox.test(x ~ data_p100$Group))

by(data_p100$total_correct_LP, data_p100$Group, pastecs::stat.desc)



anova(lm(data_p100$total_active_LP ~ data_p100$Group)) 
anova(lm(data_p100$total_correct ~ data_p100$Group)) 
anova(lm(data_p100$total_inactiveLP ~ data_p100$Group)) 
anova(lm(data_p100$time_np ~ data_p100$Group)) 
anova(lm(data_p100$NP_responsestate ~ data_p100$Group)) 
anova(lm(data_p100$total_unrewarded_correct_LP ~ data_p100$Group))




