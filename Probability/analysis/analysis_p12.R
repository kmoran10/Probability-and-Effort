## 12% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/All functions_L.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p12files <- list.files("raw_data/TEST", pattern=".*12p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 12% test type .csv
p12df <- lapply(p12files, read.csv) #reads all CSVs in filenames and makes them a list




p12cl1 <- lapply(p12df, cleandata1)
p12cl2 <- lapply(p12cl1, newcolumns)
p12cl3 <- lapply(p12cl2, summary1)
names(p12cl3) <- substr(p12files, 1, 31)
p12cl4 <- lapply(p12df, cleandata2)
p12cl5 <- lapply(p12cl4, newcolumns2)
p12cl6 <- lapply(p12cl5, summary2)
names(p12cl6) <- substr(p12files, 1, 31)

p12clA <- p12cl3 %>% 
  bind_rows

p12clB <- p12cl6 %>% 
  bind_rows

p12clean <- cbind(p12clA, p12clB[,2:3])

data_p12 <- asngrp(p12clean)




lapply(data_p12[,2:7], function(x) t.test(x ~ data_p12$Group, var.equal = TRUE))
apply(data_p12[,2:7], 2, shapiro.test)
lapply(data_p12[,2:7], function(x) wilcox.test(x ~ data_p12$Group))

by(data_p12$total_correct_LP, data_p12$Group, pastecs::stat.desc)



anova(lm(data_p12$total_active_LP ~ data_p12$Group)) 
anova(lm(data_p12$total_correct ~ data_p12$Group)) 
anova(lm(data_p12$total_inactiveLP ~ data_p12$Group)) 
anova(lm(data_p12$time_np ~ data_p12$Group)) 
anova(lm(data_p12$NP_responsestate ~ data_p12$Group)) 
anova(lm(data_p12$total_unrewarded_correct_LP ~ data_p12$Group))




