## 50% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/All functions_L.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p50files <- list.files("raw_data/TEST", pattern=".*50p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 50% test type .csv
p50df <- lapply(p50files, read.csv) #reads all CSVs in filenames and makes them a list




p50cl1 <- lapply(p50df, cleandata1)
p50cl2 <- lapply(p50cl1, newcolumns)
p50cl3 <- lapply(p50cl2, summary1)
names(p50cl3) <- substr(p50files, 1, 31)
p50cl4 <- lapply(p50df, cleandata2)
p50cl5 <- lapply(p50cl4, newcolumns2)
p50cl6 <- lapply(p50cl5, summary2)
names(p50cl6) <- substr(p50files, 1, 31)

p50clA <- p50cl3 %>% 
  bind_rows

p50clB <- p50cl6 %>% 
  bind_rows

p50clean <- cbind(p50clA, p50clB[,2:3])

data_p50 <- asngrp(p50clean)




lapply(data_p50[,2:7], function(x) t.test(x ~ data_p50$Group, var.equal = TRUE))
apply(data_p50[,2:7], 2, shapiro.test)
lapply(data_p50[,2:7], function(x) wilcox.test(x ~ data_p50$Group))

by(data_p50$total_correct_LP, data_p50$Group, pastecs::stat.desc)



anova(lm(data_p50$total_active_LP ~ data_p50$Group)) 
anova(lm(data_p50$total_correct ~ data_p50$Group)) 
anova(lm(data_p50$total_inactiveLP ~ data_p50$Group)) 
anova(lm(data_p50$time_np ~ data_p50$Group)) 
anova(lm(data_p50$NP_responsestate ~ data_p50$Group)) 
anova(lm(data_p50$total_unrewarded_correct_LP ~ data_p50$Group))




