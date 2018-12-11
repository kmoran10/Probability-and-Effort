## 25% tests

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("functions/All functions_L.R")
source("functions/cbindPad.R")
source("functions/asngrp.R")



p25files <- list.files("raw_data/TEST", pattern=".*25p.*\\.csv", full.names = TRUE) #creates a list of all files in folder 25% test type .csv
p25df <- lapply(p25files, read.csv) #reads all CSVs in filenames and makes them a list




p25cl1 <- lapply(p25df, cleandata1)
p25cl2 <- lapply(p25cl1, newcolumns)
p25cl3 <- lapply(p25cl2, summary1)
names(p25cl3) <- substr(p25files, 1, 31)
p25cl4 <- lapply(p25df, cleandata2)
p25cl5 <- lapply(p25cl4, newcolumns2)
p25cl6 <- lapply(p25cl5, summary2)
names(p25cl6) <- substr(p25files, 1, 31)

p25clA <- p25cl3 %>% 
  bind_rows

p25clB <- p25cl6 %>% 
  bind_rows

p25clean <- cbind(p25clA, p25clB[,2:3])

data_p25 <- asngrp(p25clean)




lapply(data_p25[,2:7], function(x) t.test(x ~ data_p25$Group, var.equal = TRUE))
apply(data_p25[,2:7], 2, shapiro.test)
lapply(data_p25[,2:7], function(x) wilcox.test(x ~ data_p25$Group))

by(data_p25$total_correct_LP, data_p25$Group, pastecs::stat.desc)



anova(lm(data_p25$total_active_LP ~ data_p25$Group)) 
anova(lm(data_p25$total_correct ~ data_p25$Group)) 
anova(lm(data_p25$total_inactiveLP ~ data_p25$Group)) 
anova(lm(data_p25$time_np ~ data_p25$Group)) 
anova(lm(data_p25$NP_responsestate ~ data_p25$Group)) 
anova(lm(data_p25$total_unrewarded_correct_LP ~ data_p25$Group))




