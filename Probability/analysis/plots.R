# plots?

#must have run all 4 analysis_p** R files (to have data_p** in global environment)

library(tidyverse)


data_test <- rbind(data_p12, data_p25, data_p50, data_p100)

data_test$P <- c(rep("p12",25),rep("p25",25),rep("p50",25),rep("p100",25))
  
torder <- c("p12","p25","p50","p100")

ggplot(data_test, aes(x=P, y=total_correct_LP, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)
  


ggplot(data_test, aes(x=P, y=total_inactive_LP, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)



ggplot(data_test, aes(x=P, y=mean_timetill_NP, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)



ggplot(data_test, aes(x=P, y=NP_responsestate, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)
