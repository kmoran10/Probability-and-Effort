# plots?

#must have run all 4 analysis_p** R files (to have data_p** in global environment)

setwd("~/R/Probability-and-Effort/Probability")

library(tidyverse)

source("analysis/analysis_p12.R")
source("analysis/analysis_p25.R")
source("analysis/analysis_p50.R")
source("analysis/analysis_p100.R")
rm(list = ls(pattern = "\\cl."))

##

data_test <- rbind(data_p12, data_p25, data_p50, data_p100)

data_test$P <- c(rep("p12",25),rep("p25",25),rep("p50",25),rep("p100",25))
  
torder <- c("p12","p25","p50","p100")

ggplot(data_test, aes(x=P, y=total_correct, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)
  


ggplot(data_test, aes(x=P, y=total_active_LP, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)



ggplot(data_test, aes(x=P, y=total_inactiveLP, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)



ggplot(data_test, aes(x=P, y=time_np, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)



ggplot(data_test, aes(x=P, y=NP_responsestate, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)



ggplot(data_test, aes(x=P, y=total_np, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(limits = torder)


##

npc <- data_p12 %>%
  group_by(Group) %>%
  select(total_np) %>%
  filter(Group == "Ctrl")
npe <- data_p12 %>%
  group_by(Group) %>%
  select(total_np) %>%
  filter(Group == "Expt")

alpc <- data_p12 %>%
  group_by(Group) %>%
  select(total_active_LP) %>%
  filter(Group == "Ctrl")
alpe <- data_p12 %>%
  group_by(Group) %>%
  select(total_active_LP) %>%
  filter(Group == "Expt")

mean(npc$total_np)/mean(alpc$total_active_LP)
mean(npe$total_np)/mean(alpe$total_active_LP)

##


