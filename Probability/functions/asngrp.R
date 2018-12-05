## function for assigning Group


asngrp <- function(ungpd) {
  
  library(tidyverse)
  
  pa <- ungpd[1:11,]
  pb <- pa %>%
    mutate(Group = ifelse(grepl("0$|2$|4$|6$|8$",pa$Subject), "Expt", "Ctrl"))

  px <- ungpd[12:25,]
  py <- px %>%
    mutate(Group = ifelse(grepl("0$|2$|4$|6$|8$",px$Subject), "Ctrl", "Expt"))

  grpd <- rbind(pb,py)

  return(grpd)
}

