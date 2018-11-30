#### Clean Coulb Data

CleanData <- function(data) {
  
  library(tidyverse)
  
  ## first: total_correct_LP, total_inactive_LP, mean_timetill_NP
  
  data2 <- data %>%
    select(Subject, Time.Start, Time.Finish, Duration, Enter.State, A1, A2, A3) #select only useful cols
  
  colnames(data2)[6:8] <- c("Number.A1", "Number.A2", "Number.A3") #rename cols to reflect count data
  
  data2 <- data2[!apply(is.na(data2) | data2 == "", 1, all),] #removes all NAs and empty cells
  
  dfa <- data2[,c(6:8)]
  dfa2 <- dfa[-1,]
  
  dfb <- data2[,c(1:5)]
  dfb2 <- dfb[-nrow(dfb),]
  
  ndata2 <- cbind(dfb2,dfa2)
  
  ndata2$Subject <- gsub("_", "", ndata2$Subject) #removes _ from subject name
  
  return(ndata2)
}





