#### Day 4 

library(tidyverse)
library(stringr)

## Part 1 

elfgroups <- data.frame(routes = readLines("data/day4.txt"))

elfgroups$elfroute1 <- NA
elfgroups$elfroute2 <- NA

for(i in 1:nrow(elfgroups)){
  elfgroups$elfroute1[i] <- unlist(str_split(elfgroups$routes[i], pattern = ","))[1]
  elfgroups$elfroute2[i] <- unlist(str_split(elfgroups$routes[i], pattern = ","))[2]
}

elfgroups$routes1 <- NA
elfgroups$routes2 <- NA

for(i in 1:nrow(elfgroups)){
  it <- unlist(str_split(elfgroups$elfroute1[i], pattern = "-"))[1]
  it2 <- unlist(str_split(elfgroups$elfroute1[i], pattern = "-"))[2]
  
  seq1 <- seq(as.numeric(it), as.numeric(it2), 1)
  
  it3 <- unlist(str_split(elfgroups$elfroute2[i], pattern = "-"))[1]
  it4 <- unlist(str_split(elfgroups$elfroute2[i], pattern = "-"))[2]
  
  seq2 <- seq(as.numeric(it3), as.numeric(it4), 1)
  
  elfgroups$routes1[i] <- list(seq1)
  elfgroups$routes2[i] <- list(seq2)
}

elfgroups$contains <- NA

for(i in 1:nrow(elfgroups)){
  if(all(unlist(elfgroups$routes1[i]) %in% unlist(elfgroups$routes2[i])) | 
      all(unlist(elfgroups$routes2[i]) %in% unlist(elfgroups$routes1[i]))){
    elfgroups$contains[i] <- 1
  }else{
    elfgroups$contains[i] <- 0
  }
}

sum(elfgroups$contains)

## Part 2 

elfgroups$anyoverlap <- NA

for(i in 1:nrow(elfgroups)){
  if(any(unlist(elfgroups$routes1[i]) %in% unlist(elfgroups$routes2[i])) | 
     any(unlist(elfgroups$routes2[i]) %in% unlist(elfgroups$routes1[i]))){
    elfgroups$anyoverlap[i] <- 1
  }else{
    elfgroups$anyoverlap[i] <- 0
  }
}

sum(elfgroups$anyoverlap)
