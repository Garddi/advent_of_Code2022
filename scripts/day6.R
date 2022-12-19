####### Day 6

### Part 1 

library(tidyverse)
library(tokenizers)
day6dat <- readLines("data/day6.txt")

test <- str_split(day6dat, "", n=str_length(day6dat))

unlist(test)

test2 <- str_c(unlist(test), collapse = " ")

test3 <- tokenize_ngrams(test2, n = 4)

test4 <- unlist(test3)

test5 <- str_split(test4, " ", n = 4)

duplicates <- list()

for(i in 1:4092){
  duplicates[i] <- any(test5[[i]][[1]] %in% c(test5[[i]][[2]], test5[[i]][[3]], test5[[i]][[4]]),
                       test5[[i]][[2]] %in% c(test5[[i]][[1]], test5[[i]][[3]], test5[[i]][[4]]),
                       test5[[i]][[3]] %in% c(test5[[i]][[1]], test5[[i]][[2]], test5[[i]][[4]]),
                       test5[[i]][[4]] %in% c(test5[[i]][[1]], test5[[i]][[2]], test5[[i]][[3]]))
}

test6 <- unlist(duplicates)

which(test6 == FALSE)

test5[[1579]]
test5[[1580]]
test5[[1581]]
test5[[1582]]


### Part 2 

test8 <- unlist(tokenize_ngrams(test2, n = 14))

test9 <- str_split(test8, " ", n = 14)

test10 <- lapply(test9, duplicated)

test11 <- unlist(lapply(test10, any))

which(test11 == FALSE)[1] + 13
