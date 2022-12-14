### Day 1

## Part 1

test <- readLines("data/day1.txt")

library(tidyverse)

elves <- tibble(test)
elves$number <- NA

it <- 1

for(i in 1:nrow(elves)){
  if(elves$test[i] == ""){
    it <- it + 1
  }
  elves$number[i] <- it
}

elves2 <- elves %>% 
  filter(!(test == "")) %>%
  mutate(test = as.numeric(test)) %>% 
  group_by(number) %>% 
  summarise(cal_amount = sum(test))

max(elves2$cal_amount)

## Part 2

elves3 <- elves2 %>% 
  arrange(desc(elves2$cal_amount))

sum(elves3$cal_amount[1:3])
