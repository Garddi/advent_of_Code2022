#### Day 3 

## Part 1 

# To help prioritize item rearrangement, every item type can be converted to a priority:
#   
#   Lowercase item types a through z have priorities 1 through 26.
# Uppercase item types A through Z have priorities 27 through 52.
# In the above example, the priority of the item type 
# that appears in both compartments of each rucksack is 16 
# (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157.
# 
# Find the item type that appears in both compartments of each rucksack. 
# What is the sum of the priorities of those item types?

library(tidyverse)
library(stringr)

rucksacks <- data.frame(items = readLines("data/day3.txt"))
rucksacks <- rucksacks %>% 
  mutate(comp1 = NA,
         comp2 = NA)

for(i in 1:nrow(rucksacks)){
  it <- str_length(rucksacks$items[i])
  it2 <- it/2
  
  rucksacks$comp1[i] <- substr(rucksacks$items[i], start = 1, stop = it2)
  rucksacks$comp2[i] <- substr(rucksacks$items[i], start = it2+1, stop = it)
  
}

table(str_length(rucksacks$comp1) == str_length(rucksacks$comp2))

## All good

rucksacks <- rucksacks %>% 
  mutate(shared_item = NA)

for(i in 1:nrow(rucksacks)){
  it <- str_length(rucksacks$items[i])
  it2 <- it/2
  
  tmp1 <- unlist(str_split(rucksacks$comp1[i], pattern = "", n = it2))
  tmp2 <- unlist(str_split(rucksacks$comp2[i], pattern = "", n = it2))
  
  rucksacks$shared_item[i] <- tmp1[which(tmp1 %in% tmp2)]
}

priolist <- data.frame(shared_item = c(letters, LETTERS),
                       value = c(1:52))

new_list <- left_join(rucksacks, priolist, by = c("shared_item"))

sum(new_list$value)


### Part 2

it <- 1

new_list$elfgroup <- NA

for(i in 1:nrow(rucksacks)){
  if(i == 1){
    it <- 1
  }
  
  new_list$elfgroup[i] <- it
  
  if(i %% 3 == 0){
    it <- it+1
  }
  
}

elfgroups <- new_list %>% 
  select(elfgroup, items) %>% 
  group_by(elfgroup) %>% 
  mutate(obs = row_number()) %>% 
  pivot_wider(names_from = "obs", 
              values_from = "items", 
              names_prefix="value")

elfgroups$shared_item <- NA

for(i in 1:nrow(elfgroups)){
  it <- str_length(elfgroups$value1[i])
  it2 <- str_length(elfgroups$value2[i])
  it3 <- str_length(elfgroups$value3[i])
  
  tmp1 <- unlist(str_split(elfgroups$value1[i], pattern = "", n = it))
  tmp2 <- unlist(str_split(elfgroups$value2[i], pattern = "", n = it2))
  tmp3 <- unlist(str_split(elfgroups$value3[i], pattern = "", n = it3))
  
  elfgroups$shared_item[i] <- tmp1[which(tmp1 %in% tmp2 & tmp1 %in% tmp3)]
}


tmp1[which(tmp1 %in% tmp2 & tmp1 %in% tmp3)]

newer_list <- left_join(elfgroups, priolist, by = c("shared_item"))

sum(newer_list$value)
