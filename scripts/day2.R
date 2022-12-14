### Day 2 

## Part 1

# (1 for Rock, 2 for Paper, and 3 for Scissors)
# (0 if you lost, 3 if the round was a draw, and 6 if you won)

# A for Rock, B for Paper, and C for Scissors
# X for Rock, Y for Paper, and Z for Scissors

datastuff <- readLines("data/day2.txt")

datastuff2 <- tibble(datastuff)

datastuff2 <- datastuff2 %>% 
  mutate(res_score = case_when(
    str_detect(datastuff, "A") & str_detect(datastuff, "X") ~ 4,
    str_detect(datastuff, "A") & str_detect(datastuff, "Y") ~ 8,
    str_detect(datastuff, "A") & str_detect(datastuff, "Z") ~ 3,
    str_detect(datastuff, "B") & str_detect(datastuff, "X") ~ 1,
    str_detect(datastuff, "B") & str_detect(datastuff, "Y") ~ 5,
    str_detect(datastuff, "B") & str_detect(datastuff, "Z") ~ 9,
    str_detect(datastuff, "C") & str_detect(datastuff, "X") ~ 7,
    str_detect(datastuff, "C") & str_detect(datastuff, "Y") ~ 2,
    str_detect(datastuff, "C") & str_detect(datastuff, "Z") ~ 6
  ))

sum(datastuff2$res_score)


## Part 2 

# X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
# A for Rock, B for Paper, and C for Scissors
# (1 for Rock, 2 for Paper, and 3 for Scissors)

datastuff2 <- datastuff2 %>% 
  mutate(res_score = case_when(
    str_detect(datastuff, "A") & str_detect(datastuff, "X") ~ 3,
    str_detect(datastuff, "A") & str_detect(datastuff, "Y") ~ 4,
    str_detect(datastuff, "A") & str_detect(datastuff, "Z") ~ 8,
    str_detect(datastuff, "B") & str_detect(datastuff, "X") ~ 1,
    str_detect(datastuff, "B") & str_detect(datastuff, "Y") ~ 5,
    str_detect(datastuff, "B") & str_detect(datastuff, "Z") ~ 9,
    str_detect(datastuff, "C") & str_detect(datastuff, "X") ~ 2,
    str_detect(datastuff, "C") & str_detect(datastuff, "Y") ~ 6,
    str_detect(datastuff, "C") & str_detect(datastuff, "Z") ~ 7
  ))

sum(datastuff2$res_score)

