###### Day 9 

library(tidyverse)
library(stringr)

### Part 1 

input <- readLines("data/day9.txt")

moveset <- data.frame(dir = str_extract(input, "[A-Z]"),
                      steps = as.numeric(str_extract(input, "[:digit:]{1,3}")))

playfield <- matrix(data = FALSE, nrow = 1000, ncol = 1000)

curr_pos <- c(500, 500)
tail_pos <- c(500, 500)
tail_end <- c(500, 500)

for(i in 1:nrow(moveset)){
  if(moveset$dir[i] == "L"){
    curr_pos <- c(curr_pos[1], curr_pos[2] - moveset$steps[i])
  }
  if(moveset$dir[i] == "R"){
    curr_pos <- c(curr_pos[1], curr_pos[2] + moveset$steps[i])
  }
  if(moveset$dir[i] == "U"){
    curr_pos <- c(curr_pos[1] - moveset$steps[i], curr_pos[2])
  }
  if(moveset$dir[i] == "D"){
    curr_pos <- c(curr_pos[1] +  moveset$steps[i], curr_pos[2])
  }
  if((curr_pos[1]-tail_pos[1]) < -1){
    tail_end <- c(curr_pos[1]+1, curr_pos[2])
  }
  if((curr_pos[1]-tail_pos[1]) > 1){
    tail_end <- c(curr_pos[1]-1, curr_pos[2])
  }
  if((curr_pos[2]-tail_pos[2]) < -1){
    tail_end <- c(curr_pos[1], curr_pos[2]+1)
  }
  if((curr_pos[2]-tail_pos[2]) > 1){
    tail_end <- c(curr_pos[1], curr_pos[2]-1)
  }
  if(all(tail_pos == tail_end)){
    next
  }else{
    if(moveset$dir[i] == "R"){
    playfield[tail_end[1], (tail_pos[2]+1):tail_end[2]] <- TRUE
    }
    if(moveset$dir[i] == "L"){
    playfield[tail_end[1], (tail_pos[2]-1):tail_end[2]] <- TRUE
    }
    if(moveset$dir[i] == "U"){
    playfield[(tail_pos[1]-1):tail_end[1], tail_end[2]] <- TRUE
    }
    if(moveset$dir[i] == "D"){
    playfield[(tail_pos[1]+1):tail_end[1], tail_end[2]] <- TRUE
    }
  
    tail_pos <- tail_end
  }
}

## does not log the start position, so adding ine

sum(playfield) + 1 


## checking 

moveset2 <- data.frame(dir = c("R", "U", "L", "D", "R", "D", "L", "R"),
                       steps = c(4,4,3,1,4,1,5,2))

playfield2 <- matrix(data = FALSE, nrow = 5, ncol = 5)

curr_pos2 <- c(5,1)
tail_pos2 <- c(5,1)
tail_end2 <- c(5,1)

for(i in 1:nrow(moveset2)){
  if(moveset2$dir[i] == "L"){
    curr_pos2 <- c(curr_pos2[1], curr_pos2[2] - moveset2$steps[i])
  }
  if(moveset2$dir[i] == "R"){
    curr_pos2 <- c(curr_pos2[1], curr_pos2[2] + moveset2$steps[i])
  }
  if(moveset2$dir[i] == "U"){
    curr_pos2 <- c(curr_pos2[1] - moveset2$steps[i], curr_pos2[2])
  }
  if(moveset2$dir[i] == "D"){
    curr_pos2 <- c(curr_pos2[1] +  moveset2$steps[i], curr_pos2[2])
  }
  if((curr_pos2[1]-tail_pos2[1]) < -1){
    tail_end2 <- c(curr_pos2[1]+1, curr_pos2[2])
  }
  if((curr_pos2[1]-tail_pos2[1]) > 1){
    tail_end2 <- c(curr_pos2[1]-1, curr_pos2[2])
  }
  if((curr_pos2[2]-tail_pos2[2]) < -1){
    tail_end2 <- c(curr_pos2[1], curr_pos2[2]+1)
  }
  if((curr_pos2[2]-tail_pos2[2]) > 1){
    tail_end2 <- c(curr_pos2[1], curr_pos2[2]-1)
  }
  if(all(tail_pos2 == tail_end2)){
    next
  }else{
    if(moveset2$dir[i] == "R"){
      playfield2[tail_end2[1], (tail_pos2[2]+1):tail_end2[2]] <- TRUE
    }
    if(moveset2$dir[i] == "L"){
      playfield2[tail_end2[1], (tail_pos2[2]-1):tail_end2[2]] <- TRUE
    }
    if(moveset2$dir[i] == "U"){
      playfield2[(tail_pos2[1]-1):tail_end2[1], tail_end2[2]] <- TRUE
    }
    if(moveset2$dir[i] == "D"){
      playfield2[(tail_pos2[1]+1):tail_end2[1], tail_end2[2]] <- TRUE
    }
    
    tail_pos2 <- tail_end2
  }
}

sum(playfield2)

## Here i found that mine did not log start position...

rm(moveset2, playfield2, tail_end2, tail_pos2, curr_pos2)



#### Part 2 

## I have no clue...

playfield <- matrix(data = FALSE, nrow = 50, ncol = 50)

moveset <- data.frame(dir = c("R", "U", "L", "D", "R", "D", "L", "U"),
                      steps = c(5,8,8,3,17,10,25,20))

head_pos <- c(25, 25)
sec_pos <- c(25, 25)
thr_pos <- c(25, 25)
frth_pos <- c(25, 25)
fifth_pos <- c(25, 25)
sxth_pos <- c(25, 25)
svn_pos <- c(25, 25)
eight_pos <- c(25, 25)
ninth_pos <- c(25, 25)
tenth_pos <- c(25, 25)

for(i in 1:nrow(moveset)){
  for(j in 1:moveset$steps[i]){
    
    playfield[tenth_pos[1], tenth_pos[2]] <- TRUE
    
    if(moveset$dir[i] == "U"){
      head_pos <- c(head_pos[1]-1, head_pos[2])
    }
    if(moveset$dir[i] == "D"){
      head_pos <- c(head_pos[1]+1, head_pos[2])
    }
    if(moveset$dir[i] == "L"){
      head_pos <- c(head_pos[1], head_pos[2]-1)
    }
    if(moveset$dir[i] == "R"){
      head_pos <- c(head_pos[1], head_pos[2]+1)
    }
    if(all((head_pos - sec_pos) %in% c(-1:1))){
      next
    }else{
      if((head_pos[1] - sec_pos[1]) == -2){
        sec_pos <- c(head_pos[1]+1, head_pos[2])
      }
      if((head_pos[1] - sec_pos[1]) == 2){
        sec_pos <- c(head_pos[1]-1, head_pos[2])
      }
      if((head_pos[2] - sec_pos[2]) == -2){
        sec_pos <- c(head_pos[1], head_pos[2]+1)
      } 
      if((head_pos[2] - sec_pos[2]) == 2){
        sec_pos <- c(head_pos[1], head_pos[2]-1)
      }
      if(all((sec_pos - thr_pos) %in% c(-1:1))){
        next
      }else{
        if((sec_pos[1] - thr_pos[1]) == -2){
          thr_pos <- c(sec_pos[1]+1, sec_pos[2])
        }
        if((sec_pos[1] - thr_pos[1]) == 2){
          thr_pos <- c(sec_pos[1]-1, sec_pos[2])
        }
        if((sec_pos[2] - thr_pos[2]) == -2){
          thr_pos <- c(sec_pos[1], sec_pos[2]+1)
        }
        if((sec_pos[2] - thr_pos[2]) == 2){
          thr_pos <- c(sec_pos[1], sec_pos[2]-1)
        }
        if(all((thr_pos - frth_pos) %in% c(-1:1))){
          next
        }else{
          if((thr_pos[1] - frth_pos[1]) == -2){
            frth_pos <- c(thr_pos[1]+1, thr_pos[2])
          }
          if((thr_pos[1] - frth_pos[1]) == 2){
            frth_pos <- c(thr_pos[1]-1, thr_pos[2])
          }
          if((thr_pos[2] - frth_pos[2]) == -2){
            frth_pos <- c(thr_pos[1], thr_pos[2]+1)
          }
          if((thr_pos[2] - frth_pos[2] == 2)){
            frth_pos <- c(thr_pos[1], thr_pos[2]-1)
          }
          if(all((frth_pos - fifth_pos) %in% c(-1:1))){
            next
          }else{
            if((frth_pos[1] - fifth_pos[1]) == -2){
              fifth_pos <- c(frth_pos[1]+1, frth_pos[2])
            }
            if((frth_pos[1] - fifth_pos[1]) == 2){
              fifth_pos <- c(frth_pos[1]-1, frth_pos[2])
            }
            if((frth_pos[2] - fifth_pos[1]) == -2){
              fifth_pos <- c(frth_pos[1], frth_pos[2]+1)
            }
            if((frth_pos[2] - fifth_pos[2] == 2)){
              fifth_pos <- c(frth_pos[1], frth_pos[2]-1)
            }
            if(all((fifth_pos - sxth_pos) %in% c(-1:1))){
              next
            }else{
              if((fifth_pos[1] - sxth_pos[1]) == -2){
                sxth_pos <- c(fifth_pos[1]+1, fifth_pos[2])
              }
              if((fifth_pos[1] - sxth_pos[1]) == 2){
                sxth_pos <- c(fifth_pos[1]-1, fifth_pos[2])
              }
              if((fifth_pos[2] - sxth_pos[2]) == -2){
                sxth_pos <- c(fifth_pos[1], fifth_pos[2]+1)
              }
              if((fifth_pos[2] - sxth_pos[2]) == 2){
                sxth_pos <- c(fifth_pos[1], fifth_pos[2]-1)
              }
              if(all((sxth_pos - svn_pos) %in% c(-1:1))){
                next
              }else{
                if((sxth_pos[1] - svn_pos[1]) == -2){
                  svn_pos <- c(sxth_pos[1]+1, sxth_pos[2])
                }
                if((sxth_pos[1] - svn_pos[1]) == 2){
                  svn_pos <- c(sxth_pos[1]-1, sxth_pos[2])
                }
                if((sxth_pos[2] - svn_pos[2]) == -2){
                  svn_pos <- c(sxth_pos[1], sxth_pos[2]+1)
                }
                if((sxth_pos[2] - svn_pos[2]) == 2){
                  svn_pos <- c(sxth_pos[1], sxth_pos[2]-1)
                }
                if(all((svn_pos - eight_pos) %in% c(-1:1))){
                  next
                }else{
                  if((svn_pos[1] - eight_pos[1]) == -2){
                    eight_pos <- c(svn_pos[1]+1, svn_pos[2])
                  }
                  if((svn_pos[1] - eight_pos[1]) == 2){
                    eight_pos <- c(svn_pos[1]-1, svn_pos[2])
                  }
                  if((svn_pos[2] - eight_pos[2]) == -2){
                    eight_pos <- c(svn_pos[1], svn_pos[2]+1)
                  }
                  if((svn_pos[2] - eight_pos[2]) == 2){
                    eight_pos <- c(svn_pos[1], svn_pos[2]-1)
                  }
                  if(all((eight_pos - ninth_pos) %in% c(-1:1))){
                    next
                  }else{
                    if((eight_pos[1] - ninth_pos[1]) == -2){
                      ninth_pos <- c(eight_pos[1]+1, eight_pos[2])
                    }
                    if((eight_pos[1] - ninth_pos[1]) == 2){
                      ninth_pos <- c(eight_pos[1]-1, eight_pos[2])
                    }
                    if((eight_pos[2] - ninth_pos[2]) == -2){
                      ninth_pos <- c(eight_pos[1], eight_pos[2]+1)
                    }
                    if((eight_pos[2] - ninth_pos[2]) == 2){
                      ninth_pos <- c(eight_pos[1], eight_pos[2]-1)
                    }
                    if(all((ninth_pos - tenth_pos) %in% c(-1:1))){
                      next
                    }else{
                      if((ninth_pos[1] - tenth_pos[1]) == -2){
                        tenth_pos <- c(ninth_pos[1]+1, ninth_pos[2])
                      }
                      if((ninth_pos[1] - tenth_pos[1]) == 2){
                        tenth_pos <- c(ninth_pos[1]-1, ninth_pos[2])
                      }
                      if((ninth_pos[2] - tenth_pos[2]) == -2){
                        tenth_pos <- c(ninth_pos[1], ninth_pos[2]+1)
                      }
                      if((ninth_pos[2] - tenth_pos[2]) == 2){
                        tenth_pos <- c(ninth_pos[1], ninth_pos[2]-1)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

sum(playfield)

playfield[500, 500]


playfield[735,478]

## Didnt work... Return to at some point
