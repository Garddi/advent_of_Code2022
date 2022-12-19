#### Day 5 

### Part 1 

library(tidyverse)

readLines("data/day5.txt")[1:9]

testytest <- readLines("data/day5.txt")[1:9]

## this isnt gonna be easy

str_length(testytest[1])


## 9 stacks, each line has 35 characters, starts at 2, then 6, 4 charas between

stack_id <- data.frame(stack = c(1:9), 
                       number = seq(2, 34, 4))

stack1 <- list()

for(i in 1:length(testytest)){
  stack1[i] <- unlist(str_split(testytest[i], "", n = 35))[stack_id$number[1]]
}

stack1 <- unlist(stack1)

stacker <- list()
stacktmp2 <- list()

for(j in 1:nrow(stack_id)){
  for(i in 1:length(testytest)){
    stacktmp2[i] <- unlist(str_split(testytest[i], "", n = 35))[stack_id$number[j]]
  }
  stacker[j] <- list(stacktmp2)
}

unlist(stacker)

stack1 <- rev(unlist(stacker)[1:8])
stack2 <- rev(unlist(stacker)[9:16])
stack3 <- rev(unlist(stacker)[17:24])
stack4 <- rev(unlist(stacker)[25:32])
stack5 <- rev(unlist(stacker)[33:40])
stack6 <- rev(unlist(stacker)[41:48])
stack7 <- rev(unlist(stacker)[49:56])
stack8 <- rev(unlist(stacker)[57:64])
stack9 <- rev(unlist(stacker)[65:72])

## Loading the moves

tmp1 <- readLines("data/day5.txt")
  
moveset <- data.frame(instruction = readLines("data/day5.txt")[11:length(tmp1)])

moveset$number <- NA
moveset$from <- NA
moveset$to <- NA

for(i in 1:nrow(moveset)){
  moveset$number[i] <- unlist(str_extract_all(moveset$instruction[i], "([0-9]+)"))[1]
  moveset$from[i] <- unlist(str_extract_all(moveset$instruction[i], "([0-9]+)"))[2]
  moveset$to[i] <- unlist(str_extract_all(moveset$instruction[i], "([0-9]+)"))[3]
}

stack1 <- stack1[!stack1 == c(" ")]
stack2 <- stack2[!stack2 == c(" ")]
stack3 <- stack3[!stack3 == c(" ")]
stack4 <- stack4[!stack4 == c(" ")]
stack5 <- stack5[!stack5 == c(" ")]
stack6 <- stack6[!stack6 == c(" ")]
stack7 <- stack7[!stack7 == c(" ")]
stack8 <- stack8[!stack8 == c(" ")]
stack9 <- stack9[!stack9 == c(" ")]

stacker <- list(stack1, stack2, stack3, stack4, stack5, stack6, stack7, 
                stack8, stack9)

moveset[,2:4] <- sapply(moveset[,2:4], FUN = as.numeric)

for(i in 1:nrow(moveset)){
  stacker[moveset$to[i]] <- list(c(unlist(stacker[moveset$to[i]]), 
                              rev(unlist(stacker[moveset$from[i]])[((length(unlist(stacker[moveset$from[i]]))+1)-(moveset$number[i])):length(unlist(stacker[moveset$from[i]]))])))
  
  stacker[moveset$from[i]] <- list(unlist(stacker[moveset$from[i]])[-(((length(unlist(stacker[moveset$from[i]]))+1)-(moveset$number[i])):length(unlist(stacker[moveset$from[i]])))])
  
  Sys.sleep(1)

}

str_c(unlist(lapply(stacker, last)), collapse = "")

## jesus thats ugly

### part 2

## I can remove the reverse, looks like shit coding is useful


stacker <- list(stack1, stack2, stack3, stack4, stack5, stack6, stack7, 
                stack8, stack9)

for(i in 1:nrow(moveset)){
  stacker[moveset$to[i]] <- list(c(unlist(stacker[moveset$to[i]]), 
                                   unlist(stacker[moveset$from[i]])[((length(unlist(stacker[moveset$from[i]]))+1)-(moveset$number[i])):length(unlist(stacker[moveset$from[i]]))]))
  
  stacker[moveset$from[i]] <- list(unlist(stacker[moveset$from[i]])[-(((length(unlist(stacker[moveset$from[i]]))+1)-(moveset$number[i])):length(unlist(stacker[moveset$from[i]])))])
  
}

str_c(unlist(lapply(stacker, last)), collapse = "")
