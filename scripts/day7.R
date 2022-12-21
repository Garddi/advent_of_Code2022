##### Day 7 


### Part 1 


consoleout <- data.frame(lines = readLines("data/day7.txt"))

consoleout$level <- 0

for(i in 2:nrow(consoleout)){
  consoleout$level[i] <- ifelse(str_detect(consoleout$lines[i], "^\\$\\scd\\s[A-Za-z]+$"), 
                                consoleout$level[i-1] + 1,
                                ifelse(str_detect(consoleout$lines[i], "^\\$\\scd\\s\\.\\."), 
                                       consoleout$level[i-1] - 1, 
                                       consoleout$level[i-1]))
}


consoleout <- consoleout %>% 
  mutate(fileseize = as.numeric(str_extract(consoleout$lines, "[:digit:]+")))

consoleout$is_dir <- str_detect(consoleout$lines, "\\$\\scd\\s[A-Za-z\\/]+")

consoleout$folder <- NA
consoleout$folder[which(consoleout$is_dir)] <- sapply(strsplit(consoleout$lines[which(consoleout$is_dir)], "\\s"), "[[", 3)

folder_list <- list()

for(i in 1:nrow(consoleout)){
  if(str_detect(consoleout$lines[i], "^\\$\\scd\\s[A-Za-z\\/]+")){
    start <- i
    
    curr_level <- consoleout$level[i]
    
    end <- NA
    
    for(j in start:nrow(consoleout)){
      if(consoleout$lines[j] == "$ cd .." & consoleout$level[j] == curr_level-1){
        end <- j
        break
      }else if(j == nrow(consoleout)){
        end <- j
        break
      }
    }
    
    tmp <- data.frame(folder = consoleout$folder[i],
                      file_size = consoleout$fileseize[start:end])
    
    folder_list[[i]] <- sum(tmp$file_size[which(is.na(tmp$file_size) == FALSE)])
    
  }else{
    next
  }
}

stuffs <- unlist(folder_list)

sum(stuffs[which(stuffs < 100000)])

#### Part 2 

usedspace <- 70000000 - stuffs[1]

neededspace <- 30000000 - usedspace

usables <- stuffs[which(stuffs>neededspace)]

usables[which.min(usables)]
