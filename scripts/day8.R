###### Day 8 


### Part 1 


input <- readLines("data/day8.txt")

test <- str_split(input, pattern = "")

test2 <- lapply(test, as.numeric)

test3 <- matrix(unlist(test2), ncol = 99, byrow = TRUE)

test4 <- matrix(data = FALSE, nrow = 99, ncol = 99)

for(i in 1:nrow(test3)){
  for(j in 1:ncol(test3)){
    if(i == 1 | i == 99 | j == 1 | j == 99){
      test4[i,j] <- TRUE
    }else{
      it <- test3[i,1:(j-1)]
      it2 <- test3[i, (j+1):99]
      it3 <- test3[1:(i-1),j]
      it4 <- test3[(i+1):99, j]
      
      left <- all(test3[i,j]>it)
      right <- all(test3[i,j]>it2)
      up <- all(test3[i,j]>it3)
      down <- all(test3[i,j]>it4)
      
      if(any(left,right,up,down)){
        test4[i,j] <- TRUE
      }else{
        test4[i,j] <- FALSE
      }
    }
    
  }
  
}

sum(test4)

checkingcheck <- c(1,3,2,5,3,4,8,4,2,9)

which.max(checkingcheck)

scenic_score <- matrix(0, nrow = nrow(test3), ncol = ncol(test3))

for(i in 2:(nrow(test3)-1)){
  for(j in 2:(ncol(test3)-1)){
    curr_height <- test3[i,j]
    
    it <- test3[i,1:(j-1)]
    it2 <- test3[i, (j+1):99]
    it3 <- test3[1:(i-1),j]
    it4 <- test3[(i+1):99, j]
    
    tmp1 <- which(rev(it) >= curr_height)[1]
    tmp1 <- ifelse(is.na(tmp1), length(it), tmp1)
    
    tmp2 <- which(it2 >= curr_height)[1]
    tmp2 <- ifelse(is.na(tmp2), length(it2), tmp2)
    
    tmp3 <- which(rev(it3) >= curr_height)[1]
    tmp3 <- ifelse(is.na(tmp3), length(it3), tmp3)
    
    tmp4 <- which(it4 >= curr_height)[1]
    tmp4 <- ifelse(is.na(tmp4), length(it4), tmp4)
    
    score <- tmp1*tmp2*tmp3*tmp4
    
    scenic_score[i,j] <- score
  }
}


max(scenic_score)
