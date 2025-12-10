

#gen_sudoku

library(renv)
library(dplyr)
library(Matrix)

renv::snapshot(exclude = "rgdal")
renv::status()


rm(list=ls());cat('\f')

# funs----
fi <- function(i){
  c("row" = ceiling(i/9), 
    "col" = ifelse(i %% 9 == 0, 9, i %% 9))
}
# check_rows
ckrow <- function(rn){
  return(all(sort(unname(sudok.blank[rn,]) ) == 1:9))
}

# check_cols
ckcol <- function(cn){
  return(all(sort(unname(sudok.blank[cn,]) ) == 1:9))
}
# check_blocks

ckblk <- function(bn){
  if(bn == 1){
    return(all(sort(as.vector(sudok.blank[c("r1", "r2", "r3"), c("c1", "c2", "c3")])) == 1:9))
  }
  if(bn == 2){
    return(all(sort(as.vector(sudok.blank[c("r4", "r5", "r6"), c("c1", "c2", "c3")])) == 1:9))
  }
  if(bn == 3){
    return(all(sort(as.vector(sudok.blank[c("r7", "r8", "r9"), c("c1", "c2", "c3")])) == 1:9))
  }
  if(bn == 4){
    return(all(sort(as.vector(sudok.blank[c("r1", "r2", "r3"), c("c4", "c5", "c6")])) == 1:9))
  }
  if(bn == 5){
    return(all(sort(as.vector(sudok.blank[c("r4", "r5", "r6"), c("c4", "c5", "c6")])) == 1:9))
  }
  if(bn == 6){
    return(all(sort(as.vector(sudok.blank[c("r7", "r8", "r9"), c("c4", "c5", "c6")])) == 1:9))
  }
  if(bn == 7){
    return(all(sort(as.vector(sudok.blank[c("r1", "r2", "r3"), c("c7", "c8", "c9")])) == 1:9))
  }
  if(bn == 8){
    return(all(sort(as.vector(sudok.blank[c("r4", "r5", "r6"), c("c7", "c8", "c9")])) == 1:9))
  }
  if(bn == 9){
    return(all(sort(as.vector(sudok.blank[c("r7", "r8", "r9"), c("c7", "c8", "c9")])) == 1:9))
  }
  
  
}

sudok.blank <- Matrix(data = rep(0, 9*9), 
                      nrow = 9, ncol = 9, 
                      dimnames = list(row = paste("r",1:9, sep = ""), 
                                      col = paste("c",1:9, sep = "")))



sum(sudok.blank)
sudok.blank
#sudok.blank[1:81] <- 1:81


# sudo start
sudok.blank[1,c(2,3,5:7)] <- c(1,6,8,4,2)
sudok.blank[2,c(2,4,7,8)] <- c(2,3,8,6)
sudok.blank[3,c(2,6:9)] <- c(8,6,1,4,9)
sudok.blank[4,c(1,3,8:9)] <- c(6,7,2,4)
sudok.blank[5,c(1,3,5,8)] <- c(2,1,9,8)
sudok.blank[6,c(1,3:5,9)] <- c(9,8,7,4,6)
sudok.blank[7,c(1:2, 6)] <- c(8,9,3)
sudok.blank[8,c(2,5:6,9)] <- c(6,5,1,2)
sudok.blank[9,c(5:8)] <- c(6,9,4,3)


# discovered vals
sudok.blank[14] <- 7
sudok.blank[32] <- 3
sudok.blank[35] <- 5
sudok.blank[61] <- 7
sudok.blank[79] <- 7

sudok.blank[25] <- 5
sudok.blank[27] <- 5
sudok.blank[31] <- 9
sudok.blank[34] <- 5
sudok.blank[36] <- 5
sudok.blank[49] <- 2
sudok.blank[50] <- 2
sudok.blank[60] <- 2
sudok.blank[62] <- 1
sudok.blank[74] <- 5
sudok.blank[9] <- 5

sudok.blank[13] <- 4
sudok.blank[15] <- 7
sudok.blank[18] <- 1
sudok.blank[21] <- 3
sudok.blank[28] <- 3
sudok.blank[30] <- 3
sudok.blank[39] <- 4
sudok.blank[51] <- 5

sudok.blank


# get possibilities-----
sudok.poss <- sudok.blank

df.poss <- NULL

for(i in which(sudok.poss == 0)){
  for(pn in 1:9){
    # check row
    get.rn <- unname(fi(i)["row"])
    
    # check col
    get.cn <- unname(fi(i)["col"])
    
    # check block
    if(get.rn %in% 1:3 & get.cn %in% 1:3){ get.bn <- 1; get.cells <- c(1:3, 10:12, 19:21)}
    if(get.rn %in% 1:3 & get.cn %in% 4:6){ get.bn <- 2; get.cells <- c(1:3+9, 10:12+9, 19:21+9)}
    if(get.rn %in% 1:3 & get.cn %in% 7:9){ get.bn <- 3; get.cells <- c(1:3+18, 10:12+18, 19:21+18)}
    if(get.rn %in% 4:6 & get.cn %in% 1:3){ get.bn <- 4; get.cells <- c(1:3+3, 10:12+3, 19:21+3)}
    if(get.rn %in% 4:6 & get.cn %in% 4:6){ get.bn <- 5; get.cells <- c(1:3+9+3, 10:12+9+3, 19:21+9+3)}
    if(get.rn %in% 4:6 & get.cn %in% 7:9){ get.bn <- 6; get.cells <- c(1:3+18+3, 10:12+18+3, 19:21+18+3)}
    if(get.rn %in% 7:9 & get.cn %in% 1:3){ get.bn <- 7; get.cells <- c(1:3+6, 10:12+6, 19:21+6)}
    if(get.rn %in% 7:9 & get.cn %in% 4:6){ get.bn <- 8; get.cells <- c(1:3+9+6, 10:12+9+6, 19:21+9+6)}
    if(get.rn %in% 7:9 & get.cn %in% 7:9){ get.bn <- 9; get.cells <- c(1:3+18+6, 10:12+18+6, 19:21+18+6)}
    
    if(all(c(!pn %in% sudok.poss[get.rn,], # number isn't in row
          !pn %in% sudok.poss[,get.cn], # number isn't in col
          !pn %in% sudok.poss[get.cells]))){ # number isn't in block
      print(pn)
      print(as.character(i))
      
      df.poss <- rbind(df.poss, 
                       data.frame(cell_n = as.character(i), 
                                  poss_n = pn))
    }else(
      cat("\n")
    ) 
  }
  
  
}

df.poss %>%
  as_tibble() %>%
  group_by(cell_n) %>%
  summarise(n = n(), 
            n_vals = n_distinct(poss_n), 
            min_val = min(poss_n)) %>%
  .[.$n_vals == 1,]


# note: this is so fugged up right now

