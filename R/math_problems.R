# math puzzles----

library(dplyr)
library(ggplot2)
library(renv)
library(scales)

# renv::status()
# renv::snapshot()

rm(list=ls())

# problem 1----

# Find a 10-digit number where the first digit is how many zeros in the number,
# the second digit is how many 1s in the number etc. until the tenth digit which
# is how many 9s in the number.
url.p1 <- "https://www.mathsisfun.com/puzzles/10-digit-number.html"

x <- 9000000000
n_gen <- function(x){
  require(dplyr)
  if(!x >= 1000000000){
    #stop("x must be 10 digits long")
    cx <- NA
  }else{
    x.split <- strsplit(scales::number(x, big.mark = ""), "") %>%
      unlist() %>%
      as.numeric()
    
    d0 <- sum(x.split == 0)
    d1 <- sum(x.split == 1)
    d2 <- sum(x.split == 2)
    d3 <- sum(x.split == 3)
    d4 <- sum(x.split == 4)
    d5 <- sum(x.split == 5)
    d6 <- sum(x.split == 6)
    d7 <- sum(x.split == 7)
    d8 <- sum(x.split == 8)
    d9 <- sum(x.split == 9)
    cx <- paste(d0,d1,d2,d3,d4,d5,
                 d6,d7,d8,d9, collapse = "", sep = "") %>%
      as.numeric()
  }
  
  
  # output 
  df_out <- data.frame(input = scales::number(x,big.mark = ""), 
                       check = scales::number(cx,big.mark = ""), 
                       delta = scales::number(abs(diff(c(x,cx))), 
                       big.mark = ""))
  
  
  return(df_out)
}

n_gen(9000000000)
     "0123456789"
cat('\f')

n_iters <- 2500
if(!exists("batch.run")){
  batch.run <- NULL
}
for(i in 1:n_iters){
  #print(i)
  v.nums = c(sample(1:9,size=1),
             sample(0:9, 
                    size = 9, 
                    replace = T))
  batch.run <- rbind(batch.run, 
                     n_gen(as.numeric(paste(v.nums,collapse=""))))
}

batch.run$input <- as.numeric(batch.run$input)
batch.run$check <- as.numeric(batch.run$check)
batch.run$delta <- as.numeric(batch.run$delta)

batch.run <- batch.run[complete.cases(batch.run),] %>%
  .[!duplicated(.$input),]

# ggplot(data = batch.run, 
#        aes(x = input, y = delta))+
#   geom_point()+
#   geom_smooth() +
#   scale_y_continuous(labels = scales::comma)

slice_min(batch.run, 
          order_by = delta, 
          n = 10)

ggplot(data = rbind(slice_min(batch.run, 
                              order_by = delta, 
                              prop = 0.1)), 
       aes(x = input, y = delta))+
  geom_point()+
  geom_smooth() +
  scale_y_continuous(labels = scales::comma)

n_gen(1221948330)
     "0123456789"
# list("input" = as.numeric(paste(v.nums,sep="",collapse="")),
#      "check" = n_gen(as.numeric(paste(v.nums,collapse=""))))
# scales::comma(abs(diff(c(as.numeric(paste(v.nums,sep="",collapse="")),
#                          n_gen(as.numeric(paste(v.nums,collapse="")))))))

n_check <- function(x){
  if(nchar(x) != 10){
    stop("x must be 10 digits long")
  }
  
  x.split <- strsplit(as.character(x), "") %>%
    unlist() %>%
    as.numeric()
  
  out <- table(x.split)
  
  for(i in names(out)){
    
  }
  
  return(out)
}

n_check(1534774530) 

table(1534774530)
