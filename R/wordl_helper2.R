library(renv)
library(dplyr)
library(readr)
library(curl)
#library(lexicon)
library(glue)
library(crayon)

renv::snapshot()
renv::status()

# https://web.ma.utexas.edu/users/rusin/wordle/wordlist.html
rm(list=ls());cat('\f')

guess_sol <- function(gue1 = "cakes", 
                      sol1 = "sweet"){
  require(crayon)
  temp.g <- unlist(strsplit(gue1, ""))
  temp.s <- unlist(strsplit(sol1, ""))
  
  out.df <- NULL
  for(i in 1:5){
    temp.g[i] == temp.s[i]
    temp.g[i] %in% temp.s
    
    out.df <- rbind(out.df, 
                    data.frame(ln = i, 
                               ltr = temp.g[i], 
                               green = temp.g[i] == temp.s[i], 
                               yellow = temp.g[i] %in% temp.s))
    
  }
  
  out.v <- NULL
  for(i in 1:nrow(out.df)){
    if(out.df$green[i]){
      #print("green")
      out.v <- c(out.v, 
                 bold(bgGreen(black(out.df$ltr[i]))))
      
    }else if(out.df$yellow[i]){
      #print("yellow")
      out.v <- c(out.v, 
                 bgYellow(red(out.df$ltr[i])))
      
    }else{
      out.v <- c(out.v, 
                 out.df$ltr[i])
      
    }
  }
  
  out.v <- paste(out.v, sep = "", collapse = "")
  
  return(out.v)
}

wrdl <- readr::read_csv("data/wordle_list.csv")$x
sol  <- sample(wrdl,size=1)

# GUESS 1----
g1   <- sample(wrdl, size = 1)
guess_sol(g1, sol) %>% cat()

# GUESS 2----
# must have at least 1 of the following letters:
g2 <- grep(pattern = "e", x = wrdl, value = T) %>%
  # must not have any of these letters: 
  .[!grepl("p|u|r", .)] %>%
  # must not have these letters in these positions: 
  .[!grepl("^..e.$", .)] %>% 
  # must have these letters in these positions: 
  sample(., size = 1) 

guess_sol(g2, sol) %>% cat()  

# GUESS 3----
grep(pattern = "e", x = wrdl, value = T) %>%
  # must not have any of these letters: 
  .[!grepl("p|u|r", .)] %>%
  # must not have these letters in these positions: 
  .[!grepl("^..e.$", .)]  
  # must have these letters in these positions:

# GUESS 4----

# GUESS 5----

# GUESS 6----