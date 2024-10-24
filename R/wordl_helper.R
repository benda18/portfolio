library(renv)
library(dplyr)
library(readr)
library(curl)
library(lexicon)
library(glue)
library(crayon)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f')

# funs----
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


# get dictionary of 5 letter words
lexicon::grady_augmented %>% .[nchar(.) == 5] %>%
  # guessed probably-not letters
  #.[!grepl("f|d", .)] %>% 
  # guessed probably-yes letters
  #.[grepl("y", .)] %>%
  # generic confirmed, unplaced letters
  #.[grepl("s", .)] %>% 
  # generic ruled-out letters
  .[!grepl("h", .)] %>% 
  .[!grepl("u", .)] %>% 
  .[!grepl("t", .)] %>% 
  .[!grepl("r", .)] %>% 
  .[!grepl("l", .)] %>% 
  .[!grepl("e", .)] %>% 
  .[!grepl("n", .)] %>% 
  .[!grepl("i", .)] %>% 
  # first letter in/out
  .[!grepl("^s....$", .)] %>%
  # 2nd letter in/out
  .[grepl("^.o...$", .)] %>%
  # 3rd letter in/out
  .[!grepl("^..o..$", .)] %>%
  # 4th letter in/out
  .[grepl("^...s.$", .)] %>%
  # last letter in/out
  .[grepl("^....y$", .)] %>%
  .[!grepl("^....s$", .)] %>% #grep("d", ., value = T)
  # do some more stuff
  strsplit(., "") %>%
  #lapply(., nth, 5) %>%
  unlist() %>%
  table() %>%
  sort()


# tie breaker
var1 <- "bossy"
var2 <- "mossy"

tb <- sample(c(var1, var2), 
       size = 10001, replace = T) %>% 
  table() %>%
  prop.table(); names(which(tb == max(tb)))
