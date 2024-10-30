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

yesterday.word <- "easel"

# funs----

my_guess_stats <- function(n1 = 0, n2 = 0, 
                           n3 = 2, n4 = 5, n5 = 4, 
                           n6 = 1, nl = 1){
  
}

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
#lexicon::grady_augmented %>% .[nchar(.) == 5] %>%
readr::read_csv("data/wordle_list.csv")$x %>%
  # guessed probably-not letters----
#.[!grepl("f|d", .)] %>% 
# guessed probably-yes letters----
# .[grepl("l", .)] %>%
#   .[grepl("o", .)] %>%
#   .[grepl("r", .)] %>%
#   .[grepl("a", .)] %>% 
#  .[grepl("e", .)] %>%
# generic confirmed, unplaced letters----
.[grepl("s", .)] %>% 
  .[grepl("l", .)] %>% 
  .[grepl("e", .)] %>% 
  # generic ruled-out letters----
.[!grepl("t", .)] %>% 
  .[!grepl("u", .)] %>%
  .[!grepl("n", .)] %>%
  .[!grepl("i", .)] %>%
  .[!grepl("c", .)] %>%
  .[!grepl("o", .)] %>%
  .[!grepl("r", .)] %>%
  .[!grepl("p", .)] %>%
  #  .[!grepl("a", .)] %>% 
  #  .[!grepl("e", .)] %>%
  #  .[!grepl("s", .)] %>%
  # .[!grepl("k", .)] %>%
  # .[!grepl("y", .)] %>%
  # first letter in/out----
.[!grepl("^s....$", .)] %>%
  .[!grepl("^l....$", .)] %>%
# 2nd letter in/out----
#.[!grepl("^.i...$", .)] %>%
.[grepl("^.a...$", .)] %>%
# 3rd letter in/out----
.[!grepl("^..l..$", .)] %>%
#.[grepl("^..n..$", .)] %>%
# 4th letter in/out----
.[!grepl("^...s.$", .)] %>%
.[!grepl("^...a.$", .)] %>% 
#.[!grepl("^...n.$", .)] %>%
# last letter in/out----
#.[grepl("^....y$", .)] %>%
.[!grepl("^....e$", .)] %>% sample(., 1)
# do some more stuff----
strsplit(., "") %>%
  unlist() %>%
  table() %>%
  sort()


# tie breaker
var1 <- c("brown", "drown", "crown", "frown")

tb <- sample(c(var1), 
             size = 10001, replace = T) %>% 
  table() %>%
  prop.table(); names(which(tb == max(tb)))
