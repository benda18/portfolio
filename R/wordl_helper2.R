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

g1 <- "stare"
guess_sol(g1,sol) %>% cat()

wrdl <- wrdl %>%
  .[grepl("^s....$", .)] %>%
  .[!grepl("^.t...$", .)] %>%
  .[!grepl("^....e$", .)] %>%
  .[!grepl("a", .)] %>%
  .[!grepl("r", .)] %>%
  .[grepl("t", .)] %>%
  .[grepl("e", .)] 
  


temp.l <- wrdl %>%
  grep("^s....$", ., value = T) %>%
  #grep("^.t...$", ., value = T) %>%
  #grep("^..a..$", ., value = T) %>%
  #grep("^...r.$", ., value = T) %>%
  #grep("^....e$", ., value = T) %>%
  strsplit(., "")

cat('\f')

randmode <- function(v){
  require(dplyr)
  v <- v %>% table() %>% as.data.frame() %>% tibble()
  colnames(v) <- c("ltr", "freq")
  v <- v[order(v$freq, decreasing = T),] 
  
  return(as.character(sample(slice_max(v, order_by = freq, n = 1, with_ties = T)$ltr, 
         size = 1)))
}

randmode(unlist(lapply(temp.l, nth, 1)))
randmode(unlist(lapply(temp.l, nth, 2)))
randmode(unlist(lapply(temp.l, nth, 3)))
randmode(unlist(lapply(temp.l, nth, 4)))
randmode(unlist(lapply(temp.l, nth, 5)))

