library(renv)
library(dplyr)
library(readr)
library(curl)
#library(lexicon)
library(glue)
library(crayon)
library(data.table)

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

df_w <- tibble(wrd = wrdl) %>%
  mutate(., 
         l1 = substr(wrd, 1, 1),
         l2 = substr(wrd, 2, 2), 
         l3 = substr(wrd, 3, 3), 
         l4 = substr(wrd, 4, 4), 
         l5 = substr(wrd, 5, 5))

df_w



?data.table::melt()

dfw_summary <- melt(as.data.table(df_w), 
     id.vars = c("wrd"), 
     variable.name = "ltr_pos", 
     value.name = "ltr_val") %>%
  #as_tibble() %>%
  dcast(., 
        wrd~ltr_val) %>%
  as_tibble()

melt(as.data.table(df_w), 
     id.vars = c("wrd"), 
     variable.name = "ltr_pos", 
     value.name = "ltr_val") %>%
  as_tibble() %>%
  group_by(wrd,ltr_val) %>%
  summarise(n = n())

dfw_summary %>%
  as.data.table() %>%
  melt(., 
       id.vars = "wrd") %>%
  as_tibble() %>%
  group_by(variable) %>%
  summarise(n = n(), 
            n1 = sum(value > 0),
            avg_val = mean(value)) %>%
  .[order(.$avg_val, decreasing = T),]

