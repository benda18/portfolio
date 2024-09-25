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

# build dataset----
nyt    <- lexicon::grady_augmented 

# 5 letter words only
nyt <- nyt[nchar(nyt) == 5]

# explore----
wordl.fin <- F

solution <- "" #sample(nyt,size=1)

#while(wordl.fin == F){
  guess          <- sample(nyt, size = 1);cat(guess_sol(guess, solution))
  
  nyt[grepl(pattern = "[h]", nyt) & 
    grepl(pattern = "[t]", nyt) & 
      grepl(pattern = "[r]", nyt) &
      !grepl(pattern = "[aunew]", nyt)] %>%
    sample(., size = 1)
  
  guess.outcomes <- list("rl" = unlist(strsplit(x = c("htr"), 
                                                split = "")), 
                         "wl" = unlist(strsplit(x = c("aunew"), 
                                                  split = "")))
  
  # not ltrs
  #not.ltrs <- guess.outcomes$wl
  
  # yes ltrs
  l1 <- "t"
  l2 <- NA
  l3 <- "r"
  l4 <- NA
  l5 <- NA
  
 # wordl.fin <- T
#}

  # compare guess to solution (letter & placement)

gue.lp <- unlist(strsplit(x = guess, split = ""))[unlist(strsplit(x = guess, split = "")) == 
                     unlist(strsplit(x = solution, split = ""))]
gue.lp <- ifelse(length(gue.lp) == 0, NA, gue.lp)

# compare guess to solution (letter only)
gue.lo <- guess.outcomes$rl


# new list----
# grep(pattern = "[ht]", x = c("haunt", "theme", "testy", "zebra", "phone"), 
#      value = T)
# grepl(pattern = "[h]", x = c("haunt", "theme", "testy", "zebra", "phone")) & 
#   grepl(pattern = "[t]", x = c("haunt", "theme", "testy", "zebra", "phone"))
# 
# 
# lapply(X = list("[h]", "[t]"), 
#        FUN = grepl, 
#        x = c("haunt", "theme", "testy", "zebra", "phone")) 

nyt <- nyt %>%
  .[!grepl(pattern = paste(guess.outcomes$wl, 
                           sep = "|", collapse = "|"), x = .)] %>% # remove words with not-permitted letters
  .[grepl(pattern = paste(gue.lo, sep = "|", collapse = "|"),
         x = .)] %>%
  .[grepl(pattern = glue("[{ifelse(is.na(l1), paste(letters, sep = \"\", collapse = \"\"), l1)}][{ifelse(is.na(l2), paste(letters, sep = \"\", collapse = \"\"), l2)}][{ifelse(is.na(l3), paste(letters, sep = \"\", collapse = \"\"), l3)}][{ifelse(is.na(l4), paste(letters, sep = \"\", collapse = \"\"), l4)}][{ifelse(is.na(l5), paste(letters, sep = \"\", collapse = \"\"), l5)}]"), 
          x = .)]
 
if(length(guess.outcomes$rl) == 5){
  nyt <- nyt[(lapply(FUN = `%in%`, 
              strsplit(nyt,""),
              as.list(c(guess.outcomes$rl))) %>%
         lapply(., all) %>%
         unlist() %>%
         which())]
}

nyt





# sample(c(letters,LETTERS,0:9), size = 12, replace = T) %>%
#   paste(., sep = "", collapse = "")

# "2ltrCdOD8FSs"