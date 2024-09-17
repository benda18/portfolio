library(renv)
library(dplyr)
library(readr)
library(curl)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f')
gc()

# explore----
nyt    <- readr::read_lines("https://raw.githubusercontent.com/gwstaten/wordl/main/wordlists/nytimes")
l.nyt  <- strsplit(nyt, "")
df.nyt <- data.frame(word = nyt, 
                     l1 = unlist(lapply(X = l.nyt, 
                                        FUN = nth, 
                                        n   = 1)), 
                     l2 = unlist(lapply(X = l.nyt, 
                                        FUN = nth, 
                                        n   = 2)), 
                     l3 = unlist(lapply(X = l.nyt, 
                                        FUN = nth, 
                                        n   = 3)), 
                     l4 = unlist(lapply(X = l.nyt, 
                                        FUN = nth, 
                                        n   = 4)), 
                     l5 = unlist(lapply(X = l.nyt, 
                                        FUN = nth, 
                                        n   = 5))) %>% as_tibble()

rm(l.nyt, nyt)

# game----
correct_letters <- c("e", "t", "u")

l1 <- list(correct = NA, 
           impossible = c("r", "q", "p"))
l2 <- list(correct = NA, 
           impossible = c("i", "u", "o"))
l3 <- list(correct = NA, 
           impossible = c("p", "e", "k"))
l4 <- list(correct = NA, 
           impossible = c("e", "s"))
l5 <- list(correct = "t", 
           impossible = c("n", "r"))


# df.nyt[grepl(l1$correct, x = df.nyt$l1) & 
#          !grepl(paste(l1$impossible,sep = "|", collapse = "|"), x = df.nyt$l1),]
# df.nyt[grepl(l2$correct, x = df.nyt$l2),]
# df.nyt[grepl(l3$correct, x = df.nyt$l3),]
# df.nyt[grepl(l4$correct, x = df.nyt$l4),]
# df.nyt[grepl(l5$correct, x = df.nyt$l5) & 
#          !grepl(paste(l5$impossible,sep = "|", collapse = "|"), x = df.nyt$l5),]
# 
# df.nyt[!grepl(paste(l1$impossible,sep = "|", collapse = "|"), x = df.nyt$l1),]
# df.nyt[!grepl(l2$impossible, x = df.nyt$l2),]
# df.nyt[!grepl(l3$impossible, x = df.nyt$l3),]
# df.nyt[!grepl(l4$impossible, x = df.nyt$l4),]
# df.nyt[!grepl(l5$impossible, x = df.nyt$l5),]


# reduce list down to words containing all correct letters
dfpos <- df.nyt[grepl(paste(correct_letters, sep = "|", collapse = "|"), 
             x = df.nyt$word),]

# for solved positions include words that match 'possible' 
if(!is.na(l1$correct)){
  dfpos <- dfpos[dfpos$l1 == l1$correct,]
}
if(!is.na(l2$correct)){
  dfpos <- dfpos[dfpos$l2 == l2$correct,]
}
if(!is.na(l3$correct)){
  dfpos <- dfpos[dfpos$l3 == l3$correct,]
}
if(!is.na(l4$correct)){
  dfpos <- dfpos[dfpos$l4 == l4$correct,]
}
if(!is.na(l5$correct)){
  dfpos <- dfpos[dfpos$l5 == l5$correct,]
}

# for all positions include words that match any 



dfpos[]


# for unsolved positions remove words that match 'impossible'
if(is.na(l1$correct)){
  dfpos <- dfpos[!grepl(pattern = paste(l1$impossible, sep = "|", collapse = "|"), 
                        x = dfpos$word),]
}
if(is.na(l2$correct)){
  dfpos <- dfpos[!grepl(pattern = paste(l2$impossible, sep = "|", collapse = "|"), 
                        x = dfpos$word),]
}
if(is.na(l3$correct)){
  dfpos <- dfpos[!grepl(pattern = paste(l3$impossible, sep = "|", collapse = "|"), 
                        x = dfpos$word),]
}
if(is.na(l4$correct)){
  dfpos <- dfpos[!grepl(pattern = paste(l4$impossible, sep = "|", collapse = "|"), 
                        x = dfpos$word),]
}
if(is.na(l5$correct)){
  dfpos <- dfpos[!grepl(pattern = paste(l5$impossible, sep = "|", collapse = "|"), 
                        x = dfpos$word),]
}

dfpos
