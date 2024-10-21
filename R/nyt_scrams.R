library(dplyr)
library(renv)
library(lexicon)
library(Matrix)
library(crayon)
library(igraph)
library(renv)

snapshot()
status()

rm(list=ls());cat('\f')

# vars----
scram <- "schhanircuskbelkinlefotsfaicuenrskrykrbeieoundtb"
nyt   <- lexicon::grady_augmented
nyt   <- lexicon::sw_fry_1000

# cleanup----
scram <- scram %>%
  strsplit(., "") %>%
  unlist() %>%
  matrix(., ncol = 6, byrow = T)

c(1:48) %>%
  # strsplit(., "") %>%
  # unlist() %>%
  matrix(., ncol = 6, byrow = T)

# make graph

mgraph <- graph_from_literal(1-+2, 1-+8, 1-+7, 
                             2-+1, 2-+3, 2-+9, 2-+8, 2-+7,
                             3-+4, 3-+10, 3-+9, 3-+8, 3-+2,
                             4-+5, 4-+11, 4-+10, 4-+9, 4-+3,
                             5-+6, 5-+12, 5-+11, 5-+10, 5-+4,
                             6-+12, 6-+11, 6-+5,
                             7-+1,7-+2,7-+8,7-+14,7-+13,
                             8-+2, 8-+3, 8-+9, 8-+15, 8-+14, 8-+13, 8-+7, 8-+1, 
                             9-+3, 9-+4, 9-+10, 9-+16, 9-+15, 9-+14, 9-+8, 9-+2,
                             10-+4,10-+5,10-+11,10-+17,10-+16,10-+15,10-+9,10-+3,
                             11-+5,11-+6,11-+12,11-+18,11-+17,11-+16,11-+10,11-+4,
                             simplify = T) 

plot(mgraph)

is_directed(mgraph)

# analysis----

guess.word <- "sink"

which(scram == "s")

get_n <- function(which.origin = 9,
                  mat = scram){
  ifelse(which.origin > 6, 
         mat[which.origin-6], 
         NA)
  
}
lapply(1:48, get_n) %>% unlist()

get_ne <- function(which.origin = 1){
  
}
get_e <- function(which.origin = 1){
  
}
get_se <- function(which.origin = 1){
  
}
get_s <- function(which.origin = 1){
  
}
get_sw <- function(which.origin = 1){
  
}
get_w <- function(which.origin = 1){
  
}
get_nw <- function(which.origin = 1){
  
}


matrix(1:48, ncol = 6, byrow = T)
