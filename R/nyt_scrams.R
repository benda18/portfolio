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
