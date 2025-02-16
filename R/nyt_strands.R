library(renv)
library(dplyr)
library(lexicon)
#library(igraph)
#library(Matrix)
library(data.table)

rm(list=ls());cat('\f')


the.theme <- "flour power"
the.ltrs <- "roaihcnceceemopkrtlnenfaadtheigluwopuckecabricat"

pzl.cols <- 6
pzl.rows <- 8

nyt <- lexicon::grady_augmented

# process letters-----

# error check data entry - nchars
stopifnot(nchar(the.ltrs) == pzl.cols * pzl.rows)

# make matrix

pzl.matrix <- matrix(data = unlist(strsplit(the.ltrs, "")), 
                     nrow = pzl.rows, 
                     ncol = pzl.cols, 
                     byrow = T)


pzl.matrix 

for(ir in 1:nrow(pzl.matrix)){
  print("------------ROW------------")
  for(ic in 1:ncol(pzl.matrix)){
    print("----------COLUMN------------")
    pzl.matrix[ir,ic]
    
    if(ir > 1){
      # 12:00
      print("12:00")
    }
    
    if(ir > 1 & ic < 6){
      # 1:30
      print("1:30")
    }
    
    if(ic < 6){
      # 3:00
      print("3:00")
    }
    
    if(ir < 8 & ic < 6){
      # 4:30
      print("4:30")
    }
    
    if(ir < 8){
      # 6:00
      print("6:00")
    }
    
    if(ir < 8 & ic > 1){
      # 7:30
      print("7:30")
    }
    
    if(ic > 1){
      # 9:00
      print("9:00")
    }
    
    if(ic > 1 & ir > 1){
      # 10:30
      print("10:30")
    }
    
    
  }
}

# pzl.df <- pzl.matrix %>%
#   as.data.table() %>%
#   mutate(., 
#          y = 1:nrow(.)) %>%
#   melt(., 
#        id.vars = "y", 
#        variable.name = "x") %>%
#   mutate(., 
#          x = as.numeric(gsub("V", "", x))) %>%
#   as.data.frame()
# 
# 
# pzl.df %>% plot()


igraph::graph_from_data_frame(pzl.df, directed = F) %>% plot()
