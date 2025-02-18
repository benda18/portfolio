4library(renv)
library(dplyr)
library(lexicon)
#library(igraph)
#library(Matrix)
library(data.table)

rm(list=ls());cat('\f')

guess.word <- sample(x = lexicon::sw_fry_1000, size = 1) #  hand, found, include, began, certain

the.theme <- "flour power"
the.ltrs <- "roaihcnceceemopkrtlnenfaadtheigluwopuckecabricat"

# make matrix----
# error check data entry - nchars
pzl.cols <- 6
pzl.rows <- 8
stopifnot(nchar(the.ltrs) == pzl.cols * pzl.rows)
pzl.matrix <- matrix(data = unlist(strsplit(the.ltrs, "")), 
                     nrow = pzl.rows, 
                     ncol = pzl.cols, 
                     byrow = T)

# is word possible based on input letters

stopifnot(all(guess.word %>%
                strsplit(., "") %>%
                unlist() %in% 
                unlist(strsplit(the.ltrs, ""))))

guess.word <- "pond"

pzl.matrix 

# search for word----
fun_search <- function(wrd = guess.word){
  wrd <- unlist(strsplit(wrd,""))
  
  for(ltr in 1:length(wrd)){
    
    for(ir in 1:nrow(pzl.matrix)){
      #print("------------ROW------------")
      for(ic in 1:ncol(pzl.matrix)){
        #print("----------COLUMN------------")
        print(paste(ir,ic,sep = ",", collapse = ","))
        # from_ltr 
        
        # if the letter matches the matrix index
        if(wrd[ltr] == pzl.matrix[ir,ic]){
          # check around the letter to find the next letter
        
          if(ir > 1){
            # 12:00
            if(wrd[ltr+1] == pzl.matrix[ir-1,ic]){ 
              print("true - do something")
              }
          }
          
          if(ir > 1 & ic < 6){
            # 1:30
            if(wrd[ltr+1] == pzl.matrix[ir-1,ic+1]){ 
              print("true - do something")
            }
          }
          
          if(ic < 6){
            # 3:00
            if(wrd[ltr+1] == pzl.matrix[ir,ic+1]){ 
              print("true - do something")
            }
          }
          
          if(ir < 8 & ic < 6){
            # 4:30
            if(wrd[ltr+1] == pzl.matrix[ir+1,ic+1]){ 
              print("true - do something")
            }
          }
          
          if(ir < 8){
            # 6:00
            if(wrd[ltr+1] == pzl.matrix[ir+1,ic]){ 
              print("true - do something")
            }
          }
          
          if(ir < 8 & ic > 1){
            # 7:30
            if(wrd[ltr+1] == pzl.matrix[ir,ic-1]){ 
              print("true - do something")
            }
          }
          
          if(ic > 1){
            # 9:00
            if(wrd[ltr+1] == pzl.matrix[ir-1,ic-1]){ 
              print("true - do something")
            }
          }
          
          if(ic > 1 & ir > 1-1){
            # 10:30
            if(wrd[ltr+1] == pzl.matrix[ir,ic]){ 
              print("true - do something")
            }
          }
          }
        
        
        
        
      }
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
