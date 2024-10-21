

#gen_sudoku

library(renv)
library(dplyr)
library(Matrix)

renv::snapshot()
renv::status()


sudok.blank <- Matrix(data = rep(0, 9*9), 
                      nrow = 9, ncol = 9, 
                      dimnames = list(row = paste("r",1:9, sep = ""), 
                                      col = paste("c",1:9, sep = "")))


#sudok.blank[1:81] <- 1:81


