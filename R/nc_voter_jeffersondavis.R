library(renv)
library(dplyr)
library(pdftools)
library(leaflet)
library(readr)


status()



rm(list=ls());cat('\f')
gc()

the.pdf <- "C:/Users/bende/Documents/R/play/portfolio/data/alleged_voter_names.pdf"

file_info <- pdf_info(the.pdf)

the.data <- pdf_text(pdf = the.pdf)


which.page <- sample(1:1800, size = 1)

the.data[[which.page]] 

# read_fwf(file = the.data[[which.page]] , 
#          col_positions = fwf_widths(c(10,34,100), 
#                                     col_names = c("county", "voter_name", "zip")))


out.list <- list()
for(i in 1:length(the.data)){
  out.list[[i]] <- read_fwf(file = the.data[[i]] , 
                            col_positions = fwf_empty(the.data[[i]]))
  
}
