library(renv)
library(dplyr)
library(jsonlite)
# library(devtools)
# devtools::install_github('ewenme/discogger')
library(discogger)

renv::snapshot()
renv::status()
# https://colinfay.me/data-vinyles-discogs-r/

collection_complete <- jsonlite::fromJSON(txt = "http://colinfay.me/data/collection_complete.json", 
                                          simplifyDataFrame = TRUE)