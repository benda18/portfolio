library(renv)
library(dplyr)
library(readr)
library(curl)

renv::snapshot()
renv::status()

rm(list=ls());cat('\f')
gc()

readr::read_csv("https://raw.githubusercontent.com/gwstaten/wordl/main/wordlists/nytimes")
readr::read_lines("https://raw.githubusercontent.com/gwstaten/wordl/main/wordlists/nytimes")
