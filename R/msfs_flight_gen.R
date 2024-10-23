# msfs flight generator

library(renv)
library(dplyr)
library(ggplot2)
library(readr)

status()
snapshot()

rm(list=ls())
# BANJOFIDDLE
sample(c(letters,LETTERS,0:9,"!"), size = 13) %>% paste(., sep = "", collapse = "")

# web resources----

# hand-crafted airports by software edition:
# https://flight.fandom.com/wiki/Microsoft_Flight_Simulator_(2020)/List_of_hand-crafted_airports


# airports     <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv")
# cw_countries <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/countries.csv")
# cw_regions   <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/regions.csv")
# cw_runways   <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/runways.csv")
# cw_freq      <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airport-frequencies.csv")
# cw_navaids   <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/navaids.csv")
# 
# apl <- list(airports,
#             cw_countries,
#             cw_regions,
#             cw_runways,
#             cw_freq,
#             cw_navaids)
# 
# names(apl) <- c("airports", 
#                 "cw_countries",
#                 "cw_regions",
#                 "cw_runways",
#                 "cw_freq",
#                 "cw_navaids")
# 
# saveRDS(object = apl,
#         file = "airport_list.rds")


airportlist <- base::readRDS(file = "airport_list.rds")

names(airportlist)
airportlist$airports

