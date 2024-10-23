# msfs flight generator

library(renv)
library(dplyr)
library(ggplot2)
library(readr)

status()
snapshot()

rm(list=ls())

# # web resources----
# 
# # hand-crafted airports by software edition:
# # https://flight.fandom.com/wiki/Microsoft_Flight_Simulator_(2020)/List_of_hand-crafted_airports
# 
# 
# airports     <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv")
# airports$continent[is.na(airports$continent)] <- "NA"
# 
# cw_countries <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/countries.csv")
# cw_countries$continent[is.na(cw_countries$continent)] <- "NA"
# 
# cw_regions   <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/regions.csv")
# cw_regions$continent[is.na(cw_regions$continent)] <- "NA"
# 
# cw_runways   <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/runways.csv")
# 
# cw_freq      <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airport-frequencies.csv")
# 
# cw_navaids   <- read_csv("https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/navaids.csv")
# 
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
# if(getwd() != "C:/Users/bende/Documents/R/play/portfolio/data"){
#   setwd("C:/Users/bende/Documents/R/play/portfolio/data")
# }
# 
# saveRDS(object = apl,
#         file = "airport_list.rds")

rm(list=ls());cat('\f')
airportlist <- base::readRDS(file = "airport_list.rds")

names(airportlist)
airp    <- airportlist$airports %>% .["wikipedia_link" != names(.)]
cw_coun <- airportlist$cw_countries %>% .["wikipedia_link" != names(.)]
cw_reg  <- airportlist$cw_regions %>% .["wikipedia_link" != names(.)]
cw_runw <- airportlist$cw_runways %>% .["wikipedia_link" != names(.)]
cw_nava <- airportlist$cw_navaids %>% .["wikipedia_link" != names(.)]
cw_freq <- airportlist$cw_freq %>% .["wikipedia_link" != names(.)]

df_all <- left_join(airp, cw_runw, by = c("ident" = "airport_ident")) 

