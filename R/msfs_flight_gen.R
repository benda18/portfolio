# msfs flight generator

library(renv)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(terra)
library(geosphere)

status()
snapshot(exclude = "rgdal")

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

# functions----
me2mi <- function(m){
  c(mi = m * 0.000621371)
}
# data----

airportlist <- base::readRDS(file = "data/airport_list.rds")

names(airportlist)
airp    <- airportlist$airports %>% .["wikipedia_link" != names(.)]
cw_coun <- airportlist$cw_countries %>% .["wikipedia_link" != names(.)]
cw_reg  <- airportlist$cw_regions %>% .["wikipedia_link" != names(.)]
cw_runw <- airportlist$cw_runways %>% .["wikipedia_link" != names(.)]
cw_nava <- airportlist$cw_navaids %>% .["wikipedia_link" != names(.)]
cw_freq <- airportlist$cw_freq %>% .["wikipedia_link" != names(.)]

df_all <- left_join(airp, cw_runw, by = c("ident" = "airport_ident")) 

df_all.open <- df_all[df_all$type != "closed",]

# df_all.open$type %>% unique()
# 
# df_all.open$name[which(df_all.open$ident == "KLAX")]
# 
# df_all.open %>%
#   #.[grepl(pattern = "^K.[3,3]$", x = .$ident),] %>%
#   .[grepl(pattern = "^K", x = .$ident),] %>%
#   group_by(ident, local_code, gps_code, name) %>%
#   summarise()

# remove columns unneeded----
colnames(df_all.open) %>% sort()
View(df_all.open)

df_all.open <- df_all.open[!colnames(df_all.open) %in% "home_link"]

df_all.open %>%
  group_by(latitude_deg) %>%
  summarise(n = n()) %>%
  .$n %>%
  table()

# keep ident, latitude_deg, longitude_deg

df_all.open[grepl("^NC", x = df_all.open$ident, ignore.case = T),]


head(as.data.frame(df_all.open))
tail(as.data.frame(df_all.open))


# narrow down to certain airports----

dep_apt <- "KSEZ"


dep_apt <- df_all.open[df_all.open$ident == dep_apt,]


arr_apt <- df_all.open[df_all.open$iso_country == "US" & 
                         grepl("^K", df_all.open$ident),] %>%
  .[sample(1:nrow(.), size = 10, replace = F),]



df_out <- NULL
for(i in 1:nrow(arr_apt)){
  
  df_out <- rbind(df_out, 
                  data.frame(depapt = dep_apt$ident, 
                             arrapt = arr_apt$ident[i], 
                             dist_m = geosphere::distHaversine(p1 = c(dep_apt$longitude_deg, 
                                                                      dep_apt$latitude_deg), 
                                                               p2 = c(arr_apt$longitude_deg[i], 
                                                                      arr_apt$latitude_deg[i])))) 
}

df_out$dist_mi <- me2mi(df_out$dist_m)

