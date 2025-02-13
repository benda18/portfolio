
library(renv)
library(dplyr)
library(rgdal)
library(leaflet)
library(htmlwidgets)

renv::status()
renv::snapshot()


rm(list=ls());cat('\f')

m <- leaflet() %>%
  addTiles()


lony  <- -79
latx  <-  36
zoomx <-   9 

m


setView(map = m, 
        lng = lony, lat = latx, 
        zoom = zoomx, 
        options = list())

flyTo(map = m, 
      lng = lony, 
      lat = latx, 
      zoom = zoomx, 
      options = list())

m

lon_min <- lony - .1
lon_max <- lony + .1
lat_min <- latx - .1
lat_max <- latx + .1

fitBounds(map = m, 
          lng1 = lon_min, 
          lng2 = lon_max, 
          lat1 = lat_min, 
          lat2 = lat_max)

m <- flyToBounds(map = m, 
          lng1 = lon_min, 
          lng2 = lon_max, 
          lat1 = lat_min, 
          lat2 = lat_max)

m <- clearBounds(map = m)

m

