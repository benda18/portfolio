library(dplyr)
library(renv)
library(leaflet)
library(tidycensus)
library(tigris)
library(geosphere)
library(terra)
library(ggplot2)

renv::snapshot(exclude = "rgdal")

rm(list=ls());cat('\f')

county_sub <- tigris::county_subdivisions(state = c("OH"), cb = T)

?geosphere::centroid()
?terra::centroids()
?vect
cs_centroid <- vect(county_sub$geometry) %>% centroids()


View(cs_centroid
     )

as.vector(cs_centroid)

View(county_sub)
county_sub$geometry[1]

ggplot() + 
  geom_sf(data = county_sub)
