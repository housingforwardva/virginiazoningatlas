library(tidyverse)
library(leaflet)
library(sf)
library(geojsonsf)

district_bounds <- st_read("data/GloucesterGloucesterCountyVA-ZoningDistricts.geojson")

district_data <- read_csv("data/GloucesterGloucesterCountyVA-ZoningDistricts.csv")


leaflet() |> 
  addProviderTiles("OpenStreetMap") |> 
  addPolygons(data = district_bounds)
  

