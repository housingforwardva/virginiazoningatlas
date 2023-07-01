library(tidyverse)
library(leaflet)
library(sf)
library(geojsonsf)

district_bounds <- geojson_sf("data/gloucester_county_zoning_final.geojson")

district_data <- read_csv("data/GloucesterGloucesterCountyVA-ZoningDistricts.csv")

districts <- district_bounds |> 
  left_join(district_data, by = "Abbreviated District Name")


leaflet() |> 
  addProviderTiles("OpenStreetMap") |> 
  addPolygons(data = districts, color = ~pal("Abbreivated District Name"))
  

