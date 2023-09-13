library(tidyverse)
library(janitor)
library(leaflet)
library(sf)

district_bounds <- st_read("data/GloucesterGloucesterCountyVA-ZoningDistricts.geojson", quiet = TRUE) |> 
  clean_names()

district_data <- read_csv("data/GloucesterGloucesterCountyVA-ZoningDistricts.csv")

pal <- colorFactor(palette = c("#40C0C0", "#A29DD4", "#999999"),
                   levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"))

leaflet(district_bounds |> 
          filter(x1_family_treatment == "Allowed/Conditional" | x2_family_treatment == "Allowed/Conditional" | x3_family_treatment == "Allowed/Conditional" | x4_family_treatment == "Allowed/Conditional")) |> 
  addProviderTiles("CartoDB.Positron") |> 
  addPolygons(color = "#ffffff", weight = 1, smoothFactor = 0.5,
              fillColor = ~pal(type_of_zoning_district),
              fillOpacity = 0.8)
  

