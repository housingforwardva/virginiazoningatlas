library(tidyverse)
library(sf)
library(leaflet)

transit <- st_read("data/hr/hr_transit.geojson")
transit_buff <- st_read("data/hr/hr_qmile_transitbuffer.geojson")
hr_byright2 <- st_read("data/hr/hr_byright2.geojson")

pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("R", "M"))

leaflet(hr_byright2) |> 
  addTiles() |> 
  addPolygons(weight = 1,
             color = ~pal(type),
             fillColor = ~pal(type),
             fillOpacity = 0.9) |> 
  addCircleMarkers(data = transit,
              color = "#011E41",
              radius = 0.15) |> 
  addPolygons(data = transit_buff,
              color = "white",
              weight = 0.5,
              fillColor = "grey",
              fillOpacity = 0.3)
