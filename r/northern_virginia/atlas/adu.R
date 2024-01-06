library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)

nova_adu <- st_read("data/nova/nova_adu.geojson")

nova_adu <- ms_simplify(nova_adu, keep = 0.3)


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0", "#011E41"),levels = c("R", "M", "X"))

nova_adu_map <- leaflet(nova_adu) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.65,
              popup = paste0("Jurisdiction: ",
                             nova_adu$jurisdiction,
                             "<br>",
                             "District: ",
                             nova_adu$abbrvname)) |> 
  addLegend(data = nova_adu,
            labels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
            colors = c("#8B85CA", "#40C0C0", "#011E41"),
            title = "Type of Zoning District") 


nova_adu_map

write_rds(nova_adu_map, "data/nova/nova_adu_map.rds")
