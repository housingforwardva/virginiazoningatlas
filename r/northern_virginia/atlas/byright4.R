library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)


nova_byright4 <- st_read("data/nova/nova_byright4.geojson")

nova_byright4 <- ms_simplify(nova_byright4, keep = 0.3)


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("R", "M"))

nova_byr4_map <- leaflet(nova_byright4) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.9,
              popup = paste0("Jurisdiction: ",
                             nova_byright4$nova_custom_fields_jurisdiction,
                             "<br>",
                             "District: ",
                             nova_byright4$abbrvname)) |> 
  addLegend(data = nova_byright4,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") 


nova_byr4_map

write_rds(nova_byr4_map, "data/nova/nova_byr4_map.rds")
