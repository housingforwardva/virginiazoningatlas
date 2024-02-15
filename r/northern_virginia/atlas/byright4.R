library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)


byright4 <- read_sf("data/nova/nova_vza_geo.geojson") |> 
  ms_simplify(keep = 0.1) |> 
  filter(overlay == 0) |> 
  filter(family4_treatment == "allowed")

byright4plus <- read_sf("data/nova/nova_vza_geo.geojson") |> 
  filter(overlay == 0) |> 
  filter(family4_treatment == "allowed")

st_write(byright4plus, "data/nova/nova_byright4plus.geojson")



pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("Primarily Residential", "Mixed with Residential"))

byr4_map <- leaflet(byright4) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.9,
              popup = paste0("Jurisdiction: ",
                             byright4$nova_custom_fields_jurisdiction,
                             "<br>",
                             "District: ",
                             byright4$abbrvname)) |> 
  addLegend(data = byright4,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") 


byr4_map

write_rds(byr4_map, "data/nova/byr4_map.rds")
