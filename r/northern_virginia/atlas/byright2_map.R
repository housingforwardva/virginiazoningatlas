library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)

byright2 <- read_sf("data/nova/geo/nova_vza_geo.geojson") |> 
  ms_simplify(keep = 0.1) |> 
  filter(overlay == 0) |> 
  filter(family2_treatment == "allowed" & family3_treatment == "allowed" & family4_treatment == "allowed")

byright2plus <- read_sf("data/nova/geo/nova_vza_geo.geojson") |> 
  filter(overlay == 0) |> 
  filter(family2_treatment == "allowed" & family3_treatment == "allowed" & family4_treatment == "allowed")

st_write(byright2plus, "data/nova/geo/nova_byright2plus.geojson", append = FALSE)


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("Primarily Residential", "Mixed with Residential"))

byr_map <- leaflet(byright2) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.9,
              popup = paste0("Jurisdiction: ",
                             byright2$jurisdiction,
                             "<br>",
                             "District: ",
                             byright2$abbrvname)) |> 
  addLegend(data = byright2,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") 


# byr_map

write_rds(byr_map, "data/nova/rds/byr_map.rds")
