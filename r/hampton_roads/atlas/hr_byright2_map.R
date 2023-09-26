library(tidyverse)
library(sf)
library(leaflet)

hr_byright2 <- st_read("data/hr/hr_byright2.geojson")


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("R", "M"))

hr_byr_map <- leaflet(hr_byright2) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.9,
              popup = paste0("Jurisdiction: ",
                             hr_byright2$jurisdiction,
                             "<br>",
                             "District: ",
                             hr_byright2$abbrvname)) |> 
  addLegend(data = hr_byright2,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") 


hr_byr_map

write_rds(hr_byr_map, "data/hr/hr_byr_map.rds")
