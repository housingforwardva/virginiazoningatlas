library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)


hr_byright4 <- geojson_sf("data/hr/hr_vza_geo.geojson") |> 
  filter(family4_treatment == "allowed") |> 
  select(abbrvname, type, jurisdiction, county, family1_treatment, family2_treatment, family3_treatment, family4_treatment)


hr_byright4 <- ms_simplify(hr_byright4, keep = 0.3)


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),
                   levels = c("Primarily Residential", "Mixed with Residential"))

hr_byr4_map <- leaflet(hr_byright4) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.9,
              popup = paste0("Jurisdiction: ",
                             hr_byright4$hr_custom_fields_jurisdiction,
                             "<br>",
                             "District: ",
                             hr_byright4$abbrvname)) |> 
  addLegend(data = hr_byright4,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") 


hr_byr4_map

write_rds(hr_byr4_map, "data/hr/hr_byr4_map.rds")
