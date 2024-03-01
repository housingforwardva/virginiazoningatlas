library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)

hr_adu <- geojson_sf("data/hr/hr_vza_geo.geojson") |> 
  filter(accessory_treatment == "allowed") |> 
  select(abbrvname, type, jurisdiction, county)

hr_adu <- ms_simplify(hr_adu, keep = 0.3)


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0", "#011E41"),
                   levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"))

hr_adu_map <- leaflet(hr_adu) |> 
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.65,
              popup = paste0("Jurisdiction: ",
                             hr_adu$jurisdiction,
                             "<br>",
                             "District: ",
                             hr_adu$abbrvname)) |> 
  addLegend(data = hr_adu,
            labels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
            colors = c("#8B85CA", "#40C0C0", "#011E41"),
            title = "Type of Zoning District") 


hr_adu_map

write_rds(hr_adu_map, "data/hr/hr_adu_map.rds")
