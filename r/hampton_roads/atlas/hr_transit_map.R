library(tidyverse)
library(sf)
library(leaflet)

transit <- st_read("data/hr/hr_transit.geojson")
transit_buff <- st_read("data/hr/hr_qmile_transitbuffer.geojson")
hr_byright2 <- st_read("data/hr/hr_byright2.geojson")
hr_region <- st_read("data/hr/hr_region.geojson")

pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("R", "M"))

hr_transit_map <- leaflet(hr_byright2) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(weight = 1,
             color = ~pal(type),
             fillColor = ~pal(type),
             fillOpacity = 0.9) |> 
  addCircleMarkers(data = transit,
              color = "#011E41",
              radius = 0.15,
              fillOpacity = 1,
              group = "HRT Transit Stop") |> 
  addPolygons(data = transit_buff,
              color = "white",
              weight = 0.5,
              fillColor = "grey",
              fillOpacity = 0.10,
              group = "1/4 mile buffer") |> 
  addLegend(data = hr_byright2,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") |> 
  addLegend(data = transit,
            labels = "HRT Transit Stop",
            color = "#011E41",
            group = "HRT Bus Stop") |> 
  addLegend(data = transit_buff,
            labels = "1/4 mile buffer",
            color = "grey",
            group = "1/4 mile buffer") |> 
  # addPolygons(data = hr_region,
  #             color = "#011E41",
  #             weight = 1, 
  #             fillOpacity = 0) |> 
  setView(lat = 36.847639, 
          lng = -75.988382,
          zoom = 12) |> 
  # Layers control
  addLayersControl(
    overlayGroups = c("HRT Bus Stop", "1/4 mile buffer"),
    options = layersControlOptions(collapsed = FALSE)
  )

hr_transit_map

write_rds(hr_transit_map, "data/hr/hr_transit_map.rds")
