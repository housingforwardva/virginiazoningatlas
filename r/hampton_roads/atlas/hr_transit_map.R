library(tidyverse)
library(sf)
library(leaflet)

transit <- st_read("data/hr/hr_transit_all.geojson")
transit_buff <- st_read("data/hr/hr_qrtr_buffers.geojson")
hr_byright2 <- st_read("data/hr/hr_byright2.geojson")
hr_region <- st_read("data/hr/hr_region.geojson")

pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("R", "M"))
transit_pal <-colorFactor(palette = c("#B1005F", "#E0592A", "#FFC658"), levels = c("WATA", "Hampton Roads Transit", "Suffolk Transit"))

hr_transit_map <- leaflet(hr_byright2) |> 
  addPolygons(weight = 1,
             color = ~pal(type),
             fillColor = ~pal(type),
             fillOpacity = 0.9) |> 
  addCircleMarkers(data = transit,
              color = ~transit_pal(service),
              radius = 0.15,
              fillOpacity = 1,
              fillColor = ~transit_pal(service),
              group = "Transit Stops") |> 
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
            labels = c("WATA", "Hampton Roads Transit", "Suffolk Transit"),
            color = c("#B1005F", "#E0592A", "#FFC658"),
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
    overlayGroups = c("Transit Stops", "1/4 mile buffer"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

hr_transit_map



write_rds(hr_transit_map, "data/hr/hr_transit_map.rds")

