library(tidyverse)
library(sf)
library(leaflet)

# https://www.arcgis.com/apps/mapviewer/index.html?layers=11d63c5455c8462ca63fc490061900b2
# https://opendata.dc.gov/datasets/e3896b58a4e741d48ddcda03dae9d21b_51/explore?location=38.869191%2C-77.025953%2C11.17
# https://hub.arcgis.com/datasets/fedmaps::amtrak-rail-stations/explore?location=38.070140%2C-77.278835%2C9.77

transit <- st_read("data/nova/nova_transit_all.geojson")
transit_buff <- st_read("data/nova/nova_qrtr_buffers.geojson")
nova_byright2 <- st_read("data/nova/nova_byright2.geojson")
nova_region <- st_read("data/nova/nova_region.geojson")

pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("R", "M"))
transit_pal <-colorFactor(palette = c("#B1005F", "#E0592A", "#FFC658"), levels = c("WATA", "Hampton Roads Transit", "Suffolk Transit"))

nova_transit_map <- leaflet(nova_byright2) |> 
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
  addLegend(data = nova_byright2,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") |> 
  addLegend(data = transit,
            labels = c("WATA", "Hampton Roads Transit", "Suffolk Transit"),
            color = c("#B1005F", "#E0592A", "#FFC658"),
            group = "novaT Bus Stop") |> 
  addLegend(data = transit_buff,
            labels = "1/4 mile buffer",
            color = "grey",
            group = "1/4 mile buffer") |> 
  # addPolygons(data = nova_region,
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

nova_transit_map



write_rds(nova_transit_map, "data/nova/nova_transit_map.rds")

