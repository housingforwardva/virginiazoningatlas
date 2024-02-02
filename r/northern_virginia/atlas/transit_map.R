library(tidyverse)
library(sf)
library(leaflet)

# https://www.arcgis.com/apps/mapviewer/index.html?layers=11d63c5455c8462ca63fc490061900b2
# https://opendata.dc.gov/datasets/e3896b58a4e741d48ddcda03dae9d21b_51/explore?location=38.869191%2C-77.025953%2C11.17
# https://hub.arcgis.com/datasets/fedmaps::amtrak-rail-stations/explore?location=38.070140%2C-77.278835%2C9.77

rail <- st_read("data/nova/nova_rail_stops.geojson") |> 
  mutate(type = "Rail") |> 
  select(service, type)

bus <- st_read("data/nova/nova_transit_stops.geojson")|> 
  pivot_longer(2:7,
               values_to = "check",
               names_to = "service") |> 
  filter(check == "Yes") |> 
  mutate(service = str_replace_all(service, "_", " ")) |> 
  mutate(type = "Bus") |> 
  select(service, type)

rail_buffers <- st_buffer(rail, 402.336)
bus_buffers <- st_buffer(bus, 402.336)


# mapview::mapview(rail_buffers)

transit <- rbind(rail, bus)
transit_buff <- rbind(rail_buffers, bus_buffers)
nova_byright2 <- st_read("data/nova/nova_byright2plus.geojson")
nova_region <- st_read("data/nova/nova_region.geojson")

# st_write(transit, "data/nova/nova_transit.geojson")
# st_write(transit_buff, "data/nova/nova_transit_buffers.geojson")

pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("Primarily Residential", "Mixed with Residential"))
transit_pal <-colorFactor(palette = c("#B1005F", "#E0592A"), levels = c("Bus", "Rail"))

nova_transit_map <- leaflet(nova_byright2) |> 
  addPolygons(weight = 1,
             color = ~pal(type),
             fillColor = ~pal(type),
             fillOpacity = 0.9) |> 
  addCircleMarkers(data = transit,
              color = ~transit_pal(type),
              radius = 0.15,
              fillOpacity = 1,
              fillColor = ~transit_pal(type),
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
            labels = c("Bus", "Rail"),
            color = c("#B1005F", "#E0592A"),
            group = "NOVA Transit") |> 
  addLegend(data = transit_buff,
            labels = "1/4 mile buffer",
            color = "grey",
            group = "1/4 mile buffer") |> 
  addLayersControl(
    overlayGroups = c("Transit Stops", "1/4 mile buffer"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

nova_transit_map



write_rds(nova_transit_map, "data/nova/nova_transit_map.rds")

