library(sf)
library(tidyverse)
library(rmapshaper)
library(geojsonsf)
library(leaflet)
library(mapgl)

# Upload geojson for PlanRVA boundaries and write to RDS.

# rva_region <- read_sf("data/planrva/geo/rva_region.geojson")
# 
# write_rds(rva_region, "data/planrva/rds/rva_region.rds")
# 

one <- geojson_sf("data/planrva/geo/rva_vza_geo.geojson") |> 
  ms_simplify(keep = 0.3) |> 
  filter(family1_treatment == "allowed") |> 
  filter(family2_treatment == "prohibited") |> 
  filter(family3_treatment == "prohibited") |> 
  filter(family4_treatment == "prohibited") 
  

st_write(one, "data/planrva/geo/one_only_map_update.gpkg", driver = "GPKG")

diverse <- geojson_sf("data/planrva/geo/rva_vza_geo.geojson") |> 
  ms_simplify(keep = 0.3) |> 
  filter(family1_treatment == "allowed") |> 
  filter(family2_treatment == "allowed") |> 
  filter(family3_treatment == "allowed") |> 
  filter(family4_treatment == "allowed") 


st_write(diverse, "data/planrva/geo/diverse_map.gpkg", driver = "GPKG")

# Write RDS for Single-family only detached districts.

sfd <- geojson_sf("data/planrva/geo/rva_vza_geo.geojson") |> 
  ms_simplify(keep = 0.3) |> 
  filter(sfd == "t")

st_write(sfd, "data/planrva/geo/sfd_only_map.gpkg", driver = "GPKG")

write_rds(sfd, "data/planrva/rds/rva_sfd_only_map.rds")

# Create map for 2+ family housing.

byright2 <- geojson_sf("data/planrva/geo/rva_vza_geo.geojson") |> 
filter(family2_treatment == "allowed" & family3_treatment == "allowed" & family4_treatment == "allowed") |> 
  select(abbrvname, type, jurisdiction, county, family1_treatment, family2_treatment, family3_treatment, family4_treatment)

byright2 <- ms_simplify(byright2, keep = 0.3)


pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),
                   levels = c("Primarily Residential", "Mixed with Residential"))


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

byr_map

write_rds(byr_map, "data/planrva/rds/byr_map.rds")


# Create map for 4+ family housing.

byright4 <- geojson_sf("data/planrva/geo/rva_vza_geo.geojson") |> 
  filter(family4_treatment == "allowed") |> 
  select(abbrvname, type, jurisdiction, county, family1_treatment, family2_treatment, family3_treatment, family4_treatment)

byright4 <- ms_simplify(byright4, keep = 0.3)

st_write(byright4, "data/planrva/geo/byright4_map.gpkg", driver = "GPKG")



pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),
                   levels = c("Primarily Residential", "Mixed with Residential"))



byr4_map <- leaflet(byright4) |>
  addPolygons(data = region,
              weight = 1, # Set polygon outline weight.
              color = "#011E41", # Set polygon outline color.
              fillOpacity = 0.1,
              popup = region$Jurisdiction) |>  # Set pop-up to jurisdiction name.
  addPolygons(weight = 1,
              color = ~pal(type),
              fillColor = ~pal(type),
              fillOpacity = 0.9,
              popup = paste0("Jurisdiction: ",
                             byright4$jurisdiction,
                             "<br>",
                             "District: ",
                             byright4$abbrvname)) |> 
  addLegend(data = byright4,
            labels = c("Primarily Residential", "Mixed with Residential"),
            colors = c("#8B85CA", "#40C0C0"),
            title = "Type of Zoning District") 

byr4_map

write_rds(byr4_map, "data/planrva/rds/byr4_map.rds")

transit <- st_read("data/planrva/geo/grtc_stops.geojson")
transit_buff <- st_read("data/planrva/geo/grtc_qtr_mile.geojson")
byr4_map <- read_rds("data/planrva/rds/byr4_map.rds")

pal <- colorFactor(palette =c("#8B85CA", "#40C0C0"),levels = c("Primarily Residential", "Mixed with Residential"))
transit_pal <-colorFactor(palette = c("#B1005F"), levels = c("GRTC"))

transit_map <- byr4_map |> 
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
  addLegend(data = transit,
            labels = c("GRTC"),
            color = c("#B1005F"),
            group = "GRTC Bus Stop") |> 
  addLegend(data = transit_buff,
            labels = "1/4 mile buffer",
            color = "grey",
            group = "1/4 mile buffer") |> 
  # addPolygons(data = hr_region,
  #             color = "#011E41",
  #             weight = 1, 
  #             fillOpacity = 0) |> 
  # Layers control
  addLayersControl(
    overlayGroups = c("Transit Stops", "1/4 mile buffer"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

transit_map



write_rds(transit_map, "data/planrva/rds/transit_map.rds")


