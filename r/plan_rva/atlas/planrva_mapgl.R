library(sf)
library(tidyverse)
library(rmapshaper)
library(geojsonsf)
library(leaflet)
library(mapgl)

MAPBOX_PUBLIC_TOKEN = Sys.getenv("MAPBOX_PUBLIC_TOKEN")

byright4 <- geojson_sf("data/planrva/geo/rva_vza_geo.geojson") |> 
  filter(family4_treatment == "allowed") |> 
  select(abbrvname, type, jurisdiction, county, family1_treatment, family2_treatment, family3_treatment, family4_treatment)

byright4 <- ms_simplify(byright4, keep = 0.3)

# Create a color scale function

byright4 <- byright4 |> 
  mutate(fill_color = case_when(
    type == "Primarily Residential" ~ "#8B85CA",
    type == "Mixed with Residential" ~ "#40C0C0",
    TRUE ~ "#011E41"
  ))

transit <- st_read("data/planrva/geo/grtc_stops.geojson")
transit_buff <- st_read("data/planrva/geo/grtc_qtr_mile.geojson")

# Create the mapgl map
transit_gl <- mapboxgl(style = "mapbox://styles/ericvmai/clzu65lrv00qc01pd1her0smz/draft", 
         bounds = byright4, 
         access_token = MAPBOX_PUBLIC_TOKEN) |> 
  add_fill_layer(id = "By-Right Multifamily (+4) Zoning",
                 source = byright4,
                 fill_opacity = 0.8,
                 fill_color = get_column("fill_color"),
                 fill_outline_color = "white",
                 slot = "middle") |> 
  add_circle_layer(
    id = "GRTC Stops",
    source = transit,
    circle_color = "#ffd179",
    slot = "middle",
    circle_radius = interpolate(
      property = "zoom",
      values = c(9, 16),
      stops = c(1, 10)
    )) |> 
  add_fill_layer(
    id = "Quarter Mile Transit Stop Buffer",
    source = transit_buff,
    fill_color = "grey",
    fill_opacity = 0.1,
    fill_outline_color = "black",
    slot = "middle") |> 
  add_layers_control(
    position = "top-right",
    collapsible = TRUE
  ) 


write_rds(transit_gl, "data/planrva/rds/transit_gl.rds")
