library(tidyverse)
library(hdatools)
library(leaflet)
library(sf)
library(geojsonsf)
library(mapview)
library(webshot2)
library(htmlwidgets)



region <- read_rds("data/planrva/rds/rva_region.rds")

schools <- read_rds("data/planrva/rds/schools_data.rds")

school_pal <- colorNumeric(palette = "viridis", domain = schools$per_blk_la)


school_map <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = region,
              color = "#66788d",
              weight = 1,
              fillOpacity = 0.2) |>
  # addPolygons(data = by4,
  #           fillColor = "#B1005F",
  #           fillOpacity = 1,
  #           color = "white",
  #           weight = 1) |>
  addCircleMarkers(data = schools,
                   color = ~school_pal(per_blk_la),
                   radius = 5,
                   fillOpacity = 0.8,
                   stroke = FALSE,
                   popup = ~paste("School:", name, "<br>Black and Latino Students (%):", per_blk_la)) |>
  # addCircleMarkers(data = schools_below,
  #                  color = "grey",
  #                  radius = 5,
  #                  fillOpacity = 0.8,
  #                  stroke = FALSE,
  #                  popup = ~paste("School:", name, "<br>Black and Latino Students (%):", per_blk_la)) |>
  addLegend(position = "topright",
            title = "Black/Latino <br> Students (%)",
            pal = school_pal,
            values = schools$per_blk_la,
            opacity = 0.7)

saveWidget(school_map, "data/planrva/school_map.html")


rva_tracts <- read_rds("data/planrva/rds/race_tracts.rds")

by4 <- read_rds("data/planrva/rds/byright4_map.rds")

region <- read_rds("data/planrva/rds/rva_region.rds")

breaks <- classInt::classIntervals(rva_tracts$value, n = 5, style = "jenks")$brks
pal <- colorBin("Blues", domain = rva_tracts$value, bins = breaks)

race_map <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = by4,
              fillColor = "#B1005F",
              fillOpacity = 1,
              color = "white",
              weight = 1) |>
  addPolygons(data = region,
              color = "#66788d", # Set polygon fill color.
              weight = 1, # Set polygon outline weight.
              fillOpacity = 0) |>
  addPolygons(data = rva_tracts,
              fillColor = ~pal(value),
              weight = 1,
              color = ~pal(value),
              fillOpacity = 0.5,
              popup = rva_tracts$value) |>
  addLegend(data = rva_tracts,
            title = "Median Home Value",
            labels = "Median Home Value",
            pal = pal,
            values = ~value) |>
  addLegend(data = by4,
            title = "Multifamily (4+) Zoning",
            color = "#B1005F",
            labels = "Allowed By-Right")


saveWidget(race_map, "data/planrva/race_map.html")


transit_map <- read_rds("data/planrva/rds/transit_map.rds")
region <- read_rds("data/planrva/rds/rva_region.rds")


trans_map <- transit_map |>
  addProviderTiles(providers$CartoDB.Positron)

saveWidget(trans_map, "data/planrva/trans_map.html")
