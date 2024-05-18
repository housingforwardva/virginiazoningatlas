library(sf)
library(tidyverse)
library(geojsonsf)
library(rmapshaper)


# The following geopackage was downloaded from DCR's Virginia Flood
# Risk Information System. The 100 year flood plain was sourced from the 
# National Flood Hazard Layer 1% Annual Flood Risk.

# This layer was simplified in QGIS using the “Douglas-Peucker" method with
# a tolerance of 0.0005.

flood <- st_read("apps/data/raw/one_hundred_flood_simple.gpkg")

simple_flood <- ms_simplify(flood, keep = 0.1)

# st_write(simple_flood, "apps/data/flood.geojson")
# 
# mapview::mapview(simple_flood)

write_rds(st_cast(simple_flood, "MULTIPOLYGON"), "apps/data/flood.rds")
