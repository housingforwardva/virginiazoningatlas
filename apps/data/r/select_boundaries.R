library(tidyverse)
library(sf)
library(rmapshaper)


regions <- read_csv("data/boundaries.csv") |> 
  filter(pdc == "Hampton Roads" | pdc == "Northern Virginia")

geo <- st_read("data/va_boundaries_clean.gpkg") |> 
  select(jurisdiction = Jurisdiction) |> 
  right_join(regions, by = "jurisdiction")

geo_simple <- ms_simplify(geo, keep = 0.1)

# st_write(geo_simple, "apps/data/boundaries.geojson")

  
write_rds(st_cast(geo_simple, "MULTIPOLYGON"), "apps/data/boundaries.rds")
