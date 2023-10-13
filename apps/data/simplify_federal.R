library(sf)
library(tidyverse)
library(rmapshaper)

fed_lands <- st_read("apps/data/hr_fed_land.geojson") |> 
  mutate(Name = unit_name)

simple_fed <- ms_simplify(fed_lands, keep = 0.1, keep_shapes = TRUE)

st_write(st_cast(simple_fed, "MULTIPOLYGON"), "apps/data/fed_lands.geojson")


mapview::mapview(simple_fed)
