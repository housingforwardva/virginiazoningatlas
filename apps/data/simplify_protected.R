library(sf)
library(tidyverse)
library(rmapshaper)

protected_lands <- st_read("apps/data/pad_land.geojson") |> 
  mutate(Name = Unit_Nm,
         Type = d_Des_Tp,
         Ownership = d_Mang_Nam) |> 
  select(Name, Type, Ownership)

simple_protected <- ms_simplify(protected_lands, keep = 0.1, keep_shapes = TRUE)

st_write(st_cast(simple_protected, "MULTIPOLYGON"), "apps/data/protected_lands.geojson")

mapview::mapview(simple_protected)
