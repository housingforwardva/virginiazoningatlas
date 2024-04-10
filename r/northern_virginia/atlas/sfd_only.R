library(sf)
library(tidyverse)
library(rmapshaper)

nova_sfd <- read_sf("data/nova/geo/nova_vza_geo.geojson") |> 
  ms_simplify(keep = 0.1, keep_shapes = TRUE) |> 
  filter(sfd == "t")

nova_sfd_map <- read_sf("data/nova/geo/nova_vza_geo.geojson") |> 
  filter(sfd == "t")

write_rds(nova_sfd, "data/nova/rds/nova_sfd_only_map.rds")
st_write(nova_sfd_map, "data/nova/geo/nova_sfd_only_map.geojson", append = FALSE)
