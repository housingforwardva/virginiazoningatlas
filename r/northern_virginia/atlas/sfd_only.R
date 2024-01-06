library(sf)
library(tidyverse)
library(rmapshaper)

nova_sfd <- read_sf("data/nova/nova_vza_developable.geojson") |> 
  ms_simplify(keep = 0.3) |> 
  filter(nova_custom_fields_sfd == "  t")

write_rds(nova_sfd, "data/nova/nova_sfd_only_map.rds")
