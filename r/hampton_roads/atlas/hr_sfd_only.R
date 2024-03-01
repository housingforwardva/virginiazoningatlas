library(sf)
library(tidyverse)
library(rmapshaper)

hr_sfd <- geojson_sf("data/hr/hr_vza_geo.geojson") |> 
  ms_simplify(keep = 0.3) |> 
  filter(sfd == "t")

write_rds(hr_sfd, "data/hr/hr_sfd_only_map.rds")
