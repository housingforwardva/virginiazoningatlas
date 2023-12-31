library(sf)
library(tidyverse)
library(rmapshaper)

hr_sfd <- read_sf("data/hr/hr_vza_developable.geojson") |> 
  ms_simplify(keep = 0.3) |> 
  filter(hr_custom_fields_sfd == "  t")

write_rds(hr_sfd, "data/hr/hr_sfd_only_map.rds")
