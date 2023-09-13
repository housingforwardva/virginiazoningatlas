library(sf)
library(tidyverse)

hr_sfd <- read_sf("data/hr/hr_vza_developable.geojson") |> 
  filter(hr_custom_fields_sfd == "  t")

write_rds(hr_sfd, "data/hr/hr_sfd_only_map.rds")
