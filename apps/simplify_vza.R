library(sf)
library(tidyverse)
library(rmapshaper)

hr_vza <- st_read("data/hr/hr_vza_developable.geojson")

sparse <- hr_vza |>
  select(id, type, name, overlay, family1_treatment:family4_treatment,
         hr_custom_fields_sfd, accessory_treatment)

simple <- ms_simplify(sparse, keep = 0.1)

write_rds(st_cast(simple, "MULTIPOLYGON"), "apps/hr_vza_simple.rds")

st_write(st_cast(simple, "MULTIPOLYGON"), "data/hr_vza_simple.geojson")


mapview::mapview(simple)


g1 <- sf::st_read("apps/gloucester_zoning.geojson")
g2 <- readr::read_rds("apps/zoning_simple.rds")