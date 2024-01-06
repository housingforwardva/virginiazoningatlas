library(sf)
library(leaflet)

nova_region <- read_sf("data/nova/nova_region.geojson")

write_rds(nova_region, "data/nova/nova_region.rds")