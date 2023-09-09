library(sf)
library(leaflet)

hr_region <- read_sf("data/hr/hr_region.geojson")

write_rds(hr_region, "data/hr/hr_region.rds")