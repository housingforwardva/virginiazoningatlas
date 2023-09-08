library(sf)

gl <- st_read("data/gloucester_county_zoning_final.geojson")
gl2 <- st_read("map/gloucester_zoning.geojson")

ex <- st_read("../web-map-sample-va/data/final.20220427.geojson")