library(tidyverse)
library(geojsonsf)


hr_byright2 <- geojson_sf("data/hr/hr_vza_geo.geojson") |> 
  sf::st_drop_geometry() |> 
  filter(family2_treatment == "allowed" & family3_treatment == "allowed" & family4_treatment == "allowed") |> 
  select(abbrvname, type, jurisdiction, county, family1_treatment, family2_treatment, family3_treatment, family4_treatment)


write_csv(hr_byright2, "data/hr/hr_byright2.csv")

