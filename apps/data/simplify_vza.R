library(sf)
library(tidyverse)
library(geojsonsf)
library(rmapshaper)

hr_vza <- geojson_sf("data/hr_vza_nza.geojson")
nova_vza <- geojson_sf("data/nova_vza_nza.geojson")

vza <- rbind(hr_vza, nova_vza)

sparse <- vza |>
  select(id, type, abbrvname, tooltipnotes, jurisdiction, name, overlay, family1_treatment, family2_treatment, family3_treatment,
         family4_treatment, customfielddata, accessory_treatment, acres)|> 
  filter(overlay == 0) |> 
  mutate(Zoning = name,
         Abbreviation = abbrvname,
         Jurisdiction = jurisdiction,
         Notes = tooltipnotes) |> 
  group_by(jurisdiction) |> 
  mutate(total_area = sum(acres)) |> 
  mutate(accessory_treatment = case_when(
    accessory_treatment == "nomention" ~ "prohibited",
    TRUE ~ accessory_treatment
  ))

simple <- ms_simplify(sparse, keep = 0.3, keep_shapes = TRUE)

write_rds(st_cast(simple, "MULTIPOLYGON"), "apps/hr_vza_simple.rds")

st_write(st_cast(simple, "MULTIPOLYGON"), "data/hr_vza_simple.geojson")


mapview::mapview(simple)


g1 <- sf::st_read("apps/gloucester_zoning.geojson")
g2 <- readr::read_rds("apps/zoning_simple.rds")