library(tidyverse)
library(sf)
library(geojsonsf)


hr_transit_all <- geojson_sf("apps/data/raw/hr_transit_all.geojson") |> 
  mutate(type = "Bus/Light Rail") 

nova_bus <- geojson_sf("apps/data/raw/nova_transit_stops.geojson") |> 
  pivot_longer(2:7,
               values_to = "check",
               names_to = "service") |> 
  filter(check == "Yes") |> 
  mutate(service = str_replace_all(service, "_", " ")) |> 
  mutate(type = "Bus") |> 
  select(service, type)
  

nova_rail <- geojson_sf("apps/data/raw/nova_rail_stops.geojson") |> 
  mutate(type = "Rail") |> 
  select(service, type)

transit <- rbind(hr_transit_all, nova_bus, nova_rail) |> 
  rename(Service = service)

# st_write(transit, "apps/data/transit.geojson", append = FALSE)

write_rds(transit, "apps/data/transit.rds")

