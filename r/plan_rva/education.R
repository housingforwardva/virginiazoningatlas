library(tidyverse)
library(sf)
library(geojsonsf)
library(janitor)

rva_fips <- c("51036", "51041", "51085", "51087", "51075", "51127", "51145", "51760")


schools <- geojson_sf("data/va_schools_k12.geojson") |> 
  clean_names() |> 
  filter(FIPScode %in% rva_fips) |> 
  select(name = landmk_name, county = fip_sname) |> 
  mutate(name = str_remove(name, " School"))
  

live_learn <- read_csv("data/live_learn_data.csv") |> 
  clean_names() |> 
  mutate(name = sch_name)

schools_join <- schools |> 
  left_join(live_learn, by = "name") |> 
  drop_na(fid) |> 
  select(-fid)
  


write_rds(schools_join, "data/planrva/rds/schools_data.rds")
st_write(schools_join, "data/planrva/geo/schools_data.gpkg", driver = "GPKG")


by4_neardiverseschools<- geojson_sf("data/planrva/geo/diverse_buffer_zoning.geojson") |> 
  clean_names() |> 
  st_drop_geometry() |> 
  mutate(area = as.numeric(buffer_area)) |> 
  group_by(county) |> 
  mutate(total_area = sum(area)) |> 
  filter(family4_treatment == "allowed") |> 
  group_by(county, total_area) |> 
  summarise(area = sum(area)) |> 
  mutate(pct = area/total_area)

by4_nearwhiteschools<- geojson_sf("data/planrva/geo/white_buffer_zoning.geojson") |> 
  clean_names() |> 
  st_drop_geometry() |> 
  mutate(area = as.numeric(buffer_area)) |> 
  group_by(county) |> 
  mutate(total_area = sum(area)) |> 
  filter(family4_treatment == "allowed") |> 
  group_by(county, total_area) |> 
  summarise(area = sum(area)) |> 
  mutate(pct = area/total_area)


by4_schools <- by4_nearwhiteschools |> 
  left_join(by4_neardiverseschools, by = "county") |> 
  select(county, total_area_w = total_area.x, byright4_w = area.x, pct_w = pct.x, 
         total_area_d = total_area.y, byright4_d = area.y, pct_d = pct.y) |> 
  mutate(across(everything(), ~replace_na(., 0))) |> 
  select(county, pct_w, pct_d) |> 
  pivot_longer(2:3,
               names_to = "type",
               values_to = "pct") |> 
  mutate(type = case_when(
    type == "pct_w" ~ "White majority school",
    type == "pct_d" ~ "Diverse school (40% or greater Black/Latino)"
  ))

write_rds(by4_schools, "data/planrva/rds/apt_zoning_schools.rds")


write_rds(by4_neardiverseschools, "data/planrva/rds/apts_zoning_diverseschools.rds")
write_rds(by4_neardiverseschools, "data/planrva/rds/apts_zoning_nearschools.rds")


oneonly_neardiverseschools<- geojson_sf("data/planrva/geo/diverse_buffer_zoning.geojson") |> 
  clean_names() |> 
  st_drop_geometry() |> 
  mutate(area = as.numeric(buffer_area)) |> 
  group_by(jurisdiction) |> 
  mutate(total_area = sum(area)) |> 
  filter(family1_treatment == "allowed" & family2_treatment == "prohibited" | family2_treatment == "hearing" & family3_treatment == "prohibited"  | family3_treatment == "hearing" & family4_treatment == "prohibited"  | family4_treatment == "hearing")|> 
  group_by(jurisdiction, total_area) |> 
  summarise(area = sum(area)) |> 
  mutate(pct = area/total_area)


oneonly_nearwhiteschools<- geojson_sf("data/planrva/geo/white_buffer_zoning.geojson") |> 
  clean_names() |> 
  st_drop_geometry() |> 
  mutate(area = as.numeric(buffer_area)) |> 
  group_by(jurisdiction) |> 
  mutate(total_area = sum(area)) |> 
  filter(family1_treatment == "allowed" & family2_treatment == "prohibited" | family2_treatment == "hearing" & family3_treatment == "prohibited"  | family3_treatment == "hearing" & family4_treatment == "prohibited"  | family4_treatment == "hearing")|> 
  group_by(jurisdiction, total_area) |> 
  summarise(area = sum(area)) |> 
  mutate(pct = area/total_area)


write_rds(oneonly_nearschools, "data/planrva/rds/singlefamily_only_nearschoolds.rds")

by4_highprov<- geojson_sf("data/planrva/geo/prov_10_more.geojson") |> 
  clean_names() 

|> 
  st_drop_geometry() |> 
  mutate(area = as.numeric(buffer_area)) |> 
  group_by(county) |> 
  mutate(total_area = sum(area)) |> 
  filter(family4_treatment == "allowed") |> 
  group_by(county, total_area) |> 
  summarise(area = sum(area)) |> 
  mutate(pct = area/total_area)

by4_lowprov<- geojson_sf("data/planrva/geo/prov_less_10.geojson") |> 
  clean_names() |> 
  st_drop_geometry() |> 
  mutate(area = as.numeric(buffer_area)) |> 
  group_by(county) |> 
  mutate(total_area = sum(area)) |> 
  filter(family4_treatment == "allowed") |> 
  group_by(county, total_area) |> 
  summarise(area = sum(area)) |> 
  mutate(pct = area/total_area)

by4_schools <- by4_highprov |> 
  left_join(by4_lowprov, by = "county") |> 
  select(county, total_area_high = total_area.x, byright4_high = area.x, pct_high = pct.x, 
         total_area_low = total_area.y, byright4_low = area.y, pct_low = pct.y) |> 
  mutate(across(everything(), ~replace_na(., 0))) |> 
  select(county, pct_high, pct_low) |> 
  pivot_longer(2:3,
               names_to = "type",
               values_to = "pct") |> 
  mutate(type = case_when(
    type == "pct_high" ~ "High Provisional Teacher Percentage (10% or more)",
    type == "pct_low" ~ "Low Provisional Teacher Percentage (Less than 10%)"
  ))


