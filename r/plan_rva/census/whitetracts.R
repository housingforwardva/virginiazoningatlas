library(tidycensus)
library(tidyverse)
library(mapview)

rva_fips <- c("51036", "51041", "51085", "51087", "51075", "51127", "51145", "51760")

b03002_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B03002")

b03002_vars_cleaned <- b03002_vars |> 
  separate(label, into = c("est", "total", "ethnicity", "race", "two"), sep = "!!") |> 
  filter(is.na(two)) |> 
  select(variable = name, ethnicity, race) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(across(.fns = ~str_remove_all(.x, "alone"))) |> 
  mutate(across(.fns = ~str_trim(.x)))


b03002_raw <- get_acs(
  geography = "tract",
  state = "VA",
  table = "B03002",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE,
  geometry = TRUE
) |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  subset(fips %in% rva_fips) 

b19013_raw <- get_acs(
  geography = "tract",
  state = "VA",
  table = "19013",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE,
  geometry = TRUE
) |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  subset(fips %in% rva_fips) 

rva_b03002 <- b03002_raw |> 
  right_join(b03002_vars_cleaned, by = "variable") |> 
  mutate(race = case_when(
    ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
    TRUE ~ race
  )) |> 
  group_by(GEOID, race) |> 
  summarise(estimate = sum(estimate)) |> 
  group_by(GEOID) |> 
  mutate(percent = estimate/sum(estimate))

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% rva_fips)


rva_race <- rva_b03002 |> 
  filter(race == "White")|> 
  mutate(whitemaj = case_when(
    percent > .50 ~ "Yes",
    TRUE ~ "No"))  |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  left_join(local_lookup, by = "fips")

# What localities have a large BIPOC population?

rva_bipoc <- rva_b03002 |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  left_join(local_lookup, by = "fips") |> 
  group_by(fips, name_long, race) |> 
  summarise(estimate = sum(estimate)) |> 
  mutate(race = case_when(
    race == "White" ~ "White",
    TRUE ~ "BIPOC"
  )) |> 
  group_by(fips, name_long, race) |> 
  summarise(estimate = sum(estimate)) |> 
  group_by(fips, name_long) |> 
  mutate(percent = estimate/sum(estimate)) |> 
  filter(race == "BIPOC") |> 
  mutate(county = name_long)

write_rds(hr_bipoc, "data/hr/hr_bipoc.rds")

mapview(rva_race)

write_rds(rva_race, "data/planrva/rds/rva_race_tracts.rds")
st_write(rva_race, "data/planrva/geo/rva_race_tracts_22.gpkg", driver = "GPKG")
