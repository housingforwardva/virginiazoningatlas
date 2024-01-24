library(tidycensus)
library(tidyverse)
library(mapview)

nova_fips <- c("51013", "51059", "51107", "51153", "51510", "51600", "51610", "51683", "51685")

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
  year = 2021,
  survey = "acs5",
  cache_table = TRUE,
  geometry = TRUE
) |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  subset(fips %in% nova_fips) 

nova_b03002 <- b03002_raw |> 
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
  mutate(fips = as.character(fips_full))

nova_race <- nova_b03002 |> 
  filter(race == "White")|> 
  mutate(whitemaj = case_when(
    percent > .50 ~ "Yes",
    TRUE ~ "No"))  |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  left_join(local_lookup, by = "fips")

# What localities have a large BIPOC population?

nova_bipoc <- nova_b03002 |> 
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
  filter(race == "BIPOC")

write_rds(nova_bipoc, "data/nova/nova_bipoc.rds")

# mapview(nova_race)

write_rds(nova_race, "data/nova/nova_race_tracts.rds")
st_write(nova_race, "data/nova/nova_race_tracts.gpkg", driver = "GPKG")
