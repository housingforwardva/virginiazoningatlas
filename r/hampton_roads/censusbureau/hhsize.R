library(tidyverse)
library(tidycensus)

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

years <- 2010:2021

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

b25010_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B25010")

b25010_raw <- map_dfr(years, function(yr){
  b25010_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25010",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25010_raw <- b25010_raw |>
  subset(GEOID %in% hr_fips)

b25010_vars_cleaned <- b25010_vars |>
  separate(label, into = c("est", "avg", "total", "tenure"), sep = "!!") |>
  select(variable = name, tenure) |>
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) 
 
b25010_data <- b25010_raw |>
  right_join(b25010_vars_cleaned, by = "variable") |>
  select(NAME, fips = GEOID, year, tenure, estimate) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |> 
  left_join(local_lookup, by = "fips") 

write_rds(b25010_data, "data/hr/hr_ahhsize.rds")

b25009_vars <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B25009")

b25009_raw <- map_dfr(years, function(yr){
  b25009_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25009",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25009_raw <- b25009_raw |>
  subset(GEOID %in% hr_fips)

b25009_vars_cleaned <- b25009_vars |>
  separate(label, into = c("est", "total", "tenure", "size"), sep = "!!") |>
  select(variable = name, tenure, size) |>
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  )) 

b25009_data <- b25009_raw |>
  right_join(b25009_vars_cleaned, by = "variable") |>
  select(NAME, fips = GEOID, year, tenure, size, estimate) |>
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |> 
  mutate(size = case_when(
    size == "1-person household" ~ "1-person",
    size == "2-person household" ~ "2-person",
    size == "3-person household" ~ "3-person",
    TRUE ~ "4-person or more"
  )) |>
  left_join(local_lookup, by = "fips") 

write_rds(b25009_data, "data/hr/hr_hhsize.rds")
