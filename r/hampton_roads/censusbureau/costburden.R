library(tidyverse)
library(tidycensus)

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

years <- 2010:2021

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

b25106_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25106")

b25106_raw <- map_dfr(years, function(yr) {
  b25042_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25106",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25106_raw <- b25106_raw |> 
  subset(GEOID %in% hr_fips)

b25106_vars_cleaned <- b25106_vars |> 
  separate(label, into = c("est", "total", "tenure", "income", "cb"), sep = "!!") |> 
  select(variable = name, tenure, income, cb) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner-occupied housing units" ~ "Homeowner",
    tenure == "Renter-occupied housing units" ~ "Renter"),
    cb = case_when(
      cb == "30 percent or more" ~ "Cost-burdened",
      TRUE ~ "Not cost-burdened"))

b25106_data <- b25106_raw |> 
  right_join(b25106_vars_cleaned, by = "variable") |> 
  select(NAME, year, tenure, income, cb, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |>  
  group_by(NAME, year, tenure, income, cb) |> 
  summarise(estimate = sum(estimate)) 

write_rds(b25106_data, "data/hr/hr_cb.rds")
  