library(tidyverse)
library(tidycensus)

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

years <- 2010:2021

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

years <- 2010:2021

b25007_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25007") |> 
  filter(str_length(name) < 11)

b25007_raw <- map_dfr(years, function(yr){
  b25007_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25007",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25007_raw <- b25007_raw |> 
  subset(GEOID %in% hr_fips)


b25007_vars_cleaned <- b25007_vars |> 
  separate(label, into = c("est", "total", "tenure", "age"), sep = "!!") |> 
  select(variable = name, tenure, age) |>
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  ))

b25007_data <- b25007_raw |> 
  right_join(b25007_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, age, estimate, moe) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |> 
  group_by(NAME, year) |> 
  mutate(pct = estimate/sum(estimate))


write_rds(b25007_data, "data/hr/hr_b25007.rds")
