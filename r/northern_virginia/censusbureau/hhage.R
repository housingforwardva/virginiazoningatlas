library(tidyverse)
library(tidycensus)

nova_fips <- c("51013", "51059", "51107", "51153", "51510", "51600", "51610", "51683", "51685")

years <- 2010:2022

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% nova_fips)

years <- 2010:2022

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
  subset(GEOID %in% nova_fips)


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


write_rds(b25007_data, "data/nova/nova_b25007.rds")
write_csv(b25007_data, "data/nova/nova_b25007.csv")

