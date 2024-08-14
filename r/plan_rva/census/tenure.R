library(tidyverse)
library(tidycensus)

fips <- c("51036", "51041", "51085", "51087", "51075", "51127", "51145", "51760")

years <- 2010:2022

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% fips)


b25003_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25003") |> 
  filter(str_length(name) < 11)

b25003_raw <- map_dfr(years, function(yr){
  b25003_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25003",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25003_raw <- b25003_raw |> 
  subset(GEOID %in% fips)


b25003_vars_cleaned <- b25003_vars |> 
  separate(label, into = c("est", "total", "tenure"), sep = "!!") |> 
  select(variable = name, tenure) |>
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

b25003_data <- b25003_raw |> 
  right_join(b25003_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, estimate, moe) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |> 
  group_by(NAME, year) |> 
  mutate(pct = estimate/sum(estimate))


write_rds(b25003_data, "data/planrva/rds/b25003.rds")
