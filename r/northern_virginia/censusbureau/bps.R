library(tidyverse)
library(tidycensus)
library(glue)

nova_fips <- c("51013", "51059", "51107", "51153", "51510", "51600", "51610", "51683", "51685")

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% nova_fips)

bps_years <- 2010:2022


header_rows <- read_csv("https://www2.census.gov/econ/bps/County/co2020a.txt", 
                        col_names = FALSE,
                        n_max = 2)

column_names <- header_rows |>
  select(X1:X18) |>
  t() |>
  as_tibble() |>
  mutate(group = rep(1:6, each = 3)) |>
  group_by(group) |>
  fill(V1, .direction = "updown") |>
  mutate(names = paste0(V1, ": ", V2)) |>
  pull(names)

cbps_raw <- map_df(bps_years, ~{
  raw <- read_csv(glue("https://www2.census.gov/econ/bps/County/co{.x}a.txt"), skip = 2,
                  col_names = FALSE) |>
    select(X1:X18) |>
    set_names(column_names)
  
  raw
  
})

nova_bps <- cbps_raw |> 
  mutate(year = `Survey: Date`,
         GEOID = paste0(`FIPS: State`, `FIPS: County`)) |>
  select(`1-unit: Bldgs`:GEOID) |>
  filter(GEOID %in% nova_fips) |>
  pivot_longer(`1-unit: Bldgs`:`5+ units: Value`,
               names_to = "type",
               values_to = "value") |>
  separate(type, into = c("Type", "col"), sep = ": ") |>
  pivot_wider(names_from = col,
              values_from = value) |>
  rename_with(tolower, Type:Value) |> 
  select(fips = GEOID, year, type:units) |> 
  left_join(local_lookup, by = "fips") |> 
  mutate(locality_type = case_when(
    str_detect(name_long, "City") ~ "City",
    TRUE ~ "County"
  ))


write_rds(nova_bps, "data/nova/nova_bps.rds")
write_csv(nova_bps, "data/nova/nova_bps.csv")
