library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)
library(fuzzyjoin)

nova_fips <- c("51013", "51059", "51107", "51153", "51510", "51600", "51610", "51683", "51685")

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% nova_fips)

# Get housing unit estimates from the Population Estimates Program (PEP) from 2010 to 2019. 2021 data are set to be released in March 2023. 

hu_total_raw <- get_estimates( 
  geography = "county",
  state = "VA",
  variables = "HUEST",
  year = 2019,
  time_series = TRUE
)

hu_2022 <- read_csv("data/huest_20_22.csv") |> 
  clean_names() |> 
  mutate(across(.fns = ~str_remove_all(.x, ", Virginia")),
         across(.fns = ~str_remove_all(.x, "\\."))) |> 
  select(name_long = jurisdiction, x2020, x2021, x2022) |> 
  stringdist_join(local_lookup, by = "name_long") |> 
  pivot_longer(2:4,
               names_to = "year",
               values_to = "value") |> 
  mutate(counttype = "Housing unit estimate") |> 
  mutate(year = substr(year, 2, 5)) |> 
  select(name_long = name_long.y, fips, year, counttype, value)


hu_total_clean <- hu_total_raw |>
  filter(!DATE %in% c(2, 3)) |> # Remove non-decennial 2010 counts
  mutate(year = # Translate date codes into years
           case_when(
             DATE == 1 ~ "2010",
             DATE == 4 ~ "2011",
             DATE == 5 ~ "2012",
             DATE == 6 ~ "2013",
             DATE == 7 ~ "2014",
             DATE == 8 ~ "2015",
             DATE == 9 ~ "2016",
             DATE == 10 ~ "2017",
             DATE == 11 ~ "2018",
             DATE == 12 ~ "2019")) |>
  mutate(counttype = # Add descriptions to count types
           case_when(
             DATE == 1 ~ "Census",
             TRUE ~ "Housing unit estimate")) |> 
  select( # Simplify columns
    fips = GEOID,
    year,
    counttype,
    value
  ) |> 
  right_join(local_lookup, by = "fips")


hu_data <- rbind(hu_total_clean, hu_2022)

write_rds(hu_data, "data/nova/nova_hu.rds")
write_csv(hu_data, "data/nova/nova_hu.csv")


