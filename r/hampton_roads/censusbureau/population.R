library(tidyverse)
library(tidycensus)

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

# Get population estimates from the Population Estimates Program (PEP) from 2010 to 2019. 2021 data are set to be released in March 2023. 

pep_total_raw <- get_estimates( 
  geography = "county",
  state = "VA",
  variables = "POP",
  year = 2019,
  time_series = TRUE
)

pep_2020s <- get_estimates( 
  geography = "county",
  state = "VA",
  variables = "POPESTIMATE",
  year = 2022,
  time_series = TRUE
) |> 
  filter(year != 2020) |> 
  mutate(counttype = "Population estimate") |> 
  select(fips = GEOID, year, counttype, value)

# Get decennial Census estimates.

census_raw <- get_decennial( 
  geography = "county",
  state = "VA",
  year = 2020,
  sumfile = "pl",
  variables = "P1_001N"
) 

pep_total_clean <- pep_total_raw |>
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
             DATE == 1 ~ "Census population",
             TRUE ~ "Population estimate")) |> 
  select( # Simplify columns
    fips = GEOID,
    year,
    counttype,
    value
  )

# Prep total population counts from 2020 Census summary file

census_clean <- census_raw |> 
  mutate(year = "2020", # Add year and count type columns
         counttype = "Census population") |> 
  select( # Simplify columns
    fips = GEOID,
    year,
    counttype,
    value
  )

population_data <- rbind(pep_total_clean, census_clean, pep_2020s) |> 
  subset(fips %in% hr_fips) |> 
  left_join(local_lookup, by = "fips")

write_rds(population_data, "data/hr/hr_pop.rds")
