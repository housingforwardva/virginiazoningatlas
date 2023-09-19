# What can the typical renter household afford to rent or buy?

library(tidycensus)
library(tidyverse)
library(FinCal)

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

b25119_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25119")

b25119_raw <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25119",
  year = 2021,
  survey = "acs5",
  cache_table = TRUE
)

b25119_hr <- b25119_raw |> 
  subset(GEOID %in% hr_fips)

b25119_vars_cleaned <- b25119_vars |> 
  separate(label, into = c("est", "income", "total", "tenure"), sep = "!!") |>  
  select(variable = name, tenure) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied (dollars)" ~ "Homeowner",
    tenure == "Renter occupied (dollars)" ~ "Renter"
  ))

b25119_vars_cleaned$tenure <- b25119_vars_cleaned$tenure |>  replace_na('All households')

int_rate <- 0.0718
dwn_payment <- 0.03

b25119_data <- b25119_hr |> 
  right_join(b25119_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, tenure, estimate, moe) |> 
  filter(tenure == "Renter") |> 
  mutate(third = (estimate * .3),
         third_ho = (estimate * .28)) |> 
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> 
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)))

write_rds(b25119_data, "data/hr_income.rds")



years <- 2010:2021 

b19013 <- paste0("B19013", LETTERS[2:9])

b19013_defns <- load_variables(2020, "acs5") |>
  filter(str_sub(name, end = 7) %in% b19013) |>
  filter(str_detect(name, "PR") == FALSE)

concept_to_race <- function(x) {
  out <- x |>
    str_remove_all("MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS \\(\\IN 2020 INFLATION-ADJUSTED DOLLARS\\)\\ \\(|\\)") |>
    str_to_title()
}

b19013_cleaned <- b19013_defns |>
  mutate(race = concept_to_race(concept)) |>
  separate(label, c("estimate", "medhhincome"), sep = "!!") |>
  select(variable = name, medhhincome, race) |>
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")))

b19013_raw_msa <- map_dfr(b19013, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    acs_pull <- get_acs(
      geography = "cbsa",
      table = tb,
      year = yr
    ) |>
      left_join(b19013_cleaned, by = "variable")
    acs_rearranged <- acs_pull |>
      mutate(year = yr) |>
      select(variable, year, NAME, GEOID, race, medhhincome,
             estimate, moe)
    acs_rearranged
  })
  yearly_data
}) |> 
  filter(NAME == "Virginia Beach-Norfolk-Newport News, VA-NC Metro Area")


b19013_msa <- b19013_raw_msa |> 
  mutate(across(.fns = ~str_remove_all(.x, "Alone Householder")),
         across(.fns = ~trimws(.x))) |> 
  select(year, NAME, race, estimate, moe) |> 
  transform(year = as.numeric(year),
            estimate = as.numeric(estimate))|> 
  mutate(race = case_when(
    race == "Black Or African American" ~ "Black",
    race == "Two Or More Races Householder" ~ "Multiracial",
    race == "White Alone, Not Hispanic Or Latino Householder" ~ "White, non-Hispanic",
    race == "Hispanic Or Latino Householder" ~ "Hispanic or Latino",
    TRUE ~ race
  )) |> 
  filter(race %in% c("White, non-Hispanic", "Black", "Asian", "Multiracial", "Hispanic or Latino"))

int_rate <- 0.0718
dwn_payment <- 0.03

b19013_msa <- b19013_msa |> 
  mutate(third = (estimate * .3),
         third_ho = (estimate * .28)) |> 
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> 
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)))


write_rds(b19013_msa, "data/hr/hr_b19013.rds")

