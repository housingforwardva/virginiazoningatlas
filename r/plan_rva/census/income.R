library(tidyverse)
library(tidycensus)
library(FinCal)


years <- 2022

b19013 <- paste0("B19013", LETTERS[2:9])

b19013_defns <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 7) %in% b19013) |>
  filter(str_detect(name, "PR") == FALSE)

concept_to_race <- function(x) {
  out <- x |>
    str_remove_all("MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS \\(\\IN 2021 INFLATION-ADJUSTED DOLLARS\\)\\ \\(|\\)") |>
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
  filter(NAME == "Richmond, VA Metro Area")


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

int_rate <- 0.0646 # U.S. weekly averages as of 08/22/2024
dwn_payment <- 0.03
dwn_payment20 <- 0.2

b19013_msa <- b19013_msa |> 
  mutate(third = (estimate * .3),
         third_ho = (estimate * .28)) |> 
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> 
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)/(1 - dwn_payment))) |> 
  mutate(ho_price_20d = abs(pv(int_rate/12, 360, 0, ho, 0)/(1 - dwn_payment20)))



write_rds(b19013_msa, "data/planrva/rds/rva_b19013.rds")


rva_fips <- c("51036", "51041", "51085", "51087", "51075", "51127", "51145", "51760")

b25077_defns <- load_variables(2021, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B25077") |>
  filter(str_detect(name, "PR") == FALSE) |> 
  separate(label, c("estimate", "medvalue"), sep = "!!") |> 
  select(variable = name)

b25077_raw <- get_acs(
  geography = "tract",
  state = "VA",
  table = "B25077",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE,
  geometry = TRUE
) |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  subset(fips %in% rva_fips) 


b25077 <- b25077_raw |> 
  left_join(b25077_defns, by = "variable") 

white_maj <- st_read("data/planrva/geo/rva_race_tracts_22.gpkg") |> 
  sf::st_drop_geometry()

b25077_value <- b25077 |> 
  right_join(white_maj, by = "GEOID") |> 
  select(GEOID, NAME, name_long, value = estimate.x, percent)

st_write(b25077_value, "data/planrva/geo/home_value_white_tracts.gpkg", driver = "GPKG")
  
