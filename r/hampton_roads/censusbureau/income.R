# What can the typical renter household afford to rent or buy?

library(tidycensus)
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

