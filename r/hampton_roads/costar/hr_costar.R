library(tidyverse)
library(readxl) 
library(janitor)
library(zoo)
library(lubridate)

hr_costar <- read_excel("data/hr/hr_costar_rent_market.xlsx") |> 
  clean_names() |> 
  filter(!str_detect(period, "QTD")) |> 
  mutate(year = substr(period, 1, 4)) |> 
  mutate(year = as.numeric(year)) |> 
  mutate(period = as.Date(as.yearqtr(period, format = "%Y Q%q"), frac = 1)) |> 
  filter(year >= 2010) |> 
  select(period, year, bldgs = inventory_bldgs, units = inventory_units, rent = asking_rent_per_unit, 
         rent_growth = asking_rent_percent_growth_yr, vacancy_rate = vacancy_percent,
         uc_units = under_construction_units)

write_rds(hr_costar, "data/hr/hr_costar.rds")
