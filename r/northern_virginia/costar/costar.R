library(tidyverse)
library(readxl) 
library(janitor)
library(zoo)
library(lubridate)
library(fredr)

# Data from CoStar was downloaded on 1/6/2024

nova_costar <- read_excel("data/nova/nova_costar_rent_market.xlsx") |> # Load in xlsx spreadsheet from CoStar.
  clean_names() |> # Clean up column names to be data friendly.
  filter(!str_detect(period, "QTD")) |> # Remove Quarter-to-Date estimate. 
  mutate(year = substr(period, 1, 4)) |> 
  mutate(year = as.numeric(year)) |> 
  mutate(period = as.yearqtr(period, format = "%Y Q%q"), frac = 1) |> 
  filter(year >= 2010) |> 
  select(period, year, bldgs = inventory_bldgs, units = inventory_units, rent = asking_rent_per_unit, 
         rent_growth = asking_rent_percent_growth_yr, vacancy_rate = vacancy_percent,
         uc_units = under_construction_units)

cpi_ls <- fredr(
  series_id = "CUUR0000SA0L2"
) %>% 
  select(date, value) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(year = year(date)) |> 
  mutate(period = as.yearqtr(date, format = "%Y Q%q", frac = 1)) |> 
  group_by(period) |> 
  summarise(cpi = mean(value, na.rm = TRUE))

nova_costar <- nova_costar |> 
  left_join(cpi_ls, by = "period") |> 
  transform(adj_price = (279.59450/cpi)*rent) # 2023 Q4


write_rds(nova_costar, "data/nova/nova_costar.rds")
