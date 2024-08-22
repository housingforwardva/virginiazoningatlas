library(tidyverse)
library(lubridate)
library(fredr)
library(zoo)

nova_var <- read_csv("data/home-sales.csv")

nova <- c("Washington MSA","Alexandria City", "Arlington County", 
          "Fairfax City", "Fairfax County", "Falls Church City", "Loudoun County", 
          "Manassas City", "Manassas Park City", "Prince William County")

nova_var <- nova_var |> 
  subset(name %in% nova) |> 
  mutate(period = as.yearqtr(quarter, format = "%Y Q%q"), frac = 1) |> 
  mutate(year = substr(period, 1, 4)) |> 
  mutate(year = as.numeric(year))

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

nova_var <- nova_var |> 
  left_join(cpi_ls, by = "period") |> 
  transform(adj_price = (278.47967/cpi)*med_price) 

write_rds(nova_var, "data/nova/nova_var.rds")
write_csv(nova_var, "data/nova/nova_var.csv")

nova_var_price <- read_rds("data/nova/rds/nova_var.rds") |> 
  select(geography, period, name, adj_price) |> 
  mutate(year = substr(period, 1, 4)) |> 
  mutate(year = as.numeric(year)) |> 
  filter(period == "2023 Q2" | period == "2019 Q2") |> 
  mutate(period = as.character(period)) |> 
  select(geography, name, adj_price, period)|> 
  pivot_wider(names_from = period, 
              values_from = adj_price) |> 
  janitor::clean_names() |> 
  mutate(latest = x2023_q2)|> 
  pivot_longer(3:4,
               names_to = "period",
               values_to = "med_price") |> 
  mutate(name = str_remove(name, " City"))  |> 
  mutate(name = case_when(
    name == "Fairfax" ~ "Fairfax City",
    TRUE ~ name
  )) |> 
  mutate(period = case_when(
    period == "x2023_q2" ~ "2023 Q2",
    TRUE ~ "2019 Q2"
  ))

write_rds(nova_var_price, "data/nova/rds/nova_var_price.rds")
