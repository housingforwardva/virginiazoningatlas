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

