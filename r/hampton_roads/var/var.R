library(tidyverse)
library(lubridate)
library(fredr)

hr_var <- read_csv("data/home-sales.csv")

hr <- c("Virginia Beach-Norfolk-Newport News MSA", "Chesapeake City", "Hampton City", 
        "Gloucester County", "Portsmouth City", "Newport News City", "Norfolk City", 
        "Virginia Beach City", "Suffolk City", "Surry County", "York County", "Williamsburg City",
        "Isle of Wight County", "Poquoson City", "James City County")

hr_var <- hr_var |> 
  subset(name %in% hr) |> 
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

hr_var <- hr_var |> 
  left_join(cpi_ls, by = "period") |> 
  transform(adj_price = (278.47967/cpi)*med_price) 

write_rds(hr_var, "data/hr/hr_var.rds")

