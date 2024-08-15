library(tidyverse)
library(lubridate)
library(fredr)
library(zoo)

rva_var <- read_csv("data/home-sales.csv")

rva <- c("Richmond MSA","Richmond City", "Charles City County", 
          "Chesterfield City", "Hanover County", "Henrico County", 
          "Powhatan County", "New Kent County", "Goochland County")

rva_var <- rva_var |> 
  subset(name %in% rva) |> 
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

rva_var <- rva_var |> 
  left_join(cpi_ls, by = "period") |> 
  transform(adj_price = (284.16933/cpi)*med_price) 

write_rds(rva_var, "data/planrva/rds/rva_var.rds")

