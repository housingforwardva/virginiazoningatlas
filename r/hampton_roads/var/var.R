library(tidyverse)
library(lubridate)

hr_var <- read_csv("data/home-sales.csv")

hr <- c("Virginia Beach-Norfolk-Newport News MSA", "Chesapeake City", "Hampton City", 
        "Gloucester County", "Portsmouth City", "Newport News City", "Norfolk City", 
        "Virginia Beach City", "Suffolk City", "Surry County", "York County", "Williamsburg City",
        "Isle of Wight County", "Poquoson City", "James City County")


hr_var <- hr_var |> 
  subset(name %in% hr) |> 
  mutate(period = as.Date(as.yearqtr(quarter, format = "%Y Q%q"), frac = 1))

write_rds(hr_var, "data/hr/hr_var.rds")

