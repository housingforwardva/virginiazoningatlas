library(tidyverse)
library(readxl)
library(FinCal)

oews_2022 <- read_xlsx("data/oews_22.xlsx",
                       na = c("*", "**", "#")) |> 
  janitor::clean_names(case = "snake") |> 
  filter(prim_state == "VA")  |> 
  filter(area_title == "Virginia Beach-Norfolk-Newport News, VA-NC") |> 
  filter(o_group == "major") |> 
  slice_max(tot_emp, n = 5) |> 
  select(area_title, 9, 10, tot_emp_2022 = 12, a_median_2022 = 28) 


int_rate <- 0.0718
dwn_payment <- 0.03

hr_occ <- oews_2022 |> 
  mutate(third = (a_median_2022 * .3),
         third_ho = (a_median_2022 * .28)) |> 
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> 
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)))  


write_rds(hr_occ, "data/hr/hr_occ.rds")
