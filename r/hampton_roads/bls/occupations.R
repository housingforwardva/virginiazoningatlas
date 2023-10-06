library(tidyverse)
library(readxl)
library(FinCal)

# Occupational Employment and Wage Statistics Data is downloaded from 
# https://www.bls.gov/oes/tables.htm using the link for the latest year.
# Data should be downloaded from "All data" as xls.

# Load xlsx in and clean the data & keep needed fields.

oews_2022 <- read_xlsx("data/oews_22.xlsx",
                       na = c("*", "**", "#")) |> 
  janitor::clean_names(case = "snake") |> 
  filter(prim_state == "VA")  |> 
  filter(area_title == "Virginia Beach-Norfolk-Newport News, VA-NC") |> 
  filter(o_group == "major") |> 
  slice_max(tot_emp, n = 5) |> 
  select(area_title, 9, 10, tot_emp_2022 = 12, a_median_2022 = 28) 

# Set the interest rate based on data from Freddie Mac.
# https://www.freddiemac.com/pmms
# Note the date of the interest rate: 8/31/23
# Down payment is set as the minimum.

int_rate <- 0.0718
dwn_payment <- 0.03

hr_occ <- oews_2022 |> 
  mutate(third = (a_median_2022 * .3), # Set at 30% to follow HUD standards.
         third_ho = (a_median_2022 * .28)) |> # Set at 28% of income due to lender preference.
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> # $250 monthly tax and insurance
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)))  # 30 Year-Fixed Rate Mortgage


write_rds(hr_occ, "data/hr/hr_occ.rds")
