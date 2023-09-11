library(tidyverse)
library(FinCal)

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

years <- 2016:2022

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

qcew_data <- map_dfr(hr_fips, function(hr_fips){
  yearly_data <- map_dfr(years, function(yr){
    qcew_pull <- qcewGetAreaData(yr, "a", hr_fips) |> 
      filter(own_code == 0) |> 
      filter(industry_code == 10) |> 
      select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
      mutate(year = yr)
  })
}) 

hr_oews <- qcew_data |> 
  select(fips = area_fips, year, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
  mutate(fips = as.character(fips)) |> 
  left_join(local_lookup, by = "fips")


int_rate <- 0.0718
dwn_payment <- 0.03

hr_oews <- hr_oews |> 
  mutate(third = (avg_annual_pay * .3),
         third_ho = (avg_annual_pay * .28)) |> 
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> 
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)))


write_rds(hr_oews, "data/hr/hr_oews.rds")


