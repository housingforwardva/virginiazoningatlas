library(tidyverse)
library(FinCal)


nova_fips <- c("51013", "51059", "51107", "51153", "51510", "51600", "51610", "51683", "51685")

local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% nova_fips)

years <- 2016:2022

# Create function to pull QCEW data.

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# Pull data for all years and areas.

qcew_data <- map_dfr(nova_fips, function(nova_fips){
  yearly_data <- map_dfr(years, function(yr){
    qcew_pull <- qcewGetAreaData(yr, "a", nova_fips) |> 
      filter(own_code == 0) |> 
      filter(industry_code == 10) |> 
      select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
      mutate(year = yr)
  })
}) 

# Select necessary fields and then join to lookup table.

nova_oews <- qcew_data |> 
  select(fips = area_fips, year, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
  mutate(fips = as.character(fips)) |> 
  left_join(local_lookup, by = "fips")


int_rate <- 0.0718
dwn_payment <- 0.03

nova_oews <- nova_oews |> 
  mutate(third = (avg_annual_pay * .3),
         third_ho = (avg_annual_pay * .28)) |> 
  mutate(rent = (third/12)) |> 
  mutate(ho = (third_ho/12) - 250) |> 
  mutate(ho_price = abs(pv(int_rate/12, 360, 0, ho, 0)))


write_rds(nova_oews, "data/nova/nova_oews.rds")


