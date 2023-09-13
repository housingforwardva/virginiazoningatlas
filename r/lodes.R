install.packages("devtools", repos = "http://cran.us.r-project.org")
devtools::install_github("jamgreen/lehdr")

library(tigris)
library(tidyverse)
library(sf)
library(mapview)


library(lehdr)
va_od_county <- grab_lodes(
       state = "va",
       year = 2020,
       lodes_type = "od", # origin-destination table
       job_type = "JT00", # all jobs
       agg_geo = "county",
       state_part = "main", # workers who reside and work in state
       use_cache = TRUE
   )

write_rds(va_od_county, "data/va_od_county.rds")

va_od_block <- grab_lodes(
       state = "va",
       year = 2020,
       lodes_type = "od", # origin-destination table
       job_type = "JT00", # all jobs
       agg_geo = "block",
       state_part = "main", # workers who reside and work in state
       use_cache = TRUE
   )

va_od_block_label <- va_od_block |> 
  select(year, state, geoid = h_geocode, 3:12) |> 
  pivot_longer(4:13,
               names_to = "variable",
               values_to = "estimate") |> 
  mutate(fips = substr(geoid, 1, 5)) |> 
  mutate(label = case_when(
    variable == "S000" ~ "Total number of jobs",
    variable == "SA01" ~ "Number of jobs of workers age 29 or younger",
    variable == "SA02" ~ "Number of jobs of workers age 30 to 54",
    variable == "SA03" ~ "Number of jobs for workers age 55 or older",
    variable == "SE01" ~ "Number of jobs with earnings $1250/month or less",
    variable == "SE02" ~ "Number of jobs with earnings $1251/month to $3333/month",
    variable == "SE03" ~ "Number of jobs with earnings greater than $3333/month",
    variable == "SI01" ~ "Number of jobs in Goods Producing industry sectors",
    variable == "SI02" ~ "Number of jobs in Trade, Transportation, and Utilities industry sectors",
    variable == "SI03" ~ "Number of jobs in All Other Services industry sectors"
  ))
    


write_rds(va_od_block_label,"data/va_od_block.rds")
