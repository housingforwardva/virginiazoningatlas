library(tidyverse)


hr_byright2 <- read_rds("data/hr/hr_vza_nogeo.rds") |> 
  select(id, jurisdiction, abbrvname, overlay, 6:9, area) |> # Select data fields.
  filter(overlay == 0) |> # Filter out overlay districts.
  filter(family2_treatment == "allowed" & family3_treatment == "allowed" & family4_treatment == "allowed")


write_csv(hr_byright2, "data/hr/hr_byright2.csv")

