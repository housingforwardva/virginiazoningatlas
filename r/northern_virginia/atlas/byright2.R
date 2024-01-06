library(tidyverse)

# The following code filters for zoning districts that allow for 2+ family housing by-right,
# which acts as a proxy for "Missing Middle" housing.


nova_byright2 <- read_rds("data/nova/nova_vza_nogeo.rds") |> 
  select(id, jurisdiction, abbrvname, overlay, 6:9, area) |> # Select data fields.
  filter(overlay == 0) |> # Filter out overlay districts.
  filter(family2_treatment == "allowed" & family3_treatment == "allowed" & family4_treatment == "allowed")


write_csv(nova_byright2, "data/nova/nova_byright2.csv")

