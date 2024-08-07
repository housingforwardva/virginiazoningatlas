library(tidyverse)
library(geojsonsf)
library(sf)

# Prep the PlanRVA region

# Read in the geojson provided by the NZA, which already has "undevelopable" land removed.

rva_punched <- geojson_sf("data/rva/geo/xxx.geojson") 

# Data prep

rva_vza_data <- rva_punched |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment,
         # accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         acres, jurisdiction, county, customfielddata, tooltipnotes, expired) |> 
  mutate(type = case_when( # Correct the labels for Type of Zoning District field.
    type == "Non-Residential" ~ "Nonresidential",
    type == "Residential" ~ "Primarily Residential",
    type == "Mixed" ~ "Mixed with Residential",
    type == "Overlay Not Affecting Use" ~ "Overlay Not Affecting Use"
  )) |>
  mutate(jurisdiction = case_when( # Reformat town names.
    jurisdiction == "Hanover - Ashland" ~ "Ashland",
    TRUE ~ jurisdiction
  )) |> 
  filter(overlay == FALSE) |> # Remove overlay districts from the analysis.
  mutate(acres = as.numeric(acres))

# Parse the custom field data which is provided within a single column.

rva_vza_clean_data <- rva_vza_data |> 
  separate(customfielddata, into = c("min1", "min2", "min3", "min4", "max1", "max2", "max3", "max4", "sfd", "thonly"), sep = ',') |> 
  mutate(across(min1:thonly, .fns = ~str_remove_all(.x, '"')),
         across(min1:thonly,.fns = ~str_remove_all(.x, "\\{")),
         across(min1:thonly,.fns = ~str_remove_all(.x, "\\}")),
         across(min1:thonly,.fns = ~str_remove_all(.x, "41_116:")),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_117:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_118:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_119:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_125:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_126:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_127:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '41_128:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '45_121:')),
         across(min1:thonly,.fns = ~str_remove_all(.x, '45_122:'))) |> 
  mutate(across(min1:thonly,.fns = ~str_trim(.x))) 


rva_vza_nogeo <- rva_vza_clean_data |> 
  st_drop_geometry() 

# Filter for Mercatus Center data quality check
# 

# mercatus_rva <- rva_vza_nogeo |> 
#   filter(sfd == "f") |> 
#   filter(family4_treatment == "prohibited") |> 
#   filter(family3_treatment == "prohibited") |> 
#   filter(family2_treatment == "prohibited") |> 
#   filter(family1_treatment == "allowed")
# 
# write_csv(mercatus_rva, "data/rva/csv/mercatus_sf.csv")


st_write(rva_vza_clean_data, "data/rva/geo/rva_vza_geo.geojson", append = FALSE)
write_rds(rva_vza_clean_data, "data/rva/rds/rva_vza_geo.rds", compress = "none")
write_rds(rva_vza_nogeo, "data/rva/rds/rva_vza_nogeo.rds", compress = "none")

