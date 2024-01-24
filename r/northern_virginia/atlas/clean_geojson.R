library(tidyverse)
library(geojsonsf)
library(sf)

# Prep the Northern Virginia Zoning Atlas data for analysis.

# Read in the geojson provided by the NZA, which already has "undevelopable" land removed.

nova_vza <- geojson_sf("data/nova/nova_vza_nza.geojson") 

# Data prep

nova_vza_data <- nova_vza |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment,
         accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         acres, jurisdiction, customfielddata) |> 
  mutate(type = case_when( # Correct the labels for Type of Zoning District field.
    type == "X" ~ "Nonresidential",
    type == "R" ~ "Primarily Residential",
    type == "M" ~ "Mixed with Residential",
    type == "O" ~ "Overlay Not Affecting Use"
  )) |>  
  mutate(jurisdiction = case_when( # Reformat town names.
    jurisdiction == "Prince William - Dumfries" ~ "Dumfries",
    jurisdiction == "Prince William - Occoquan" ~ "Occoquan",
    jurisdiction == "Prince William - Haymarket" ~ "Haymarket",
    jurisdiction == "Prince William - Quantico" ~ "Quantico",
    jurisdiction == "Fairfax - Vienna" ~ "Vienna",
    jurisdiction == "Fairfax - Clifton" ~ "Clifton", 
    jurisdiction == "Loudoun - Hamilton" ~ "Hamilton",
    jurisdiction == "Loudoun - Purcellville" ~ "Purcellville",
    jurisdiction == "Loudoun - Lovettsville" ~ "Lovettsville",
    jurisdiction == "Loudoun - Round Hill" ~ "Round Hill",
    jurisdiction == "Loudoun - Hillsboro" ~ "Hillsboro",
    jurisdiction == "Loudoun - Leesburg" ~ "Leesburg",
    jurisdiction == "Loudoun - Middleburg" ~ "Middleburg",
    TRUE ~ jurisdiction
  )) |> 
  filter(overlay == 0) # Remove overlay districts from the analysis.

# Parse the custom field data which is provided within a single column.

nova_vza_clean_data <- nova_vza_data |> 
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


nova_vza_nogeo <- nova_vza_clean_data |> 
  st_drop_geometry()



write_rds(nova_vza_clean_data, "data/nova/nova_vza_geo.rds", compress = "none") 
write_rds(nova_vza_nogeo, "data/nova/nova_vza_nogeo.rds", compress = "none")

