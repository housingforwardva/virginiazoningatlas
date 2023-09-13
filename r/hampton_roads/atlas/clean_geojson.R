library(tidyverse)
library(geojsonsf)
library(sf)

# Clean the zoning atlas data for analysis in R.

hr_vza <- geojson_sf("data/hr/hr_vza_developable.geojson") |> # Load in geojson, which has had areas of wetlands/water (10 contiguous acres or more) and federal lands clipped.
  st_drop_geometry() # Drop the geometry, as it is not needed for the analysis.


hr_vza_mapped <- hr_vza |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment,
         accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         area, jurisdiction,
         hr_custom_fields_min1, hr_custom_fields_min2, hr_custom_fields_min3, hr_custom_fields_min4, hr_custom_fields_sfd) |> 
  mutate(type = case_when( # Correct the labels for Type of Zoning District field.
    type == "X" ~ "Nonresidential",
    type == "R" ~ "Primarily Residential",
    type == "M" ~ "Mixed with Residential",
    type == "O" ~ "Overlay Not Affecting Use"
  )) |> 
  mutate(jurisdiction = case_when( # Fix the label for the Town of Surry.
    jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
    TRUE ~ jurisdiction
  )) |> 
  mutate(sfd = str_trim(hr_custom_fields_sfd))  # Trim whitespace from the sfd column.

hr_vza_geo <- geojson_sf("data/hr/hr_vza_developable.geojson") # Load in geojson, which has had areas of wetlands/water (10 contiguous acres or more) and federal lands clipped.

hr_vza_geomap <- hr_vza_geo |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment,
         accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         area, jurisdiction, tooltipnotes,
         hr_custom_fields_min1, hr_custom_fields_min2, hr_custom_fields_min3, hr_custom_fields_min4, hr_custom_fields_sfd) |> 
  mutate(type = case_when( # Correct the labels for Type of Zoning District field.
    type == "X" ~ "Nonresidential",
    type == "R" ~ "Primarily Residential",
    type == "M" ~ "Mixed with Residential",
    type == "O" ~ "Overlay Not Affecting Use"
  )) |> 
  mutate(jurisdiction = case_when( # Fix the label for the Town of Surry.
    jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
    TRUE ~ jurisdiction
  )) |> 
  mutate(sfd = str_trim(hr_custom_fields_sfd))  # Trim whitespace from the sfd column.


hr_vza_raw <- hr_vza |> 
  mutate(type = case_when( # Correct the labels for Type of Zoning District field.
    type == "X" ~ "Nonresidential",
    type == "R" ~ "Primarily Residential",
    type == "M" ~ "Mixed with Residential",
    type == "O" ~ "Overlay Not Affecting Use"
  )) |> 
  mutate(jurisdiction = case_when( # Fix the label for the Town of Surry.
    jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
    TRUE ~ jurisdiction
  )) |> 
  mutate(sfd = str_trim(hr_custom_fields_sfd))  # Trim whitespace from the sfd column.


write_rds(hr_vza_mapped, "data/hr/hr_vza_nogeo.rds", compress = "none") # Write to rds.
write_rds(hr_vza_geomap, "data/hr/hr_vza_geo.rds", compress = "none")
write_csv(hr_vza_raw, "data/hr/hr_vza_data.csv")
