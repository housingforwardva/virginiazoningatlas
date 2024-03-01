library(tidyverse)
library(geojsonsf)
library(sf)

# Clean the zoning atlas data for analysis in R. 

# Create a data table with zoning analysis data without geometry ----

hr_vza <- geojson_sf("data/hr/hr_vza_ogr_no_geom.geojson") |> 
  st_drop_geometry()

hr_punched <- geojson_sf("data/hr/hr_vza.geojson")|> 
  select(abbrvname = `Abbreviated District Name`, jurisdiction = Jurisdiction, county = County, acres) |> 
  left_join(hr_vza, by = c('abbrvname', 'jurisdiction', 'county'))


# Data prep

hr_vza_data <- hr_punched |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment,
         accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         acres, jurisdiction, county, customfielddata) |> 
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
  filter(overlay == 0) # Remove overlay districts from the analysis.

hr_vza_clean_data <- hr_vza_data |> 
  separate(customfielddata, into = c("min1", "min2", "min3", "min4", "sfd", "thonly"), sep = ',') |> 
  mutate(across(min1:thonly, .fns = ~str_remove_all(.x, '"')),
         across(min1:thonly, .fns = ~str_remove_all(.x, "\\{")),
         across(min1:thonly, .fns = ~str_remove_all(.x, "\\}")),
         across(min1:thonly, .fns = ~str_remove_all(.x, "41_116:")),
         across(min1:thonly, .fns = ~str_remove_all(.x, '41_117:')),
         across(min1:thonly, .fns = ~str_remove_all(.x, '41_118:')),
         across(min1:thonly, .fns = ~str_remove_all(.x, '41_119:')),
         across(min1:thonly, .fns = ~str_remove_all(.x, '45_121:')),
         across(min1:thonly, .fns = ~str_remove_all(.x, '45_122:'))) |> 
  mutate(across(min1:thonly, .fns = ~str_trim(.x)))



hr_vza_nogeo <- hr_vza_clean_data |> 
  st_drop_geometry() 


st_write(hr_vza_clean_data, "data/hr/hr_vza_geo.geojson", append = FALSE)
write_rds(hr_vza_clean_data, "data/hr/hr_vza_geo.rds", compress = "none") 
write_rds(hr_vza_nogeo, "data/hr/hr_vza_nogeo.rds", compress = "none")


# hr_vza <- geojson_sf("data/hr/hr_vza_developable.geojson") |> # Load in geojson, which has had areas of wetlands/water (10 contiguous acres or more) and federal lands clipped.
#   st_drop_geometry() # Drop the geometry, as it is not needed for the analysis.

# hr_vza_data <- hr_vza |> 
#   select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
#          family1_treatment, family2_treatment, family3_treatment, family4_treatment,
#          accessory_treatment, plannedresidential_treatment,
#          accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
#          area, jurisdiction,
#          hr_custom_fields_min1, hr_custom_fields_min2, hr_custom_fields_min3, hr_custom_fields_min4, hr_custom_fields_sfd) |> 
#   mutate(type = case_when( # Correct the labels for Type of Zoning District field.
#     type == "X" ~ "Nonresidential",
#     type == "R" ~ "Primarily Residential",
#     type == "M" ~ "Mixed with Residential",
#     type == "O" ~ "Overlay Not Affecting Use"
#   )) |> 
#   mutate(jurisdiction = case_when( # Fix the label for the Town of Surry.
#     jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
#     TRUE ~ jurisdiction
#   )) |> 
#   mutate(sfd = str_trim(hr_custom_fields_sfd))  # Trim whitespace from the sfd column.

# Create a data table with zoning analysis data with geometry ----

# hr_vza_geo <- geojson_sf("data/hr/hr_vza_developable.geojson") # Load in geojson, which has had areas of wetlands/water (10 contiguous acres or more) and federal lands clipped.
# 
# hr_vza_geomap <- hr_vza_geo |> 
#   select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
#          family1_treatment, family2_treatment, family3_treatment, family4_treatment,
#          accessory_treatment, plannedresidential_treatment,
#          accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
#          area, jurisdiction, tooltipnotes,
#          hr_custom_fields_min1, hr_custom_fields_min2, hr_custom_fields_min3, hr_custom_fields_min4, hr_custom_fields_sfd) |> 
#   mutate(type = case_when( # Correct the labels for Type of Zoning District field.
#     type == "X" ~ "Nonresidential",
#     type == "R" ~ "Primarily Residential",
#     type == "M" ~ "Mixed with Residential",
#     type == "O" ~ "Overlay Not Affecting Use"
#   )) |> 
#   mutate(jurisdiction = case_when( # Fix the label for the Town of Surry.
#     jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
#     TRUE ~ jurisdiction
#   )) |> 
#   mutate(sfd = str_trim(hr_custom_fields_sfd))  # Trim whitespace from the sfd column.
# 
# hr_vza_raw <- hr_vza |> 
#   mutate(type = case_when( # Correct the labels for Type of Zoning District field.
#     type == "X" ~ "Nonresidential",
#     type == "R" ~ "Primarily Residential",
#     type == "M" ~ "Mixed with Residential",
#     type == "O" ~ "Overlay Not Affecting Use"
#   )) |> 
#   mutate(jurisdiction = case_when( # Fix the label for the Town of Surry.
#     jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
#     TRUE ~ jurisdiction
#   )) |> 
#   mutate(sfd = str_trim(hr_custom_fields_sfd))  # Trim whitespace from the sfd column.
# 
# # Write to rds.
# 
# write_rds(hr_vza_data, "data/hr/hr_vza_nogeo.rds", compress = "none") 
# write_rds(hr_vza_geomap, "data/hr/hr_vza_geo.rds", compress = "none")
# write_csv(hr_vza_raw, "data/hr/hr_vza_data.csv")
