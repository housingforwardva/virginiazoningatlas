library(tidyverse)
library(geojsonsf)
library(sf)

# Prep the PlanRVA region

# Read in the geojson provided by the NZA, which already has "undevelopable" land removed.

rva_punched <- geojson_sf("data/planrva/geo/rva_vza.geojson") 

rva_custom <- geojson_sf("data/planrva/geo/rva_vza_customdata.geojson") |> 
  janitor::clean_names() |> 
  select(abbrvname = abbreviated_district_name, jurisdiction, county, customfielddata) |> 
  st_drop_geometry()


rva_custom_punched <- rva_punched |> 
  left_join(rva_custom, by = c("abbrvname", "jurisdiction", "county"))

rva_cfd <- read_csv("data/planrva/csv/planrva_cfd.csv") |> 
  janitor::clean_names() |> 
  select(abbrvname = abbreviated_district_name, jurisdiction, county, customfielddata) |>  
  separate(customfielddata, into = c("min1", "min2", "min3", "min4", "max1", "max2", "max3", "max4", "sfd", "thonly", "adu_def"), sep = ',') |> 
  mutate(across(.fns = ~str_remove_all(.x, '"')),
         across(.fns = ~str_remove_all(.x, "\\{")),
         across(.fns = ~str_remove_all(.x, "\\}"))) |> 
  mutate(across(min1:adu_def,.fns = ~str_remove_all(.x, "41_116:")),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_117:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_118:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_119:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_125:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_126:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_127:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '41_128:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '45_121:')),
         across(min1:adu_def,.fns = ~str_remove_all(.x, '45_122:')),
         across(min1:adu_def, .fns = ~str_remove_all(.x,'50_135:'))) |> 
  mutate(across(min1:adu_def,.fns = ~str_trim(.x))) |> 
  mutate(across(min1:max4, .fns = ~as.numeric(.x)))

rva_punched_cfd <- rva_punched |> 
  inner_join(rva_cfd,
            by = c("abbrvname", "jurisdiction"))


head <- head(rva_punched_cfd, 5)

# Data prep

rva_vza_data <- rva_punched_cfd |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment,
         # accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         acres, jurisdiction, county = county.y, tooltipnotes, expired,
         min1, min2, min3, min4, max1, max2, max3, max4, sfd, thonly, adu_def) |> 
  mutate(type = case_when( # Correct the labels for Type of Zoning District field.
    type == "X" ~ "Nonresidential",
    type == "R" ~ "Primarily Residential",
    type == "M" ~ "Mixed with Residential",
    type == "O" ~ "Overlay Not Affecting Use")) |>
  mutate(jurisdiction = case_when( # Reformat town names.
    jurisdiction == "Hanover - Ashland" ~ "Ashland",
    TRUE ~ jurisdiction)) |> 
  filter(overlay == 0) |> # Remove overlay districts from the analysis.
  mutate(acres = as.numeric(acres))

# write_csv(data, "data/planrva/csv/rva_vza.csv")

# Parse the custom field data which is provided within a single column.


five <- head(rva_vza_clean_data, 20)

rva_vza_nogeo <- rva_vza_clean_data |> 
  st_drop_geometry() 


st_write(rva_vza_clean_data, "data/planrva/geo/rva_vza_geo.geojson", append = FALSE)
write_rds(rva_vza_clean_data, "data/planrva/rds/rva_vza_geo.rds", compress = "none")
write_rds(rva_vza_nogeo, "data/planrva/rds/rva_vza_nogeo.rds", compress = "none")

