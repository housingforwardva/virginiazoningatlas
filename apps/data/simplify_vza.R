library(sf)
library(tidyverse)
library(geojsonsf)
library(rmapshaper)

hr_vza <- geojson_sf("data/hr_vza_nza.geojson") |> 
  mutate(region = "Hampton Roads")

nova_vza <- geojson_sf("data/nova_vza_nza.geojson") |> 
  mutate(region = "Northern Virginia")

vza <- rbind(hr_vza, nova_vza) 

sparse <- vza |>
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment, tooltipnotes,
         accessory_owner_required, accessory_family_required, accessory_elderly_only, accessory_renter_prohibited,
         acres, jurisdiction, county, customfielddata, region) |> 
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
    jurisdiction == "Surry - Surry (town)" ~ "Surry (town)",
    jurisdiction == "Isle of Wight - Smithfield" ~ "Smithfield", 
    jurisdiction == "Isle of Wight - Windsor" ~ "Windsor",
    TRUE ~ jurisdiction
  )) |> 
  filter(overlay == 0) |>  # Remove overlay districts from the analysis.
  mutate(Zoning = name,
         Abbreviation = abbrvname,
         Jurisdiction = jurisdiction,
         Notes = tooltipnotes) |> 
  group_by(jurisdiction) |> 
  mutate(total_area = sum(acres)) |> 
  mutate(accessory_treatment = case_when(
    accessory_treatment == "nomention" ~ "prohibited",
    TRUE ~ accessory_treatment
  ))

# Parse the custom field data which is provided within a single column.

sparse_clean <- sparse |> 
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


simple <- ms_simplify(sparse_clean, keep = 0.1, keep_shapes = TRUE)

write_rds(st_cast(simple, "MULTIPOLYGON"), "apps/data/vza_simple.rds")

st_write(st_cast(simple, "MULTIPOLYGON"), "apps/data/vza_simple.geojson")


mapview::mapview(simple)


g1 <- sf::st_read("apps/gloucester_zoning.geojson")
g2 <- readr::read_rds("apps/zoning_simple.rds")