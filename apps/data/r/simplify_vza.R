library(sf)
library(tidyverse)
library(geojsonsf)
library(rmapshaper)

hr_vza <- geojson_sf("apps/data/raw/hr_vza_geo.geojson") |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment, tooltipnotes,
         acres, jurisdiction, county, sfd) |> 
  mutate(region = "Hampton Roads") |> 
  ms_simplify(keep = 0.1, keep_shapes = TRUE)
  

nova_vza <- geojson_sf("apps/data/raw/nova_vza_geo.geojson") |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment, tooltipnotes,
         acres, jurisdiction, county, sfd) |> 
  mutate(region = "Northern Virginia") |> 
  ms_simplify(keep = 0.1, keep_shapes = TRUE)

rva_vza <- geojson_sf("apps/data/raw/rva_vza_geo.geojson") |> 
  select(id, type, abbrvname, name, overlay, # Select the fields needed for analysis.
         family1_treatment, family2_treatment, family3_treatment, family4_treatment,
         accessory_treatment, plannedresidential_treatment, tooltipnotes,
         acres, jurisdiction, county, sfd) |> 
  mutate(region = "PlanRVA") |> 
  ms_simplify(keep = 0.1, keep_shapes = TRUE)

vza <- rbind(hr_vza, nova_vza, rva_vza) 

sparse <- vza |>
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
  mutate(Zoning = name,
         Abbreviation = abbrvname,
         Jurisdiction = jurisdiction,
         Notes = tooltipnotes) |> 
  group_by(jurisdiction) |> 
  mutate(total_area = sum(acres)) |> 
  mutate(accessory_treatment = case_when(
    accessory_treatment == "nomention" ~ "prohibited",
    TRUE ~ accessory_treatment
  )) |>
  st_cast("MULTIPOLYGON") %>%
  mutate(fill_color = case_when(
    type == "Primarily Residential" ~ "#8B85CA",
    type == "Mixed with Residential" ~ "#40C0C0",
    type == "Nonresidential" ~ "#011E41",
    TRUE ~ "#FFFFFF"
  ), 
  highlight_color = case_when(
    type == "Primarily Residential" ~ "#8B85CAff",
    type == "Mixed with Residential" ~ "#40C0C0ff",
    type == "Nonresidential" ~ "#011E41ff",
    TRUE ~ "#FFFFFFff"
  )) |> 
  st_make_valid()


# simple <- ms_simplify(sparse, keep = 0.1, keep_shapes = TRUE)

write_rds(st_cast(sparse, "MULTIPOLYGON"), "apps/data/vza_simple.rds")

# st_write(st_cast(simple, "MULTIPOLYGON"), "apps/data/vza_simple.geojson", append = FALSE)
# 
# 
# mapview::mapview(simple)

