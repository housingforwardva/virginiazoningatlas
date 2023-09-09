library(tidyverse)
library(tidycensus)
library(tigris)
library(mapview)

# B25061 Rent Asked
# B25056 Contract Rent

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")

b25061_vars <- load_variables(2020, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B25061")

b25061_raw <- get_acs(
  geography = "block group",
  state = "VA",
  table = "B25061",
  year = 2020,
  survey = "acs5",
  cache_table = TRUE
) |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  subset(fips %in% hr_fips)

b25061_vars_clean <- b25061_vars |> 
  separate(label, into = c("est", "total", "rent_asked"), sep = "!!") |> 
  select(variable = name, rent_asked) |>
  drop_na()

b25061_data <- b25061_raw |> 
  right_join(b25061_vars_clean, by = "variable") |> 
  select(fips, geoid = GEOID, rent = rent_asked, estimate)
  

b25056_vars <- load_variables(2020, "acs5") |>
  filter(str_sub(name, end = 6) %in% "B25056")

b25056_raw <- get_acs(
  geography = "block group",
  state = "VA",
  table = "B25056",
  year = 2020,
  survey = "acs5",
  cache_table = TRUE
) |> 
  mutate(fips = substr(GEOID, 1, 5)) |> 
  subset(fips %in% hr_fips)

b25056_vars_clean <- b25056_vars |> 
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") |> 
  select(variable = name, cash, rent)

b25056_data <- b25056_raw |> 
  right_join(b25056_vars_clean, by = "variable") |> 
  select(fips, geoid = GEOID, cash, rent, estimate) |> 
  mutate(rent = case_when(
    cash == "No cash rent" ~ "No cash rent",
    TRUE ~ rent
  )) |> 
  drop_na() |> 
  select(fips, geoid, rent, estimate)


# The median earnings in the past 12 months in 2020 dollars for individual 
# with high school degree in the VB-NORFOLK-NEWPORT NEWS, VA-NC METRO PART (Table B20004)
# was $32,507.

# Affordable rent at this median earnings is $812.67.

low_rent <- c("No cash rent", "Less than $100", "$100 to $149", "$150 to $199", 
              "$200 to $249", "$250 to $299", "$300 to $349", "$350 to $399",
              "$400 to $449", "$450 to $499", "$500 to $549", "$550 to $599",
              "$600 to $649", "$650 to $699", "$700 to $749", "$750 to $799")


affordable_units <- rbind(b25056_data, b25061_data) |> 
  group_by(fips, geoid, rent) |> 
  summarise(estimate = sum(estimate)) |> 
  subset(rent %in% low_rent) |> 
  group_by(geoid) |> 
  summarise(units = sum(estimate))


hr_od_blocks <- read_rds("data/va_od_block.rds") |> 
  mutate(geoid = substr(geoid,1, 12)) |> 
  filter(variable == "SE01") |> 
  group_by(fips, geoid, label) |> 
  summarise(estimate = sum(estimate)) |> 
  right_join(affordable_units, by = "geoid")

write_rds(hr_od_blocks, "data/hr/hr_od_blk_grps.rds")

hr_od <- read_rds("data/hr/hr_od_blk_grps.rds")

hr_fips <- c("51073", "51093", "51095","51175", "51181", "51199", "51550", "51620", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830")


local_lookup <- read_csv("data/local_lookup.csv") |> 
  select(fips = fips_full, name_long) |> 
  mutate(fips = as.character(fips)) |> 
  subset(fips %in% hr_fips)

hr_blk_grps <- block_groups("VA") |>
  mutate(geoid = GEOID) |> 
  right_join(hr_od, by = "geoid") |> 
  select(fips, geoid, units, estimate) |> 
  mutate(ratio = as.numeric(estimate/units)) |> 
  mutate(ratio = ifelse(is.finite(ratio), ratio, NA)) |> 
  left_join(local_lookup, by = "fips") |> 
  filter(geoid != "511990504011") |> 
  drop_na(name_long)

library(RColorBrewer)

rc1 <- colorRampPalette(colors = c("#8B85CA", "#ADDCB1"), space = "Lab")(1)

## Make vector of colors for values larger than 0 (180 colors)
rc2 <- colorRampPalette(colors = c("#ADDCB1", "#011E41"), space = "Lab")(50)

## Combine the two color palettes
rampcols <- c(rc1, rc2)



library(mapview)
library(leaflet)

pal <- colorNumeric(palette = rampcols, domain = (hr_blk_grps$ratio))

leaflet(hr_blk_grps,) |> 
  addPolygons(
              fillColor = ~pal(ratio),
              fillOpacity = 0.8,
              weight = 1,
              popup = paste0(
                "Jurisdiction: ",
                hr_blk_grps$name_long,
                "<br>",
                "Ratio: ", hr_blk_grps$ratio
              )
              ) |> 
  addTiles() |> 
  addLegend("bottomright", pal = pal, values = ~ratio,
            opacity = 1
  )



mapview(hr_blk_grps, zcol = "ratio")


