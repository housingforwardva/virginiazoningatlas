library(tidyverse)
library(lubridate)
library(fredr)
library(zoo)

rva_var <- read_csv("data/home-sales.csv")

rva <- c("Richmond MSA","Washington MSA", "Virginia Beach-Norfolk-Newport News MSA", 
         "Richmond City", "Charles City County", 
          "Chesterfield City", "Hanover County", "Henrico County", 
          "Powhatan County", "New Kent County", "Goochland County")

rva_var <- rva_var |> 
  subset(name %in% rva) |> 
  mutate(period = as.yearqtr(quarter, format = "%Y Q%q"), frac = 1) |> 
  mutate(year = substr(period, 1, 4)) |> 
  mutate(year = as.numeric(year))


library(httr)
library(readxl)

url <- "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_po_state.xls"

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".xls")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Read the Excel file into a data frame
df <- read_excel(temp_file) |> 
  filter(state == "VA")

# Remove the temporary file
unlink(temp_file)

df$period <- paste0(df$yr, " ", "Q", df$qtr) 

df <- df |> mutate(period = as.yearqtr(period, format = "%Y Q%q"), frac = 1)

# gdp <- fredr(
#   series_id = "A011RE1Q156NBEA"
# ) %>% 
#   select(date, value) %>% 
#   mutate(date = as.Date(date)) %>% 
#   mutate(value = as.numeric(value)) %>% 
#   mutate(year = year(date)) |> 
#   mutate(period = as.yearqtr(date, format = "%Y Q%q", frac = 1)) |> 
#   group_by(period) |> 
#   summarise(gdp = mean(value, na.rm = TRUE))

rva_var <- rva_var |> 
  left_join(df, by = "period") |> 
  transform(adj_price = (396.29/index_nsa)*med_price) 

write_rds(rva_var, "data/planrva/rds/rva_var.rds")

