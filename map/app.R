#
# This is a sandbox app for developing
# the Virginia Zoning Atlas using a single 
# locality as a demo.

library(shiny)
library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(shinyWidgets)

zoning <- st_read("gloucester_zoning.geojson", quiet = TRUE) |> 
  clean_names()
  

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarLayout(
      mainPanel = leafletOutput(outputId = 'map'),
        sidebarPanel(
          selectInput(
            inputId = "pick_one",
            label = "1-Family Treatment",
            choices = c(" ", "Allowed/Condition", "Public Hearing", "Prohibited", "Overlay")
          ),
          selectInput(
            inputId = "pick_two",
            label = "2-Family Treatment",
            choices = c(" ", "Allowed/Condition", "Public Hearing", "Prohibited", "Overlay")
          ),
          selectInput(
            inputId = "pick_three",
            label = "3-Family Treatment",
            choices = c(" ", "Allowed/Condition", "Public Hearing", "Prohibited", "Overlay")
          ),
          pickerInput(
            inputId = "pick_four",
            label = "4+-Family Treatment",
            choices = c(" ", "Allowed/Condition", "Public Hearing", "Prohibited", "Overlay")
          )))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pal <- colorFactor(palette = c("#40C0C0", "#A29DD4", "#999999"),
                     levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"))
  
  map <- reactive({
    zoning 
      
  })
  
output$map <- renderLeaflet({
  
  leaflet(map()) |> 
    addProviderTiles("CartoDB.Positron") |> 
    addPolygons(color = "#ffffff", weight = 1, smoothFactor = 0.5,
                fillColor = ~pal(type_of_zoning_district),
                fillOpacity = 0.8)


})
}

# Run the application 
shinyApp(ui = ui, server = server)
