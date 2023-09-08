library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(htmltools)
library(shinyWidgets)
library(janitor)

zoning <- readr::read_rds("zoning_simple.rds") |>
  st_cast("MULTIPOLYGON")

ui <- page_fluid(
  # Ensure the content takes the full width and height of the screen
  tags$head(
    tags$style(HTML("
      /* Full-screen map styling */
      #map {
        background-color: #e5e5e5; /* Placeholder background color for the map */
        position: absolute;
      }

      /* Style for the floating menu */
      .floating-panel {
        z-index: 1000;
        background-color: rgba(255, 255, 255, 0.5); /* Semi-transparent background */
      }

      /* Hover effect for the floating menu */
      .floating-panel:hover {
        background-color: rgba(255, 255, 255, 0.8); /* Less transparent background on hover */
      }
    "))
  ),
  fluidRow(
    style = "height: 100%", 
    absolutePanel(
      width = "300px",
      class = "floating-panel",
      top = 80, left = 20, 
      p("This is a placeholder for text about the Virginia Zoning Atlas"),
      checkboxInput("single_family", "1-Family Housing"),
      conditionalPanel(
        condition = "input.single_family == true",
        checkboxGroupInput("single_family_options",
                            label = NULL,
                            choices = c("Allowed/Conditional", "Public Hearing"), 
                            selected = c("Allowed/Conditional", "Public Hearing"))
      ),
      checkboxInput("two_family", "2-Family Housing"),
      conditionalPanel(
        condition = "input.two_family == true",
        checkboxGroupInput("two_family_options",
                           label = NULL,
                           choices = c("Allowed/Conditional", "Public Hearing", "Prohibited"), 
                           selected = c("Allowed/Conditional", "Public Hearing", "Prohibited"))
      ),
      checkboxInput("three_family", "3-Family Housing"),
      conditionalPanel(
        condition = "input.three_family == true",
        checkboxGroupInput("three_family_options",
                           label = NULL,
                           choices = c("Allowed/Conditional", "Public Hearing", "Prohibited"), 
                           selected = c("Allowed/Conditional", "Public Hearing", "Prohibited"))
      ),
      checkboxInput("four_family", "4+ Family Housing"),
      conditionalPanel(
        condition = "input.four_family == true",
        checkboxGroupInput("four_family_options",
                           label = NULL,
                           choices = c("Allowed/Conditional", "Public Hearing"), 
                           selected = c("Allowed/Conditional", "Public Hearing"))
      ),
      chooseSliderSkin("Flat", color = "#40C0C0"),
      sliderInput("opacity", "Zone opacity", min = 0, max = 100, 
                  value = 80, ticks = FALSE)
    ), 
    mainPanel(
      width = 12,
      leafletOutput("map", width = "95%", height = "95vh")
    )
  )
)

server <- function(input, output, session) {
  
  pal <- colorFactor(palette = c("#40C0C0", "#A29DD4", "#999999"),
                     levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"))
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      setView(lng = -76.6814467, lat = 37.4091757, zoom = 11) %>%
      addPolygons(data = zoning, color = "#ffffff", weight = 1, smoothFactor = 0.5,
                  fillColor = ~pal(type_of_zoning_district),
                  fillOpacity = 0.8)
  })
  
  observeEvent(input$opacity, {
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = zoning, color = "#ffffff", weight = 1, smoothFactor = 0.5,
                  fillColor = ~pal(type_of_zoning_district),
                  fillOpacity = input$opacity / 100)
  })
  
  filtered_zoning <- reactive({
    if (input$single_family) {
      output <- dplyr::filter(zoning, x1_family_treatment %in% input$single_family_options)
    }
  })
  
  observeEvent(filtered_zoning(), {
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = filtered_zoning(), color = "#ffffff", weight = 1, smoothFactor = 0.5,
                  fillColor = ~pal(type_of_zoning_district),
                  fillOpacity = input$opacity / 100)
  })
  
}

shinyApp(ui = ui, server = server)