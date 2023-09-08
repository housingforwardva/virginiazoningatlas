library(shiny)
library(rdeck)
library(bslib)
library(sf)
library(htmltools)
library(shinyWidgets)
library(janitor)

# zoning <- st_read("gloucester_zoning.geojson", quiet = TRUE) |> 
#   clean_names() %>%
#   dplyr::mutate(fill_color = dplyr::case_when(
#     type_of_zoning_district == "Primarily Residential" ~ "#40C0C0",
#     type_of_zoning_district == "Mixed with Residential" ~ "#A29DD4",
#     type_of_zoning_district == "Nonresidential" ~ "#999999"
#   )) %>%
#   dplyr::select(type_of_zoning_district,
#                 fill_color,
#                 x1_family_treatment,
#                 full_district_name,
#                 overlay)

zoning <- readr::read_rds("zoning_simple.rds") |>
  st_cast("MULTIPOLYGON")

# pal <- colorFactor(palette = c("#40C0C0", "#A29DD4", "#999999"),
#                    levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"))

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
      rdeckOutput("map", width = "95%", height = "95vh")
    )
  )
)

server <- function(input, output, session) {
  

  
  output$map <- renderRdeck({
    rdeck(map_style = mapbox_light(), 
          initial_view_state = view_state(center = c(-76.6814467, 37.4091757),
                                          zoom = 11)) %>%
      add_polygon_layer(data = zoning, get_fill_color = fill_color, opacity = 0.8, 
                        id = "zoning_layer", get_polygon = geometry, get_line_color = "#ffffff",
                        get_line_width = 10)
  })
  
  observeEvent(input$opacity, {
    rdeck_proxy("map") %>%
      update_polygon_layer(id = "zoning_layer",
                  opacity = input$opacity / 100)
  })
  
  filtered_zoning <- reactive({
    if (input$single_family) {
      output <- dplyr::filter(zoning, x1_family_treatment %in% input$single_family_options)
    }
  })
  
  observeEvent(filtered_zoning(), {
    rdeck_proxy("map") %>%
      update_polygon_layer(data = filtered_zoning(), id = "zoning_layer")
  })
  
}

shinyApp(ui = ui, server = server)