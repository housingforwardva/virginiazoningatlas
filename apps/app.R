library(shiny)
library(shinyalert)
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

# subs <- readr::read_rds("data/subdivisions.rds")

MAPBOX_ACCESS_TOKEN = Sys.getenv("MAPBOX_ACCESS_TOKEN")

zoning <- readr::read_rds("data/vza_simple.rds") |>
  st_cast("MULTIPOLYGON") %>%
    dplyr::mutate(fill_color = dplyr::case_when(
      type == "R" ~ "#8B85CA",
      type == "M" ~ "#40C0C0",
      type == "X" ~ "#011E41",
      TRUE ~ "#FFFFFF"
    ), 
    highlight_color = dplyr::case_when(
      type == "R" ~ "#8B85CAff",
      type == "M" ~ "#40C0C0ff",
      type == "X" ~ "#011E41ff",
      TRUE ~ "#FFFFFFff"
    )) 

fed <- sf::st_read("data/protected_lands.geojson")

flood <- sf::st_read("data/flood.geojson")

transit <- sf::st_read("data/hr_transit_all.geojson")

juris <- sf::st_read("data/boundaries.geojson")

type_choices <- c(
  "Prohibited" = "prohibited",
  "Allowed/Conditional" = "allowed",
  "Public Hearing" = "hearing"
)

accessory_choices <- c(
  "Allowed As of Right" = "allowed",
  "Allowed Only After Public Hearing" = "hearing",
  "Prohibited" = "prohibited"
)

base_map <- c(
  "Light" = mapbox_light(),
  "Satellite" = mapbox_satellite()
)

base_selected <- "Light"

local_list <- sort(unique(zoning$jurisdiction))


# pal <- colorFactor(palette = c("#40C0C0", "#A29DD4", "#999999"),
#                    levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"))

ui <- page_fluid(
  
  tags$head(
    
    # Add Google Analytics script (https://shiny.posit.co/r/articles/build/google-analytics/)
    
    includeHTML("vza-google-analytics.html"),
    
    # Ensure the content takes the full width and height of the screen
    
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
      
      .shiny-input-container {
          margin-bottom: 2px; /* Adjust this value as desired */
      }
      
      .checkbox-opts .shiny-options-group {
        margin-left: 10px;  /* Adjust as needed for indentation */
        font-size: 0.8em;   /* Adjust as needed for font size */
        margin-bottom: 2px;
      }
      
      .my-legend {
        display: block;
        margin-bottom: 10px;
      }
      
      .my-legend .legend-scale ul {
        margin: 0;
        margin-bottom: 5px;
        padding: 0;
        float: left;
        list-style: none;
        display: block;
      }
        
      .my-legend .legend-scale ul li {
        font-size: 80%;
        list-style: none;
        margin-left: 0;
        line-height: 18px;
        margin-bottom: 2px;
        display: block;
      }
        
      .my-legend ul.legend-labels li span {
        display: block;
        float: left;
        height: 16px;
        width: 30px;
        margin-right: 5px;
        margin-left: 0;
        border: 1px solid #999;
      }
        
      .my-legend .legend-source {
        font-size: 70%;
        color: #999;
        clear: both;
      }
        
      .my-legend a {
        color: #777;
      }
        
      
      .selectize-dropdown {
        font-size: 0.8em;
      } 
      
    "))
  ),
  fluidRow( 
    style = "height: 100%", 
    absolutePanel(
      width = "150px",
      class = "floating-panel",
      bottom = 50, right = 20, 
      selectInput("basemap", "Choose basemap",
                  choices = base_map,
                  selected = base_selected)),
    absolutePanel(
      width = "300px",
      class = "floating-panel",
      top = 20, left = 20, 
      img(src = "hfv_logo.png", style = "width: 150px;"),
      p("This interactive map shows how outdated zoning laws make it hard to build diverse, affordable housing."),
      p("Use checkboxes below to filter zoning districts in the map.", style = "font-size: 80%;"),
      p("Type of Zoning District", style = "color: gray; font-weight: bold; margin-bottom: 5px;"),
      div(class = "my-legend",
          HTML(
            "
            <div class='legend-scale'>
              <ul class='legend-labels'>
                <li><span style='background:#8B85CA;'></span>Primarily Residential</li>
                <li><span style='background:#40C0C0;'></span>Mixed with Residential</li>
                <li><span style='background:#011E41ff;'></span>Nonresidential</li>
              </ul>
            </div>
            "
          )),
      br(),
      br(),
      br(),
      checkboxInput("single_family", "1-Family Housing"),
      div(class = "checkbox-opts", 
          conditionalPanel(
            condition = "input.single_family == true",
            checkboxGroupInput("single_family_options",
                               label = NULL,
                               choices = type_choices, 
                               selected = type_choices)
              )
          ),
      style = "font-size: 0.9em",
      checkboxInput("two_family", "2-Family Housing"),
      div(class = "checkbox-opts",
          conditionalPanel(
            condition = "input.two_family == true",
            checkboxGroupInput("two_family_options",
                               label = NULL,
                               choices = type_choices, 
                               selected = type_choices)
          )
          ),
      checkboxInput("three_family", "3-Family Housing"),
      div(class = "checkbox-opts", 
          conditionalPanel(
            condition = "input.three_family == true",
            checkboxGroupInput("three_family_options",
                               label = NULL,
                               choices = type_choices, 
                               selected = type_choices)
          )
        ),
      checkboxInput("four_family", "4+ Family Housing"),
      div(class = "checkbox-opts", 
          conditionalPanel(
            condition = "input.four_family == true",
            checkboxGroupInput("four_family_options",
                               label = NULL,
                               choices = type_choices, 
                               selected = type_choices)
          )
          ),
      checkboxInput("accessory", "Accessory Dwelling Units"),
      div(class = "checkbox-opts",
          conditionalPanel(
            condition = "input.accessory == true",
            checkboxGroupInput("accessory_options",
                               label = NULL,
                               choices = accessory_choices, 
                               selected = accessory_choices)
          )
        ),
      hr(),
      p("Use the drop down to focus in on a specific jurisdiction or multiple.", style = "font-size: 80%;"),
      pickerInput("select_juris",
                  label = "Select Jurisdiction",
                  multiple = TRUE,
                  choices = local_list,
                  selected = local_list),
      hr(),
      checkboxInput("transit_stops", "Show transit stops", value = FALSE),
      checkboxInput("flood", "Show 1% Annual Flood Hazard", value = FALSE),
      hr(),
      chooseSliderSkin("Flat", color = "#40C0C0"),
      sliderInput("opacity", "Zone opacity", min = 0, max = 100, 
                  value = 80, ticks = FALSE),
      br(),
      textOutput("text")
    ), 
    mainPanel(
      width = 12,
      rdeckOutput("map", width = "99%", height = "98vh")
    )
  )
)

server <- function(input, output, session) {
  
  shinyalert("Disclaimer",
             "<b>HousingForward Virginia</b> created the Virginia Zoning Atlas for public informational purposes only. 
             Given the huge amount of info it contains and the possibility of changing conditions (in zoning codes, 
             parcel/jurisdiction boundaries, & state laws), here's our disclaimer!: We hereby present this map 
             without any warranty, either express or implied, about its validity or accuracy, or its suitability 
             for legal, engineering, or land survey purposes. As we all know, zoning is fluid. If you notice an error, 
             we encourage you to <a href = 'mailto:eric@housingforwardva.org?subject = Virginia Zoning Atlas'> contact us </a>.",
             type = "warning",
             html = TRUE)
  
  shinyalert("Beta Version", 
             "The following iteration of the Virginia Zoning Atlas is a beta version. The final iteration of the 
             Virginia Zoning Atlas will seek to employ additional functionality and information not currently 
             presented.<br>
             
             For example, what you see does not currently account for zoning district overlays, which in some localities 
             impacts building requirements.", 
             type = "info",
             html = TRUE)
  
  shinyalert("Best Viewing Experience", 
             "The Virginia Zoning Atlas is best viewed on a desktop, laptop, or tablet.", 
             type = "info",
             html = TRUE)
  
  
  output$map <- renderRdeck({
    rdeck(map_style = input$basemap, theme = "light",
          initial_bounds = zoning,
          layer_selector = FALSE) %>%
      add_polygon_layer(data = zoning, get_fill_color = fill_color, opacity = 0.8, 
                        id = "zoning_layer", get_polygon = geometry, get_line_color = "#ffffff",
                        get_line_width = 10, pickable = TRUE, auto_highlight = TRUE, highlight_color = highlight_color,
                        tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes), name = "Zoning") %>%
      add_polygon_layer(data = juris, name = "Jurisdiction", stroked = TRUE, filled = FALSE, pickable = TRUE,
                        auto_highlight = TRUE, get_line_color = "#f2e70a", get_polygon = geometry,
                        get_line_width = 100) |> 
      add_polygon_layer(data = fed, opacity = 0.5, filled = TRUE, get_fill_color = "#A9A9A9", get_polygon = geometry, get_line_color = "#ffffff",
                        get_line_width = 10, pickable = TRUE, auto_highlight = TRUE, highlight_color = "#606060",
                        tooltip = c(Ownership, Type, Name), name = "Protected Land") %>%
      add_polygon_layer(data = flood, opacity = 0.5, filled = TRUE, get_fill_color = "#5E1914", get_polygon = geometry,
                        get_line_color = "#ffffff", get_line_width = 10, pickable = FALSE, name = "Flood Hazard", id = "flood",
                        visible = FALSE) |> 
      add_scatterplot_layer(data = transit, get_position = geometry, name = "Public Transit", 
                            radius_min_pixels = 2, visible = FALSE, 
                            id = "transit_layer", 
                            get_fill_color = scale_color_category(service, palette = c("#ffd179", "#ff8e91", "#e67a54")))
  })
  
  
  observeEvent(input$opacity, {
    rdeck_proxy("map") %>%
      update_polygon_layer(id = "zoning_layer",
                  opacity = input$opacity / 100)
  })
  
  
  # Write a zoning filter that talks to all of the different options
  zoning_filter <- reactive({
    sf_opts <- input$single_family_options
    f2_opts <- input$two_family_options
    f3_opts <- input$three_family_options
    f4_opts <- input$four_family_options
    acc_opts <- input$accessory_options
    locality <- input$select_juris
    
    output <- zoning %>%
      dplyr::filter(
        family1_treatment %in% sf_opts,
        family2_treatment %in% f2_opts,
        family3_treatment %in% f3_opts,
        family4_treatment %in% f4_opts,
        accessory_treatment %in% acc_opts,
        jurisdiction %in% locality
      ) |> 
      dplyr::mutate(selected_acres = sum(acres)) |> 
      dplyr::mutate(total_jurisdiction = sum(unique(total_area))) |> 
      dplyr::mutate(pct = scales::percent(selected_acres/total_jurisdiction), accuracy = 0.1)

                         
    
    output
    
  })
  
  # filtered_zoning <- reactive({
  # 
  #   # if (input$single_family) {
  #   #   output <- dplyr::filter(zoning, family1_treatment %in% input$single_family_options)
  #   # }
  #   
  #   # if (input$two_family) {
  #   #   output <- dplyr::filter(zoning, family2_treatment %in% input$two_family_options)
  #   # }
  #   # 
  #   # if (input$three_family) {
  #   #   output <- dplyr::filter(zoning, family3_treatment %in% input$three_family_options)
  #   # }
  #   # 
  #   # if (input$four_family) {
  #   #   output <- dplyr::filter(zoning, family4_treatment %in% input$four_family_options)
  #   # }
  #   
  #   # return(output)
  # })
  
  observeEvent(zoning_filter(), {
    rdeck_proxy("map") %>%
      update_polygon_layer(data = zoning_filter(), id = "zoning_layer")
  })
  
  observeEvent(input$single_family, {
    if (input$single_family) {
      selected_values <- setdiff(input$single_family_options, "prohibited")
      updateCheckboxGroupInput(session, "single_family_options", selected = selected_values)
    } else {
      updateCheckboxGroupInput(session, "single_family_options", selected = type_choices)
    }
  })
  
  observeEvent(input$two_family, {
    if (input$two_family) {
      selected_values <- setdiff(input$two_family_options, "prohibited")
      updateCheckboxGroupInput(session, "two_family_options", selected = selected_values)
    } else {
      updateCheckboxGroupInput(session, "two_family_options", selected = type_choices)
    }
  })
  
  observeEvent(input$three_family, {
    if (input$three_family) {
      selected_values <- setdiff(input$three_family_options, "prohibited")
      updateCheckboxGroupInput(session, "three_family_options", selected = selected_values)
    } else {
      updateCheckboxGroupInput(session, "three_family_options", selected = type_choices)
    }
  })
  
  observeEvent(input$four_family, {
    if (input$four_family) {
      selected_values <- setdiff(input$four_family_options, "prohibited")
      updateCheckboxGroupInput(session, "four_family_options", selected = selected_values)
    } else {
      updateCheckboxGroupInput(session, "four_family_options", selected = type_choices)
    }
  })
  
  observeEvent(input$accessory, {
    if (input$accessory) {
      selected_values <- setdiff(input$accessory_options, "prohibited")
      updateCheckboxGroupInput(session, "accessory_options", selected = selected_values)
    } else {
      updateCheckboxGroupInput(session, "accessory_options", selected = type_choices)
    }
  })
  
  observeEvent(input$transit_stops, {
    proxy <- rdeck_proxy("map")
    
    if (input$transit_stops) {
      proxy %>%
        update_scatterplot_layer(id = "transit_layer", visible = TRUE)
    } else {
      proxy %>% 
        update_scatterplot_layer(id = "transit_layer", visible = FALSE)
    }
  })
  
  observeEvent(input$flood, {
    proxy <- rdeck_proxy("map")
    
    if (input$flood) {
      proxy %>%
        update_polygon_layer(id = "flood", visible = TRUE)
    } else {
      proxy %>% 
        update_polygon_layer(id = "flood", visible = FALSE)
    }
  })
  
  output$text <- renderText({

    pct_value <- unique(zoning_filter()$pct)

    paste("Percent of total developable land based on selected districts:", pct_value)

  })
  
}

shinyApp(ui = ui, server = server)
