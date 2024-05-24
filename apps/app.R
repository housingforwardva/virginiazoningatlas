library(magrittr)
library(sf)
library(rdeck)
library(shinyWidgets)
library(shinyjs)
useShinyjs()

MAPBOX_ACCESS_TOKEN = Sys.getenv("MAPBOX_ACCESS_TOKEN")
options(rdeck.mapbox_access_token = MAPBOX_ACCESS_TOKEN)

# remotes::install_github("walkerke/mapboxapi")

icon_atlas <- "https://raw.githubusercontent.com/visgl/deck.gl-data/master/website/icon-atlas.png"
icon_mapping <- jsonlite::fromJSON("https://raw.githubusercontent.com/visgl/deck.gl-data/master/website/icon-atlas.json")

zoning <- readr::read_rds("data/vza_simple.rds")

sfd <- dplyr::filter(zoning, sfd == "t")
apts <- dplyr::filter(zoning, family4_treatment %in% c("allowed", "hearing"))
adus <- dplyr::filter(zoning, type == "Primarily Residential", accessory_treatment != "prohibited")
missmiddle <- dplyr::filter(zoning, family2_treatment != "prohibited", family3_treatment != "prohibited", family4_treatment != "prohibited")


type_choices <- c(
  "Prohibited" = "prohibited",
  "Allowed By-Right" = "allowed",
  "Public Hearing" = "hearing"
)

accessory_choices <- c(
  "Allowed By-Right" = "allowed",
  "Public Hearing" = "hearing",
  "Prohibited" = "prohibited"
)

base_map <- c(
  "Light" = rdeck::mapbox_light(),
  "Satellite" = rdeck::mapbox_satellite()
)

base_selected <- "Light"

# nova_list <- zoning |> 
#   dplyr::filter(region == "Northern Virginia") |> 
#   dplyr::pull(jurisdiction) |>
#   unique() |> 
#   sort()

nova_list <- c("Alexandria", "Arlington", "Clifton", "Dumfries", "Fairfax", 
               "Fairfax (city)", "Falls Church", "Hamilton", "Haymarket", "Herndon", 
               "Hillsboro", "Leesburg", "Loudoun", "Lovettsville", "Manassas", 
               "Manassas Park", "Middleburg", "Occoquan", "Prince William", 
               "Purcellville", "Round Hill", "Vienna")

hrva_list <- c("Chesapeake", "Franklin (city)", "Gloucester", "Hampton", "Isle of Wight", 
               "James City", "Newport News", "Norfolk", "Poquoson", "Portsmouth", 
               "Smithfield", "Southampton", "Suffolk", "Surry", "Surry (town)", 
               "Virginia Beach", "Williamsburg", "Windsor", "York")

local_list <- list(
  "Northern Virginia" = nova_list,
  "Hampton Roads" = hrva_list
)

ui <- bslib::page_fluid(
  
  shiny::tags$head(
    
    # Add Google Analytics script (https://shiny.posit.co/r/articles/build/google-analytics/)
    
    # includeHTML("vza-google-analytics.html"),
    
    # Ensure the content takes the full width and height of the screen
    
    shiny::tags$style(HTML("
      /* Full-screen map styling */
      #map {
        background-color: #e5e5e5; /* Placeholder background color for the map */
        position: absolute;
      }
      
      /* Style for the floating menu */
      .floating-panel {
        z-index: 1000;
        background-color: rgba(255, 255, 255, 0.5); 
        overflow-y: auto;
        max-height: 100vh; 
        max-width: 300px; 
        
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
  shiny::fluidRow( 
    style = "height: 100%", 
    shiny::absolutePanel(
      width = "150px",
      class = "floating-panel",
      bottom = 50, right = 20, 
      shiny::selectInput("basemap", "Choose basemap",
                         choices = base_map,
                         selected = base_selected)),
    shiny::absolutePanel(
      top = 10,
      right = 20,
      width = "150px",
      class = "floating-panel",
      shiny::htmlOutput("text")
    ),
    shiny::absolutePanel(
      width = "300px",
      class = "floating-panel",
      fixed = TRUE,
      top = 20, left = 20, 
      shiny::img(src = "hfv_logo.png", style = "width: 150px;"),
      shiny::p("Explore zoning in Virginia with the menu options below."),
      # p("This interactive map shows how outdated zoning laws make it hard to build diverse, affordable housing."),
      # p("Use checkboxes below to filter zoning districts in the map.", style = "font-size: 80%;"),
      shiny::p("Type of Zoning District", style = "color: gray; font-weight: bold; margin-bottom: 5px;"),
      shiny::div(class = "my-legend",
                 shiny::HTML(
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
      shiny::br(),
      shiny::br(),
      shiny::hr(),
      bslib::accordion_panel(
        "Geographic filters",
        icon = bsicons::bs_icon("chevron-down"),
        shiny::p("Use the drop down to focus in on a specific jurisdiction or multiple.", style = "font-size: 80%;"),
        shinyWidgets::virtualSelectInput("select_juris",
                                         label = "Select Jurisdiction",
                                         multiple = TRUE,
                                         choices = local_list,
                                         selected = unlist(unname(local_list))),
        mapboxapi::mapboxGeocoderInput("geocode", access_token = MAPBOX_ACCESS_TOKEN,
                                       placeholder = "Zoom to address",
                                       proximity = c(-77.43428, 37.53851))
      ),
      shiny::hr(),
      bslib::accordion_panel(
        "Show me where people can build", icon = bsicons::bs_icon("chevron-down"),
        shinyWidgets::prettySwitch("sf_switch", "Single-family detached only"),
        shinyWidgets::prettySwitch("apt_switch", "Apartments (4+ units)"),
        shinyWidgets::prettySwitch("adu_switch", "Accessory Dwelling Units"),
        shinyWidgets::prettySwitch("msm_switch", "Missing Middle Housing")
      ),
      shiny::hr(),
      bslib::accordion_panel(
        " Advanced filters", icon = bsicons::bs_icon("chevron-down"), id = "advanced_filters", value = "advanced_filters",
        checkboxInput("single_family", "1-Family Housing"),
        shiny::div(class = "checkbox-opts", 
                   shiny::conditionalPanel(
                     condition = "input.single_family == true",
                     checkboxGroupInput("single_family_options",
                                        label = NULL,
                                        choices = type_choices, 
                                        selected = type_choices)
                   )
        ),
        style = "font-size: 0.9em",
        shiny::checkboxInput("two_family", "2-Family Housing"),
        shiny::div(class = "checkbox-opts",
                   shiny::conditionalPanel(
                     condition = "input.two_family == true",
                     shiny::checkboxGroupInput("two_family_options",
                                               label = NULL,
                                               choices = type_choices, 
                                               selected = type_choices)
                   )
        ),
        shiny::checkboxInput("three_family", "3-Family Housing"),
        shiny::div(class = "checkbox-opts", 
                   shiny::conditionalPanel(
                     condition = "input.three_family == true",
                     shiny::checkboxGroupInput("three_family_options",
                                               label = NULL,
                                               choices = type_choices, 
                                               selected = type_choices)
                   )
        ),
        shiny::checkboxInput("four_family", "4+ Family Housing"),
        shiny::div(class = "checkbox-opts", 
                   shiny::conditionalPanel(
                     condition = "input.four_family == true",
                     shiny::checkboxGroupInput("four_family_options",
                                               label = NULL,
                                               choices = type_choices, 
                                               selected = type_choices)
                   )
        ),
        shiny::checkboxInput("accessory", "Accessory Dwelling Units"),
        shiny::div(class = "checkbox-opts",
                   shiny::conditionalPanel(
                     condition = "input.accessory == true",
                     shiny::checkboxGroupInput("accessory_options",
                                               label = NULL,
                                               choices = accessory_choices, 
                                               selected = accessory_choices)
                   )
        )
      )
      ,
      shiny::hr(),
      bslib::accordion_panel(
        "Additional layers",
        icon = bsicons::bs_icon("chevron-down"),
        shiny::checkboxInput(
          "transit_stops", 
          "Show transit stops", 
          value = FALSE
        ), 
        shiny::checkboxInput(
          "flood", 
          "Show 1% Annual Flood Hazard", 
          value = FALSE
        ),
        shiny::checkboxInput(
          "locality_boundaries",
          "Show locality boundaries",
          value = FALSE
        ),
        shiny::checkboxInput(
          "protected",
          "Show protected lands",
          value = FALSE
        )
      ), 
      shiny::hr(),
      shinyWidgets::chooseSliderSkin("Flat", color = "#40C0C0"),
      shiny::sliderInput("opacity", "Zone opacity", min = 0, max = 100, 
                         value = 80, ticks = FALSE)
      # br(),
      # textOutput("text")
    ), 
    shiny::mainPanel(
      width = 12,
      rdeck::rdeckOutput("map", width = "99%", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  
  shinyalert::shinyalert("Disclaimer",
                         "<b>HousingForward Virginia</b> created the Virginia Zoning Atlas for public informational purposes only.
             Given the huge amount of info it contains and the possibility of changing conditions (in zoning codes,
             parcel/jurisdiction boundaries, & state laws), here's our disclaimer!: We hereby present this map
             without any warranty, either express or implied, about its validity or accuracy, or its suitability
             for legal, engineering, or land survey purposes. As we all know, zoning is fluid. If you notice an error,
             we encourage you to <a href = 'mailto:eric@housingforwardva.org?subject = Virginia Zoning Atlas'> contact us </a>.",
                         type = "warning",
                         html = TRUE, 
                         inputId = "disclaimer",
                         callbackJS = "
        function() {
          Shiny.setInputValue('alert_shown', Math.random());
        }
      "
  )
  
  
  shinyalert::shinyalert("Best Viewing Experience",
                         "The Virginia Zoning Atlas is best viewed on a desktop, laptop, or tablet.",
                         type = "info",
                         html = TRUE)
  
  
  output$map <- rdeck::renderRdeck({
    rdeck::rdeck(map_style = mapbox_light(), theme = "light",
                 initial_bounds = zoning,
                 layer_selector = FALSE) # %>%
    # rdeck::add_polygon_layer(
    #   data = zoning,
    #   get_fill_color = fill_color,
    #   opacity = 0.8,
    #   id = "zoning_layer",
    #   get_polygon = geometry,
    #   get_line_color = "#ffffff",
    #   get_line_width = 10,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   highlight_color = highlight_color,
    #   tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
    #   name = "Zoning",
    #   visible = FALSE
    # ) %>%
    # rdeck::add_polygon_layer(
    #   data = juris,
    #   name = "Jurisdiction",
    #   stroked = TRUE,
    #   filled = FALSE,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   get_line_color = "#f2e70a",
    #   get_polygon = geometry,
    #   get_line_width = 100,
    #   visible = FALSE
    # )  |>
    # rdeck::add_polygon_layer(
    #   data = fed,
    #   opacity = 0.5,
    #   filled = TRUE,
    #   get_fill_color = "#A9A9A9",
    #   get_polygon = geometry,
    #   get_line_color = "#ffffff",
    #   get_line_width = 10,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   highlight_color = "#606060",
    #   tooltip = c(Ownership, Type, Name),
    #   name = "Protected Land",
    #   visible = FALSE
    # ) %>%
    # rdeck::add_polygon_layer(
    #   data = sfd,
    #   get_fill_color = fill_color,
    #   opacity = 0.8,
    #   id = "sfd",
    #   get_polygon = geometry,
    #   get_line_color = "#ffffff",
    #   get_line_width = 10,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   highlight_color = highlight_color,
    #   tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
    #   name = "Zoning",
    #   visible = FALSE
    # ) %>%
    # rdeck::add_polygon_layer(
    #   data = adus,
    #   get_fill_color = fill_color,
    #   opacity = 0.8,
    #   id = "adus",
    #   get_polygon = geometry,
    #   get_line_color = "#ffffff",
    #   get_line_width = 10,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   highlight_color = highlight_color,
    #   tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
    #   name = "Zoning",
    #   visible = FALSE
    # ) %>%
    # rdeck::add_polygon_layer(
    #   data = apts,
    #   get_fill_color = fill_color,
    #   opacity = 0.8,
    #   id = "apts",
    #   get_polygon = geometry,
    #   get_line_color = "#ffffff",
    #   get_line_width = 10,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   highlight_color = highlight_color,
    #   tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
    #   name = "Zoning",
    #   visible = FALSE
    # ) %>%
    # rdeck::add_polygon_layer(
    #   data = missmiddle,
    #   get_fill_color = fill_color,
    #   opacity = 0.8,
    #   id = "missmiddle",
    #   get_polygon = geometry,
    #   get_line_color = "#ffffff",
    #   get_line_width = 10,
    #   pickable = TRUE,
    #   auto_highlight = TRUE,
    #   highlight_color = highlight_color,
    #   tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
    #   name = "Zoning",
    #   visible = FALSE
    # )
  })
  
  shiny::observeEvent(input$disclaimer, {
    rdeck::rdeck_proxy("map") %>%
      rdeck::add_polygon_layer(
        data = zoning,
        get_fill_color = fill_color,
        opacity = 0.8,
        id = "zoning_layer",
        get_polygon = geometry,
        get_line_color = "#ffffff",
        get_line_width = 10,
        pickable = TRUE,
        auto_highlight = TRUE,
        highlight_color = highlight_color,
        tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
        name = "Zoning",
        visible = TRUE
      )    %>%
      rdeck::add_polygon_layer(
        data = sfd,
        get_fill_color = fill_color,
        opacity = 0.8,
        id = "sfd",
        get_polygon = geometry,
        get_line_color = "#ffffff",
        get_line_width = 10,
        pickable = TRUE,
        auto_highlight = TRUE,
        highlight_color = highlight_color,
        tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
        name = "Zoning",
        visible = FALSE
      ) %>%
      rdeck::add_polygon_layer(
        data = adus,
        get_fill_color = fill_color,
        opacity = 0.8,
        id = "adus",
        get_polygon = geometry,
        get_line_color = "#ffffff",
        get_line_width = 10,
        pickable = TRUE,
        auto_highlight = TRUE,
        highlight_color = highlight_color,
        tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
        name = "Zoning",
        visible = FALSE
      ) %>%
      rdeck::add_polygon_layer(
        data = apts,
        get_fill_color = fill_color,
        opacity = 0.8,
        id = "apts",
        get_polygon = geometry,
        get_line_color = "#ffffff",
        get_line_width = 10,
        pickable = TRUE,
        auto_highlight = TRUE,
        highlight_color = highlight_color,
        tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
        name = "Zoning",
        visible = FALSE
      ) %>%
      rdeck::add_polygon_layer(
        data = missmiddle,
        get_fill_color = fill_color,
        opacity = 0.8,
        id = "missmiddle",
        get_polygon = geometry,
        get_line_color = "#ffffff",
        get_line_width = 10,
        pickable = TRUE,
        auto_highlight = TRUE,
        highlight_color = highlight_color,
        tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes),
        name = "Zoning",
        visible = FALSE
      )
  })
  
  
  shiny::observeEvent(input$opacity, {
    rdeck::rdeck_proxy("map") %>%
      rdeck::update_polygon_layer(id = "zoning_layer",
                                  opacity = input$opacity / 100,
                                  get_fill_color = fill_color, ,
                                  get_polygon = geometry, get_line_color = "#ffffff",
                                  get_line_width = 10, get_elevation = 1000)
  })
  
  
  geo_outputs <- shiny::eventReactive(input$geocode, {
    loc <- mapboxapi::geocoder_as_sf(input$geocode) %>%
      dplyr::mutate(icon = "marker",
                    Address = stringr::str_remove(full_address, ", United States"))
    
    loc_buffer <- sf::st_buffer(sf::st_transform(loc, 3968), 2000) |> 
      sf::st_transform(4326)
    
    list(marker = loc,
         buffer = loc_buffer)
    
  })
  
  
  shiny::observeEvent(geo_outputs(), {
    
    g <- geo_outputs()
    
    rdeck::rdeck_proxy("map", initial_bounds = sf::st_bbox(g$buffer)) %>%
      rdeck::add_icon_layer(
        id = "chosen_location",
        name = "Selected location",
        data = g$marker,
        get_position = sf_column(),
        size_scale = 30,
        get_color = "#000000",
        
        # icon params
        get_icon = icon,
        icon_atlas = icon_atlas,
        icon_mapping = icon_mapping,
        
        # interactivity
        pickable = TRUE,
        auto_highlight = TRUE,
        tooltip = Address
      )
  })
  
  
  # Write a zoning filter that talks to all of the different options
  # zoning_filter <- shiny::reactive({
  #   sf_opts <- input$single_family_options
  #   f2_opts <- input$two_family_options
  #   f3_opts <- input$three_family_options
  #   f4_opts <- input$four_family_options
  #   acc_opts <- input$accessory_options
  #   locality <- input$select_juris
  # 
  #   if (input$sf_switch) {
  #     output <- zoning %>%
  #       dplyr::filter(
  #         sfd == "t",
  #         jurisdiction %in% locality
  #       ) |>
  #       dplyr::mutate(selected_acres = sum(acres)) |>
  #       dplyr::mutate(total_jurisdiction = sum(unique(total_area))) |>
  #       dplyr::mutate(pct = scales::percent(selected_acres/total_jurisdiction), accuracy = 0.1)
  #   } else {
  #     output <- zoning %>%
  #       dplyr::filter(
  #         family1_treatment %in% sf_opts,
  #         family2_treatment %in% f2_opts,
  #         family3_treatment %in% f3_opts,
  #         family4_treatment %in% f4_opts,
  #         accessory_treatment %in% acc_opts,
  #         jurisdiction %in% locality
  #       ) |>
  #       dplyr::mutate(selected_acres = sum(acres)) |>
  #       dplyr::mutate(total_jurisdiction = sum(unique(total_area))) |>
  #       dplyr::mutate(pct = scales::percent(selected_acres/total_jurisdiction), accuracy = 0.1)
  #   }
  # 
  # 
  # 
  # 
  # 
  #   output
  # 
  # })
  # 
  # shiny::observeEvent(zoning_filter(), {
  #   rdeck::rdeck_proxy("map") %>%
  #     rdeck::add_polygon_layer(data = zoning_filter(), get_fill_color = fill_color, opacity = 0.8,
  #                       id = "zoning_layer", get_polygon = geometry, get_line_color = "#ffffff",
  #                       get_line_width = 10, pickable = TRUE, auto_highlight = TRUE, highlight_color = highlight_color,
  #                       tooltip = c(Abbreviation, Zoning, Jurisdiction, Notes), name = "Zoning")
  # })
  
  shiny::observeEvent(input$single_family, {
    if (input$single_family) {
      selected_values <- setdiff(input$single_family_options, "prohibited")
      shiny::updateCheckboxGroupInput(session, "single_family_options", selected = selected_values)
    } else {
      shiny::updateCheckboxGroupInput(session, "single_family_options", selected = type_choices)
    }
  })
  
  shiny::observeEvent(input$two_family, {
    if (input$two_family) {
      selected_values <- setdiff(input$two_family_options, "prohibited")
      shiny::updateCheckboxGroupInput(session, "two_family_options", selected = selected_values)
    } else {
      shiny::updateCheckboxGroupInput(session, "two_family_options", selected = type_choices)
    }
  })
  
  shiny::observeEvent(input$three_family, {
    if (input$three_family) {
      selected_values <- setdiff(input$three_family_options, "prohibited")
      shiny::updateCheckboxGroupInput(session, "three_family_options", selected = selected_values)
    } else {
      shiny::updateCheckboxGroupInput(session, "three_family_options", selected = type_choices)
    }
  })
  
  shiny::observeEvent(input$four_family, {
    if (input$four_family) {
      selected_values <- setdiff(input$four_family_options, "prohibited")
      shiny::updateCheckboxGroupInput(session, "four_family_options", selected = selected_values)
    } else {
      shiny::updateCheckboxGroupInput(session, "four_family_options", selected = type_choices)
    }
  })
  
  shiny::observeEvent(input$accessory, {
    if (input$accessory) {
      selected_values <- setdiff(input$accessory_options, "prohibited")
      shiny::updateCheckboxGroupInput(session, "accessory_options", selected = selected_values)
    } else {
      shiny::updateCheckboxGroupInput(session, "accessory_options", selected = type_choices)
    }
  })
  
  shiny::observe({
    proxy <- rdeck::rdeck_proxy("map")
    
    if (input$sf_switch) {
      proxy %>%
        rdeck::update_polygon_layer(id = "zoning_layer", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "sfd", visible = TRUE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "apts", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "adus", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "missmiddle", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000)
      
    } else if (input$apt_switch) {
      proxy %>%
        rdeck::update_polygon_layer(id = "zoning_layer", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "sfd", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "apts", visible = TRUE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "adus", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "missmiddle", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000)
      
    } else if (input$adu_switch) {
      proxy %>%
        rdeck::update_polygon_layer(id = "zoning_layer", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "sfd", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "apts", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "adus", visible = TRUE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "missmiddle", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000)
      
    } else if (input$msm_switch) {
      proxy %>%
        rdeck::update_polygon_layer(id = "zoning_layer", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "sfd", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "apts", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "adus", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "missmiddle", visible = TRUE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000)
      
    } else {
      proxy %>%
        rdeck::update_polygon_layer(id = "zoning_layer", visible = TRUE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "sfd", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "apts", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "adus", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000) %>%
        rdeck::update_polygon_layer(id = "missmiddle", visible = FALSE,
                                    get_fill_color = fill_color,
                                    get_polygon = geometry,
                                    get_line_color = "#ffffff",
                                    get_line_width = 10,
                                    get_elevation = 1000)
    }
  })
  
  shiny::observeEvent(input$sf_switch, {
    if (input$sf_switch) {
      # Modify the switches
      shinyWidgets::updatePrettySwitch(inputId = "apt_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "adu_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "msm_switch", value = FALSE)
    }
  })
  
  shiny::observeEvent(input$adu_switch, {
    if (input$adu_switch) {
      # Modify the switches
      shinyWidgets::updatePrettySwitch(inputId = "sf_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "apt_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "msm_switch", value = FALSE)
    }
    
  })
  
  shiny::observeEvent(input$apt_switch, {
    if (input$apt_switch) {
      # Modify the switches
      shinyWidgets::updatePrettySwitch(inputId = "sf_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "adu_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "msm_switch", value = FALSE)
    }
    
  })
  
  shiny::observeEvent(input$msm_switch, {
    if (input$msm_switch) {
      # Modify the switches
      shinyWidgets::updatePrettySwitch(inputId = "sf_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "apt_switch", value = FALSE)
      shinyWidgets::updatePrettySwitch(inputId = "adu_switch", value = FALSE)
    }
    
  })
  
  shiny::observeEvent(input$transit_stops, {
    proxy <- rdeck::rdeck_proxy("map")
    
    transit <- readr::read_rds("data/transit.rds")
    
    if (input$transit_stops) {
      proxy %>%
        rdeck::add_scatterplot_layer(
          data = transit,
          get_position = geometry,
          name = "Public Transit",
          radius_min_pixels = 2,
          visible = TRUE,
          id = "transit_layer",
          pickable = TRUE,
          get_fill_color = "#ffd179",
          tooltip = Service
        ) 
    } else {
      proxy %>%
        rdeck::update_scatterplot_layer(id = "transit_layer", visible = FALSE,
                                        get_position = geometry, 
                                        get_fill_color = "#ffd179",
                                        get_line_color = "#000000ff",
                                        get_line_width = 1,
                                        get_radius = 1)
    }
  })
  # 
  shiny::observeEvent(input$flood, {
    proxy <- rdeck::rdeck_proxy("map")
    
    flood <- readr::read_rds("data/flood.rds")
    
    if (input$flood) {
      proxy %>%
        rdeck::add_polygon_layer(
          data = flood,
          opacity = 0.5,
          filled = TRUE,
          get_fill_color = "#5E1914",
          get_polygon = geom,
          get_line_color = "#ffffff",
          get_line_width = 10,
          pickable = FALSE,
          name = "Flood Hazard",
          id = "flood",
          visible = TRUE
        )
    } else {
      proxy %>%
        rdeck::update_polygon_layer(
          id = "flood",
          visible = FALSE,
          get_fill_color = "#5E1914",
          get_polygon = geom,
          get_line_color = "#ffffff",
          get_line_width = 10,
          get_elevation = 1000
        )
    }
  })
  
  shiny::observeEvent(input$locality_boundaries, {
    proxy <- rdeck::rdeck_proxy("map")
    
    juris <- readr::read_rds("data/boundaries.rds")
    
    if (input$locality_boundaries) {
      proxy %>%
        rdeck::add_polygon_layer(
          data = juris,
          name = "Jurisdiction",
          id = "juris",
          stroked = TRUE,
          filled = FALSE,
          pickable = TRUE,
          auto_highlight = TRUE,
          get_line_color = "#f2e70a",
          get_polygon = geom,
          get_line_width = 100,
          visible = TRUE
        )
    } else {
      proxy %>%
        rdeck::update_polygon_layer(
          id = "juris",
          visible = FALSE,
          get_polygon = geometry,
          get_fill_color = "#ffffff",
          get_line_color = "#f2e70a",
          get_line_width = 10,
          get_elevation = 1000
        )
    }
  })
  
  shiny::observeEvent(input$protected, {
    proxy <- rdeck::rdeck_proxy("map")
    
    fed <- readr::read_rds("data/protected_lands.rds")
    
    if (input$protected) {
      proxy %>%
        rdeck::add_polygon_layer(
          data = fed,
          opacity = 0.5,
          filled = TRUE,
          get_fill_color = "#A9A9A9",
          get_polygon = geometry,
          get_line_color = "#ffffff",
          get_line_width = 10,
          pickable = TRUE,
          auto_highlight = TRUE,
          highlight_color = "#606060",
          tooltip = c(Ownership, Type, Name),
          name = "Protected Land",
          id = "fed",
          visible = TRUE
        )
    } else {
      proxy %>%
        rdeck::update_polygon_layer(
          id = "fed",
          visible = FALSE,
          get_polygon = geometry,
          get_fill_color = "#A9A9A9",
          get_line_color = "#ffffff",
          get_line_width = 10,
          get_elevation = 1000
        )
    }
  })
  
  observeEvent(input$basemap, {
    rdeck::rdeck_proxy("map", map_style = input$basemap)
  })
  # 
  #   output$text <- renderText({
  # 
  #     pct_value <- unique(zoning_filter()$pct)
  # 
  #     paste("Percent of total developable land based on selected districts: <strong>", pct_value, "</strong>")
  # 
  #   })
  
}

shiny::shinyApp(ui = ui, server = server)