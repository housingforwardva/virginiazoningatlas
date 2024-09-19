library(magrittr)
library(sf)
library(mapgl)
library(shinyWidgets)
sf::sf_use_s2(FALSE)

MAPBOX_ACCESS_TOKEN <- Sys.getenv("MAPBOX_ACCESS_TOKEN")

zoning <- readr::read_rds("data/vza_simple.rds") |> dplyr::ungroup()
# Get a merged layer that is zoning with geometries dissolved across the possible filter groups to display when zoomed out (TBD)

# Construct the tooltip column
zoning$tooltip <- paste0(
  '<div style="padding: 3px; font-size: 10px;">',  # Set width and smaller font size
  '<div><strong>Abbreviation </strong><span style="float: right;">', zoning$Abbreviation, '</span></div>',
  '<div><strong>Zoning </strong><span style="float: right;">', zoning$Zoning, '</span></div>',
  '<div><strong>Jurisdiction </strong><span style="float: right;">', zoning$Jurisdiction, '</span></div>',
  '<div><strong>Notes </strong></div>',
  '<div>', zoning$Notes, '</div>',
  '</div>'
)


# Refactor - this shouldn't be needed as we can use set_filter()

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
  "Light" = mapbox_style("light"),
  "Satellite" = mapbox_style("satellite")
)

base_selected <- "Light"

nova_list <- c("Alexandria", "Arlington", "Clifton", "Dumfries", "Fairfax", 
               "Fairfax (city)", "Falls Church", "Hamilton", "Haymarket", "Herndon", 
               "Hillsboro", "Leesburg", "Loudoun", "Lovettsville", "Manassas", 
               "Manassas Park", "Middleburg", "Occoquan", "Prince William", 
               "Purcellville", "Round Hill", "Vienna")

rva_list <- c("Ashland", "Charles City", "Chesterfield", "Goochland", "Hanover", 
              "Henrico", "New Kent", "Powhatan", "Richmond (city)")

hrva_list <- c("Chesapeake", "Franklin (city)", "Gloucester", "Hampton", "Isle of Wight", 
               "James City", "Newport News", "Norfolk", "Poquoson", "Portsmouth", 
               "Smithfield", "Southampton", "Suffolk", "Surry", "Surry (town)", 
               "Virginia Beach", "Williamsburg", "Windsor", "York")

local_list <- list(
  "Northern Virginia" = nova_list,
  "PlanRVA" = rva_list,
  "Hampton Roads" = hrva_list
)

ui <- bslib::page_fluid(
  
  shiny::tags$head(
    
    shiny::tags$title("Virginia Zoning Atlas"),
    
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
        max-width: 350px; 
        
      }
      
      .sub-panel {
        z-index: 1000;
        background-color: rgba(255, 255, 255, 0.5);
      }

      /* Hover effect for the floating menu */
      .floating-panel:hover {
        background-color: rgba(255, 255, 255, 0.8); /* Less transparent background on hover */
      }
      
      .shiny-input-container {
          margin-bottom: 2px; /* Adjust this value as desired */
      }
      
      .vscomp-wrapper {
        z-index: 1001 !important;
      }
      .vscomp-dropdown {
        z-index: 1002 !important;
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
      
       ")),
    tags$script(HTML("
      $(document).on('click', '.vscomp-wrapper', function() {
        if ($(this).hasClass('show-dropdown')) {
          $('.pretty').css('pointer-events', 'none');
        } else {
          $('.pretty').css('pointer-events', 'auto');
        }
      });
      $(document).on('click', function(event) {
        if (!$(event.target).closest('.vscomp-wrapper').length) {
          $('.pretty').css('pointer-events', 'auto');
        }
      });
    "))
  ),
  shiny::fluidRow( 
    style = "height: 100%;", 
    # shiny::absolutePanel(
    #   width = "150px",
    #   class = "sub-panel",
    #   bottom = 50, right = 20, 
    #   shiny::selectInput("basemap", "Choose basemap",
    #               choices = base_map,
    #               selected = base_selected)),
    shiny::absolutePanel(
      top = 10,
      right = 20,
      width = "150px",
      class = "sub-panel",
      shiny::htmlOutput("text")
    ),
    shiny::absolutePanel(
      width = "325px",
      class = "floating-panel",
      fixed = TRUE,
      top = 20, left = 20, 
      shiny::a(
        shiny::img(src = "hfv_logo.png", style = "width: 150px;"),
        href = "https://housingforwardva.org/"
      ),
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
        span(
          style = "display: flex; align-items: center;",
          shinyWidgets::prettySwitch("sf_switch", "Single-family detached only"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Where you can build only single family detached housing"
          )
        ),
        span(
          style = "display: flex; align-items: center;",
          shinyWidgets::prettySwitch("apt_switch", "Apartments (4+ units)"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Where you can build larger apartments by-right or through a public hearing"
          )
        ),
        uiOutput("apartment_options"),
        span(
          style = "display: flex; align-items: center;",
          shinyWidgets::prettySwitch("adu_switch", "Accessory Dwelling Units"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Where you can build ADUs in primarily residential zoning districts"
          )
        ),
        uiOutput("adu_options"),
        span(
          style = "display: flex; align-items: center;",
          shinyWidgets::prettySwitch("msm_switch", "Missing Middle Housing"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Where you can build 2-4 unit housing by-right or through a public hearing"
          )
        ), 
        uiOutput("msm_options")
      ),
      shiny::hr(),
      bslib::accordion_panel(
        "Additional layers",
        id = "additional_layers",
        value = "additional_layers_accordion",
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
                         value = 80, ticks = FALSE),
      br(),
      br()
    ), 
    shiny::mainPanel(
      width = 12,
      mapgl::mapboxglOutput("map", width = "99%", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  
  shinyalert::shinyalert("Disclaimer",
                         "<b>HousingForward Virginia</b> created the Virginia Zoning Atlas for public informational purposes only.
                         We hereby present this map without any warranty, either express or implied, about its validity or accuracy, 
                         or its suitability for legal, engineering, or land survey purposes. 
                         As we all know, zoning is fluid. If you notice an error, we encourage you to <a href = 'mailto:eric@housingforwardva.org?subject = Virginia Zoning Atlas'> contact us </a>.",
                         type = "warning",
                         html = TRUE, 
                         inputId = "disclaimer"
  )
  
  # shinyalert::shinyalert("Beta Version",
  #            "The following iteration of the Virginia Zoning Atlas is a beta version. The final iteration of the
  #            Virginia Zoning Atlas will seek to employ additional functionality and information not currently
  #            presented.<br>
  # 
  #            For example, what you see does not currently account for zoning district overlays, which in some localities
  #            impacts building requirements.",
  #            type = "info",
  #            html = TRUE)
  
  # shinyalert::shinyalert("Best Viewing Experience",
  #            "The Virginia Zoning Atlas is best viewed on a desktop, laptop, or tablet.",
  #            type = "info",
  #            html = TRUE, 
  #            inputId = "best_viewing")
  
  
  # Let's try initializing the data on map load
  # The issue with this is how `set_style()` works within the proxy - I wonder if 
  # we need to change `fit_bounds()`
  bounds <- as.vector(sf::st_bbox(zoning))
  
  output$map <- mapgl::renderMapboxgl({
    mapgl::mapboxgl(style = "mapbox://styles/ericvmai/clzu65lrv00qc01pd1her0smz/draft", 
                    bounds = bounds, 
                    access_token = MAPBOX_ACCESS_TOKEN) %>%
      # Here, we adjust the source layer's tolerance (for demonstration)
      mapgl::add_source(
        id = "zoning-source",
        data = zoning,
        tolerance = 0.1
      ) %>%
      mapgl::add_fill_layer(
        id = "zoning",
        source = "zoning-source",
        fill_opacity = 0.8,
        fill_color = get_column("fill_color"),
        # fill_outline_color = "white",
        tooltip = "tooltip",
        slot = "middle",
        hover_options = list(
          fill_opacity = 1
        )
      ) %>%
      mapgl::add_navigation_control(position = "bottom-right")
  })
  
  # Adjust opacity with set_paint_property
  shiny::observeEvent(input$opacity, {
    mapgl::mapboxgl_proxy("map") %>%
      set_paint_property(
        layer = "zoning",
        name = "fill-opacity",
        value = input$opacity / 100
      )
  })
  
  shiny::observeEvent(input$select_juris, {
    selected_locs <- dplyr::filter(zoning, jurisdiction %in% input$select_juris)
    
    mapgl::mapboxgl_proxy("map") %>%
      fit_bounds(selected_locs, animate = TRUE)
  })
  
  # Handle the geocoder with new mapgl features
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
    
    mapgl::mapboxgl_proxy("map") %>%
      mapgl::clear_markers() %>%
      mapgl::fit_bounds(g$buffer, animate = TRUE) %>%
      mapgl::add_markers(
        data = g$marker,
        color = "black",
        popup = "Address", 
        draggable = TRUE
      )
    
  })
  
  # If input$geocode goes to NULL, remove the marker
  shiny::observe({
    if (is.null(input$geocode)) {
      mapgl::mapboxgl_proxy("map") %>%
        mapgl::clear_markers()
    }
  })
  
  # Handle the switches
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
  
  # Render apartment options
  output$apartment_options <- renderUI({
    req(input$apt_switch)
    if (input$apt_switch) {
      tagList(
        div(
          style = "margin-left: 20px;",
          checkboxInput("apt_by_right", "By-Right", value = TRUE),
          checkboxInput("apt_public_hearing", "Public Hearing", value = TRUE)
        )
      )
    }
  })
  
  # Render ADU options
  output$adu_options <- renderUI({
    req(input$adu_switch)
    if (input$adu_switch) {
      tagList(
        div(
          style = "margin-left: 20px;",
          checkboxInput("adu_by_right", "By-Right", value = TRUE),
          checkboxInput("adu_public_hearing", "Public Hearing", value = TRUE)
        )
      )
    }
  })
  
  # Render missing middle options
  output$msm_options <- renderUI({
    req(input$msm_switch)
    if (input$msm_switch) {
      tagList(
        div(
          style = "margin-left: 20px;",
          checkboxInput("msm_by_right", "By-Right", value = TRUE),
          checkboxInput("msm_public_hearing", "Public Hearing", value = TRUE)
        )
      )
    }
  })
  
  # Observer that handles the jurisdiction and switch filters
  current_layer <- shiny::reactiveVal("zoning_layer")
  
  # Set up the filter value logic for apartments, ADUs, and missing middle
  get_filter_values <- function(by_right, public_hearing) {
    values <- c()
    if (isTRUE(by_right)) values <- c(values, "allowed")
    if (isTRUE(public_hearing)) values <- c(values, "hearing")
    return(values)
  }
  
  # Reactive expression for the apartment filter
  apartment_filter <- reactive({
    if (!is.null(input$apt_switch) && isTRUE(input$apt_switch)) {
      filter_values <- get_filter_values(input$apt_by_right, input$apt_public_hearing)
      if (length(filter_values) > 0) {
        return(c("in", "family4_treatment", filter_values))
      } else {
        return(c("in", "family4_treatment", "blank"))
      }
    }
    return(NULL)
  })
  
  # Reactive expression for the ADU filter
  adu_filter <- reactive({
    if (!is.null(input$adu_switch) && isTRUE(input$adu_switch)) {
      filter_values <- get_filter_values(input$adu_by_right, input$adu_public_hearing)
      if (length(filter_values) > 0) {
        return(c("in", "accessory_treatment", filter_values))
      } else {
        return(c("in", "accessory_treatment", "blank"))
      }
    }
    return(NULL)
  })
  
  # Reactive expression for the missing middle filter
  msm_filter <- reactive({
    if (!is.null(input$msm_switch) && isTRUE(input$msm_switch)) {
      filter_values <- get_filter_values(input$msm_by_right, input$msm_public_hearing)
      if (length(filter_values) > 0) {
        return(list(
          c("in", "family2_treatment", filter_values),
          c("in", "family3_treatment", filter_values),
          c("in", "family4_treatment", filter_values)
        ))
      } else {
        return(list(
          c("in", "family2_treatment", "blank"),
          c("in", "family3_treatment", "blank"),
          c("in", "family4_treatment", "blank")
        ))
      }
    }
    return(NULL)
  })
  
  shiny::observe({
    proxy <- mapgl::mapboxgl_proxy("map") 
    
    if (length(input$select_juris) > 0) {
      
      proxy %>%
        set_layout_property(
          "zoning",
          "visibility",
          "visible"
        )
      
      if (input$sf_switch) {
        
        proxy %>%
          set_filter(
            "zoning",
            list("all",
                 c("==", "sfd", "t"),
                 c("in", "Jurisdiction", input$select_juris)
            )
          )
        
        current_layer("sfd")
        
      } else if (input$apt_switch) {
        
        filters <- list("all",
                        apartment_filter(),
                        c("in", "Jurisdiction", input$select_juris))
        
        # Remove NULL elements from the filters list
        filters <- filters[!sapply(filters, is.null)]
        
        proxy %>%
          set_filter(
            "zoning",
            filters
          )
        
        current_layer("apts")
        
      } else if (input$adu_switch) {
        
        filters <- list("all",
                        c("==", "type", "Primarily Residential"),
                        adu_filter(),
                        c("in", "Jurisdiction", input$select_juris))
        
        # Remove NULL elements from the filters list
        filters <- filters[!sapply(filters, is.null)]
        
        proxy %>%
          set_filter(
            "zoning",
            filters
          )
        
        current_layer("adus")
        
      } else if (input$msm_switch) {
        
        msm_filters <- msm_filter()
        
        filters <- c(
          list("all"),
          msm_filters,
          list(c("in", "Jurisdiction", input$select_juris))
        )
        
        # Remove NULL elements from the filters list
        filters <- filters[!sapply(filters, is.null)]
        
        proxy %>%
          set_filter(
            "zoning",
            filters
          )
        
        current_layer("missmiddle")
        
      } else {
        
        proxy %>%
          set_filter(
            "zoning",
            list("all",
                 c("in", "Jurisdiction", input$select_juris)
            )
          )
        
        current_layer("zoning_layer")
        
      }
    } else {
      proxy %>%
        set_layout_property(
          "zoning",
          "visibility",
          "none"
        )
    }
  })
  
  shiny::observeEvent(input$transit_stops, {
    proxy <- mapgl::mapboxgl_proxy("map")
    
    transit <- readr::read_rds("data/transit.rds")
    
    if (input$transit_stops) {
      proxy %>%
        mapgl::add_circle_layer(
          id = "transit_layer",
          source = transit,
          circle_color = "#ffd179",
          tooltip = "service",
          circle_radius = interpolate(
            property = "zoom",
            values = c(9, 16),
            stops = c(1, 10)
          )
        )
      
    } else {
      proxy %>%
        mapgl::clear_layer("transit_layer")
    }
  })
  
  shiny::observeEvent(input$flood, {
    proxy <- mapgl::mapboxgl_proxy("map")
    
    flood <- readr::read_rds("data/flood.rds")
    
    if (input$flood) {
      
      proxy %>%
        add_fill_layer(
          id = "flood",
          source = flood,
          fill_color = "#a9d1c9",
          fill_opacity = 0.5
        )
      
    } else {
      proxy %>%
        clear_layer("flood")
    }
  })
  
  shiny::observeEvent(input$locality_boundaries, {
    proxy <- mapgl::mapboxgl_proxy("map")
    
    juris <- readr::read_rds("data/boundaries.rds") %>%
      sf::st_cast("MULTILINESTRING")
    
    if (input$locality_boundaries) {
      proxy %>%
        mapgl::add_line_layer(
          id = "juris",
          source = juris,
          line_color = "#808080",
          line_width = 2
        )
    } else {
      proxy %>%
        mapgl::clear_layer("juris")
    }
  })
  
  shiny::observeEvent(input$protected, {
    proxy <- mapgl::mapboxgl_proxy("map")
    
    fed <- readr::read_rds("data/protected_lands.rds")
    
    if (input$protected) {
      proxy %>%
        mapgl::add_fill_layer(
          id = "fed",
          source = fed,
          fill_color = "#A9A9A9",
          tooltip = "Name",
          hover_options = list(
            fill_color = "#606060"
          )
        )
      
    } else {
      proxy %>%
        mapgl::clear_layer("fed")
    }
  })
  
  # TODO: figure out the basemap switcher, doesn't currently work the way I want because
  # of the way `setStyle()` works - it always re-draws the map.
  # I'd need to identify the currently-active filter and persist that.
  
  # Reactive for currently visible areas
  visible_areas <- reactive({
    req(input$disclaimer)
    req(input$map_bbox)
    
    box <- input$map_bbox |>
      unlist() |> 
      st_bbox(crs = st_crs(4326)) |> 
      st_as_sfc() |> 
      st_sf()
    
    locs <- dplyr::filter(zoning, jurisdiction %in% input$select_juris)
    
    sf::st_filter(locs, box)
    
  })
  
  
  text_calc <- reactive({
    # Don't calculate until second button has been clicked
    req(input$disclaimer)
    
    cur <- current_layer()
    
    viz <- visible_areas()
    
    # Currently _selected_ locality
    if (cur == "zoning_layer") {
      cur_locality <- dplyr::filter(zoning, jurisdiction %in% input$select_juris)
    } else if (cur == "sfd") {
      cur_locality <- dplyr::filter(zoning, jurisdiction %in% input$select_juris, sfd == "t")
    } else if (cur == "apts") {
      apt_filter_values <- get_filter_values(input$apt_by_right, input$apt_public_hearing)
      cur_locality <- dplyr::filter(zoning, jurisdiction %in% input$select_juris, family4_treatment %in% apt_filter_values)
    } else if (cur == "adus") {
      adu_filter_values <- get_filter_values(input$adu_by_right, input$adu_public_hearing)
      cur_locality <- dplyr::filter(zoning, jurisdiction %in% input$select_juris, type == "Primarily Residential", accessory_treatment %in% adu_filter_values)
    } else if (cur == "missmiddle") {
      msm_filter_values <- get_filter_values(input$msm_by_right, input$msm_public_hearing)
      cur_locality <- dplyr::filter(zoning, jurisdiction %in% input$select_juris,
                                    family2_treatment %in% msm_filter_values,
                                    family3_treatment %in% msm_filter_values,
                                    family4_treatment %in% msm_filter_values)
    }
    
    full_locality <- dplyr::filter(zoning, jurisdiction %in% input$select_juris)
    
    # This represents the current _locality_
    pct_value <- cur_locality %>%
      dplyr::mutate(selected_acres = sum(acres)) |>
      dplyr::mutate(total_jurisdiction = sum(unique(full_locality$total_area))) |>
      dplyr::mutate(pct = scales::percent(selected_acres/total_jurisdiction), accuracy = 0.1) %>%
      dplyr::pull(pct) %>%
      unique()
    
    # All of Virginia
    if (cur == "zoning_layer") {
      all_va <- zoning
    } else if (cur == "sfd") {
      all_va <- dplyr::filter(zoning, sfd == "t")
    } else if (cur == "apts") {
      apt_filter_values <- get_filter_values(input$apt_by_right, input$apt_public_hearing)
      all_va <- dplyr::filter(zoning, family4_treatment %in% apt_filter_values)
    } else if (cur == "adus") {
      adu_filter_values <- get_filter_values(input$adu_by_right, input$adu_public_hearing)
      all_va <- dplyr::filter(zoning, type == "Primarily Residential", accessory_treatment %in% adu_filter_values)
    } else if (cur == "missmiddle") {
      msm_filter_values <- get_filter_values(input$msm_by_right, input$msm_public_hearing)
      all_va <- dplyr::filter(zoning,
                              family2_treatment %in% msm_filter_values,
                              family3_treatment %in% msm_filter_values,
                              family4_treatment %in% msm_filter_values)
    }
    
    full_va <- zoning
    
    pct_value_va <- all_va %>%
      dplyr::mutate(selected_acres = sum(acres)) |>
      dplyr::mutate(total_jurisdiction = sum(unique(full_va$total_area))) |>
      dplyr::mutate(pct = scales::percent(selected_acres/total_jurisdiction), accuracy = 0.1) %>%
      dplyr::pull(pct) %>%
      unique()
    
    # Current view
    
    if (cur == "zoning_layer") {
      cur_viz <- viz
    } else if (cur == "sfd") {
      cur_viz <- dplyr::filter(viz, sfd == "t")
    } else if (cur == "apts") {
      apt_filter_values <- get_filter_values(input$apt_by_right, input$apt_public_hearing)
      cur_viz <- dplyr::filter(viz, family4_treatment %in% apt_filter_values)
    } else if (cur == "adus") {
      adu_filter_values <- get_filter_values(input$adu_by_right, input$adu_public_hearing)
      cur_viz <- dplyr::filter(viz, type == "Primarily Residential", accessory_treatment %in% adu_filter_values)
    } else if (cur == "missmiddle") {
      msm_filter_values <- get_filter_values(input$msm_by_right, input$msm_public_hearing)
      cur_viz <- dplyr::filter(viz,
                               family2_treatment %in% msm_filter_values,
                               family3_treatment %in% msm_filter_values,
                               family4_treatment %in% msm_filter_values)
    }
    
    full_viz <- viz
    
    pct_value_viz <- cur_viz %>%
      dplyr::mutate(selected_acres = sum(acres)) |>
      dplyr::mutate(total_jurisdiction = sum(unique(full_viz$acres))) |>
      dplyr::mutate(pct = scales::percent(selected_acres/total_jurisdiction), accuracy = 0.1) %>%
      dplyr::pull(pct) %>%
      unique()
    
    # Debug
    
    
    paste(
      "<strong>Percent of total developable land based on:</strong>",
      "<hr style='margin-top: 5px; margin-bottom: 5px;'>",
      "<div style='font-size: 0.9em;'>",  # Add this line to reduce font size
      "Current view: <br> <strong>",  pct_value_viz, "</strong><br>",
      "Selected localities:<br> <strong>", pct_value, "</strong><br>",
      "All of Virginia: <br> <strong>", pct_value_va, "</strong>"
    )
    
    
  })
  
  output$text <- renderText({
    
    req(text_calc())
    
    text_calc()
    # current_layer()
    
  })
  
}

shiny::shinyApp(ui = ui, server = server)