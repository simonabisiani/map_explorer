library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(ggplot2)
library(DT)
library(geosphere)
library(paletteer)
library(bslib)
library(thematic)
library(googlesheets4)
library(plotly)

source('data.R')
source('text.R')

thematic_shiny(
  font = "auto",
  bg = "auto",
  fg = "auto")

# UI
ui <- page_navbar(
  theme = bslib::bs_theme(
    bg = "#edede9",
    fg = "#293241",
    primary = "#000814",
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto")
  ),
  
  nav_panel("Home",
            br(),
            br(),
            # article title & name
            fluidRow(
              column(1),
              column(
                7,
                align = "left",
                h1(
                  "Exploring the Spatial Properties of Local News Coverage",
                  br(),
                  style = 'font-size: 45px; font-weight: bold;'
                ),
                h4(
                  style = 'font-size:16px;',
                  "10th August 2025 | ",
                  a(
                    href = "https://simonabisiani.github.io/",
                    target = "_blank",
                    "Simona Bisiani"
                  )
                ),
                br(),
                text0,
                br(),
                text_grid1,
                br(),
                text_grid2,
                br(),
                text1,
                br()
              ),
              column(4)
            ),
            
            
  ),
  
  nav_panel("Map Explorer",
            sidebarLayout(
              sidebarPanel(
                width = 4,
                div(class = "sidebar-section",
                    selectInput("outlet", "Select News Outlet:",
                                choices = c("Choose outlet..." = "", sort(outlet_list)),
                                selected = "")
                ),
                
                conditionalPanel(
                  condition = "input.outlet != ''",
                  
                  # Outlet Information
                  div(class = "sidebar-section",
                      div(class = "outlet-info",
                          htmlOutput("outlet_info")
                      )
                  ),
                  br(),
                  br(),
                  
                  # Spatial Coverage Metrics
                  div(class = "sidebar-section",
                      h5("Spatial Coverage Metrics"),
                      DT::dataTableOutput("spatial_metrics")  # Remove height parameter
                  ),
                  br(),
                  br(),
                  
                  # Top Locations
                  div(class = "sidebar-section",
                      h5("Top 5 Covered Locations"),
                      DT::dataTableOutput("top_locations")  # Remove height parameter
                  )
                ),
              ),
              
              mainPanel(
                width = 8,
                leafletOutput("map", height = "100vh")
              ),
            ),
  )
)

# Server
server <- function(input, output) {
  
  output$grid_plot_articles <- renderPlotly({
    grid_data$hover_text <- paste("Domain:", grid_data$domain, "<br>Articles:", grid_data$articles_n)
    
    p <- ggplot(grid_data, aes(x = col, y = -row, text = hover_text)) +
      geom_tile(aes(fill = articles_n)) +
      facet_wrap(~owner, nrow = 4) +
      scale_fill_gradient(
        low = "#d4d4c5",
        high = "#2f3e46",
        name = "Articles",
        na.value = "white",
        limits = c(0, 8427),
        breaks = c(0, 2500, 5000, 8427)
      ) +
      theme_void() +
      theme(
        strip.text = element_text(size = 10, color = "#293241"), # Match fg color
        panel.spacing = unit(0.5, "lines"),
        plot.background = element_rect(fill = "#edede9", color = NA)
      ) + 
      coord_fixed()
    
    plotly_obj <- ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Roboto", color = "#293241"),
        paper_bgcolor = "#edede9",  # Match bslib bg
        plot_bgcolor = "#edede9",    # Match bslib bg
        dragmode = FALSE,
        xaxis = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        xaxis2 = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis2 = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        xaxis3 = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis3 = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        xaxis4 = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis4 = list(showline = FALSE, zeroline = FALSE, showgrid = FALSE),
        hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      ) %>%
      config(displayModeBar = FALSE)
    
    # Add borders to heatmap traces
    for (i in seq_along(plotly_obj$x$data)) {
      if (plotly_obj$x$data[[i]]$type == "heatmap") {
        plotly_obj$x$data[[i]]$xgap <- 1  # Gap between tiles horizontally
        plotly_obj$x$data[[i]]$ygap <- 1  # Gap between tiles vertically
      }
    }

    # Manually adjust annotation positions for facet titles
    for (i in seq_along(plotly_obj$x$layout$annotations)) {
      if (!is.null(plotly_obj$x$layout$annotations[[i]]$xanchor)) {
        plotly_obj$x$layout$annotations[[i]]$xanchor <- "left"
        plotly_obj$x$layout$annotations[[i]]$x <- 0  # Adjust x position as needed
      }
    }

      plotly_obj %>%
      layout(
        font = list(family = "Roboto", color = "#293241"),
        paper_bgcolor = "#edede9",
        plot_bgcolor = "#edede9") %>%
      config(displayModeBar = FALSE)
    
    hide_colorbar(plotly_obj)
  })
  
  output$metrics_table <- DT::renderDataTable({
    DT::datatable(
      metrics_definitions,
      options = list(
        pageLength = 8,
        searching = FALSE,
        info = FALSE,
        paging = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Dimension",
        backgroundColor = "#f8f9fa",
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        columns = 1:4,
        fontSize = "12px",  # Smaller font
        padding = "4px"     # Less padding
      )
  })
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$outlet != "")
    
    # Get all data for the outlet
    outlet_data <- data[data$domain == input$outlet,]
    
    # Remove rows with missing coordinates
    outlet_data <- outlet_data[!is.na(outlet_data$Latitude) & !is.na(outlet_data$Longitude),]
    
    
    # Validate that we have data
    validate(
      need(nrow(outlet_data) > 0, "No data available for selected filters")
    )
    
    return(outlet_data)
  })
  
  # Get outlet metadata
  outlet_metadata <- reactive({
    req(input$outlet != "")
    outlet_info <- data[data$domain == input$outlet,][1,]
    return(outlet_info)
  })
  
  # Outlet information display
  output$outlet_info <- renderUI({
    info <- outlet_metadata()
    HTML(paste0(
      "<strong>Domain:</strong> ", info$domain, "<br>",
      "<strong>Title:</strong> ", info$TITLE, "<br>",
      "<strong>Owner:</strong> ", info$owner.x, "<br>",
      "<strong>Registered Address:</strong> ", info$REGISTERED_ADDRESS, "<br>",
      "<strong>Office Address:</strong> ", info$OFFICE_ADDRESS, "<br>",
      "<strong>Primary Location:</strong> ", info$primary_location.x, "<br>",
      "<strong>Coverage Area:</strong> ", info$coverage_area_description, "<br>"
    ))
  })
  
  output$spatial_metrics <- DT::renderDataTable({
    info <- outlet_metadata()
    data <- filtered_data()
    
    metrics_data <- data.frame(
      Metric = c("Total Articles", "Number of Locations", "Cluster", "Area (km²)", "Radius (km)", "Districts", "Entropy", "Gini", "Moran's I", "Distance CV", "Within 10km (%)"),
      Value = c(
        info$articles_n,
        nrow(unique(data[c("Latitude", "Longitude")])),
        info$New_cluster_name,
        round(info$Area, 1),
        round(info$Radius, 1),
        info$Districts,
        round(info$Entropy, 3),
        round(info$Gini, 3),
        round(info$MoranI, 3),
        round(info$DistCV, 3),
        paste0(round(info$Pct10km, 1), "%")
      )
    )
    
    DT::datatable(metrics_data, 
                  options = list(pageLength = 10, searching = FALSE, 
                                 info = FALSE, paging = FALSE, ordering = FALSE),
                  rownames = FALSE)
  })
  
  output$top_locations <- DT::renderDataTable({
    data <- filtered_data()
    
    # Count unique articles per location using resolved_url as unique identifier
    location_counts <- data %>%
      group_by(value) %>%
      summarise(Stories = n_distinct(resolved_url), .groups = 'drop') %>%
      arrange(desc(Stories)) %>%
      head(5) %>%
      rename(Location = value)
    
    DT::datatable(location_counts, 
                  options = list(pageLength = 5, searching = FALSE, 
                                 info = FALSE, paging = FALSE, ordering = FALSE),
                  rownames = FALSE)
  })
  
  output$map <- renderLeaflet({
    # Create base map
    map <- create_base_map(zoom = ifelse(input$outlet == "", 5, 6))
    
    if(input$outlet != "") {
      outlet_data <- filtered_data()
      outlet_info <- outlet_metadata()
      
      if(nrow(outlet_data) > 0) {
        map <- map %>%
          fitBounds(
            lng1 = min(outlet_data$Longitude), lat1 = min(outlet_data$Latitude),
            lng2 = max(outlet_data$Longitude), lat2 = max(outlet_data$Latitude)
          ) %>%
          addHeatmap(
            data = outlet_data,
            lng = ~Longitude, lat = ~Latitude,
            radius = 20, blur = 15, max = 0.05,
            gradient = c("0.0" = "#542788", "0.5" = "#fee0b6", "1.0" = "#b35806")
          ) %>%
          addCircleMarkers(
            data = outlet_data,
            lng = ~Longitude, lat = ~Latitude,
            radius = 1, fillOpacity = 0.4,
            color = "#465454ff", stroke = FALSE
          ) %>%
          addMarkers(
            lng = outlet_info$outlet_lon, lat = outlet_info$outlet_lat,
            popup = paste0("<b>", outlet_info$domain, "</b><br>",
                           outlet_info$primary_location.x, "<br>",
                           "Owner: ", outlet_info$owner.x),
            icon = makeIcon(
              iconUrl = "Adjust_4.png",
              iconWidth = 25, iconHeight = 25,
              iconAnchorX = 12, iconAnchorY = 41
            )
          )
      }
    }
    
    map
  })
  
  # Clear map when outlet changes
  observe({
    if(input$outlet == "") {
      leafletProxy("map") %>% clearShapes() %>% clearMarkers()
    }
  })
  
}

shinyApp(ui = ui, server = server)
