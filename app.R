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

# data <- readRDS("../../../geographic-local-media-classifier/replication_data_geo_newsmapper/analysis_data.rds")
# 
# data2 <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=657017351#gid=657017351',
#                                    sheet = 'Domains_PINF_cluster')
# 
# data <- data |> left_join(data2, by = 'domain')
# data <- data |> select(column_names <- c("article_text", "value", "Latitude", "Longitude", "LAD24CD", "domain", "resolved_url", "article_author", "owner.x", "date", "constructed_week", "stratum_id", "primary_location.x", "outlet_lat", "outlet_lon", "TITLE", "REGISTERED_ADDRESS", "OFFICE_ADDRESS", "articles_n", "coverage_area_description", "scope", "New_cluster_name", "Area", "Radius", "Districts", "Entropy", "Gini", "MoranI", "DistCV", "Pct10km"))
# 
# saveRDS(data, "analysis_data.rds")
# 
# data <- readRDS("analysis_data.rds")
# data <- data |> select(-c(article_text, constructed_week, stratum_id))
# saveRDS(data, "analysis_data1.rds", compress = "xz")  # Best compression

data <- readRDS('analysis_data1.rds')
outlet_list <- unique(data$domain)


# Data
data3 <- readRDS("analysis_small.rds")

outlet_info <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=1038895799#gid=1038895799",
  sheet = "domains_w_coverage_geocoded"
) %>%
  dplyr::select(
    domain,
    articles_n,
    owner,
    coverage_area_description,
    primary_location,
    lat,
    lon,
    scope
  )

# outlets_clustered <- googlesheets4::read_sheet(
#   ss = "https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=1038895799#gid=1038895799",
#   sheet = "clustering_results"
# ) %>%
#   dplyr::select(1:10)

# # Add ranks
# outlets_clustered <- outlets_clustered %>%
#   mutate(
#     gini_rank = rank(-Gini),
#     convex_rank = rank(-Area),
#     radius_rank = rank(-Radius),
#     lsoa_rank = rank(-Districts),
#     morans_rank = rank(-MoranI),
#     # location_rank = rank(-DistCV),
#     entropy_rank = rank(-Entropy),
#   )

data_stats <- data3 %>% left_join(outlet_info, by = "domain") %>%
  mutate(primary_lat = lat, primary_lon = lon) %>%
  dplyr::select(-lat, -lon) |> 
  group_by(owner, domain, articles_n) |>
  summarise(locations_n = sum(n), .groups = 'drop') |>
  mutate(
    owner = ifelse(
      owner %in%
        c('Reach PLC', 'Newsquest Media Group Ltd', 'National World Plc'),
      owner,
      'Other'
    )
  ) %>%
  group_by(owner) %>%
  arrange(desc(articles_n)) %>%
  mutate(
    outlet_id = row_number(),
    col = ((outlet_id - 1) %% 7) + 1,
    row = ceiling(outlet_id / 7)
  ) %>%
  ungroup()

owner_order <- data_stats %>%
  count(owner, sort = TRUE) %>%
  pull(owner)

grid_data <- data_stats %>%
  mutate(owner = factor(owner, levels = rev(owner_order))) |>
  arrange(desc(locations_n))


text0 <- p(
  br(),
  "Local news plays a crucial role in shaping community identity, political participation, and accountability (Hayes & Lawless, 2018; Usher, 2019). Yet,",
  strong("traditional methods of identifying what makes news local"),
  "— such as newsroom addresses or self-declared coverage areas—",
  strong("often fail to capture the true geography that news outlets focus on"),
  " in their reporting.",
  br(),
  br(),
  
  "Media consolidation and digital transformation have changed how local news is produced, frequently centralising production and weakening the connection between newsrooms and their communities (Sharman, 2021; Karlsson & Rowe, 2019; Freeman, 2020).",
  br(),
  br(),
  
  "This app offers an interactive way to explore the distinctive spatial patterns of local news coverage, directly derived from the geographic focus present within the content itself. Using advanced computational methods to extract and analyse location mentions across thousands of UK news articles, we reveal how different outlets vary in their geographic reach and specificity.",
)

# Updated text_grid1 with plot layout:
text_grid1 <- fluidRow(
  column(4,
         br(),
         h3('Data'),
         p(
           'Our analysis uses the ', strong('UKTwitNewsCor dataset'), ', containing over ', strong('2.5 million articles'), ' published between 2020-2022 across 360 UK local outlets. We applied a ', strong('two-stage stratified sampling approach:'), 'first stratifying by geography and publisher, then sampling eight weekdays per stratum-year. This produced a final sample of ', strong('465,105 articles (18.35% of corpus)'), ' while maintaining complete spatial and publisher coverage.'
         )
  ),
  column(8,
         br(),
         br(),
         plotOutput("grid_plot_articles")
  )
)

text_grid2 <- HTML(
  "<p>We identified locations using <strong>SpaCy's transformer-based named entity recognition</strong>, then matched them against UK gazetteers (Ordnance Survey Open Names and OpenStreetMap). When multiple coordinates existed for a location, we used <strong>gemma2-9b LLM</strong> to select the most appropriate match based on article context. Our pipeline processed <strong>465,105 articles</strong>, successfully identifying locations in <strong>365,714 articles (78.6%)</strong>. Of the <strong>1,657,425 location mentions</strong> extracted, <strong>1,388,720 (83.8%) were successfully geocoded</strong>, resulting in a final analytical sample of <strong>347,291 articles</strong> covering <strong>158,288 unique UK locations</strong>.<p>"
)

text1 <- fluidRow(
  column(4,
         div(
           h2("Feature Engineering"),
           br(),
           p("We characterise local news coverage using a ", strong("four-dimensional framework"), ":"),
           br(),
           p(strong("1. Spatial Extent:"), " Captures the geographic footprint."),
           br(),
           p(strong("2. Administrative Reach:"), " Reflects coverage across administrative boundaries."),
           br(),
           p(strong("3. Spatial Heterogeneity:"), " Assesses diversity and clustering patterns."),
           br(),
           p(strong("4. Distance Decay:"), " Examines how coverage intensity decreases with distance from the outlet's primary location.")
         )
  ),
  column(8,
         br(),
         br(),
         DT::dataTableOutput("metrics_table")
  )
)


# Create metrics definition table
metrics_definitions <- data.frame(
  Dimension = c(
    "Spatial Extent", "Spatial Extent", 
    "Administrative Reach", "Administrative Reach",
    "Spatial Heterogeneity", "Spatial Heterogeneity",
    "Distance Decay", "Distance Decay"
  ),
  Metric = c(
    "Area (km²)", "Radius (km)",
    "Districts", "Gini",
    "Entropy", "Moran's I",
    "Distance CV", "Within 10km (%)"
  ),
  Definition = c(
    "Area of minimum convex polygon enclosing all locations within the 75% radius",
    "Smallest radius from primary location that contains 75% of all location mentions by frequency",
    "Total number of unique administrative districts covered by outlet's location mentions",
    "Inequality measure across demographic area types using ONS Supergroup classifications (0 = equal, 1 = concentrated)",
    "Shannon's entropy of mention proportions across districts, measuring coverage evenness",
    "Spatial autocorrelation with 50km binary weights (positive = clustering, negative = dispersion, ~0 = random)",
    "Coefficient of variation of distances from outlet's primary location to all mentioned locations",
    "Fraction of all location mentions falling within 10km of outlet's primary location"
  ),
  Interpretation = c(
    "Larger areas indicate broader territorial coverage within core region",
    "Larger radius indicates wider geographic reach of core coverage (75% of mentions)",
    "More districts suggest wider administrative coverage across boundaries",
    "Higher values indicate concentrated coverage in specific demographic contexts",
    "Higher values indicate more balanced attention across administrative districts",
    "Positive values indicate spatially clustered coverage patterns",
    "Higher values indicate more geographically scattered/dispersed coverage",
    "Higher percentages indicate more localised coverage focus"
  )
)

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
  
  # Grid plot for articles
  output$grid_plot_articles <- renderPlot({
    ggplot(grid_data, aes(x = col, y = -row)) +
      geom_tile(aes(fill = articles_n), color = "white") +
      facet_wrap(~owner, ncol = 4) +
      scale_fill_gradient(
        low = "#d4d4c5",
        high = "#2f3e46",
        name = "Number of Articles",
        na.value = "white",
        limits = c(0, 8427),
        breaks = c(0, 500, 2000, 5000, 8427),  # Fewer breaks
        guide = guide_colorbar(
          show.limits = TRUE,
          title.position = "top",
          title.hjust = 0.5,
          barwidth = unit(5, "cm"),  # Make legend wider
          barheight = unit(0.5, "cm")
        )
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        panel.spacing = unit(0.5, "lines")
      )
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
  
  # Main Map (simplified - fixed settings)
  output$map <- renderLeaflet({
    if(input$outlet == "") {
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 2.5, lat = 55.3, zoom = 6)
    } else {
      outlet_data <- filtered_data()
      outlet_info <- outlet_metadata()
      
      # Create base map
      map <- leaflet() %>%
        addProviderTiles("CartoDB.Positron")
      
      if(nrow(outlet_data) > 0) {
        map <- map %>%
          fitBounds(
            lng1 = min(outlet_data$Longitude), lat1 = min(outlet_data$Latitude),
            lng2 = max(outlet_data$Longitude), lat2 = max(outlet_data$Latitude)
          )
        
        # Add heatmap (fixed settings)
        if(nrow(outlet_data) > 5) {
          map <- map %>%
            addHeatmap(
              data = outlet_data,
              lng = ~Longitude, lat = ~Latitude,
              radius = 20,
              blur = 15,
              max = 0.05,
              gradient = c("0.0" = "#542788", "0.5" = "#fee0b6", "1.0" = "#b35806")
            )
        }
        
        # Add story points (fixed settings)
        map <- map %>%
          addCircleMarkers(
            data = outlet_data,
            lng = ~Longitude, lat = ~Latitude,
            radius = 0.8,
            fillOpacity = 0.4,
            color = "#2c3e50",
            stroke = FALSE,
            popup = ~paste0("<b>Story Location</b><br>",
                            "Coordinates: ", round(Latitude, 3), ", ", round(Longitude, 3), "<br>",
                            "URL: <a href='", resolved_url, "' target='_blank'>", 
                            substr(resolved_url, 1, 50), "...</a>")
          )
        
        # Add outlet location (fixed settings)
        if(!is.na(outlet_info$outlet_lat) && !is.na(outlet_info$outlet_lon)) {
          map <- map %>%
            addMarkers(
              lng = outlet_info$outlet_lon, lat = outlet_info$outlet_lat,
              popup = paste0("<b>", outlet_info$domain, "</b><br>",
                             outlet_info$primary_location.x, "<br>",
                             "Owner: ", outlet_info$owner.x),
              icon = makeIcon(
                iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
                iconWidth = 25, iconHeight = 41,
                iconAnchorX = 12, iconAnchorY = 41
              )
            )
        }
      }
      
      map
    }
  })
  
  # Clear map when outlet changes
  observe({
    if(input$outlet == "") {
      leafletProxy("map") %>% clearShapes() %>% clearMarkers()
    }
  })
}

shinyApp(ui = ui, server = server)

