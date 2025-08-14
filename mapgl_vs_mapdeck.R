library(shiny)
library(mapdeck)
library(mapgl)  # Add this line
library(sf)     # Add this for spatial data handling

set_token("pk.eyJ1Ijoic2ltb25hYmlzaWFuaSIsImEiOiJjbWU3OTZ4ZnMwMjExMmlwZ3ZrYngwbHVoIn0.TTSqd7FYLM5QJaaSWGYrig")

# ---- 2. Random example data over the UK ----
set.seed(42)
uk_points <- data.frame(
  lon = runif(50, -6, 2),       # UK longitudes
  lat = runif(50, 50, 58),      # UK latitudes
  value = sample(1:100, 50)
)

# Convert to sf object for mapgl
uk_points_sf <- st_as_sf(uk_points, coords = c("lon", "lat"), crs = 4326)

# ---- 3. UI ----
ui <- fluidPage(
  titlePanel("Mapdeck vs Mapgl in Shiny"),
  fluidRow(
    column(6, h3("Mapdeck"), mapdeckOutput("mapdeck_map", height = "500px")),
    column(6, h3("Mapgl"), mapboxglOutput("mapgl_map", height = "500px"))
  )
)

# ---- 4. Server ----
server <- function(input, output, session) {

  # Mapdeck example
  output$mapdeck_map <- renderMapdeck({
    mapdeck(style = mapdeck_style("light"), pitch = 0, zoom = 5,
            location = c(-2, 54)) %>%
      add_scatterplot(
        data = uk_points,
        lat = "lat",
        lon = "lon",
        radius = 20000,
        fill_colour = "value",
        legend = TRUE
      )
  })

  # Mapgl example (corrected)
  output$mapgl_map <- renderMapboxgl({
    mapboxgl(
      style = mapbox_style("light"),  # Set map style
      center = c(-2, 54),               # Center on UK
      zoom = 5                          # Set zoom level
    ) |>
      add_source(id = "uk_data", data = uk_points_sf) |>  # Add data source
      add_circle_layer(
        id = "points",
        source = "uk_data",             # Reference the source
        circle_color = "blue",
        circle_radius = 8,
        circle_opacity = 0.7
      )
  })
}

shinyApp(ui, server)


# Try this super simple test:
mapboxgl(
  style = "https://demotiles.maplibre.org/style.json",
  center = c(0, 50),
  zoom = 4
)
