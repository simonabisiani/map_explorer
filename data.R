# Sys.setenv(MAPBOX_PUBLIC_TOKEN='pk.eyJ1Ijoic2ltb25hYmlzaWFuaSIsImEiOiJjbWU3OTZ4ZnMwMjExMmlwZ3ZrYngwbHVoIn0.TTSqd7FYLM5QJaaSWGYrig')

gs4_deauth()

create_base_map <- function(zoom = 5) {
  leaflet(options = leafletOptions(minZoom = 6, maxZoom = 15)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = 2, lat = 55.3, zoom = zoom) |> 
    setMaxBounds(lng1 = -8.7, lat1 = 49.8, lng2 = 1.8, lat2 = 60.9)
}

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

outlets_clustered <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=1038895799#gid=1038895799", sheet = "clustering_results") %>%
  dplyr::select(c(
    "Domain",
    "Area",
    "Radius",
    "Districts",
    "Entropy",
    "Gini",
    "MoranI",
    "DistCV"
  ))

# Add ranks
outlets_clustered <- outlets_clustered %>%
  mutate(
    gini_rank = rank(-Gini),
    convex_rank = rank(-Area),
    radius_rank = rank(-Radius),
    lsoa_rank = rank(-Districts),
    morans_rank = rank(-MoranI),
    location_rank = rank(-DistCV),
    entropy_rank = rank(-Entropy),
  )

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
    row = ((outlet_id - 1) %% 4) + 1,
    col = ceiling(outlet_id / 4)
  ) %>%
  ungroup()

owner_order <- data_stats %>%
  count(owner, sort = TRUE) %>%
  pull(owner)

grid_data <- data_stats %>%
  mutate(owner = factor(owner, levels = rev(owner_order))) |>
  arrange(desc(locations_n))
