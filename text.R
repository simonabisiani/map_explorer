# Texts
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
  column(
    4,
    h3("Data"),
    p(
      "Our analysis uses the ", strong("UKTwitNewsCor dataset"), ", containing over ", strong("2.5 million articles"), " published between 2020-2022 across 360 UK local outlets. We applied a ", strong("two-stage stratified sampling approach:"), "first stratifying by geography and publisher, then sampling eight weekdays per stratum-year. This produced a final sample of ", strong("465,105 articles (18.35% of corpus)"), " while maintaining complete spatial and publisher coverage.",
      br(),
      'Each', icon('square'), 'represent a news outlet. The colour is the number of articles. Range is 31 to 8,427. Hover over a', icon('square'), 'to see information about the outlet.'),
  ),
  column(
    8,
    br(),
    plotlyOutput("grid_plot_articles"),
    br()
  )
)

text_grid2 <- HTML(
  "<p>We identified locations using <strong>SpaCy's transformer-based named entity recognition</strong>, then matched them against UK gazetteers (Ordnance Survey Open Names and OpenStreetMap). When multiple coordinates existed for a location, we used <strong>gemma2-9b LLM</strong> to select the most appropriate match based on article context. Our pipeline processed <strong>465,105 articles</strong>, successfully identifying locations in <strong>365,714 articles (78.6%)</strong>. Of the <strong>1,657,425 location mentions</strong> extracted, <strong>1,388,720 (83.8%) were successfully geocoded</strong>, resulting in a final analytical sample of <strong>347,291 articles</strong> covering <strong>158,288 unique UK locations</strong>.<p>"
)

text1 <- fluidRow(
  column(
    4,
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
  column(
    8,
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