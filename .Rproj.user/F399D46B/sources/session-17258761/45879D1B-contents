# Install required packages if not already installed
required_packages <- c("leaflet", "dplyr", "readr", "htmlwidgets", "jsonlite")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries with error checking
for(package in required_packages) {
  tryCatch(
    {
      library(package, character.only = TRUE)
    },
    error = function(e) {
      message(sprintf("Error loading package %s: %s", package, e))
    }
  )
}

# Read the data with error handling
tryCatch(
  {
    df <- read_csv("partners_data_with_coords.csv", 
                   col_types = cols(
                     Institution = col_character(),
                     City = col_character(),
                     Country = col_character(),
                     CHAMNHA = col_double(),
                     HEAT = col_double(),
                     ENBEL = col_double(),
                     GHAP = col_double(),
                     HAPI = col_double(),
                     BioHEAT = col_double(),
                     HIGH_Horizons = col_double(),
                     Funder = col_double(),
                     lon = col_double(),
                     lat = col_double()
                   ))
  },
  error = function(e) {
    stop("Error reading CSV file: Make sure 'partners_data_with_coords.csv' is in your working directory")
  }
)

# Convert data frame columns to appropriate types
df <- as.data.frame(df)

# Define project colors to match the screenshot
project_colors <- c(
  'CHAMNHA' = '#4B9CD3',     # Blue
  'HEAT' = '#E94F64',        # Pink/Red
  'ENBEL' = '#00A087',       # Teal
  'GHAP' = '#E74C3C',        # Red
  'HAPI' = '#3498DB',        # Light Blue
  'BioHEAT' = '#E67E22',     # Orange
  'HIGH_Horizons' = '#2C3E50' # Dark Gray
)

# Create base map
m <- leaflet(df) %>%
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
           attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
  setView(lng = 20, lat = 0, zoom = 3)

# Add markers for each institution
for(i in 1:nrow(df)) {
  # Calculate number of projects
  projects <- names(project_colors)[c(df[i,]$CHAMNHA == 1,
                                      df[i,]$HEAT == 1,
                                      df[i,]$ENBEL == 1,
                                      df[i,]$GHAP == 1,
                                      df[i,]$HAPI == 1,
                                      df[i,]$BioHEAT == 1,
                                      df[i,]$HIGH_Horizons == 1)]
  
  num_projects <- length(projects)
  
  if(num_projects > 0) {
    # Create popup content
    popup_content <- paste0(
      "<div style='font-family: Arial; min-width: 200px;'>",
      "<h4>", df[i,]$Institution, "</h4>",
      "<b>Location:</b> ", df[i,]$City, ", ", df[i,]$Country, "<br>",
      "<b>Projects:</b><br>",
      paste(projects, collapse = "<br>"),
      "</div>"
    )
    
    # Calculate marker size
    radius <- (num_projects * 5) + 8
    
    # Add marker
    if(df[i,]$Funder == 1) {
      # Funder marker
      m <- m %>% addCircleMarkers(
        lng = df[i,]$lon,
        lat = df[i,]$lat,
        radius = radius,
        color = "black",
        weight = 2,
        fillColor = "black",
        fillOpacity = 0.7,
        popup = popup_content
      )
    } else {
      # Institution marker
      m <- m %>% addCircleMarkers(
        lng = df[i,]$lon,
        lat = df[i,]$lat,
        radius = radius,
        color = "white",
        weight = 1,
        fillColor = project_colors[projects[1]],
        fillOpacity = 0.7,
        popup = popup_content
      )
    }
  }
}

# Add title with adjusted style and position
title_html <- paste0(
  "<div style='",
  "padding: 6px 8px;",
  "background: white;",
  "background: rgba(255,255,255,0.9);",
  "box-shadow: 0 0 15px rgba(0,0,0,0.2);",
  "border-radius: 5px;",
  "line-height: 1.2;",
  "'>",
  "<h3 style='",
  "margin: 0 0 5px 0;",
  "color: #333;",
  "font-size: 16px;",
  "'>Global Health Research Partnership Network</h3>",
  "<p style='",
  "margin: 0;",
  "font-size: 12px;",
  "color: #666;",
  "font-style: italic;",
  "'>Size indicates number of project participations</p>",
  "</div>"
)

m <- m %>% addControl(
  html = title_html,
  position = "topleft",
  className = "map-title"
)

# Add legend with consistent styling
legend_html <- paste0(
  "<div style='",
  "padding: 6px 8px;",
  "background: white;",
  "background: rgba(255,255,255,0.9);",
  "box-shadow: 0 0 15px rgba(0,0,0,0.2);",
  "border-radius: 5px;",
  "line-height: 1.5;",
  "'>",
  "<strong>Research Programs</strong><br>",
  paste(sapply(names(project_colors), function(proj) {
    sprintf("<span style='color: %s;'>●</span> %s<br>",
            project_colors[proj], proj)
  }), collapse = ""),
  "<span style='color: black;'>●</span> Funding Organization",
  "</div>"
)

m <- m %>% addControl(
  html = legend_html,
  position = "bottomleft",
  className = "info legend"
)

# Save the map
saveWidget(m, file = "partnership_map_r.html", selfcontained = TRUE) 