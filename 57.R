# Load required packages with error handling
tryCatch({
  library(tidyverse)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggrepel)
  library(ggspatial)
  library(osmdata)
  library(memoise)
}, error = function(e) {
  message("Error loading packages: ", e$message)
  quit(status = 1)
})

# Define the color scheme
category_colors <- c(
  "Research" = "#0F1F2C",
  "Finance and Programmes" = "#90876E",
  "Engagement, Advocacy and Capacity Building" = "#1E4611",
  "Policy" = "#CD1A1B"
)

# Load and process the CSV data
tryCatch({
  message("Loading base data...")
  # Read CSV with encoding handling
  base_data <- read.csv("base.csv", sep = ";", stringsAsFactors = FALSE, 
                       fileEncoding = "UTF-8", na.strings = c("", "NA")) %>%
    # Remove rows with missing coordinates
    filter(!is.na(lon) & !is.na(lat)) %>%
    # Convert columns to numeric, replacing NA with 0
    mutate(across(c(Research, Finance_programmes, Policy, Data_Providers, 
                   `Engagement, Advocacy, and Capacity Building`), 
                 ~ifelse(is.na(.), 0, as.numeric(as.character(.))))) %>%
    # Create derived columns
    mutate(
      institution_type = ifelse(Data_Providers == 1, "Data Provider", "Other"),
      has_research = Research == 1,
      has_finance = Finance_programmes == 1,
      has_engagement = `Engagement, Advocacy, and Capacity Building` == 1,
      has_policy = Policy == 1
    )
  
  message("Data processed successfully")
  
  # Convert to SF object
  base_sf <- st_as_sf(base_data, coords = c("lon", "lat"), crs = 4326)
  sf_use_s2(FALSE)
  message("SF object created successfully")
  
}, error = function(e) {
  message("Error processing data: ", e$message)
  quit(status = 1)
})

# Update the theme function with refined styling
theme_map_clean <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray95", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20, "mm"),
      legend.position = "right",
      legend.box.spacing = unit(2, "mm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

# Function to add points and labels with updated styling
add_points_and_labels <- function(p, data, bbox = NULL) {
  if (!is.null(bbox)) {
    data <- st_crop(data, bbox)
  }
  
  # Add base shapes - triangles for Data Providers, circles for others
  p <- p +
    geom_sf(data = data %>% filter(institution_type == "Data Provider"),
            shape = 24, size = 3, fill = "white", color = "black", stroke = 0.5) +
    geom_sf(data = data %>% filter(institution_type != "Data Provider"),
            shape = 21, size = 3, fill = "white", color = "black", stroke = 0.5)  # Changed to circle
  
  # Add category-specific points
  for (category in names(category_colors)) {
    category_data <- switch(category,
      "Research" = filter(data, has_research),
      "Finance and Programmes" = filter(data, has_finance),
      "Engagement, Advocacy and Capacity Building" = filter(data, has_engagement),
      "Policy" = filter(data, has_policy)
    )
    
    p <- p +
      geom_sf(data = category_data,
              shape = ifelse(category_data$institution_type == "Data Provider", 24, 21),  # Triangle or circle
              size = 3,
              fill = category_colors[category],
              color = "black",
              stroke = 0.5,
              alpha = 0.8)
  }
  
  # Add legend
  p <- p +
    scale_shape_manual(
      name = "Institution Type",
      values = c("Data Provider" = 24, "Other" = 21),
      labels = c("Data Provider", "Other Partner")
    ) +
    scale_fill_manual(
      name = "Focus Areas",
      values = category_colors,
      guide = guide_legend(override.aes = list(shape = 21))  # Use circles in legend
    )
  
  return(p)
}

# Create Cape Town map
create_cape_town_map <- function() {
  message("Creating Cape Town map...")
  bbox <- st_bbox(c(xmin = 18.35, ymin = -34.00, xmax = 18.50, ymax = -33.85))
  
  p <- ggplot() +
    geom_sf(data = ne_countries(scale = "large", returnclass = "sf"),
            fill = "white", color = "gray80") +
    theme_map_clean() +
    labs(title = "Cape Town Region",
         subtitle = "Research and Collaboration Network")
  
  p <- add_points_and_labels(p, base_sf, bbox)
  p + coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"]))
}

# Create Gauteng map
create_gauteng_map <- function() {
  message("Creating Gauteng Region map...")
  bbox <- st_bbox(c(xmin = 27.80, ymin = -26.40, xmax = 28.40, ymax = -25.60))
  
  p <- ggplot() +
    geom_sf(data = ne_countries(scale = "large", returnclass = "sf"),
            fill = "white", color = "gray80") +
    theme_map_clean() +
    labs(title = "Gauteng Region (Johannesburg & Pretoria)",
         subtitle = "Research and Collaboration Network")
  
  p <- add_points_and_labels(p, base_sf, bbox)
  p + coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
               ylim = c(bbox["ymin"], bbox["ymax"]))
}

# Create Europe map with refined styling
create_europe_map <- function() {
  message("Creating Europe map...")
  bbox <- st_bbox(c(xmin = -20, ymin = 30, xmax = 50, ymax = 70))
  
  p <- ggplot() +
    # Add subtle sea color
    geom_rect(aes(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
              fill = "#F8FBFF", color = NA) +  # Very light blue for sea
    # Add countries with subtle land color
    geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"),
            fill = "#F5F5F5", color = "gray80") +  # Light gray for land
    theme_map_clean()
  
  p <- add_points_and_labels(p, base_sf, bbox)
  
  # Add labels with improved spacing
  p <- p +
    geom_text_repel(
      data = st_crop(base_sf, bbox),
      aes(label = Institution, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      force = 4,
      max.overlaps = 30,
      box.padding = 1.2,
      point.padding = 0.5,
      min.segment.length = 0.1,
      segment.color = "gray50",
      segment.size = 0.2,
      seed = 42
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE,
             crs = st_crs("+proj=lambert +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10"))  # Lambert projection for better Europe view
  
  return(p)
}

# Create Southern Africa map with refined styling and inset markers
create_southern_africa_map <- function() {
  message("Creating Southern Africa map...")
  bbox <- st_bbox(c(xmin = -25, ymin = -35, xmax = 45, ymax = 5))
  
  # Create data frame for inset map locations
  inset_locations <- data.frame(
    city = c("Johannesburg & Pretoria", "Cape Town"),
    lon = c(28.0473, 18.4241),
    lat = c(-26.2041, -33.9249)
  ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  p <- ggplot() +
    # Add subtle sea color
    geom_rect(aes(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
              fill = "#F8FBFF", color = NA) +  # Very light blue for sea
    # Add countries with subtle land color
    geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"),
            fill = "#F5F5F5", color = "gray80") +  # Light gray for land
    theme_map_clean()
  
  # Add empty red squares for inset map locations
  p <- p +
    geom_sf(data = inset_locations,
            shape = 22,  # Empty square
            size = 3,
            fill = "white",
            color = "#CD1A1B",  # Red outline
            stroke = 1)
  
  p <- add_points_and_labels(p, base_sf, bbox)
  
  # Add labels with improved spacing
  p <- p +
    geom_text_repel(
      data = st_crop(base_sf, bbox),
      aes(label = Institution, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      force = 4,
      max.overlaps = 30,
      box.padding = 1.2,
      point.padding = 0.5,
      min.segment.length = 0.1,
      segment.color = "gray50",
      segment.size = 0.2,
      seed = 42
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE)
  
  return(p)
}

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Generate and save all maps
message("Generating all maps...")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Function to save both PDF and PNG versions
save_map <- function(map, name) {
  ggsave(file.path("output", paste0(name, "_", timestamp, ".pdf")),
         map, width = 12, height = 8, device = cairo_pdf)
  ggsave(file.path("output", paste0(name, "_", timestamp, ".png")),
         map, width = 12, height = 8, dpi = 300)
}

# Create and save all maps
maps <- list(
  cape_town = create_cape_town_map(),
  gauteng = create_gauteng_map(),
  southern_africa = create_southern_africa_map(),
  europe = create_europe_map()
)

# Save all maps
walk2(maps, names(maps), save_map)

message("All maps created and saved in output directory with timestamp: ", timestamp)
