# Load required packages
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("rnaturalearth")) install.packages("rnaturalearth"); library(rnaturalearth)
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata"); library(rnaturalearthdata)
if (!require("ggrepel")) install.packages("ggrepel"); library(ggrepel)
if (!require("ggspatial")) install.packages("ggspatial"); library(ggspatial)
if (!require("osmdata")) install.packages("osmdata"); library(osmdata)
if (!require("memoise")) install.packages("memoise"); library(memoise)

# Define the color scheme
category_colors <- c(
  "Research" = "#0F1F2C",
  "Finance and Programmes" = "#90876E",
  "Engagement, Advocacy and Capacity Building" = "#1E4611",
  "Policy" = "#CD1A1B"
)

# Load and process the CSV data
message("Loading base data...")
file_path <- "C:/Users/CraigParker/OneDrive - Wits PHR/Desktop/Wellcome_climate_center/base.csv"

if (!file.exists(file_path)) {
  alt_paths <- c("base.csv", "../base.csv", "./base.csv", "data/base.csv")
  for (path in alt_paths) {
    if (file.exists(path)) {
      file_path <- path
      break
    }
  }
}

# Read and clean data
base_data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    institution_type = ifelse(Data_Providers == 1, "Data Provider", "Other"),
    has_research = Research == 1,
    has_finance = Finance_programmes == 1,
    has_engagement = `Engagement..Advocacy..and.Capacity.Building` == 1,
    has_policy = Policy == 1
  )

# Convert to SF object
base_sf <- st_as_sf(base_data, coords = c("lon", "lat"), crs = 4326)
sf_use_s2(FALSE)

# Define consistent theme
theme_map_clean <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
      plot.margin = margin(20, 20, 20, 20, "mm"),
      legend.position = "right",
      legend.box.spacing = unit(2, "mm"),
      axis.text = element_blank(),  # Remove axis text
      axis.title = element_blank()  # Remove axis titles
    )
}

# Function to add points and labels
add_points_and_labels <- function(p, data, bbox = NULL) {
  if (!is.null(bbox)) {
    data <- st_crop(data, bbox)
  }
  
  # Add base shapes
  p <- p +
    geom_sf(data = data %>% filter(institution_type == "Data Provider"),
            shape = 24, size = 3, fill = "white", color = "black", stroke = 0.5) +
    geom_sf(data = data %>% filter(institution_type != "Data Provider"),
            shape = 22, size = 3, fill = "white", color = "black", stroke = 0.5)
  
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
              shape = ifelse(category_data$institution_type == "Data Provider", 24, 22),
              size = 3, fill = category_colors[category], color = "black",
              stroke = 0.5, alpha = 0.8)
  }
  
  # Add labels with smart placement
  p <- p +
    geom_text_repel(
      data = data,
      aes(label = Institution, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      force = 2,
      max.overlaps = 30,
      box.padding = 0.8,
      min.segment.length = 0.1,
      segment.color = "gray50",
      segment.size = 0.2,
      seed = 42  # For consistent label placement
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

# Create Europe map with wider view
create_europe_map <- function() {
  message("Creating Europe map...")
  bbox <- st_bbox(c(xmin = -20, ymin = 30, xmax = 50, ymax = 70))  # Wider view of Europe
  
  p <- ggplot() +
    geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"),
            fill = "white", color = "gray80") +
    theme_map_clean()  # No title/subtitle for minimalist design
  
  p <- add_points_and_labels(p, base_sf, bbox) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE)  # Remove padding
  
  return(p)
}

# Create Southern Africa map with city markers
create_southern_africa_map <- function() {
  message("Creating Southern Africa map...")
  bbox <- st_bbox(c(xmin = -25, ymin = -35, xmax = 45, ymax = 5))
  
  # Create data frame for city markers
  cities <- data.frame(
    city = c("Johannesburg", "Cape Town"),
    lon = c(28.0473, 18.4241),
    lat = c(-26.2041, -33.9249)
  ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  p <- ggplot() +
    geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"),
            fill = "white", color = "gray80") +
    theme_map_clean()
  
  # Add city markers
  p <- p +
    geom_sf(data = cities,
            shape = 22,  # Square
            size = 3,
            fill = "#CD1A1B",  # Red
            color = "black",
            stroke = 0.5)
  
  # Add points and labels with increased force
  p <- p +
    geom_sf(data = base_sf %>% filter(institution_type == "Data Provider"),
            shape = 24, size = 3, fill = "white", color = "black", stroke = 0.5) +
    geom_sf(data = base_sf %>% filter(institution_type != "Data Provider"),
            shape = 22, size = 3, fill = "white", color = "black", stroke = 0.5)
  
  # Add category-specific points
  for (category in names(category_colors)) {
    category_data <- switch(category,
      "Research" = filter(base_sf, has_research),
      "Finance and Programmes" = filter(base_sf, has_finance),
      "Engagement, Advocacy and Capacity Building" = filter(base_sf, has_engagement),
      "Policy" = filter(base_sf, has_policy)
    )
    
    p <- p +
      geom_sf(data = category_data,
              shape = ifelse(category_data$institution_type == "Data Provider", 24, 22),
              size = 3,
              fill = category_colors[category],
              color = "black",
              stroke = 0.5,
              alpha = 0.8)
  }
  
  # Add labels with increased force and better spacing
  p <- p +
    geom_text_repel(
      data = st_crop(base_sf, bbox),
      aes(label = Institution, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      force = 4,  # Increased force
      max.overlaps = 30,
      box.padding = 1.2,  # Increased padding
      point.padding = 0.5,
      min.segment.length = 0.1,
      segment.color = "gray50",
      segment.size = 0.2,
      seed = 42
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE)  # Remove padding
  
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
