# Make sure your file starts EXACTLY like this
# (with NO OpenStreetMap references)

# Add these packages near the top of your script
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("rnaturalearth")) {
  message("Installing rnaturalearth package for simple basemaps...")
  install.packages("rnaturalearth")
  library(rnaturalearth)
}
if (!require("rnaturalearthdata")) {
  message("Installing rnaturalearthdata package...")
  install.packages("rnaturalearthdata")
  library(rnaturalearthdata)
}
if (!require("ggrepel")) {
  install.packages("ggrepel")
  library(ggrepel)
}
if (!require("ggspatial")) {
  install.packages("ggspatial")
  library(ggspatial)
}

# Use a standard font that's available on all systems
main_font <- ""  # Empty string will use the default font

# Load and process the CSV data with hardcoded path
message("Loading base data...")
# Try to verify if the file exists
file_path <- "C:/Users/CraigParker/OneDrive - Wits PHR/Desktop/Wellcome_climate_center/base.csv"
if (!file.exists(file_path)) {
  message("WARNING: File not found at: ", file_path)
  message("Attempting to search for base.csv in current directory and parent directories...")
  
  # Alternative paths to try
  alt_paths <- c(
    "base.csv",
    "../base.csv",
    "./base.csv",
    "data/base.csv"
  )
  
  for (path in alt_paths) {
    if (file.exists(path)) {
      message("Found file at: ", path)
      file_path <- path
      break
    }
  }
}

# Try to read the file
tryCatch({
  base_data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
  message("Successfully loaded data with ", nrow(base_data), " rows")
}, error = function(e) {
  stop("Could not load the file. Error: ", e$message)
})

# Modified data cleaning section to use just the four focus areas
base_data <- base_data %>%
  # Remove rows with missing coordinates
  filter(!is.na(lon) & !is.na(lat)) %>%
  # Create an institution_type field based on available columns
  mutate(
    institution_type = case_when(
      `Data_Providers` == 1 ~ "Data Provider",
      `Official.Partners` == 1 ~ "Official Partner",
      TRUE ~ "Other"
    ),
    # Instead of primary_focus, create separate columns for each focus area
    has_policy = ifelse(Policy == 1, TRUE, FALSE),
    has_research = ifelse(Research == 1, TRUE, FALSE),
    has_finance = ifelse(Finance_programmes == 1, TRUE, FALSE),
    has_engagement = ifelse(`Engagement..Advocacy..and.Capacity.Building` == 1, TRUE, FALSE),
    # Count number of focus areas (for point size)
    focus_areas = rowSums(across(c(Policy, Research, `Engagement..Advocacy..and.Capacity.Building`, Finance_programmes), ~ifelse(is.na(.), 0, .)))
  )

# Convert to SF object for mapping
message("Converting to spatial data...")
base_sf <- st_as_sf(base_data, coords = c("lon", "lat"), crs = 4326)

# Add this right after loading the sf package
# Turn off s2 geometry for less strict operations
sf_use_s2(FALSE)

# Modified data cleaning section to use just the four focus areas
base_data <- base_data %>%
  # Remove rows with missing coordinates
  filter(!is.na(lon) & !is.na(lat)) %>%
  # Create an institution_type field based on available columns
  mutate(
    institution_type = case_when(
      `Data_Providers` == 1 ~ "Data Provider",
      `Official.Partners` == 1 ~ "Official Partner",
      TRUE ~ "Other"
    ),
    # Instead of primary_focus, create separate columns for each focus area
    has_policy = ifelse(Policy == 1, TRUE, FALSE),
    has_research = ifelse(Research == 1, TRUE, FALSE),
    has_finance = ifelse(Finance_programmes == 1, TRUE, FALSE),
    has_engagement = ifelse(`Engagement..Advocacy..and.Capacity.Building` == 1, TRUE, FALSE),
    # Count number of focus areas (for point size)
    focus_areas = rowSums(across(c(Policy, Research, `Engagement..Advocacy..and.Capacity.Building`, Finance_programmes), ~ifelse(is.na(.), 0, .)))
  )

# Convert to SF object for mapping
message("Converting to spatial data...")
base_sf <- st_as_sf(base_data, coords = c("lon", "lat"), crs = 4326)

# Add this right after loading the sf package
# Turn off s2 geometry for less strict operations
sf_use_s2(FALSE)

# First, let's add a function to load boundaries from the boundary folder
load_boundaries <- function() {
  boundary_path <- "boundary"  # Path to your boundary folder
  
  # Try to find province boundaries
  province_files <- list.files(boundary_path, pattern = "province|admin1", full.names = TRUE)
  ward_files <- list.files(boundary_path, pattern = "ward|admin4", full.names = TRUE)
  
  boundaries <- list()
  
  if (length(province_files) > 0) {
    message("Found province boundary file: ", province_files[1])
    boundaries$provinces <- st_read(province_files[1])
  }
  
  if (length(ward_files) > 0) {
    message("Found ward boundary file: ", ward_files[1])
    boundaries$wards <- st_read(ward_files[1])
  }
  
  return(boundaries)
}

# Update the bounding boxes to be slightly wider
cape_town_bbox <- c(18.35, -34.00, 18.50, -33.85)  # Zoomed out for Cape Town
gauteng_bbox <- c(27.95, -26.25, 28.15, -26.13)    # Zoomed out for Johannesburg

# Update create_urban_map function to include more context
create_urban_map <- function(city_name, bbox, border_color = "#FF5500") {
  message(paste("Creating", city_name, "detailed URBAN level map..."))
  
  # Load boundaries
  boundaries <- load_boundaries()
  
  # Define bounding box
  xmin <- bbox[1]
  xmax <- bbox[3]
  ymin <- bbox[2]
  ymax <- bbox[4]
  
  # Create bounding box polygon
  bbox_polygon <- st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  ))) %>% st_sfc(crs = 4326)
  
  # Try to read roads from boundary folder
  roads_file <- list.files("boundary", pattern = "roads|streets", full.names = TRUE)[1]
  if (!is.null(roads_file) && file.exists(roads_file)) {
    roads <- st_read(roads_file)
    roads_cropped <- st_intersection(roads, bbox_polygon)
  }
  
  # Try to read suburbs/neighborhoods
  suburbs_file <- list.files("boundary", pattern = "suburbs|neighborhoods|wards", full.names = TRUE)[1]
  if (!is.null(suburbs_file) && file.exists(suburbs_file)) {
    suburbs <- st_read(suburbs_file)
    suburbs_cropped <- st_intersection(suburbs, bbox_polygon)
  }
  
  # Create base map
  p <- ggplot()
  
  # Add boundaries and context layers
  if (exists("suburbs_cropped")) {
    p <- p + geom_sf(data = suburbs_cropped, fill = NA, color = "gray90", size = 0.1)
  }
  
  if (!is.null(boundaries$provinces)) {
    provinces_cropped <- st_intersection(boundaries$provinces, bbox_polygon)
    p <- p + geom_sf(data = provinces_cropped, fill = NA, color = "gray50", size = 0.3)
  }
  
  if (exists("roads_cropped")) {
    p <- p + 
      geom_sf(data = roads_cropped, color = "gray70", size = 0.2) +
      # Major roads in slightly darker gray
      geom_sf(data = subset(roads_cropped, grepl("major|primary|trunk", roads_cropped$type, ignore.case = TRUE)), 
              color = "gray40", size = 0.4)
  }
  
  # Filter institutions for this city
  city_name_parts <- unlist(strsplit(city_name, " & "))
  city_matches <- sapply(city_name_parts, function(x) {
    grepl(x, base_data$City, ignore.case = TRUE)
  })
  city_sf <- base_sf[rowSums(city_matches) > 0, ]
  
  # Add institution points
  if(nrow(city_sf) > 0) {
    p <- p +
      geom_sf(data = filter(city_sf, has_policy), 
              color = "#CD1A1B",
              aes(shape = institution_type, size = focus_areas),
              alpha = 0.9) +
      geom_sf(data = filter(city_sf, has_research), 
              color = "#0F1F2C",
              aes(shape = institution_type, size = focus_areas),
              alpha = 0.9) +
      geom_sf(data = filter(city_sf, has_finance), 
              color = "#90876E",
              aes(shape = institution_type, size = focus_areas),
              alpha = 0.9) +
      geom_sf(data = filter(city_sf, has_engagement), 
              color = "#1E4611",
              aes(shape = institution_type, size = focus_areas),
              alpha = 0.9)
  }
  
  # Rest of your existing code for labels, legends, etc...
  # (Keep the existing styling code)
  
  # Add a scale bar and north arrow
  p <- p +
    ggspatial::annotation_scale(
      location = "br",
      width_hint = 0.3,
      style = "ticks"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_minimal(),
      height = unit(1, "cm"),
      width = unit(1, "cm")
    )
  
  # Force the coordinates but allow a tiny bit of expansion for context
  p <- p + coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = TRUE,
    expand_multiplier = 0.02  # Allow 2% expansion for context
  )
  
  return(p)
}

# Update regional map to ensure all labels are visible
create_regional_map <- function() {
  # Widen the bounding box for Southern Africa
  xmin <- 15  # Further west (was ~16)
  xmax <- 35  # Further east (was ~33)
  ymin <- -35 # Further south (was ~-34)
  ymax <- -20 # Further north (was ~-22)
  
  # Get base layers
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  coastline <- ne_coastline(scale = "medium", returnclass = "sf")
  
  # Filter for Southern African institutions
  southern_africa_sf <- base_sf[base_sf$lat >= ymin & base_sf$lat <= ymax & 
                                  base_sf$lon >= xmin & base_sf$lon <= xmax, ]
  
  # Get major cities in view
  cities_in_view <- data.frame(
    name = c("Cape Town", "Johannesburg", "Pretoria", "Durban", "Maputo", 
             "Harare", "Lusaka", "Bulawayo", "Bloemfontein", "Port Elizabeth"),
    latitude = c(-33.92, -26.20, -25.75, -29.85, -25.97, 
                 -17.83, -15.42, -20.15, -29.12, -33.96),
    longitude = c(18.42, 28.05, 28.19, 31.03, 32.57, 
                  31.05, 28.28, 28.58, 26.21, 25.62)
  )
  
  # Create the base map with wider view
  p <- ggplot() +
    geom_sf(data = countries, fill = "antiquewhite1", color = "gray80", size = 0.2)
  
  # Add institution labels with adjusted settings
  if(nrow(southern_africa_sf) > 0) {
    institution_labels <- st_coordinates(southern_africa_sf) %>%
      as.data.frame() %>%
      bind_cols(Institution = southern_africa_sf$Institution)
    
    p <- p + 
      ggrepel::geom_text_repel(
        data = institution_labels,
        aes(x = X, y = Y, label = Institution),
        size = 3,    # Smaller font size (was 4)
        fontface = "plain", # Changed from bold to plain
        color = "black",
        box.padding = 0.5,  # Reduced padding
        point.padding = 0.5,
        force = 8,    # Reduced force
        max.overlaps = Inf,
        bg.color = "white",
        bg.r = 0.15,  # Smaller background
        segment.size = 0.3,
        min.segment.length = 0,
        direction = "both",
        seed = 42
      )
  }
  
  # Add city labels with adjusted settings
  if(nrow(cities_in_view) > 0) {
    p <- p + 
      ggrepel::geom_text_repel(
        data = cities_in_view,
        aes(x = longitude, y = latitude, label = name),
        size = 2.5,  # Smaller font size (was 3.5)
        fontface = "plain",
        color = "gray30",
        box.padding = 0.4,
        point.padding = 0.4,
        force = 6,
        max.overlaps = Inf,
        bg.color = "white",
        bg.r = 0.1,
        segment.size = 0.2,
        min.segment.length = 0,
        seed = 42
      )
  }
  
  # Rest of your existing map code...
  # (Keep the existing styling, legends, etc.)
  
  # Enforce the wider view
  p <- p + coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)
  
  return(p)
}

# Add this before the create_regional_map function
# Standalone wrapper function for safe intersection that can be shared across functions
safe_intersection <- function(x, y) {
  tryCatch({
    st_intersection(x, y)
  }, error = function(e) {
    message("Intersection error: ", e$message)
    message("Attempting to fix invalid geometries...")
    x_valid <- st_make_valid(x)
    y_valid <- st_make_valid(y)
    tryCatch({
      st_intersection(x_valid, y_valid)
    }, error = function(e2) {
      message("Still failed after fixing, returning NULL")
      return(NULL)
    })
  })
}

# Now create the maps
message("Starting map creation with FIXED zoom levels...")

# Map for Cape Town
cape_town_map <- create_urban_map("Cape Town", cape_town_bbox, "#FF5500")

# Map for Johannesburg and Pretoria region (Gauteng)
gauteng_map <- create_urban_map("Johannesburg", gauteng_bbox, "#FF5500")

# Map for Southern Africa
southern_africa_map <- create_regional_map()

# Check city names in your data
city_counts <- table(base_data$City)
print(city_counts)

# Check if Johannesburg institutions exist
joburg_institutions <- base_data[grepl("Johannesburg", base_data$City, ignore.case=TRUE), c("Institution", "City")]
print(joburg_institutions)

# Save the maps
message("Saving maps to PDF files...")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Create and save maps with higher resolution
ggsave("output/cape_town_map.pdf", cape_town_map, 
       width = 11, height = 9,    # Slightly larger dimensions
       device = cairo_pdf, dpi = 300)
ggsave("output/gauteng_map.pdf", gauteng_map, 
       width = 11, height = 9,    # Slightly larger dimensions
       device = cairo_pdf, dpi = 300)
ggsave("output/southern_africa_map.pdf", southern_africa_map, width = 14, height = 10, 
       device = cairo_pdf, dpi = 300)

message("All maps created successfully!")