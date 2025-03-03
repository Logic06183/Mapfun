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
if (!require("osmdata")) {
  install.packages("osmdata")
  library(osmdata)
}
if (!require("memoise")) {
  install.packages("memoise")
  library(memoise)
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

# Define the new color scheme
category_colors <- c(
  "Research" = "#0F1F2C",
  "Finance and Programmes" = "#90876E",
  "Engagement, Advocacy and Capacity Building" = "#1E4611",
  "Policy" = "#CD1A1B"
)

# Modified data cleaning section
base_data <- base_data %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    institution_type = case_when(
      `Data_Providers` == 1 ~ "Data Provider",
      TRUE ~ "Other"
    ),
    # Create columns for each focus area
    has_research = ifelse(Research == 1, TRUE, FALSE),
    has_finance = ifelse(Finance_programmes == 1, TRUE, FALSE),
    has_engagement = ifelse(`Engagement..Advocacy..and.Capacity.Building` == 1, TRUE, FALSE),
    has_policy = ifelse(Policy == 1, TRUE, FALSE),
    # Count focus areas
    focus_areas = rowSums(across(c(has_research, has_finance, has_engagement, has_policy)))
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

# Add caching function for OSM data
cached_get_osm_data <- memoise(function(bbox) {
  message("Fetching OSM data (cached if previously retrieved)...")
  
  # Only get major roads to reduce data volume
  roads_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway", 
                   value = c("motorway", "trunk", "primary", "secondary"))
  
  tryCatch({
    roads_sf <- osmdata_sf(roads_query)$osm_lines
    return(roads_sf)
  }, error = function(e) {
    message("Could not fetch OSM data: ", e$message)
    return(NULL)
  })
})

# Fix the create_urban_map function to include Pretoria and Soweto in Gauteng map
create_urban_map <- function(city_name, bbox, border_color = "#FF5500") {
  message(paste("Creating", city_name, "detailed URBAN level map (REVISED VERSION)..."))
  
  # Define bounding box
  xmin <- bbox[1]
  xmax <- bbox[3]
  ymin <- bbox[2]
  ymax <- bbox[4]
  
  message(paste("Using bbox:", paste(bbox, collapse=", ")))
  
  # Get high-resolution Natural Earth data
  countries <- ne_countries(scale = "large", returnclass = "sf")
  states <- ne_states(country = "south africa", returnclass = "sf")
  
  # Get cached OSM data
  roads_sf <- cached_get_osm_data(bbox)
  
  # IMPORTANT CHANGE: Filter institutions for this city with special handling for gauteng region
  city_sf <- NULL
  if (city_name == "Johannesburg") {
    # For Johannesburg, include Pretoria and Soweto as well
    city_sf <- base_sf[grepl("Johannesburg|Pretoria|Soweto|Tshwane", base_data$City, ignore.case = TRUE), ]
    message(paste("Found", nrow(city_sf), "institutions in Gauteng region"))
    print(unique(city_sf$City))  # Print cities for debugging
  } else {
    # Regular filtering for other cities
    city_name_parts <- unlist(strsplit(city_name, " & "))
    city_matches <- sapply(city_name_parts, function(x) {
      grepl(x, base_data$City, ignore.case = TRUE)
    })
    city_sf <- base_sf[rowSums(city_matches) > 0, ]
  }
  
  # Create a combined dataset with focus areas as explicit factors
  all_points <- data.frame() # Empty dataframe to start
  
  if(nrow(city_sf) > 0) {
    # For each focus area, create a subset and tag it
    policy_points <- filter(city_sf, has_policy)
    if(nrow(policy_points) > 0) {
      policy_points$focus_area <- "Policy"
      all_points <- rbind(all_points, policy_points)
    }
    
    research_points <- filter(city_sf, has_research)
    if(nrow(research_points) > 0) {
      research_points$focus_area <- "Research"
      all_points <- rbind(all_points, research_points)
    }
    
    finance_points <- filter(city_sf, has_finance)
    if(nrow(finance_points) > 0) {
      finance_points$focus_area <- "Finance and Programmes"
      all_points <- rbind(all_points, finance_points)
    }
    
    engagement_points <- filter(city_sf, has_engagement)
    if(nrow(engagement_points) > 0) {
      engagement_points$focus_area <- "Engagement, Advocacy and Capacity Building"
      all_points <- rbind(all_points, engagement_points)
    }
    
    # Set as factor if points exist
    if(nrow(all_points) > 0) {
      all_points$focus_area <- factor(all_points$focus_area)
    }
  }
  
  # Create the map with visible differences
  p <- ggplot() +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    geom_sf(data = countries, fill = "antiquewhite1", color = "gray80", size = 0.2) +
    geom_sf(data = states, fill = NA, color = "gray70", size = 0.3)
  
  # Add roads if available
  if (!is.null(roads_sf)) {
    p <- p + geom_sf(data = roads_sf, color = "gray70", size = 0.2)
  }
  
  # Add the institution points with explicit aesthetic mapping
  if(nrow(all_points) > 0) {
    p <- p +
      geom_sf(data = all_points, 
              aes(color = focus_area, shape = institution_type),
              size = 3,
              alpha = 0.9)
  }
  
  # Add institution labels
  if(nrow(city_sf) > 0) {
    institution_labels <- st_coordinates(city_sf) %>%
      as.data.frame() %>%
      bind_cols(Institution = city_sf$Institution)
    
    p <- p + 
      ggrepel::geom_text_repel(
        data = institution_labels,
        aes(x = X, y = Y, label = Institution),
        size = 3.5,
        color = "black",
        box.padding = 0.7,
        point.padding = 0.7,
        force = 10,
        max.overlaps = Inf,
        bg.color = "white",
        bg.r = 0.15,
        segment.size = 0.3,
        min.segment.length = 0,
        seed = 42
      )
  }
  
  # Add a timestamp and title to confirm this is a new map
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  p <- p +
    ggtitle(paste(city_name, "- Map created:", timestamp)) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  # Add explicit scales and guides
  p <- p +
    scale_color_manual(
      name = "Focus Areas",
      values = c(
        "Research" = "#0F1F2C",
        "Finance and Programmes" = "#90876E",
        "Engagement, Advocacy and Capacity Building" = "#1E4611",
        "Policy" = "#CD1A1B"
      )
    ) +
    scale_shape_manual(
      name = "Institution Type",
      values = c("Data Provider" = 18, "Official Partner" = 16, "Other" = 15)
    ) +
    guides(
      color = guide_legend(order = 1, override.aes = list(size = 4)),
      shape = guide_legend(order = 2)
    )
  
  # ADD THIS EXPLICIT ZOOM LEVEL - use the bbox directly
  p <- p + 
    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      expand = FALSE
    ) +
    # Add scale bar and north arrow
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
  
  return(p)
}

# Update the bounding boxes to be exact
cape_town_bbox <- c(18.35, -34.00, 18.50, -33.85)  # Zoomed out for Cape Town
gauteng_bbox <- c(27.80, -26.40, 28.40, -25.60)  # Much wider view for Gauteng region

# Update regional map to ensure all labels are visible
create_regional_map <- function() {
  message("Creating Southern Africa regional map...")
  
  # Define wider bounding box for Southern Africa
  xmin <- -25  # Further west to accommodate new layout
  xmax <- 50   # Further east
  ymin <- -35  # South enough to include Cape Town
  ymax <- 5    # North enough to include relevant countries
  
  # Get base maps
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Create the main plot
  p <- ggplot() +
    geom_sf(data = world, fill = "white", color = "gray80") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      plot.margin = margin(10, 30, 10, 10, "mm")
    )
  
  # Add points with different shapes for data providers and others
  p <- p + 
    geom_sf(data = base_sf %>% filter(institution_type == "Data Provider"),
            aes(size = focus_areas), 
            shape = 24,  # Triangle
            fill = "white", 
            color = "black", 
            stroke = 0.5) +
    geom_sf(data = base_sf %>% filter(institution_type != "Data Provider"),
            aes(size = focus_areas), 
            shape = 22,  # Square
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
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
              aes(size = focus_areas),
              shape = ifelse(category_data$institution_type == "Data Provider", 24, 22),
              fill = category_colors[category],
              color = "black",
              stroke = 0.5,
              alpha = 0.8)
  }
  
  # Add labels with repel
  p <- p + 
    geom_text_repel(
      data = base_sf,
      aes(label = Institution, geometry = geometry),
      stat = "sf_coordinates",
      size = 2.5,
      force = 1,
      max.overlaps = 20,
      box.padding = 0.5,
      segment.color = "gray50"
    )
  
  # Customize the legend
  p <- p +
    scale_size_continuous(
      name = "Number of Focus Areas",
      range = c(2, 6),
      breaks = 1:4
    ) +
    guides(
      size = guide_legend(override.aes = list(shape = 22, fill = "gray50")),
      shape = guide_legend(title = "Institution Type")
    )
  
  # Add the European inset map
  europe_bbox <- c(-10, 35, 30, 60)
  europe_data <- st_crop(base_sf, st_bbox(c(xmin = europe_bbox[1], ymin = europe_bbox[2],
                                           xmax = europe_bbox[3], ymax = europe_bbox[4])))
  
  europe_map <- ggplot() +
    geom_sf(data = world, fill = "white", color = "gray80") +
    geom_sf(data = europe_data,
            aes(size = focus_areas),
            shape = ifelse(europe_data$institution_type == "Data Provider", 24, 22),
            fill = "white",
            color = "black",
            stroke = 0.5) +
    coord_sf(xlim = c(europe_bbox[1], europe_bbox[3]),
            ylim = c(europe_bbox[2], europe_bbox[4])) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = "gray50"),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      axis.text = element_text(size = 6),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
  
  # Add the inset map
  p <- p +
    annotation_custom(
      grob = ggplotGrob(europe_map),
      xmin = 20,  # Adjust these coordinates to position the inset
      xmax = 45,
      ymin = -20,
      ymax = -5
    ) +
    coord_sf(xlim = c(xmin, xmax),
            ylim = c(ymin, ymax))
  
  # Add title and legend
  p <- p +
    labs(
      title = "Institution Network Map",
      subtitle = "Southern Africa and European Partners",
      caption = paste("Categories:",
                     "\nResearch (#0F1F2C)",
                     "\nFinance and Programmes (#90876E)",
                     "\nEngagement, Advocacy and Capacity Building (#1E4611)",
                     "\nPolicy (#CD1A1B)")
    )
  
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

# Clear the environment of existing map objects
rm(list = c("cape_town_map", "gauteng_map", "southern_africa_map"), envir = .GlobalEnv)

# Force recreation of the maps with print statements
message("Creating maps with VISIBLY DIFFERENT style...")

# Map for Cape Town
cape_town_map <- create_urban_map("Cape Town", cape_town_bbox)
print("Cape Town map created")

# Map for Johannesburg
gauteng_map <- create_urban_map("Johannesburg", gauteng_bbox)
print("Johannesburg map created")

# Map for Southern Africa
southern_africa_map <- create_regional_map()
print("Southern Africa map created")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Save the maps with a timestamp to avoid caching
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save Cape Town map
ggsave(file.path("output", paste0("cape_town_map_", timestamp, ".pdf")), 
       cape_town_map, 
       width = 11, 
       height = 9,
       device = cairo_pdf)

ggsave(file.path("output", paste0("cape_town_map_", timestamp, ".png")), 
       cape_town_map, 
       width = 11, 
       height = 9,
       dpi = 300)

# Save Johannesburg map
ggsave(file.path("output", paste0("gauteng_map_", timestamp, ".pdf")), 
       gauteng_map, 
       width = 11, 
       height = 9,
       device = cairo_pdf)

ggsave(file.path("output", paste0("gauteng_map_", timestamp, ".png")), 
       gauteng_map, 
       width = 11, 
       height = 9,
       dpi = 300)

# Save Southern Africa map
ggsave(file.path("output", paste0("southern_africa_map_", timestamp, ".pdf")), 
       southern_africa_map, 
       width = 14, 
       height = 10,
       device = cairo_pdf)

ggsave(file.path("output", paste0("southern_africa_map_", timestamp, ".png")), 
       southern_africa_map, 
       width = 14, 
       height = 10,
       dpi = 300)

message("Maps created and saved in output directory with timestamp: ", timestamp)