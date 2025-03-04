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

# Define the new color scheme with exact colors requested
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

# First, let's add a function to load boundaries from the specified path
load_admin_boundaries <- function() {
  # Path to the level 4 administrative boundaries
  boundary_path <- "ZAF_AdminBoundaries_candidate/zaf_admbnda_adm4_sadb_ocha_20201109"
  
  # Check if the directory exists
  if (!dir.exists(boundary_path)) {
    message("Warning: Administrative boundary directory not found at: ", boundary_path)
    return(NULL)
  }
  
  # Look for shapefiles in the directory
  shp_files <- list.files(boundary_path, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(shp_files) == 0) {
    message("Warning: No shapefiles found in: ", boundary_path)
    return(NULL)
  }
  
  # Read the first shapefile found
  message("Loading administrative boundaries from: ", shp_files[1])
  boundaries <- st_read(shp_files[1], quiet = TRUE)
  
  # Ensure the CRS is WGS84 (EPSG:4326)
  if (st_crs(boundaries)$epsg != 4326) {
    message("Transforming boundaries to WGS84 (EPSG:4326)")
    boundaries <- st_transform(boundaries, 4326)
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

# Update the create_simplified_urban_map function to incorporate admin boundaries
create_simplified_urban_map <- function(city_name, bbox) {
  message(paste("Creating", city_name, "detailed URBAN level map with admin boundaries..."))
  
  # Filter institutions for this city
  city_sf <- base_sf %>% 
    filter(City == city_name | 
             (City == "Pretoria" & city_name == "Johannesburg") |
             (City == "Soweto" & city_name == "Johannesburg"))
  
  # Load administrative boundaries
  admin_boundaries <- load_admin_boundaries()
  
  # Filter boundaries for the specific area
  if (!is.null(admin_boundaries)) {
    bbox_sf <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                                   xmax = bbox[3], ymax = bbox[4]), crs = 4326))
    area_boundaries <- st_intersection(admin_boundaries, bbox_sf)
  } else {
    area_boundaries <- NULL
  }
  
  # Create a base map with improved styling
  p <- ggplot() +
    # Add a light blue background for water
    geom_rect(aes(xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4]),
              fill = "#E6F3FF", color = NA) +
    # Add a light beige background for land
    geom_sf(data = st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                                       xmax = bbox[3], ymax = bbox[4]), crs = 4326)),
            fill = "#F5F5DC", color = NA)
  
  # Add coastline for Cape Town if applicable
  if (city_name == "Cape Town") {
    coastline <- ne_coastline(scale = "large", returnclass = "sf")
    cape_coast <- st_crop(coastline, st_bbox(c(xmin = bbox[1]-0.1, ymin = bbox[2]-0.1, 
                                               xmax = bbox[3]+0.1, ymax = bbox[4]+0.1), crs = 4326))
    p <- p + geom_sf(data = cape_coast, color = "#4682B4", size = 0.5)
  }
  
  # Add administrative boundaries if available
  if (!is.null(area_boundaries) && nrow(area_boundaries) > 0) {
    p <- p + 
      geom_sf(data = area_boundaries, 
              fill = NA,
              color = "#808080",
              size = 0.2,
              alpha = 0.7)
  }
  
  # Try to get major roads using a cached approach
  roads_file <- file.path("cache", paste0(city_name, "_roads.rds"))
  if (!dir.exists("cache")) dir.create("cache")
  
  if (file.exists(roads_file)) {
    major_roads <- readRDS(roads_file)
  } else {
    roads_query <- opq(bbox = bbox) %>%
      add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary"))
    major_roads <- osmdata_sf(roads_query)$osm_lines
    saveRDS(major_roads, roads_file)
  }
  
  # Add major roads if available
  if (!is.null(major_roads) && nrow(major_roads) > 0) {
    p <- p + geom_sf(data = major_roads, color = "#A9A9A9", size = 0.3, alpha = 0.7)
  }
  
  # Apply improved theme
  p <- p + 
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#E6F3FF", color = NA),
      panel.grid.major = element_line(color = "#E0E0E0", size = 0.2),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 8, color = "#808080"),
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.background = element_rect(fill = "white", color = "#D0D0D0"),
      legend.key = element_rect(fill = "white"),
      plot.margin = margin(10, 10, 10, 10, "mm")
    ) +
    labs(
      title = paste(city_name, "Health and Climate Network"),
      subtitle = "Showing key partnerships and research institutions",
      caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d"))
    )
  
  # Always use full institution names
  city_sf$display_name <- city_sf$Institution
  
  # Add points with proper shapes
  # Data providers - triangles
  p <- p + 
    geom_sf(data = city_sf %>% filter(institution_type == "Data Provider"),
            aes(shape = "Data Provider"),
            size = 3,
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
  # Other partners - squares
  p <- p + 
    geom_sf(data = city_sf %>% filter(institution_type != "Data Provider"),
            aes(shape = "Other Partners"),
            size = 3,
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
  # Add category-specific points
  for (category in names(category_colors)) {
    category_data <- switch(category,
                            "Research" = filter(city_sf, has_research),
                            "Finance and Programmes" = filter(city_sf, has_finance),
                            "Engagement, Advocacy and Capacity Building" = filter(city_sf, has_engagement),
                            "Policy" = filter(city_sf, has_policy)
    )
    
    p <- p + 
      geom_sf(data = category_data,
              aes(shape = ifelse(institution_type == "Data Provider", 
                                 "Data Provider", "Other Partners"),
                  fill = category),
              size = 3,
              color = "black",
              stroke = 0.5,
              alpha = 0.8)
  }
  
  # Add improved labels
  p <- p + 
    ggrepel::geom_text_repel(
      data = city_sf,
      aes(label = display_name, geometry = geometry),
      stat = "sf_coordinates",
      size = 3.5,
      force = 10,
      max.overlaps = 30,
      box.padding = 0.7,
      segment.color = "#808080",
      min.segment.length = 0.1,
      seed = 42,
      bg.color = "white",
      bg.r = 0.15
    )
  
  # Add proper legend
  p <- p + 
    scale_shape_manual(
      name = "Institution Type",
      values = c("Data Provider" = 24, "Other Partners" = 22),  # Triangle and Square
      labels = c("Data Provider", "Other Partners")
    ) +
    scale_fill_manual(
      name = "Focus Areas",
      values = category_colors,
      labels = names(category_colors)
    ) +
    guides(
      shape = guide_legend(order = 1, override.aes = list(size = 3)),
      fill = guide_legend(order = 2, override.aes = list(size = 3))
    )
  
  # Set the map extent
  p <- p + 
    coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4]),
      expand = FALSE
    ) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.2) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_minimal(),
      height = unit(1, "cm"),
      width = unit(1, "cm")
    )
  
  return(p)
}

# Update the create_regional_map function for Southern Africa with better styling
create_regional_map <- function(use_short_names = FALSE) {
  message("Creating Southern Africa regional map...")
  
  # Define tighter bounding box for Southern Africa
  xmin <- 10   # East enough to include Mozambique
  xmax <- 35   # West enough to include South Africa
  ymin <- -35  # South enough to include Cape Town
  ymax <- -10  # North enough to include relevant countries
  
  # FILTER FOR ONLY SOUTHERN AFRICAN INSTITUTIONS
  southern_africa_sf <- base_sf %>% 
    filter(grepl("South Africa|Zimbabwe|Mozambique|Botswana|Namibia|Lesotho|Eswatini|Zambia|Malawi", Country))
  
  # Verify we have data
  message(paste("Found", nrow(southern_africa_sf), "institutions in Southern Africa"))
  
  # Get improved base maps with better styling
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Create the main plot with improved land/sea contrast
  p <- ggplot() +
    # Add subtle blue sea background
    geom_rect(aes(xmin = -180, xmax = 180, ymin = -90, ymax = 90), 
              fill = "#E6F3FF", color = NA) +  # Lighter blue for water
    # Land masses with subtle fill
    geom_sf(data = world, fill = "#F5F5DC", color = "#A0A0A0", size = 0.3) +  # Improved land color
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#E6F3FF", color = NA),  # Match water color
      panel.grid.major = element_line(color = "#E0E0E0", size = 0.2),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),  # Remove axis text for cleaner look
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.background = element_rect(fill = "white", color = "#D0D0D0"),
      legend.key = element_rect(fill = "white"),
      plot.margin = margin(10, 10, 10, 10, "mm")
    ) +
    labs(
      title = "Southern Africa Health and Climate Network",
      subtitle = "Showing key partnerships and research institutions",
      caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d"))
    )
  
  # Add points with STANDARDIZED SIZE and SHAPES
  # Data providers - triangles
  p <- p + 
    geom_sf(data = southern_africa_sf %>% filter(institution_type == "Data Provider"),
            aes(shape = "Data Provider"),
            size = 3,    # STANDARDIZED SIZE
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
  # Other partners - squares (not circles)
  p <- p + 
    geom_sf(data = southern_africa_sf %>% filter(institution_type != "Data Provider"),
            aes(shape = "Other Partners"),
            size = 3,    # STANDARDIZED SIZE
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
  # Add category-specific points with STANDARDIZED SIZE
  for (category in names(category_colors)) {
    category_data <- switch(category,
                            "Research" = filter(southern_africa_sf, has_research),
                            "Finance and Programmes" = filter(southern_africa_sf, has_finance),
                            "Engagement, Advocacy and Capacity Building" = filter(southern_africa_sf, has_engagement),
                            "Policy" = filter(southern_africa_sf, has_policy)
    )
    
    p <- p + 
      geom_sf(data = category_data,
              aes(shape = ifelse(institution_type == "Data Provider", 
                                 "Data Provider", "Other Partners"),
                  fill = category),
              size = 3,    # STANDARDIZED SIZE
              color = "black",
              stroke = 0.5,
              alpha = 0.8)
  }
  
  # Always use full institution names
  southern_africa_sf$display_name <- southern_africa_sf$Institution
  
  # Add red squares to mark urban map regions FIRST (so labels can be placed to avoid them)
  p <- p + 
    # Cape Town square
    annotate("rect", xmin = 18.4241 - 0.3, xmax = 18.4241 + 0.3, 
             ymin = -33.9249 - 0.3, ymax = -33.9249 + 0.3,
             color = "red", fill = NA, size = 1) +
    # Johannesburg/Pretoria square
    annotate("rect", xmin = 28.0473 - 0.3, xmax = 28.0473 + 0.3, 
             ymin = -26.2041 - 0.3, ymax = -26.2041 + 0.3,
             color = "red", fill = NA, size = 1) +
    # Labels for the squares - positioned BELOW the squares
    annotate("text", x = 18.4241, y = -33.9249 - 0.6, 
             label = "Cape Town", color = "red", fontface = "bold", size = 3) +
    annotate("text", x = 28.0473, y = -26.2041 - 0.6, 
             label = "Johannesburg/Pretoria", color = "red", fontface = "bold", size = 3)
  
  # Add improved labels with better placement - AVOID RED SQUARES
  p <- p + 
    ggrepel::geom_text_repel(
      data = southern_africa_sf,
      aes(label = display_name, geometry = geometry),
      stat = "sf_coordinates",
      size = 3.5,
      force = 15,          # Increased force to push labels apart
      max.overlaps = 20,   # Allow more overlaps but still try to minimize
      box.padding = 0.8,   # More padding around text
      point.padding = 0.5,
      segment.size = 0.3,  # Thinner leader lines
      min.segment.length = 0.1,
      seed = 42,           # Consistent placement
      direction = "both",  # Allow labels in all directions
      nudge_x = 0.5,       # Nudge labels slightly to the right
      bg.color = "white",  # White background for better readability
      bg.r = 0.15,         # Rounded corners on label backgrounds
      # Avoid placing labels near the red squares
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    )
  
  # Add proper legend for both shape and fill
  p <- p + 
    scale_shape_manual(
      name = "Institution Type",
      values = c("Data Provider" = 24, "Other Partners" = 22),  # Triangle and Square
      labels = c("Data Provider", "Other Partners")
    ) +
    scale_fill_manual(
      name = "Focus Areas",
      values = category_colors,
      labels = names(category_colors)
    ) +
    guides(
      shape = guide_legend(order = 1, override.aes = list(size = 3)),
      fill = guide_legend(order = 2, override.aes = list(size = 3))
    )
  
  # Use the bbox directly for explicit zoom level
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

# Update Europe map function to use full names and consistent styling
create_europe_map <- function(use_short_names = FALSE) {
  message("Creating Europe map...")
  
  # Define Europe bounding box
  europe_bbox <- c(-10, 35, 30, 65)
  
  # Filter for European institutions only
  europe_data <- base_sf %>% 
    filter(grepl("Austria|Belgium|Denmark|Estonia|Finland|France|Germany|Greece|Ireland|Italy|Netherlands|Norway|United Kingdom|Spain|Portugal|Sweden|Switzerland", Country))
  
  # Get base maps
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Create the main plot with improved styling
  p <- ggplot() +
    geom_rect(aes(xmin = -180, xmax = 180, ymin = -90, ymax = 90), 
              fill = "#E6F3FF", color = NA) +
    geom_sf(data = world, fill = "#F5F5DC", color = "#A0A0A0", size = 0.3) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#E6F3FF", color = NA),
      panel.grid.major = element_line(color = "#E0E0E0", size = 0.2),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.background = element_rect(fill = "white", color = "#D0D0D0"),
      legend.key = element_rect(fill = "white"),
      plot.margin = margin(10, 10, 10, 10, "mm")
    ) +
    labs(
      title = "European Partners",
      subtitle = "Health and Climate Research Network",
      caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d"))
    )
  
  # Always use full institution names
  europe_data$display_name <- europe_data$Institution
  
  # Add points with proper shapes
  # Data providers - triangles
  p <- p + 
    geom_sf(data = europe_data %>% filter(institution_type == "Data Provider"),
            aes(shape = "Data Provider"),
            size = 3,
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
  # Other partners - squares
  p <- p + 
    geom_sf(data = europe_data %>% filter(institution_type != "Data Provider"),
            aes(shape = "Other Partners"),
            size = 3,
            fill = "white", 
            color = "black", 
            stroke = 0.5)
  
  # Add category-specific points
  for (category in names(category_colors)) {
    category_data <- switch(category,
                            "Research" = filter(europe_data, has_research),
                            "Finance and Programmes" = filter(europe_data, has_finance),
                            "Engagement, Advocacy and Capacity Building" = filter(europe_data, has_engagement),
                            "Policy" = filter(europe_data, has_policy)
    )
    
    p <- p + 
      geom_sf(data = category_data,
              aes(shape = ifelse(institution_type == "Data Provider", 
                                 "Data Provider", "Other Partners"),
                  fill = category),
              size = 3,
              color = "black",
              stroke = 0.5,
              alpha = 0.8)
  }
  
  # Add improved labels
  p <- p + 
    ggrepel::geom_text_repel(
      data = europe_data,
      aes(label = display_name, geometry = geometry),
      stat = "sf_coordinates",
      size = 3.5,
      force = 10,
      max.overlaps = 20,
      box.padding = 0.7,
      segment.color = "#808080",
      min.segment.length = 0.1,
      seed = 42,
      bg.color = "white",
      bg.r = 0.15
    )
  
  # Add proper legend
  p <- p + 
    scale_shape_manual(
      name = "Institution Type",
      values = c("Data Provider" = 24, "Other Partners" = 22),  # Triangle and Square
      labels = c("Data Provider", "Other Partners")
    ) +
    scale_fill_manual(
      name = "Focus Areas",
      values = category_colors,
      labels = names(category_colors)
    ) +
    guides(
      shape = guide_legend(order = 1, override.aes = list(size = 3)),
      fill = guide_legend(order = 2, override.aes = list(size = 3))
    )
  
  # Set the map extent
  p <- p + 
    coord_sf(
      xlim = c(europe_bbox[1], europe_bbox[3]),
      ylim = c(europe_bbox[2], europe_bbox[4]),
      expand = FALSE
    ) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.2) +
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

# Clear the environment of existing map objects
rm(list = c("cape_town_map", "gauteng_map", "southern_africa_map", "europe_map", 
            "europe_map_short", "europe_map_long"), envir = .GlobalEnv)

# Force recreation of the maps with print statements
message("Creating maps with revised style...")

# Map for Cape Town - SIMPLIFIED
cape_town_map <- create_simplified_urban_map("Cape Town", cape_town_bbox)
print("Cape Town map created")

# Map for Johannesburg/Gauteng - SIMPLIFIED
gauteng_map <- create_simplified_urban_map("Johannesburg", gauteng_bbox)
print("Johannesburg map created")

# Map for Southern Africa - SOUTHERN AFRICA ONLY
southern_africa_map <- create_regional_map(use_short_names = FALSE)  # Use full names
print("Southern Africa map created")

# Map for Europe - SEPARATE MAP (both versions)
europe_map_short <- create_europe_map(use_short_names = TRUE)   # Short names
europe_map_long <- create_europe_map(use_short_names = FALSE)   # Long names
print("Europe maps created (both short and long name versions)")

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

# Save Europe maps (both versions)
ggsave(file.path("output", paste0("europe_map_short_", timestamp, ".pdf")), 
       europe_map_short, 
       width = 14, 
       height = 10,
       device = cairo_pdf)

ggsave(file.path("output", paste0("europe_map_short_", timestamp, ".png")), 
       europe_map_short, 
       width = 14, 
       height = 10,
       dpi = 300)

ggsave(file.path("output", paste0("europe_map_long_", timestamp, ".pdf")), 
       europe_map_long, 
       width = 14, 
       height = 10,
       device = cairo_pdf)

ggsave(file.path("output", paste0("europe_map_long_", timestamp, ".png")), 
       europe_map_long, 
       width = 14, 
       height = 10,
       dpi = 300)

message("All maps created and saved in output directory with timestamp: ", timestamp)