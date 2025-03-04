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

# After reading the CSV file, add this diagnostic code to understand the data structure
message("Column names in the CSV file:")
message(paste(colnames(base_data), collapse = ", "))

# Print a sample of the data to understand the category columns
message("Sample of category columns (first 5 rows):")
print(head(base_data[, c("Institution", "Research", "Policy", "Engagement..Advocacy..and.Capacity.Building", "Finance_programmes")], 5))

# Count institutions by category to understand distribution
message("Category distribution in the data:")
message("Research: ", sum(base_data$Research == 1, na.rm = TRUE))
message("Policy: ", sum(base_data$Policy == 1, na.rm = TRUE))
message("Engagement: ", sum(base_data$Engagement..Advocacy..and.Capacity.Building == 1, na.rm = TRUE))
message("Finance: ", sum(base_data$Finance_programmes == 1, na.rm = TRUE))

# 1. DEFINE THE EXACT COLORS - these are used throughout the code
RESEARCH_COLOR <- "#0F1F2C"       # Dark blue for Research
FINANCE_COLOR <- "#90876E"        # Tan for Finance and Programmes
ENGAGEMENT_COLOR <- "#1E4611"     # Green for Engagement
POLICY_COLOR <- "#CD1A1B"         # Red for Policy

# Add this before the map creation functions
# Define shape values for consistent use throughout the code
shape_values <- c("Data Provider" = 24, "Official Partner" = 21, "Other" = 21)

# 2. Create a consistent manual legend function for all maps
create_manual_legend <- function() {
  grid::grobTree(
    # Background rectangle with border
    grid::rectGrob(width = 0.95, height = 0.95, 
                   gp = grid::gpar(fill = "white", col = "#D0D0D0", lwd = 1)),
    
    # Title
    grid::textGrob("Primary Focus Area", x = 0.1, y = 0.95, just = "left",
                   gp = grid::gpar(fontsize = 11, fontface = "bold", fontfamily = "serif")),
    
    # Research (dark blue) - CIRCLE instead of square
    grid::circleGrob(x = 0.15, y = 0.8, r = 0.035,
                     gp = grid::gpar(fill = RESEARCH_COLOR, col = "black", lwd = 0.5)),
    grid::textGrob("Research", x = 0.25, y = 0.8, just = "left",
                   gp = grid::gpar(fontsize = 10, fontfamily = "serif")),
    
    # Finance (tan) - CIRCLE instead of square
    grid::circleGrob(x = 0.15, y = 0.7, r = 0.035,
                     gp = grid::gpar(fill = FINANCE_COLOR, col = "black", lwd = 0.5)),
    grid::textGrob("Finance and Programmes", x = 0.25, y = 0.7, just = "left",
                   gp = grid::gpar(fontsize = 10, fontfamily = "serif")),
    
    # Engagement (green) - CIRCLE instead of square
    grid::circleGrob(x = 0.15, y = 0.6, r = 0.035,
                     gp = grid::gpar(fill = ENGAGEMENT_COLOR, col = "black", lwd = 0.5)),
    grid::textGrob("Engagement, Advocacy and Capacity Building", x = 0.25, y = 0.6, just = "left",
                   gp = grid::gpar(fontsize = 10, fontfamily = "serif")),
    
    # Policy (red) - CIRCLE instead of square
    grid::circleGrob(x = 0.15, y = 0.5, r = 0.035,
                     gp = grid::gpar(fill = POLICY_COLOR, col = "black", lwd = 0.5)),
    grid::textGrob("Policy", x = 0.25, y = 0.5, just = "left",
                   gp = grid::gpar(fontsize = 10, fontfamily = "serif")),
    
    # Separator line
    grid::linesGrob(x = grid::unit(c(0.05, 0.95), "npc"), 
                    y = grid::unit(c(0.43, 0.43), "npc"),
                    gp = grid::gpar(col = "#D0D0D0", lwd = 1)),
    
    # Institution Type section
    grid::textGrob("Institution Type", x = 0.1, y = 0.37, just = "left",
                   gp = grid::gpar(fontsize = 11, fontface = "bold", fontfamily = "serif")),
    
    # Data Provider (triangle)
    grid::polygonGrob(x = c(0.15, 0.12, 0.18), y = c(0.27, 0.20, 0.20),
                      gp = grid::gpar(fill = "black", col = "black")),
    grid::textGrob("Data Provider", x = 0.25, y = 0.23, just = "left",
                   gp = grid::gpar(fontsize = 10, fontfamily = "serif")),
    
    # Official Partner & Other (now using circle instead of square)
    grid::circleGrob(x = 0.15, y = 0.13, r = 0.035,
                     gp = grid::gpar(fill = "black", col = "black", lwd = 0.5)),
    grid::textGrob("Official Partner / Other", x = 0.25, y = 0.13, just = "left",
                   gp = grid::gpar(fontsize = 10, fontfamily = "serif")),
    
    # Position the legend OUTSIDE the main plot area
    vp = grid::viewport(x = 1.15, y = 0.5, width = 0.3, height = 0.6)
  )
}

# 3. Add this function to add the manual legend to any map
add_manual_legend <- function(p) {
  # Remove any existing fill legend
  p <- p + guides(fill = "none", shape = "none")
  
  # Add the manual legend and expand the right margin to make room for it
  p + annotation_custom(create_manual_legend()) +
    theme(plot.margin = margin(0.5, 6, 0.5, 0.5, "cm"))
}

# 4. MOVE SOUTHERN AFRICA MAP FURTHER LEFT
create_regional_map <- function() {
  message("Creating Southern Africa regional map with FT-inspired styling...")
  
  # Move map even further left
  xmin <- -5     # Decreased MORE to show more to the west
  xmax <- 35    # Keep the same eastern boundary
  ymin <- -35   # South enough to include Cape Town
  ymax <- -10   # North enough to include relevant countries
  
  # FILTER FOR ONLY SOUTHERN AFRICAN INSTITUTIONS
  southern_africa_sf <- base_sf %>% 
    filter(grepl("South Africa|Zimbabwe|Mozambique|Botswana|Namibia|Lesotho|Eswatini|Zambia|Malawi", Country))
  
  # Calculate primary category based on the CSV data (1 = yes, 0 = no)
  southern_africa_sf <- southern_africa_sf %>%
    mutate(primary_category = case_when(
      has_policy == TRUE ~ "Policy",
      has_engagement == TRUE ~ "Engagement, Advocacy and Capacity Building",
      has_finance == TRUE ~ "Finance and Programmes",
      has_research == TRUE ~ "Research",
      TRUE ~ "Other"
    ))
  
  # Cache Natural Earth data to avoid repeated downloads
  if (!exists("world_cached", envir = .GlobalEnv)) {
    message("Caching Natural Earth data for faster map generation...")
    assign("world_cached", ne_countries(scale = "medium", returnclass = "sf"), envir = .GlobalEnv)
  }
  
  # Use cached data
  world <- get("world_cached", envir = .GlobalEnv)
  
  # Create base map
  p <- ggplot() +
    # Soft blue background for water
    geom_rect(aes(xmin = -180, xmax = 180, ymin = -90, ymax = 90), 
              fill = "#e6eef5", color = NA) +
    # Countries
    geom_sf(data = world, 
            fill = "#f9f9f2", color = "#d9d9d9", size = 0.3) +
    # Apply FT theme
    create_ft_theme() +
    labs(
      title = "Southern Africa Health and Climate Network",
      subtitle = "Showing primary focus area for each institution",
      caption = paste0("Source: Health and Climate Network Partners Database • Generated: ", format(Sys.Date(), "%d %B %Y"))
    )
  
  # Add red squares for urban map regions
  p <- p + 
    # Cape Town square
    annotate("rect", xmin = 18.4241 - 0.3, xmax = 18.4241 + 0.3, 
             ymin = -33.9249 - 0.3, ymax = -33.9249 + 0.3,
             color = "#CD1A1B", fill = NA, size = 1, alpha = 0.9) +
    # Johannesburg/Pretoria square
    annotate("rect", xmin = 28.0473 - 0.3, xmax = 28.0473 + 0.3, 
             ymin = -26.2041 - 0.3, ymax = -26.2041 + 0.3,
             color = "#CD1A1B", fill = NA, size = 1, alpha = 0.9) +
    # Labels for the squares
    annotate("text", x = 18.4241, y = -33.9249 - 0.7, 
             label = "Cape Town", color = "#CD1A1B", fontface = "bold", 
             size = 3.5, family = "serif") +
    annotate("text", x = 28.0473, y = -26.2041 - 0.7, 
             label = "Johannesburg/Pretoria", color = "#CD1A1B", fontface = "bold", 
             size = 3.5, family = "serif")
  
  # Add points by category to ensure correct colors
  # Policy (red) points
  policy_points <- southern_africa_sf %>% filter(has_policy == TRUE)
  p <- p + geom_sf(data = policy_points, 
                   aes(shape = institution_type), 
                   fill = POLICY_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Engagement (green) points
  engagement_points <- southern_africa_sf %>% 
    filter(has_policy == FALSE & has_engagement == TRUE)
  p <- p + geom_sf(data = engagement_points, 
                   aes(shape = institution_type), 
                   fill = ENGAGEMENT_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Finance (tan) points
  finance_points <- southern_africa_sf %>% 
    filter(has_policy == FALSE & has_engagement == FALSE & has_finance == TRUE)
  p <- p + geom_sf(data = finance_points, 
                   aes(shape = institution_type), 
                   fill = FINANCE_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Research (dark blue) points
  research_points <- southern_africa_sf %>% 
    filter(has_policy == FALSE & has_engagement == FALSE & 
             has_finance == FALSE & has_research == TRUE)
  p <- p + geom_sf(data = research_points, 
                   aes(shape = institution_type), 
                   fill = RESEARCH_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Add labels
  p <- p + ggrepel::geom_text_repel(
    data = southern_africa_sf,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5,
    fontface = "bold",
    family = "serif",
    force = 25,
    max.overlaps = 50,
    box.padding = 0.9,
    point.padding = 0.5,
    segment.size = 0.4,
    segment.color = "#505050",
    min.segment.length = 0.1,
    seed = 42,
    direction = "both",
    nudge_x = 0.2,
    bg.color = "white",
    bg.r = 0.15
  )
  
  # Set the map extent
  p <- p + coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.3, 
                                style = "ticks", text_family = "serif") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true", 
      style = north_arrow_fancy_orienteering(text_family = "serif"),
      height = unit(1.2, "cm"), width = unit(1.2, "cm")
    )
  
  # Apply the manual legend
  p <- add_manual_legend(p)
  
  return(p)
}

# 7. APPLY SAME STYLING TO CAPE TOWN MAP
create_simplified_urban_map <- function(city_name, bbox) {
  message(paste("Creating", city_name, "detailed URBAN level map with FT styling..."))
  
  # Filter institutions for this city
  city_sf <- base_sf %>% 
    filter(City == city_name | 
             (City == "Pretoria" & city_name == "Johannesburg") |
             (City == "Soweto" & city_name == "Johannesburg"))
  
  # Calculate primary category
  city_sf <- city_sf %>%
    mutate(primary_category = case_when(
      has_policy == TRUE ~ "Policy",
      has_engagement == TRUE ~ "Engagement, Advocacy and Capacity Building",
      has_finance == TRUE ~ "Finance and Programmes",
      has_research == TRUE ~ "Research",
      TRUE ~ "Other"
    ))
  
  # Create a better styled base map
  p <- ggplot() +
    # Water color
    geom_rect(aes(xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4]),
              fill = "#e6eef5", color = NA) +
    # Land color with subtle texture
    geom_sf(data = st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                                       xmax = bbox[3], ymax = bbox[4]), crs = 4326)),
            fill = "#f9f9f2", color = NA)
  
  # Add city-specific features
  if (city_name == "Cape Town") {
    # For Cape Town, add better coastline
    coastline <- ne_coastline(scale = "large", returnclass = "sf")
    cape_coast <- st_crop(coastline, st_bbox(c(xmin = bbox[1]-0.1, ymin = bbox[2]-0.1, 
                                               xmax = bbox[3]+0.1, ymax = bbox[4]+0.1), crs = 4326))
    
    # Add some simplified roads for context
    major_roads <- ne_download(scale = "large", type = "roads", category = "cultural", 
                               returnclass = "sf")
    
    if (!is.null(major_roads)) {
      cape_roads <- st_crop(major_roads, st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                                                   xmax = bbox[3], ymax = bbox[4]), crs = 4326))
      
      p <- p + 
        geom_sf(data = cape_coast, color = "#4682B4", size = 0.5) +
        geom_sf(data = cape_roads, color = "#d9d9d9", size = 0.3, alpha = 0.7)
    } else {
      p <- p + geom_sf(data = cape_coast, color = "#4682B4", size = 0.5)
    }
    
  } else if (city_name == "Johannesburg") {
    # For Johannesburg, add subtle but useful base features
    urban_areas <- ne_download(scale = "large", type = "urban_areas", category = "cultural", 
                               returnclass = "sf")
    
    if (!is.null(urban_areas)) {
      gauteng_urban <- st_crop(urban_areas, st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                                                      xmax = bbox[3], ymax = bbox[4]), crs = 4326))
      p <- p + geom_sf(data = gauteng_urban, fill = "#f0f0f0", color = "#d9d9d9", size = 0.2, alpha = 0.7)
    }
    
    # Add major roads for context
    major_roads <- ne_download(scale = "large", type = "roads", category = "cultural", 
                               returnclass = "sf")
    
    if (!is.null(major_roads)) {
      gauteng_roads <- st_crop(major_roads, st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                                                      xmax = bbox[3], ymax = bbox[4]), crs = 4326))
      p <- p + geom_sf(data = gauteng_roads, color = "#d0d0d0", size = 0.3, alpha = 0.7)
    }
  }
  
  # Apply FT theme
  p <- p + create_ft_theme() +
    labs(
      title = paste(city_name, "Health and Climate Network"),
      subtitle = "Showing primary focus area for each institution",
      caption = paste0("Source: Health and Climate Network Partners Database • Generated: ", format(Sys.Date(), "%d %B %Y"))
    )
  
  # Add points by category to ensure correct colors
  # Policy (red) points
  policy_points <- city_sf %>% filter(has_policy == TRUE)
  p <- p + geom_sf(data = policy_points, 
                   aes(shape = institution_type), 
                   fill = POLICY_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Engagement (green) points
  engagement_points <- city_sf %>% 
    filter(has_policy == FALSE & has_engagement == TRUE)
  p <- p + geom_sf(data = engagement_points, 
                   aes(shape = institution_type), 
                   fill = ENGAGEMENT_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Finance (tan) points
  finance_points <- city_sf %>% 
    filter(has_policy == FALSE & has_engagement == FALSE & has_finance == TRUE)
  p <- p + geom_sf(data = finance_points, 
                   aes(shape = institution_type), 
                   fill = FINANCE_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Research (dark blue) points
  research_points <- city_sf %>% 
    filter(has_policy == FALSE & has_engagement == FALSE & 
             has_finance == FALSE & has_research == TRUE)
  p <- p + geom_sf(data = research_points, 
                   aes(shape = institution_type), 
                   fill = RESEARCH_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Add labels with FT styling
  p <- p + ggrepel::geom_text_repel(
    data = city_sf,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5,
    fontface = "bold",
    family = "serif",
    force = 20,
    max.overlaps = 50,
    box.padding = 0.9,
    point.padding = 0.5,
    segment.size = 0.3,
    segment.color = "#505050",
    min.segment.length = 0.1,
    seed = 42,
    direction = "both",
    bg.color = "white",
    bg.r = 0.15
  )
  
  # Add proper legend with verified colors
  p <- p + 
    scale_shape_manual(
      name = "Institution Type",
      values = shape_values,
      labels = c("Data Provider", "Official Partner", "Other")
    ) +
    scale_fill_manual(
      name = "Primary Focus Area",
      values = c(
        "Research" = RESEARCH_COLOR,
        "Finance and Programmes" = FINANCE_COLOR,
        "Engagement, Advocacy and Capacity Building" = ENGAGEMENT_COLOR,
        "Policy" = POLICY_COLOR
      ),
      labels = c("Research", "Finance and Programmes", 
                 "Engagement, Advocacy and Capacity Building", "Policy"),
      guide = guide_legend(override.aes = list(
        shape = 21,
        size = 4,
        fill = c(RESEARCH_COLOR, FINANCE_COLOR, ENGAGEMENT_COLOR, POLICY_COLOR)
      ))
    )
  
  # Set the map extent
  p <- p + coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.3, 
                                style = "ticks", text_family = "serif") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true", 
      style = north_arrow_fancy_orienteering(text_family = "serif"),
      height = unit(1.2, "cm"), width = unit(1.2, "cm")
    )
  
  return(p)
}

# 8. APPLY SAME STYLING TO EUROPE MAP
create_europe_map <- function() {
  message("Creating Europe map with FT styling...")
  
  # Define Europe bounding box
  europe_bbox <- c(-10, 35, 30, 65)
  
  # Filter for European institutions only
  europe_data <- base_sf %>% 
    filter(grepl("Austria|Belgium|Denmark|Estonia|Finland|France|Germany|Greece|Ireland|Italy|Netherlands|Norway|United Kingdom|Spain|Portugal|Sweden|Switzerland", Country))
  
  # Calculate primary category with correct priority
  europe_data <- europe_data %>%
    mutate(primary_category = case_when(
      has_policy == TRUE ~ "Policy",
      has_engagement == TRUE ~ "Engagement, Advocacy and Capacity Building",
      has_finance == TRUE ~ "Finance and Programmes",
      has_research == TRUE ~ "Research",
      TRUE ~ "Other"
    ))
  
  # Get improved base maps
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Create the main plot with FT styling
  p <- ggplot() +
    geom_rect(aes(xmin = -180, xmax = 180, ymin = -90, ymax = 90), 
              fill = "#e6eef5", color = NA) +
    geom_sf(data = world, fill = "#f9f9f2", color = "#d9d9d9", size = 0.3) +
    create_ft_theme() +
    labs(
      title = "European Partners - Health and Climate Network",
      subtitle = "Showing primary focus area for each institution",
      caption = paste0("Source: Health and Climate Network Partners Database • Generated: ", format(Sys.Date(), "%d %B %Y"))
    )
  
  # Add points by category to ensure correct colors
  # Policy (red) points
  policy_points <- europe_data %>% filter(has_policy == TRUE)
  p <- p + geom_sf(data = policy_points, 
                   aes(shape = institution_type), 
                   fill = POLICY_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Engagement (green) points
  engagement_points <- europe_data %>% 
    filter(has_policy == FALSE & has_engagement == TRUE)
  p <- p + geom_sf(data = engagement_points, 
                   aes(shape = institution_type), 
                   fill = ENGAGEMENT_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Finance (tan) points
  finance_points <- europe_data %>% 
    filter(has_policy == FALSE & has_engagement == FALSE & has_finance == TRUE)
  p <- p + geom_sf(data = finance_points, 
                   aes(shape = institution_type), 
                   fill = FINANCE_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Research (dark blue) points
  research_points <- europe_data %>% 
    filter(has_policy == FALSE & has_engagement == FALSE & 
             has_finance == FALSE & has_research == TRUE)
  p <- p + geom_sf(data = research_points, 
                   aes(shape = institution_type), 
                   fill = RESEARCH_COLOR,
                   size = 3.5, color = "black", stroke = 0.5)
  
  # Add labels with FT styling
  p <- p + ggrepel::geom_text_repel(
    data = europe_data,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5,
    fontface = "bold",
    family = "serif",
    force = 10,
    max.overlaps = 30,
    box.padding = 0.7,
    point.padding = 0.5,
    segment.size = 0.3,
    segment.color = "#505050",
    min.segment.length = 0.1,
    seed = 42,
    bg.color = "white",
    bg.r = 0.15
  )
  
  # Add proper legend with verified colors
  p <- p + 
    scale_shape_manual(
      name = "Institution Type",
      values = shape_values,
      labels = c("Data Provider", "Official Partner", "Other")
    ) +
    scale_fill_manual(
      name = "Primary Focus Area",
      values = c(
        "Research" = RESEARCH_COLOR,
        "Finance and Programmes" = FINANCE_COLOR,
        "Engagement, Advocacy and Capacity Building" = ENGAGEMENT_COLOR,
        "Policy" = POLICY_COLOR
      ),
      labels = c("Research", "Finance and Programmes", 
                 "Engagement, Advocacy and Capacity Building", "Policy"),
      guide = guide_legend(override.aes = list(
        shape = 21,
        size = 4,
        fill = c(RESEARCH_COLOR, FINANCE_COLOR, ENGAGEMENT_COLOR, POLICY_COLOR)
      ))
    )
  
  # Set the map extent
  p <- p + coord_sf(xlim = c(europe_bbox[1], europe_bbox[3]), 
                    ylim = c(europe_bbox[2], europe_bbox[4]), 
                    expand = FALSE) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.3, 
                                style = "ticks", text_family = "serif") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true", 
      style = north_arrow_fancy_orienteering(text_family = "serif"),
      height = unit(1.2, "cm"), width = unit(1.2, "cm")
    )
  
  return(p)
}

# Clear the environment of existing map objects
rm(list = c("cape_town_map", "gauteng_map", "southern_africa_map", "europe_map"), 
   envir = .GlobalEnv, inherits = FALSE)

# Force recreation of the maps with print statements
message("Creating maps with revised style...")

# Map for Cape Town - SIMPLIFIED
cape_town_map <- create_simplified_urban_map("Cape Town", cape_town_bbox)
cape_town_map <- add_manual_legend(cape_town_map)
print("Cape Town map created")

# Map for Johannesburg/Gauteng - SIMPLIFIED
gauteng_map <- create_simplified_urban_map("Johannesburg", gauteng_bbox)
gauteng_map <- add_manual_legend(gauteng_map)
print("Johannesburg map created")

# Map for Southern Africa - SOUTHERN AFRICA ONLY
southern_africa_map <- create_regional_map()
# Legend already added in the function
print("Southern Africa map created")

# Map for Europe - SINGLE VERSION with full institution names
europe_map <- create_europe_map()
europe_map <- add_manual_legend(europe_map)
print("Europe map created")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Save the maps with a timestamp to avoid caching
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save all four maps (with the same formatting as before)
ggsave(file.path("output", paste0("cape_town_map_", timestamp, ".pdf")), 
       cape_town_map, width = 11, height = 9, device = cairo_pdf)
ggsave(file.path("output", paste0("cape_town_map_", timestamp, ".png")), 
       cape_town_map, width = 11, height = 9, dpi = 300)

ggsave(file.path("output", paste0("gauteng_map_", timestamp, ".pdf")), 
       gauteng_map, width = 11, height = 9, device = cairo_pdf)
ggsave(file.path("output", paste0("gauteng_map_", timestamp, ".png")), 
       gauteng_map, width = 11, height = 9, dpi = 300)

ggsave(file.path("output", paste0("southern_africa_map_", timestamp, ".pdf")), 
       southern_africa_map, width = 14, height = 10, device = cairo_pdf)
ggsave(file.path("output", paste0("southern_africa_map_", timestamp, ".png")), 
       southern_africa_map, width = 14, height = 10, dpi = 300)

ggsave(file.path("output", paste0("europe_map_", timestamp, ".pdf")), 
       europe_map, width = 14, height = 10, device = cairo_pdf)
ggsave(file.path("output", paste0("europe_map_", timestamp, ".png")), 
       europe_map, width = 14, height = 10, dpi = 300)

message("All maps created and saved in output directory with timestamp: ", timestamp)