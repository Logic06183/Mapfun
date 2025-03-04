# Load required libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggspatial)

# Define colors for categories
RESEARCH_COLOR <- "#0F1F2C"       # Dark blue for Research
FINANCE_COLOR <- "#90876E"        # Tan for Finance and Programmes
ENGAGEMENT_COLOR <- "#1E4611"     # Green for Engagement
POLICY_COLOR <- "#CD1A1B"         # Red for Policy

# Define shape values
shape_values <- c("Data Provider" = 24, "Official Partner" = 21, "Other" = 21)

# Load data
message("Loading base data...")
base_csv <- read.csv("base.csv", sep = ";", stringsAsFactors = FALSE)
print(paste("Successfully loaded data with", nrow(base_csv), "rows"))

# Convert to spatial data
base_sf <- st_as_sf(base_csv, coords = c("lon", "lat"), crs = 4326)

# Add category flags and create better labels
# Also exclude institutions where Gueladio_Cisse = 1
base_sf <- base_sf %>%
  filter(Gueladio_Cisse != 1) %>%  # Exclude Gueladio's projects
  mutate(
    has_research = Research == 1,
    has_policy = Policy == 1,
    has_engagement = Engagement..Advocacy..and.Capacity.Building == 1,
    has_finance = Finance_programmes == 1,
    institution_type = case_when(
      Data_Providers == 1 ~ "Data Provider",
      Official.Partners == 1 ~ "Official Partner",
      TRUE ~ "Other"
    ),
    # Create better labels - more descriptive than Short_Name but not as long as Institution
    display_name = case_when(
      # Universities - keep "University of X" format
      grepl("University", Institution) ~ gsub("University of ", "Univ. of ", Institution),
      # Institutes - keep "X Institute" format
      grepl("Institute", Institution) ~ Institution,
      # For others, use the full name if it's not too long, otherwise use Short_Name
      nchar(Institution) <= 30 ~ Institution,
      TRUE ~ Short_Name
    ),
    # Flag for Johannesburg institutions to ensure they get labeled
    is_joburg = City == "Johannesburg"
  )

# Create a focused map of Europe and Africa only
create_focused_map <- function() {
  message("Creating focused map of Europe and Africa...")
  
  # Get only Europe and Africa from Natural Earth
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Crop the world data to our region of interest
  africa_europe <- st_crop(world, xmin = -25, xmax = 45, ymin = -40, ymax = 70)
  
  # Filter points to only include those in our region
  filtered_sf <- base_sf %>%
    filter(
      st_coordinates(.)[,1] >= -25 &
      st_coordinates(.)[,1] <= 45 &
      st_coordinates(.)[,2] >= -40 &
      st_coordinates(.)[,2] <= 70
    )
  
  # Create the map with ONLY the cropped data
  p <- ggplot() +
    # Background for water
    geom_sf(data = africa_europe, 
            fill = "#f9f9f2", color = "#d9d9d9", size = 0.3) +
    # Apply theme
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      axis.text = element_text(size = 9),
      axis.title = element_blank(),
      panel.grid.major = element_line(color = "#e5e5e5", size = 0.2),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = "#e6eef5", color = NA)
    ) +
    labs(
      title = "European and African Partners - Health and Climate Network",
      subtitle = "Showing primary focus area for each institution",
      caption = paste0("Source: Health and Climate Network Partners Database • Generated: ", 
                      format(Sys.Date(), "%d %B %Y"))
    )
  
  # Add points for each category
  # Research (dark blue)
  research_points <- filtered_sf %>% filter(has_research == TRUE)
  p <- p + geom_sf(data = research_points, 
                  aes(shape = institution_type), 
                  fill = RESEARCH_COLOR,
                  size = 3.5, color = "black", stroke = 0.5)
  
  # Finance (tan)
  finance_points <- filtered_sf %>% filter(has_finance == TRUE)
  p <- p + geom_sf(data = finance_points, 
                  aes(shape = institution_type), 
                  fill = FINANCE_COLOR,
                  size = 3.5, color = "black", stroke = 0.5)
  
  # Engagement (green)
  engagement_points <- filtered_sf %>% filter(has_engagement == TRUE)
  p <- p + geom_sf(data = engagement_points, 
                  aes(shape = institution_type), 
                  fill = ENGAGEMENT_COLOR,
                  size = 3.5, color = "black", stroke = 0.5)
  
  # Policy (red)
  policy_points <- filtered_sf %>% filter(has_policy == TRUE)
  p <- p + geom_sf(data = policy_points, 
                  aes(shape = institution_type), 
                  fill = POLICY_COLOR,
                  size = 3.5, color = "black", stroke = 0.5)
  
  # Add labels for non-Johannesburg institutions
  p <- p + geom_text_repel(
    data = filtered_sf %>% filter(!is_joburg),
    aes(geometry = geometry, label = display_name),
    stat = "sf_coordinates",
    size = 2.8,
    family = "serif",
    force = 3,
    max.overlaps = 40,
    box.padding = 0.6,
    point.padding = 0.4,
    min.segment.length = 0,
    segment.color = "#888888",
    segment.size = 0.3
  )
  
  # Add labels specifically for Johannesburg institutions with higher priority
  p <- p + geom_text_repel(
    data = filtered_sf %>% filter(is_joburg),
    aes(geometry = geometry, label = display_name),
    stat = "sf_coordinates",
    size = 2.8,
    family = "serif",
    force = 4,  # Higher force to spread labels more
    max.overlaps = 50,  # Allow more overlaps for Joburg
    box.padding = 0.7,
    point.padding = 0.5,
    min.segment.length = 0,
    segment.color = "#666666",
    segment.size = 0.4,
    nudge_x = 2,  # Nudge labels to the right
    direction = "y"  # Spread vertically
  )
  
  # Add scale bar
  p <- p + 
    annotation_scale(
      location = "bl",
      width_hint = 0.2,
      style = "bar",
      text_family = "serif",
      text_col = "#333333",
      bar_cols = c("#333333", "#ffffff")
    )
  
  # Set the shape scale
  p <- p + scale_shape_manual(values = shape_values)
  
  return(p)
}

# Create the map
focused_map <- create_focused_map()
print("Europe and Africa focused map created")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Save the map with a timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save the map
ggsave(file.path("output", paste0("europe_africa_map_", timestamp, ".pdf")), 
       focused_map, width = 14, height = 10, device = cairo_pdf)
ggsave(file.path("output", paste0("europe_africa_map_", timestamp, ".png")), 
       focused_map, width = 14, height = 10, dpi = 300)

message("Europe and Africa map created and saved in output directory with timestamp: ", timestamp)
