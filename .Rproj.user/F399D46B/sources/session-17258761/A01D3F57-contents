# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # for data manipulation
  sf,           # for spatial data
  rnaturalearth,# for country boundaries
  rnaturalearthdata,
  ggspatial,    # for north arrow
  viridis,      # for color palettes
  showtext,     # for custom fonts
  RColorBrewer  # for color palettes
)

# Load Google Fonts
font_add_google("Montserrat", "montserrat")
font_add_google("Open Sans", "opensans")
showtext_auto()

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Read the data
df <- read_csv("partners_data_with_coords.csv")

# Calculate total projects for each institution
df <- df %>%
  mutate(total_projects = CHAMNHA + HEAT + ENBEL + GHAP + HAPI + BioHEAT + HIGH_Horizons)

# Determine primary project and multi-project status
df <- df %>%
  mutate(
    primary_project = case_when(
      Funder == 1 ~ "Funder",
      total_projects > 1 ~ "Multiple Projects",
      CHAMNHA == 1 ~ "CHAMNHA",
      HEAT == 1 ~ "HEAT",
      ENBEL == 1 ~ "ENBEL",
      GHAP == 1 ~ "GHAP",
      HAPI == 1 ~ "HAPI",
      BioHEAT == 1 ~ "BioHEAT",
      HIGH_Horizons == 1 ~ "HIGH_Horizons",
      TRUE ~ "None"
    )
  )

# Convert to sf object
partners_sf <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define updated color scheme
project_colors <- c(
  'CHAMNHA' = '#4B9CD3',        # Blue
  'HEAT' = '#E94F64',           # Pink/Red
  'ENBEL' = '#00A087',          # Teal
  'GHAP' = '#E74C3C',           # Red
  'HAPI' = '#3498DB',           # Light Blue
  'BioHEAT' = '#E67E22',        # Orange
  'HIGH_Horizons' = '#2C3E50',  # Dark Gray
  'Multiple Projects' = '#9B59B6', # Purple
  'Funder' = '#2C3E50'          # Dark Gray
)

# Create the map
ggplot() +
  # Base world map
  geom_sf(data = world,
          fill = "white",
          color = "gray80",
          size = 0.2) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              color = primary_project),
          alpha = 0.9) +
  
  # Customize colors
  scale_color_manual(
    values = project_colors,
    name = "Project Participation",
    breaks = names(project_colors)
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(3, 10),  # Slightly larger points since no labels
    breaks = c(1, 2, 3, 4, 5, 6, 7)
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    text = element_text(family = "opensans"),
    plot.title = element_text(family = "montserrat", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "montserrat", size = 12, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(family = "montserrat", face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = alpha("#F0F8FF", 0.2), color = NA)
  ) +
  
  # Set map extent
  coord_sf(
    xlim = c(-130, 150),
    ylim = c(-50, 70),
    expand = FALSE
  ) +
  
  # Add titles
  labs(
    title = "Global Health Research Partnership Network",
    subtitle = "Size indicates number of project participations",
    caption = "Data: Research Collaboration Network"
  )

# Save the map
ggsave("partnership_map_no_labels.pdf", width = 16, height = 9, dpi = 300)
ggsave("partnership_map_no_labels.png", width = 16, height = 9, dpi = 300) 