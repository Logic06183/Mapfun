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
  RColorBrewer, # for color palettes
  ggrepel      # for text label positioning
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

# Convert to sf object
partners_sf <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define project colors
project_colors <- c(
  'CHAMNHA' = '#4B9CD3',     # Blue
  'HEAT' = '#E94F64',        # Pink/Red
  'ENBEL' = '#00A087',       # Teal
  'GHAP' = '#E74C3C',        # Red
  'HAPI' = '#3498DB',        # Light Blue
  'BioHEAT' = '#E67E22',     # Orange
  'HIGH_Horizons' = '#2C3E50' # Dark Gray
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
              color = if_else(Funder == 1, "Funder", "Partner")),
          alpha = 0.7) +
  
  # Add institution labels with ggrepel
  geom_text_repel(
    data = partners_sf,
    aes(label = Institution,
        geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    force = 2,
    max.overlaps = 15
  ) +
  
  # Customize colors
  scale_color_manual(
    values = c(
      "Partner" = "#3498DB",
      "Funder" = "#2C3E50"
    ),
    name = "Institution Type"
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(2, 8),
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
ggsave("partnership_map_ggplot.pdf", width = 16, height = 9, dpi = 300)
ggsave("partnership_map_ggplot.png", width = 16, height = 9, dpi = 300) 