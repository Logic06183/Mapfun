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

# Get Africa map data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Read the data
df <- read_csv("partners_data_with_coords.csv")

# Filter for African institutions
african_countries <- africa$name
df <- df %>%
  filter(Country %in% african_countries)

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

# Create a vector of countries with partners
partner_countries <- unique(df$Country)

# Add a column to the Africa map data to indicate partner countries
africa <- africa %>%
  mutate(has_partners = name %in% partner_countries)

# Create the map
ggplot() +
  # Base Africa map with yellow fill for partner countries
  geom_sf(data = africa,
          aes(fill = has_partners),
          color = "gray80",
          size = 0.2) +
  
  # Add partner points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              color = primary_project),
          alpha = 0.9) +
  
  # Add institution labels
  geom_text_repel(
    data = partners_sf,
    aes(label = Institution,
        geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "bold",
    box.padding = 0.7,
    point.padding = 0.4,
    force = 2,
    max.overlaps = 30,
    color = "black",
    bg.color = "white",
    bg.r = 0.15
  ) +
  
  # Customize colors for points
  scale_color_manual(
    values = project_colors,
    name = "Project Participation",
    breaks = names(project_colors)
  ) +
  
  # Customize colors for choropleth
  scale_fill_manual(
    values = c("TRUE" = alpha("#FFE0B2", 0.7),  # Light yellow for partner countries
               "FALSE" = "white"),               # White for non-partner countries
    guide = "none"  # Hide the legend for the fill
  ) +
  
  # Customize size scale
  scale_size_continuous(
    name = "Number of Projects",
    range = c(3, 10),
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
  
  # Set map extent to focus on Africa
  coord_sf(
    xlim = c(-20, 50),
    ylim = c(-40, 40),
    expand = FALSE
  ) +
  
  # Add titles
  labs(
    title = "African Health Research Partnership Network",
    subtitle = "Size indicates number of project participations",
    caption = "Data: Research Collaboration Network"
  )

# Save the map
ggsave("african_partnership_map_with_labels.pdf", width = 16, height = 16, dpi = 300)
ggsave("african_partnership_map_with_labels.png", width = 16, height = 16, dpi = 300)

# Data validation and summary statistics
cat("\nSummary of African Partners:\n")
cat("============================\n")

# Total number of institutions
cat("\nTotal African institutions:", nrow(df))

# Project participation counts
cat("\n\nParticipation by Project:")
project_counts <- data.frame(
  CHAMNHA = sum(df$CHAMNHA),
  HEAT = sum(df$HEAT),
  ENBEL = sum(df$ENBEL),
  GHAP = sum(df$GHAP),
  HAPI = sum(df$HAPI),
  BioHEAT = sum(df$BioHEAT),
  HIGH_Horizons = sum(df$HIGH_Horizons)
)
print(project_counts)

# Institutions by country
cat("\n\nInstitutions by Country:\n")
country_counts <- df %>%
  group_by(Country) %>%
  summarise(
    Institutions = n(),
    Total_Project_Participations = sum(total_projects)
  ) %>%
  arrange(desc(Total_Project_Participations))
print(country_counts)

# Multi-project institutions
cat("\n\nInstitutions with Multiple Projects:\n")
multi_project <- df %>%
  filter(total_projects > 1) %>%
  select(Institution, Country, total_projects) %>%
  arrange(desc(total_projects))
print(multi_project)

# List of all institutions and their projects
cat("\n\nDetailed Project Participation:\n")
institution_projects <- df %>%
  select(Institution, Country, CHAMNHA, HEAT, ENBEL, GHAP, HAPI, BioHEAT, HIGH_Horizons, total_projects) %>%
  arrange(desc(total_projects), Country, Institution)
print(institution_projects)

# Save detailed summary to CSV
write.csv(institution_projects, 
          "african_partners_summary.csv", 
          row.names = FALSE)

cat("\nDetailed summary has been saved to 'african_partners_summary.csv'\n") 