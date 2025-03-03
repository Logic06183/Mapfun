# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to install and load a package
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    if (!require(package, character.only = TRUE)) {
      stop(paste("Package", package, "failed to install/load"))
    }
  }
}

# Install and load required packages individually
required_packages <- c(
  "tidyverse",    # for data manipulation
  "sf",           # for spatial data
  "rnaturalearth",# for country boundaries
  "rnaturalearthdata",
  "ggspatial",    # for north arrow
  "viridis",      # for color palettes
  "showtext",     # for custom fonts
  "RColorBrewer", # for color palettes
  "ggrepel",      # for text label positioning
  "ggthemes",     # for scientific themes
  "scales",       # for better scale formatting
  "ggraph",       # for network visualization
  "ggnewscale",   # for multiple fill scales
  "cowplot",      # for combining plots
  "extrafont"     # for better font handling in PDF
)

# Install and load each package
for (pkg in required_packages) {
  install_and_load(pkg)
}

# Load Google Fonts
font_add_google("Montserrat", "montserrat")
font_add_google("Open Sans", "opensans")
showtext_auto()

# Define HEAT contributing countries
heat_countries <- c(
  "South Africa", 
  "Zimbabwe", 
  "Nigeria", 
  "Ghana", 
  "Cameroon", 
  "Benin", 
  "Burkina Faso", 
  "Côte d'Ivoire",
  "Mauritania", 
  "Ethiopia", 
  "Kenya", 
  "Uganda", 
  "Tanzania", 
  "Malawi", 
  "Mozambique", 
  "Zambia", 
  "Rwanda", 
  "Botswana",
  "Dem. Rep. Congo",
  "Sierra Leone", 
  "Lesotho", 
  "Senegal",
  "Namibia"
)

# Get Africa and Europe map data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Combine Africa and Europe for a single map
africa_europe <- bind_rows(africa, europe)

# Add a check to verify all countries are recognized in the map data
cat("\nVerifying HEAT countries in map data:\n")
cat("====================================\n")
missing_countries <- setdiff(heat_countries, africa_europe$name)
if(length(missing_countries) > 0) {
  cat("\nWarning: The following HEAT countries may have different names in the map data:\n")
  print(missing_countries)
  cat("\nAvailable country names in map data:\n")
  print(sort(africa_europe$name))
}

# Read the cleaned data
df <- read_csv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv") %>%
  mutate(Partners = as.character(Partners))  # Convert Partners to character

# Filter for African and European institutions
all_countries <- africa_europe$name
df_filtered <- df %>%
  filter(Country %in% all_countries)

# Calculate total projects for each institution
df_filtered <- df_filtered %>%
  mutate(total_projects = CHAMNHA + HEAT + ENBEL + GHAP + HAPI + BioHEAT + HIGH_Horizons)

# Create leadership categories based on the columns
df_filtered <- df_filtered %>%
  mutate(leadership = case_when(
    Gueladio_Cisse == 1 & Matthew_Chersich == 1 ~ "Joint Projects",
    Gueladio_Cisse == 1 ~ "Gueladio Cisse",
    Matthew_Chersich == 1 ~ "Matthew Chersich",
    Pilot_Projects == 1 ~ "Pilot Projects",
    TRUE ~ NA_character_
  ))

# Prepare the map data - mark HEAT contributing countries
africa_europe <- africa_europe %>%
  mutate(
    has_partners = ifelse(name %in% heat_countries, "Contributing Countries", "Other Countries"),
    # Use a more subtle, colorblind-friendly yellow (#FFF7BC)
    highlight_color = ifelse(name %in% heat_countries, "#FFFAC8", "white")
  )

# Convert to sf object for plotting partner locations
partners_sf <- df_filtered %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define leadership colors - colorblind-friendly palette
# Avoiding problematic red-green combinations, using more distinguishable colors
leadership_colors <- c(
  "Gueladio Cisse" = "#3182BD",  # Blue (more accessible)
  "Matthew Chersich" = "#E6550D", # Orange (better than red for colorblindness)
  "Joint Projects" = "#756BB1"    # Purple (distinguishable)
)

# Create a custom graticule for the map with light grid lines
custom_graticule <- st_graticule(
  africa_europe, 
  lat = seq(-40, 70, 10), 
  lon = seq(-20, 60, 10),
  crs = 4326
)

# Create the map of Africa and Europe
map <- ggplot() +
  # Add light graticule for scientific precision
  geom_sf(data = custom_graticule,
          color = alpha("gray80", 0.5),
          size = 0.2) +
  
  # Base map with highlighted countries
  geom_sf(data = africa_europe,
          aes(fill = highlight_color),
          color = "gray40",           
          size = 0.3) +
  
  # Set the fill manually
  scale_fill_identity() +
  
  # Add partner points sized by project count and colored by leadership
  geom_sf(data = partners_sf,
          aes(size = total_projects, color = leadership),
          alpha = 0.85,
          shape = 21,
          stroke = 0.7,
          fill = "white") +
  
  # Set leadership colors
  scale_color_manual(values = leadership_colors, 
                     name = "Project Leadership",
                     na.value = "gray50") +
  
  # Set size scale with clear breaks
  scale_size_continuous(name = "Number of Projects",
                        breaks = 1:7,
                        limits = c(1, 7),
                        range = c(2, 7),
                        guide = guide_legend(
                          override.aes = list(color = "black", stroke = 0.5),
                          title.position = "top",
                          title.hjust = 0.5
                        )) +
  
  # Add precisely positioned labels for all institutions
  geom_text_repel(
    data = partners_sf,
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "plain",
    family = "opensans",
    force = 2,
    max.overlaps = 15,
    box.padding = 0.5,
    point.padding = 0.2,
    segment.color = "gray40",
    segment.size = 0.3,
    min.segment.length = 0,
    bg.color = "white",
    bg.r = 0.1
  ) +
  
  # Add title and legend
  labs(
    title = "HE²AT Center",
    subtitle = "Research Partnerships in Africa and Europe",
    caption = paste0("Figure prepared for Wellcome Trust application | Data: Research Collaboration Network | ", format(Sys.Date(), "%B %Y"))
  ) +
  
  # Apply a clean theme
  theme_minimal(base_family = "opensans") +
  
  # Customize the theme
  theme(
    text = element_text(family = "opensans"),
    plot.title = element_text(family = "montserrat", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(10, 10, 10, 10),
    legend.box.background = element_rect(color = "gray80", fill = alpha("white", 0.8), size = 0.3),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  
  # Set the aspect ratio for better display of Africa and Europe
  coord_sf(xlim = c(-20, 60), ylim = c(-40, 70), expand = FALSE)

# Add a north arrow
map <- map +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering(
      fill = c("gray40", "white"),
      line_col = "gray20"
    ),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  # Add scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    bar_cols = c("gray20", "white"),
    text_family = "opensans",
    pad_x = unit(1.2, "in"),
    pad_y = unit(0.2, "in")
  )

# Create a legend for contributing countries
contributing_legend <- data.frame(
  category = c("Contributing Countries", "Other Countries"),
  color = c("#FFFAC8", "white")
)

# Add HEAT contributing countries legend
map <- map +
  guides(
    fill = guide_legend(title = "HE²AT Center", order = 1),
    size = guide_legend(title = "Number of Projects", order = 2),
    color = guide_legend(title = "Project Leadership", order = 3)
  )

# Create a simple legend for Contributing Countries
contributing_legend_plot <- ggplot(contributing_legend, aes(x = 1, y = 1, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  theme_void() +
  labs(title = "HE²AT Center") +
  theme(
    plot.title = element_text(family = "montserrat", size = 10, face = "bold", hjust = 0),
    legend.position = "bottom"
  )

# Create a title panel
title_panel <- ggplot() +
  theme_void() +
  labs(
    title = "HE²AT Center",
    subtitle = "Research Partnerships in Africa and Europe"
  ) +
  theme(
    plot.title = element_text(family = "montserrat", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "opensans", size = 14, hjust = 0.5)
  )

# -------------------------------------------------------------------------
# Save the map as a high-quality PDF for publication
# -------------------------------------------------------------------------

# A4 dimensions in inches (landscape orientation with high-resolution)
width_a4 <- 11.69
height_a4 <- 8.27

# Create the PDF with appropriate dimensions
ggsave(
  "HE2AT_partnership_map_for_Wellcome.pdf", 
  map,
  width = width_a4, 
  height = height_a4, 
  dpi = 600,
  device = cairo_pdf
)

# Also save as PNG for quick reference
ggsave(
  "HE2AT_partnership_map_for_Wellcome.png", 
  map,
  width = width_a4, 
  height = height_a4, 
  dpi = 300
)

cat("\nCreated high-quality partnership map PDF for Wellcome Trust application.\n")
cat("File saved as: HE2AT_partnership_map_for_Wellcome.pdf\n")

# Display the map
print(map)

# Data validation and summary statistics
cat("\nSummary of African and European Partners:\n")
cat("============================================\n")

# Total number of institutions
cat("\nTotal African and European institutions:", nrow(df_filtered))

# Project participation counts
cat("\n\nParticipation by Project:")
project_counts <- data.frame(
  CHAMNHA = sum(df_filtered$CHAMNHA),
  "HE²AT" = sum(df_filtered$HEAT),
  ENBEL = sum(df_filtered$ENBEL),
  GHAP = sum(df_filtered$GHAP),
  HAPI = sum(df_filtered$HAPI),
  BioHEAT = sum(df_filtered$BioHEAT),
  HIGH_Horizons = sum(df_filtered$HIGH_Horizons)
)

print(project_counts)

# Institutions by country
cat("\n\nInstitutions by Country:\n")
country_counts <- df_filtered %>%
  group_by(Country) %>%
  summarise(
    Institutions = n(),
    Projects = sum(total_projects)
  )
print(country_counts)

# Multi-project institutions
cat("\n\nInstitutions with Multiple Projects:\n")
multi_project <- df_filtered %>%
  filter(total_projects > 1) %>%
  select(Institution, Country, total_projects) %>%
  arrange(desc(total_projects))
print(multi_project)

# List of all institutions and their projects
cat("\n\nDetailed Project Participation:\n")
institution_projects <- df_filtered %>%
  select(Institution, Country, CHAMNHA, HEAT, ENBEL, GHAP, HAPI, BioHEAT, HIGH_Horizons, total_projects) %>%
  arrange(desc(total_projects), Country, Institution)
print(institution_projects)

# Save detailed summary to CSV
write.csv(institution_projects, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/africa_europe_partners_summary.csv", 
          row.names = FALSE)

cat("\nDetailed summary has been saved to 'africa_europe_partners_summary.csv'\n")

# Print summary statistics
cat("\nSummary of Leadership Distribution:\n")
cat("=================================\n")
print(table(df_filtered$leadership))

# Save detailed summary to CSV
leadership_summary <- df_filtered %>%
  select(Institution, Country, leadership, total_projects) %>%
  arrange(leadership, desc(total_projects))

write.csv(leadership_summary, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/africa_europe_partners_leadership_summary.csv", 
          row.names = FALSE)

cat("\nDetailed leadership summary has been saved to 'africa_europe_partners_leadership_summary.csv'\n")