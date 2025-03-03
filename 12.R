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
  "ggnewscale"    # for multiple fill scales
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
    highlight_color = ifelse(name %in% heat_countries, "#FFF7BC", "white")
  )

# Convert to sf object for plotting partner locations
partners_sf <- df_filtered %>%
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Define leadership colors to match the image
leadership_colors <- c(
  "Gueladio Cisse" = "#4682B4",  # Steel blue
  "Matthew Chersich" = "#B22222", # Firebrick red
  "Joint Projects" = "#9370DB"    # Medium purple
)

# Create the map of Africa and Europe
map <- ggplot() +
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
          alpha = 0.8,
          shape = 21,
          stroke = 0.5,
          fill = "white") +
  
  # Set leadership colors
  scale_color_manual(values = leadership_colors, 
                     name = "Project Leadership",
                     na.value = "gray50") +
  
  # Set size scale
  scale_size_continuous(name = "Number of Projects",
                        breaks = 1:7,
                        range = c(2, 7)) +
  
  # Add labels for larger partners
  geom_text_repel(
    data = partners_sf %>% filter(total_projects >= 1),
    aes(label = Institution, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    family = "opensans",
    force = 1,
    max.overlaps = 15,
    box.padding = 0.5,
    point.padding = 0.2,
    segment.color = "gray50",
    segment.size = 0.3,
    min.segment.length = 0
  ) +
  
  # Add title and legend
  labs(
    title = "HE²AT Center",
    subtitle = "Research Partnerships in Africa and Europe",
    caption = "Data: Research Collaboration Network\nFebruary 2025"
  ) +
  
  # Apply a clean theme
  theme_minimal() +
  
  # Customize the theme
  theme(
    text = element_text(family = "montserrat"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray50"),
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  ) +
  
  # Set the aspect ratio for better display of Africa and Europe
  coord_sf(xlim = c(-20, 60), ylim = c(-40, 70), expand = FALSE)

# Add a north arrow
map <- map +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering(
      fill = c("gray40", "white"),
      line_col = "gray20"
    )
  ) +
  # Add scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    bar_cols = c("gray20", "white"),
    text_family = "montserrat"
  ) +
  # Adjust position to avoid overlap with north arrow
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in")
  )

# Create a separate legend for contributing countries
contributing_legend <- data.frame(
  category = c("Contributing Countries", "Other Countries"),
  color = c("#FFF7BC", "white")
)

# Add HEAT contributing countries legend
map <- map +
  guides(
    fill = guide_legend(title = "HE²AT Center", order = 1),
    size = guide_legend(title = "Number of Projects", order = 2),
    color = guide_legend(title = "Project Leadership", order = 3)
  )

# Save the map
ggsave("africa_europe_partnership_map.png", map, width = 12, height = 10, dpi = 300)

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
    Total_Project_Participations = sum(total_projects)
  ) %>%
  arrange(desc(Total_Project_Participations))
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
cat("================================\n")
print(table(df_filtered$leadership))

# Save detailed summary to CSV
leadership_summary <- df_filtered %>%
  select(Institution, Country, leadership, total_projects) %>%
  arrange(leadership, desc(total_projects))

write.csv(leadership_summary, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/africa_europe_partners_leadership_summary.csv", 
          row.names = FALSE)

cat("\nDetailed leadership summary has been saved to 'africa_europe_partners_leadership_summary.csv'\n")

# Define project columns for network creation
project_cols <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", "HAPI", "BioHEAT", "HIGH_Horizons")

# Create edges based on project co-participation
edges_df <- data.frame()
institutions <- unique(df_filtered$Institution)
n <- length(institutions)

for(i in 1:(n-1)) {
  for(j in (i+1):n) {
    inst1 <- institutions[i]
    inst2 <- institutions[j]
    
    # Get rows for both institutions
    row1 <- df_filtered[df_filtered$Institution == inst1, ]
    row2 <- df_filtered[df_filtered$Institution == inst2, ]
    
    # Count shared projects
    shared <- sum(
      sapply(project_cols, function(col) {
        row1[[col]] == 1 & row2[[col]] == 1
      })
    )
    
    if(shared > 0) {
      edges_df <- rbind(edges_df, data.frame(
        from = inst1,
        to = inst2,
        weight = shared
      ))
    }
  }
}

# Create nodes data frame
nodes_df <- data.frame(
  name = institutions,
  leadership = case_when(
    df_filtered$Gueladio_Cisse == 1 & df_filtered$Matthew_Chersich == 1 ~ "Joint Projects",
    df_filtered$Gueladio_Cisse == 1 ~ "Gueladio Cisse",
    df_filtered$Matthew_Chersich == 1 ~ "Matthew Chersich",
    df_filtered$Pilot_Projects == 1 ~ "Pilot Projects",
    TRUE ~ "Other"
  )[match(institutions, df_filtered$Institution)]
)

# Create the graph object
graph <- tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df)

# Create the layout
graph_layout <- create_layout(graph, 'fr')

# Create the network plot
network_plot <- ggraph(graph_layout) +
  geom_edge_link(aes(width = weight),
                 alpha = 0.2, 
                 color = "gray50") +
  geom_node_point(aes(color = leadership),
                  size = 10) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 size = 3,
                 max.overlaps = 100) +
  scale_edge_width(range = c(0.5, 2)) +
  scale_color_manual(values = c(
    'Gueladio Cisse' = '#4682B4',  # Steel blue
    'Matthew Chersich' = '#B22222', # Firebrick red
    'Joint Projects' = '#9370DB',    # Medium purple
    'Pilot Projects' = '#9370DB',    # Same purple for pilot projects
    'Other' = '#9370DB'              # Same purple for other
  )) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(color = "Project Leadership",
       edge_width = "Number of Shared Projects")

# Print and save the plot
print(network_plot)
ggsave("africa_europe_partnership_network.pdf", 
       plot = network_plot,
       width = 16, 
       height = 16, 
       dpi = 300) 