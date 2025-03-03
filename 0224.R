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

# Add a check to verify all countries are recognized in the map data
cat("\nVerifying HEAT countries in map data:\n")
cat("====================================\n")
missing_countries <- setdiff(heat_countries, africa$name)
if(length(missing_countries) > 0) {
  cat("\nWarning: The following HEAT countries may have different names in the map data:\n")
  print(missing_countries)
  cat("\nAvailable country names in map data:\n")
  print(sort(africa$name))
}

# Get Africa map data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Read the cleaned data
df <- read_csv("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/partners_cleaned.csv") %>%
  mutate(Partners = as.character(Partners))  # Convert Partners to character

# Filter for African institutions
african_countries <- africa$name
df_africa <- df %>%
  filter(Country %in% african_countries)

# Calculate total projects for each institution
df_africa <- df_africa %>%
  mutate(total_projects = CHAMNHA + HEAT + ENBEL + GHAP + HAPI + BioHEAT + HIGH_Horizons)

# Create leadership categories based on the columns
df_africa <- df_africa %>%
  mutate(leadership = case_when(
    Gueladio_Cisse == 1 & Matthew_Chersich == 1 ~ "Joint Projects",
    Gueladio_Cisse == 1 ~ "Gueladio Cisse",
    Matthew_Chersich == 1 ~ "Matthew Chersich",
    Pilot_Projects == 1 ~ "Pilot Projects",
    TRUE ~ "Other"
  ))

# Convert to sf object
partners_sf <- df_africa %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Create a vector of countries with partners
partner_countries <- unique(df$Country)

# Add a column to the Africa map data to indicate HEAT countries
africa <- africa %>%
  mutate(has_partners = name %in% heat_countries)

# Define leadership colors
leadership_colors <- c(
  'Gueladio Cisse' = '#2C85B2',    # Blue
  'Matthew Chersich' = '#B23A48',   # Red
  'Joint Projects' = '#7E4F88'      # Purple
)

# Create the map
map <- ggplot() +
  # Base Africa map
  geom_sf(data = africa,
          aes(fill = has_partners),
          color = "gray40",           
          size = 0.3) +
  
  # Add graticules
  geom_sf(data = st_graticule(africa, lat = seq(-40, 40, 10), lon = seq(-20, 50, 10)),
          color = alpha("gray60", 0.3),
          size = 0.1) +
  
  # First fill scale for countries
  scale_fill_manual(
    values = c(
      "TRUE" = alpha("#FFD700", 0.4),
      "FALSE" = "white"
    ),
    name = expression(paste("HE"^2, "AT Center")),
    labels = "Contributing Countries",
    breaks = c("TRUE")
  ) +
  
  # Add new fill scale for points
  new_scale("fill") +
  
  # Add partner points with filled colors
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              fill = leadership),    # This fills the circles based on leadership
          color = "gray40",         # Dark gray border for all circles
          alpha = 0.85,
          stroke = 0.5,
          shape = 21) +             # Filled circle with border
  
  # Add labels for all institutions
  geom_text_repel(
    data = partners_sf,
    aes(label = Institution,
        geometry = geometry),
    stat = "sf_coordinates",
    size = 2.8,
    fontface = "bold",
    box.padding = 0.8,
    point.padding = 0.5,
    force = 10,
    max.overlaps = 50,
    color = "black",
    bg.color = "white",
    bg.r = 0.15,
    min.segment.length = 0.3,
    segment.color = "gray50",
    segment.size = 0.2,
    nudge_x = 1,
    nudge_y = 1,
    direction = "both"
  ) +
  
  # Second fill scale for leadership points
  scale_fill_manual(
    values = leadership_colors,
    name = "Project Leadership",
    breaks = c('Gueladio Cisse', 'Matthew Chersich', 'Joint Projects'),
    guide = guide_legend(override.aes = list(
      size = 5,
      alpha = 0.85,
      shape = 21
    ))
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
    plot.title = element_text(family = "montserrat", size = 16, face = "bold", hjust = 0, margin = margin(b = 20)),
    plot.subtitle = element_text(family = "montserrat", size = 12, hjust = 0, margin = margin(b = 20)),
    legend.position = "right",
    legend.title = element_text(family = "montserrat", face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.box = "vertical",
    legend.box.background = element_rect(color = "gray80", fill = "white", size = 0.3),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    panel.background = element_rect(fill = alpha("#F0F8FF", 0.2), color = NA),
    legend.key.size = unit(1.2, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.spacing = unit(0.4, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Add north arrow
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  # Set map extent to focus on Africa
  coord_sf(
    xlim = c(-20, 50),
    ylim = c(-40, 40),
    expand = FALSE
  ) +
  
  # Update labels
  labs(
    title = "African Health Research Partnership Network",
    subtitle = expression(paste("Project Leadership and Collaborations (HE"^2, "AT Center)")),
    caption = paste0("Data: Research Collaboration Network\n",
                     format(Sys.Date(), "%B %Y"))
  ) +
  
  # Update the map boundaries for HEAT countries
  geom_sf(data = africa %>% filter(has_partners),
          fill = NA,
          color = "#FFD700",
          size = 0.8)

# Display the map
print(map)

# Save the map
ggsave("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partnership_map_leadership.pdf", 
       width = 16, height = 16, dpi = 300,
       device = cairo_pdf)

ggsave("C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partnership_map_leadership.png", 
       width = 16, height = 16, dpi = 300,
       bg = "white")

# Data validation and summary statistics
cat("\nSummary of African Partners:\n")
cat("============================\n")

# Total number of institutions
cat("\nTotal African institutions:", nrow(df_africa))

# Project participation counts
cat("\n\nParticipation by Project:")
project_counts <- data.frame(
  CHAMNHA = sum(df_africa$CHAMNHA),
  "HE²AT" = sum(df_africa$HEAT),
  ENBEL = sum(df_africa$ENBEL),
  GHAP = sum(df_africa$GHAP),
  HAPI = sum(df_africa$HAPI),
  BioHEAT = sum(df_africa$BioHEAT),
  HIGH_Horizons = sum(df_africa$HIGH_Horizons)
)

print(project_counts)

# Institutions by country
cat("\n\nInstitutions by Country:\n")
country_counts <- df_africa %>%
  group_by(Country) %>%
  summarise(
    Institutions = n(),
    Total_Project_Participations = sum(total_projects)
  ) %>%
  arrange(desc(Total_Project_Participations))
print(country_counts)

# Multi-project institutions
cat("\n\nInstitutions with Multiple Projects:\n")
multi_project <- df_africa %>%
  filter(total_projects > 1) %>%
  select(Institution, Country, total_projects) %>%
  arrange(desc(total_projects))
print(multi_project)

# List of all institutions and their projects
cat("\n\nDetailed Project Participation:\n")
institution_projects <- df_africa %>%
  select(Institution, Country, CHAMNHA, HEAT, ENBEL, GHAP, HAPI, BioHEAT, HIGH_Horizons, total_projects) %>%
  arrange(desc(total_projects), Country, Institution)
print(institution_projects)

# Save detailed summary to CSV
write.csv(institution_projects, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partners_summary.csv", 
          row.names = FALSE)

cat("\nDetailed summary has been saved to 'african_partners_summary.csv'\n")

# Print summary statistics
cat("\nSummary of Leadership Distribution:\n")
cat("================================\n")
print(table(df_africa$leadership))

# Save detailed summary to CSV
leadership_summary <- df_africa %>%
  select(Institution, Country, leadership, total_projects) %>%
  arrange(leadership, desc(total_projects))

write.csv(leadership_summary, 
          "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/Partners/african_partners_leadership_summary.csv", 
          row.names = FALSE)

cat("\nDetailed leadership summary has been saved to 'african_partners_leadership_summary.csv'\n")

# Define project columns for network creation
project_cols <- c("CHAMNHA", "HEAT", "ENBEL", "GHAP", "HAPI", "BioHEAT", "HIGH_Horizons")

# Create edges based on project co-participation
edges_df <- data.frame()
institutions <- unique(df_africa$Institution)
n <- length(institutions)

for(i in 1:(n-1)) {
  for(j in (i+1):n) {
    inst1 <- institutions[i]
    inst2 <- institutions[j]
    
    # Get rows for both institutions
    row1 <- df_africa[df_africa$Institution == inst1, ]
    row2 <- df_africa[df_africa$Institution == inst2, ]
    
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
    df_africa$Gueladio_Cisse == 1 & df_africa$Matthew_Chersich == 1 ~ "Joint Projects",
    df_africa$Gueladio_Cisse == 1 ~ "Gueladio Cisse",
    df_africa$Matthew_Chersich == 1 ~ "Matthew Chersich",
    df_africa$Pilot_Projects == 1 ~ "Pilot Projects",
    TRUE ~ "Other"
  )[match(institutions, df_africa$Institution)]
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
    'Gueladio Cisse' = '#2C85B2',    # Blue
    'Matthew Chersich' = '#B23A48',   # Red
    'Joint Projects' = '#7E4F88',     # Purple
    'Pilot Projects' = '#7E4F88',     # Same purple for pilot projects
    'Other' = '#7E4F88'              # Same purple for other
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
ggsave("african_partnership_network.pdf", 
       plot = network_plot,
       width = 16, 
       height = 16, 
       dpi = 300) 