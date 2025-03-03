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

# Get Africa continent data
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
africa_borders <- ne_countries(scale = "medium", returnclass = "sf")

# Define all HE²AT study countries
heat_countries <- c(
  # Core study countries
  "South Africa", "Zimbabwe", "Nigeria", "Ghana", 
  "Cameroon", "Benin", "Burkina Faso", "Côte d'Ivoire",
  # Additional study countries
  "Mali", "Ethiopia", "Kenya", "Uganda", "Tanzania", 
  "Malawi", "Mozambique", "Zambia", "Rwanda", "Botswana",
  "Democratic Republic of the Congo", "Sierra Leone", "Lesotho", "Senegal",
  "Namibia"
)

# Create color scheme for different project types
project_colors <- c(
  "CHAMNHA" = "#1E88E5",       # Strong blue
  "HE²AT" = "#00897B",         # Teal green
  "Multiple Projects" = "#D81B60",  # Magenta
  "HIGH" = "#FF7043",          # Orange
  "ENBEL" = "#7B1FA2",         # Purple
  "GHAP" = "#FFC107"           # Amber yellow
)

# Simplify the partners_data structure
partners_data <- tribble(
  ~institution, ~city, ~country, ~lat, ~lon, ~projects,
  
  # South Africa Partners
  "Wits Reproductive Health Institute", "Johannesburg", "South Africa", -26.2041, 28.0473, 
  c("CHAMNHA", "HE²AT", "HIGH", "ENBEL", "GHAP"),
  "University of Cape Town", "Cape Town", "South Africa", -33.9249, 18.4241, 
  c("HE²AT", "GHAP"),
  "IBM Research Africa", "Johannesburg", "South Africa", -26.1715, 28.0328, 
  c("HE²AT", "GHAP"),
  "South African Medical Research Council", "Pretoria", "South Africa", -25.7449, 28.1879, 
  c("CHAMNHA", "GHAP"),
  "South African Medical Research Council", "Cape Town", "South Africa", -33.9249, 18.4241, 
  c("CHAMNHA", "GHAP"),
  "University of Pretoria", "Pretoria", "South Africa", -25.7545, 28.2314, 
  c("HE²AT", "GHAP"),
  
  # Zimbabwe Partners
  "CeSHHAR", "Harare", "Zimbabwe", -17.8252, 31.0335, 
  c("CHAMNHA", "HE²AT", "HIGH", "ENBEL", "GHAP"),
  
  # Kenya Partners
  "KEMRI-Wellcome Trust", "Kilifi", "Kenya", -3.6307, 39.8499, 
  c("CHAMNHA", "HIGH"),
  "Aga Khan University", "Nairobi", "Kenya", -1.2921, 36.8219, 
  c("CHAMNHA", "HIGH", "ENBEL", "GHAP"),
  
  # Burkina Faso Partners
  "Institut de Recherche en Sciences de la Santé", "Ouagadougou", "Burkina Faso", 12.3714, -1.5197, 
  c("CHAMNHA", "GHAP"),
  
  # Côte d'Ivoire Partners
  "University Peleforo Gon Coulibaly", "Korhogo", "Côte d'Ivoire", 9.4557, -5.6290, 
  c("HE²AT", "GHAP"),
  "Felix Houphouët Boigny University", "Abidjan", "Côte d'Ivoire", 5.3484, -4.0147, 
  c("HE²AT"),
  "Nangui Abrogoua University", "Abidjan", "Côte d'Ivoire", 5.3828, -4.0197, 
  c("HE²AT"),
  "Centre Suisse de Recherches Scientifiques", "Abidjan", "Côte d'Ivoire", 5.3600, -4.0083, 
  c("HE²AT"),
  
  # Botswana Partners
  "University of Botswana", "Gaborone", "Botswana", -24.6282, 25.9231, 
  c("ENBEL", "GHAP"),
  
  # Tanzania Partners
  "Muhimbili University", "Dar es Salaam", "Tanzania", -6.7924, 39.2083, 
  c("HIGH")
)

# Process the data
partners_processed <- partners_data %>%
  mutate(
    project_count = sapply(projects, length),
    type = ifelse(project_count > 1, "Multiple Projects", projects[[1]]),
    short_name = case_when(
      str_detect(institution, "Wits") ~ "Wits RHI",
      str_detect(institution, "Institut de Recherche") ~ "IRSS",
      str_detect(institution, "CeSHHAR") ~ "CeSHHAR",
      str_detect(institution, "Cape Town") ~ "UCT",
      str_detect(institution, "Peleforo") ~ "UPGC",
      str_detect(institution, "IBM") ~ "IBM Research",
      str_detect(institution, "KEMRI") ~ "KEMRI-WTRP",
      str_detect(institution, "Medical Research Council") ~ "SAMRC",
      str_detect(institution, "Felix Houphouët") ~ "UFHB",
      str_detect(institution, "Nangui") ~ "UNA",
      str_detect(institution, "Centre Suisse") ~ "CSRS",
      str_detect(institution, "Aga Khan") ~ "AKU",
      str_detect(institution, "University of Botswana") ~ "UB",
      str_detect(institution, "Muhimbili") ~ "MUHAS",
      TRUE ~ institution
    )
  )

# Convert to sf object
partners_sf <- partners_processed %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Get countries with partners
countries_with_partners <- unique(partners_data$country)

# Prepare highlighted countries
highlighted_countries <- africa %>% 
  filter(name %in% heat_countries) %>%
  mutate(category = "Research Contributing Countries")

# Create the map
ggplot() +
  # Base layer - world boundaries
  geom_sf(data = africa_borders,
          fill = "#F5F5F5",
          color = "gray90",
          size = 0.1) +
  
  # African continent
  geom_sf(data = africa, 
          fill = "#FAFAFA", 
          color = "gray80", 
          size = 0.2) +
  
  # Add graticules
  geom_sf(data = st_graticule(lat = seq(-30, 30, 10), lon = seq(-20, 40, 10), crs = 4326),
          color = alpha("gray70", 0.3),
          size = 0.1) +
  
  # Add study countries
  geom_sf(data = highlighted_countries,
          fill = "#FFA726",
          alpha = 0.3,
          color = "#666666",
          size = 0.5) +
  
  # Add partner institution points
  geom_sf(data = partners_sf,
          aes(color = type,
              size = project_count),
          alpha = 0.9,
          stroke = 0.5) +
  
  # Add point halos
  geom_sf(data = partners_sf,
          aes(size = project_count),
          color = "white",
          fill = NA,
          stroke = 1.2,
          alpha = 0.5) +
  
  # Add country labels
  geom_sf_text(
    data = africa %>% 
      filter(name %in% countries_with_partners) %>%
      st_centroid(),
    aes(label = name),
    size = 2.5,
    fontface = "italic",
    color = alpha("#424242", 0.7),
    check_overlap = TRUE
  ) +
  
  # Add institution labels
  geom_text_repel(
    data = partners_sf,
    aes(label = short_name,
        geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    fontface = "bold",
    bg.color = alpha("white", 0.7),
    bg.r = 0.1,
    box.padding = 0.6,
    point.padding = 0.4,
    min.segment.length = 0.2,
    force = 3,
    max.overlaps = 20,
    seed = 42
  ) +
  
  # Set colors
  scale_color_manual(
    values = project_colors,
    name = "Project Type"
  ) +
  
  # Set point sizes
  scale_size_continuous(
    name = "Number of\nProjects",
    range = c(3, 9),
    breaks = c(1, 2, 3, 4, 5),
    labels = c("1", "2", "3", "4", "5")
  ) +
  
  # Theme customization
  theme_minimal() +
  theme(
    text = element_text(family = "opensans"),
    plot.title = element_text(family = "montserrat", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "montserrat", size = 14, color = "#555555", hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_text(family = "opensans", size = 9, color = "#666666", margin = margin(t = 15)),
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(l = 10),
    legend.title = element_text(family = "montserrat", face = "bold", size = 10),
    legend.text = element_text(family = "opensans", size = 9),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = alpha("#F0F8FF", 0.2), color = NA),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
    axis.text = element_text(size = 8, color = "#555555"),
    axis.title = element_blank()
  ) +
  
  # Set map extent
  coord_sf(
    xlim = c(-20, 50),
    ylim = c(-40, 40),
    expand = FALSE
  ) +
  
  # Add titles
  labs(
    title = "African Research Partners Network",
    subtitle = "GHAP and Related Research Projects",
    caption = "Data: GHAP Research Collaboration Network | Map: 2025"
  )

# Save outputs
ggsave("african_partners_map_ghap_2025.pdf", 
       width = 14, 
       height = 14,
       dpi = 350)

ggsave("african_partners_map_ghap_2025.png", 
       width = 14, 
       height = 14,
       dpi = 350)