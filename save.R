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

# Create partners dataset
partners_data <- tribble(
  ~Institution, ~City, ~Country, ~CHAMNHA, ~HEAT, ~HIGH, ~ENBEL, ~GHAP, ~HAPI, ~BioHEAT, ~HIGH_Horizons,
  "Wits Planetary Health Research", "Johannesburg", "South Africa", 1, 1, 1, 1, 1, 1, 1, 0,
  "Wits Reproductive Health Institute", "Johannesburg", "South Africa", 1, 1, 1, 1, 1, 1, 1, 0,
  "Wits Health Consortium", "Johannesburg", "South Africa", 1, 1, 1, 1, 1, 1, 1, 1,
  "University of Cape Town", "Cape Town", "South Africa", 0, 1, 0, 0, 1, 0, 0, 0,
  "IBM Research Africa", "Johannesburg", "South Africa", 0, 1, 0, 0, 1, 0, 0, 0,
  "South African Medical Research Council", "Pretoria", "South Africa", 1, 0, 0, 0, 1, 0, 0, 0,
  "South African Medical Research Council", "Cape Town", "South Africa", 1, 0, 0, 0, 1, 0, 0, 0,
  "University of Pretoria", "Pretoria", "South Africa", 0, 1, 0, 0, 1, 0, 0, 0,
  "University of the Witwatersrand", "Johannesburg", "South Africa", 0, 0, 0, 1, 0, 1, 1, 0,
  "Stellenbosch University", "Stellenbosch", "South Africa", 0, 0, 0, 0, 0, 0, 1, 0,
  "CeSHHAR", "Harare", "Zimbabwe", 1, 1, 1, 1, 1, 1, 0, 1,
  "KEMRI-Wellcome Trust", "Kilifi", "Kenya", 1, 0, 1, 0, 0, 0, 0, 1,
  "Aga Khan University", "Nairobi", "Kenya", 1, 0, 1, 1, 1, 0, 0, 0,
  "Institut de Recherche en Sciences de la Santé", "Ouagadougou", "Burkina Faso", 1, 0, 0, 0, 1, 0, 0, 0,
  "Higher Institute of Public Health", "Ouagadougou", "Burkina Faso", 0, 1, 0, 0, 0, 0, 0, 0,
  "Institute of Public Health", "Bujumbura", "Burundi", 0, 1, 0, 0, 0, 0, 0, 0,
  "University Peleforo Gon Coulibaly", "Korhogo", "Côte d'Ivoire", 0, 1, 0, 0, 1, 0, 0, 0,
  "Felix Houphouët Boigny University", "Abidjan", "Côte d'Ivoire", 0, 1, 0, 0, 0, 0, 0, 0,
  "Nangui Abrogoua University", "Abidjan", "Côte d'Ivoire", 0, 1, 0, 0, 0, 0, 0, 0,
  "Centre Suisse de Recherches Scientifiques", "Abidjan", "Côte d'Ivoire", 0, 1, 0, 0, 0, 0, 0, 0,
  "Institute of Public Health", "Nouakchott", "Mauritania", 0, 1, 0, 0, 0, 0, 0, 0,
  "Makerere University", "Kampala", "Uganda", 0, 1, 0, 0, 0, 0, 0, 0,
  "Medical School of Rwanda", "Kigali", "Rwanda", 0, 1, 0, 0, 0, 0, 0, 0,
  "Ziguinchor University", "Ziguinchor", "Senegal", 0, 1, 0, 0, 0, 0, 0, 0,
  "IRESSEF", "Dakar", "Senegal", 0, 1, 0, 0, 0, 0, 0, 0,
  "Cheikh Anta Diop University", "Dakar", "Senegal", 0, 1, 0, 0, 0, 0, 0, 0,
  "International Health Support Centre", "N'Djamena", "Chad", 0, 1, 0, 0, 0, 0, 0, 0,
  "University of Lome", "Lome", "Togo", 0, 1, 0, 0, 0, 0, 0, 0,
  "University of Botswana", "Gaborone", "Botswana", 0, 0, 0, 1, 1, 0, 0, 0,
  "Muhimbili University", "Dar es Salaam", "Tanzania", 0, 0, 1, 0, 0, 0, 0, 0,
  "Federal University of Technology", "Akure", "Nigeria", 0, 1, 0, 0, 0, 0, 0, 0,
  "University of Yaoundé", "Yaoundé", "Cameroon", 0, 1, 0, 0, 0, 0, 0, 0,
  "Midlands State University", "Gweru", "Zimbabwe", 0, 1, 0, 0, 0, 0, 0, 0
) %>%
  mutate(
    total_projects = rowSums(across(CHAMNHA:HIGH_Horizons)),
    type = if_else(total_projects > 1, "Multiple Projects", "Single Project")
  )

# Add coordinates for each city
cities_coords <- tribble(
  ~City, ~lat, ~lon,
  "Johannesburg", -26.2041, 28.0473,
  "Cape Town", -33.9249, 18.4241,
  "Pretoria", -25.7449, 28.1879,
  "Stellenbosch", -33.9321, 18.8602,
  "Harare", -17.8252, 31.0335,
  "Kilifi", -3.6307, 39.8499,
  "Nairobi", -1.2921, 36.8219,
  "Ouagadougou", 12.3714, -1.5197,
  "Bujumbura", -3.3822, 29.3644,
  "Korhogo", 9.4557, -5.6290,
  "Abidjan", 5.3600, -4.0083,
  "Nouakchott", 18.0735, -15.9582,
  "Kampala", 0.3476, 32.5825,
  "Kigali", -1.9441, 30.0619,
  "Ziguinchor", 12.5665, -16.2733,
  "Dakar", 14.7167, -17.4677,
  "N'Djamena", 12.1348, 15.0557,
  "Lome", 6.1375, 1.2123,
  "Gaborone", -24.6282, 25.9231,
  "Dar es Salaam", -6.7924, 39.2083,
  "Akure", 7.2571, 5.2058,
  "Yaoundé", 3.8480, 11.5021,
  "Gweru", -19.4605, 29.8321
)

# Join coordinates with partners data and convert to sf
partners_sf <- partners_data %>%
  left_join(cities_coords, by = "City") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Create the map
ggplot() +
  # Base layer - African continent with HE²AT countries highlighted
  geom_sf(data = africa %>% 
            mutate(is_heat = name %in% heat_countries,
                   category = if_else(is_heat, "HE²AT Contributing Partners", "Other Countries")), 
          aes(fill = category),
          color = "gray80", 
          size = 0.2) +
  
  # Add graticules
  geom_sf(data = st_graticule(lat = seq(-40, 40, 10), lon = seq(-20, 50, 10), crs = 4326),
          color = alpha("gray70", 0.2),
          size = 0.1) +
  
  # Add partner institution points
  geom_sf(data = partners_sf,
          aes(size = total_projects,
              color = type),
          alpha = 0.9,
          stroke = 0.5) +
  
  # Add point halos
  geom_sf(data = partners_sf,
          aes(size = total_projects),
          color = "white",
          fill = NA,
          stroke = 1.2,
          alpha = 0.5) +
  
  # Add institution labels
  geom_text_repel(
    data = partners_sf,
    aes(label = Institution,
        geometry = geometry,
        color = type),
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
  
  # Customize colors and legend
  scale_fill_manual(
    values = c(
      "HE²AT Contributing Partners" = alpha("#FFE0B2", 0.7),  # Lighter yellow fill
      "Other Countries" = "#FAFAFA"    # This will be in legend but not shown
    ),
    breaks = c("HE²AT Contributing Partners"),  # Only show this in legend
    name = NULL  # Remove legend title
  ) +
  
  # Add borders for HE²AT countries
  geom_sf(data = africa %>% 
            filter(name %in% heat_countries),
          fill = NA,
          color = "#FFA726",  # Darker yellow border
          size = 0.4) +
  
  scale_color_manual(
    values = c(
      "Multiple Projects" = "#D81B60",
      "Single Project" = "#1E88E5"
    ),
    name = "Project Type"
  ) +
  
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
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = alpha("#F0F8FF", 0.2), color = NA),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
    axis.text = element_text(size = 8, color = "#555555"),
    axis.title = element_blank()
  ) +
  
  # Set map extent to focus on Africa
  coord_sf(
    xlim = c(-20, 50),
    ylim = c(-40, 40),
    expand = FALSE
  ) +
  
  # Add titles
  labs(
    title = "African Research Partners Network",
    subtitle = "Heat and Health Research Projects",
    caption = "Data: Research Collaboration Network | Map: 2025"
  )

# Save outputs
ggsave("african_partners_map_2025.pdf", width = 14, height = 14, dpi = 350)
ggsave("african_partners_map_2025.png", width = 14, height = 14, dpi = 350)