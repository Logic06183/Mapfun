# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tmap, sf, rnaturalearth, rnaturalearthdata, dplyr)

# Create study countries data
study_countries <- data.frame(
  name = c(
    # National Registry
    "Austria", "Belgium", "Greece", "Sweden",
    # Sub-national database
    "Italy", "South Africa",
    # Hospital data
    "Benin", "Malawi", "Tanzania", "Uganda", "Uruguay",
    "Argentina", "Bolivia", "Ecuador", "Guatemala",
    "Honduras", "Dominican Republic"
  ),
  registry_type = c(
    rep("National Registry", 4),
    rep("Sub-national database", 2),
    rep("Hospital data", 11)
  )
)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join study countries with world map
world_study <- world %>%
  left_join(study_countries, by = c("name_long" = "name"))

# Set tmap mode to plot
tmap_mode("plot")

# Create subset for labels
label_countries <- world_study %>%
  filter(!is.na(registry_type))

# Create the map
tm <- tm_shape(world_study,
               bbox = c(-100, -60, 50, 70)) +
  tm_borders(col = "#666666", lwd = 0.3) +
  tm_fill(
    col = "registry_type",
    palette = c(
      "National Registry" = "#4575b4",
      "Sub-national database" = "#74add1",
      "Hospital data" = "#abd9e9"
    ),
    title = "Registry Type",
    alpha = 0.8,
    id = "name",
    na.color = "#f5f5f5"
  ) +
  tm_shape(label_countries) +
  tm_text(
    "name_long", 
    size = 0.7,
    col = "#2b2b2b",
    fontface = "plain",
    auto.placement = TRUE,
    remove.overlap = TRUE,
    along.lines = FALSE,
    just = "left",
    xmod = 0.5,
    ymod = 0.5
  ) +
  tm_layout(
    frame = TRUE,
    frame.lwd = 0.5,
    bg.color = "white",
    inner.margins = c(0.02, 0.02, 0.02, 0.12),
    outer.margins = 0,
    legend.position = c("right", "bottom"),
    legend.text.size = 0.5,
    legend.title.size = 0.6,
    legend.frame = FALSE,
    legend.bg.color = "white",
    legend.bg.alpha = 1,
    asp = 0.6,
    legend.outside = TRUE,
    legend.outside.size = 0.15
  ) +
  tm_scale_bar(
    position = c("left", "bottom"),
    text.size = 0.4,
    width = 0.15,
    text.color = "#2b2b2b"
  ) +
  tm_compass(
    position = c("left", "top"),
    size = 1.2,
    type = "8star",
    color.light = "#2b2b2b"
  )

# Create locations data for specific points
locations <- data.frame(
  name = c("Lazio", "Soweto"),
  lon = c(12.5, 27.9),
  lat = c(41.9, -26.2),
  type = c("Sub-national database", "Sub-national database")
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Add locations to the map
tm <- tm +
  tm_shape(locations) +
  tm_dots(
    size = 0.12,
    col = "#d73027",
    shape = 21,
    border.col = "white",
    border.lwd = 0.5
  ) +
  tm_text(
    "name",
    size = 0.5,
    col = "#2b2b2b",
    fontface = "plain",
    auto.placement = TRUE,
    just = "left",
    xmod = 0.4
  )

# Save high resolution PDF for publication
tmap_save(tm, 
          filename = "HEAT_study_map_scientific.pdf", 
          width = 10,
          height = 8,
          units = "in", 
          dpi = 600)

# Save PNG version for presentations
tmap_save(tm, 
          filename = "HEAT_study_map_scientific.png", 
          width = 10,
          height = 8,
          units = "in", 
          dpi = 300)