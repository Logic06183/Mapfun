message("Starting debug script...")

# Step 1: Load required packages
library(tidyverse)
library(sf)
message("Packages loaded")

# Step 2: Read CSV
base_data <- read.csv("base.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
message("CSV loaded")

# Step 3: Filter and create basic columns
base_data <- base_data %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(
    institution_type = ifelse(Data_Providers == 1, "Data Provider", "Other")
  )
message("Basic filtering done")

# Step 4: Create additional columns one by one
base_data$has_research <- base_data$Research == 1
message("Research column added")

base_data$has_finance <- base_data$Finance_programmes == 1
message("Finance column added")

base_data$has_engagement <- base_data$`Engagement, Advocacy, and Capacity Building` == 1
message("Engagement column added")

base_data$has_policy <- base_data$Policy == 1
message("Policy column added")

# Step 5: Convert to SF object
base_sf <- st_as_sf(base_data, coords = c("lon", "lat"), crs = 4326)
message("Converted to SF object")

# Print summary
message("Debug complete. Printing summary:")
print(summary(base_sf))
