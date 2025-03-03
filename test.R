library(tidyverse)

# Test CSV reading
file_path <- "C:/Users/CraigParker/OneDrive - Wits Health Consortium/PHR PC/Desktop/leaflet_map/Mapfun/base.csv"
print(paste("File exists:", file.exists(file_path)))

data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
print("Data loaded successfully")
print(head(data))
