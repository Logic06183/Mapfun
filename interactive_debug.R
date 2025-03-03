# Load tidyverse
library(tidyverse)

# Read the CSV file
message("Reading CSV...")
df <- read.csv("base.csv", sep = ";", stringsAsFactors = FALSE, 
               fileEncoding = "UTF-8", na.strings = c("", "NA"))
message("CSV read complete")

# Print structure
message("Data structure:")
str(df)

# Check for missing values
message("\nMissing values:")
sapply(df, function(x) sum(is.na(x)))

# Print first few rows
message("\nFirst few rows:")
print(head(df))

# Try to process the data
message("\nProcessing data...")
processed_df <- df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  mutate(across(c(Research, Finance_programmes, Policy, Data_Providers, 
                 `Engagement, Advocacy, and Capacity Building`), 
               ~ifelse(is.na(.), 0, as.numeric(as.character(.))))) %>%
  mutate(
    institution_type = ifelse(Data_Providers == 1, "Data Provider", "Other"),
    has_research = Research == 1,
    has_finance = Finance_programmes == 1,
    has_engagement = `Engagement, Advocacy, and Capacity Building` == 1,
    has_policy = Policy == 1
  )
message("Processing complete")

# Print processed structure
message("\nProcessed data structure:")
str(processed_df)
