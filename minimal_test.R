message("Starting test...")

# Basic CSV read
data <- read.csv("base.csv", sep = ";")
message("CSV read complete")

# Print structure
str(data)
message("Structure printed")

# Print first few rows
print(head(data))
message("Head printed")
