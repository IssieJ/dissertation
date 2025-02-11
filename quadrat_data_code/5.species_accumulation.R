# Load necessary library
library(vegan)

# Load data
data <- read.csv("your_data.csv")

# Extract species data
species_data <- species_accum_curve[, 5:ncol(species_accum_curve)]

# Convert to numeric and handle missing values
species_data <- as.data.frame(lapply(species_data, as.numeric))
species_data[is.na(species_data)] <- 0

# Remove rows/columns with no observations
species_data <- species_data[rowSums(species_data) > 0, colSums(species_data) > 0]

# Verify the structure of the cleaned data
summary(species_data)

# Calculate and plot the species accumulation curve
sac <- specaccum(species_data)
plot(sac, main = "Species Accumulation Curve", xlab = "Quadrats", ylab = "Cumulative Species")
