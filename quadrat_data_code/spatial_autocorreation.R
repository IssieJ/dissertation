## SPATIAL AUTOCORRELATION TESTING

install.packages(c("sp", "spdep", "dplyr"))
library(sp)
library(spdep)
library(tidyverse)

# Split the 'coordinates' column into 'latitude' and 'longitude'
coords_only <- coords_only %>%
  separate(coordinates, into = c("latitude", "longitude"), sep = ",", convert = TRUE)

# Create a spatial weights matrix based on distance
y_matrix <- as.matrix(coords_only[, c("longitude", "latitude")])
x <- c(55.89444, -2.13089)  # Replace with your reference point

distances <- spDistsN1(y_matrix, x, longlat = TRUE)

dists <- spDists(coords_only, longlat = TRUE)  # Distance matrix in kilometers
threshold <- 10  # Define a threshold distance (e.g., 10 km)
weights <- dists <= threshold  # Binary matrix (TRUE if within threshold)
