library(readxl)
library(tidyverse)


# load dataset with coords
view(all_sections)


# testing for spatial autocorrelation

install.packages(c("sp", "spdep", "dplyr"))
library(sp)
library(spdep)
library(dplyr)

# Example: Create a spatial weights matrix based on distance
coordinates <- cbind(data$longitude, data$latitude)
dists <- spDists(coordinates, longlat = TRUE)  # Distance matrix in kilometers
threshold <- 10  # Define a threshold distance (e.g., 10 km)
weights <- dists <= threshold  # Binary matrix (TRUE if within threshold)

