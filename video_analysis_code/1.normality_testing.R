# 1: NORMALITY TESTING ON OSCULUM SIZE CHANGE MEASUREMENTS

# January 2025

# install required libraries
install.packages("nortest")

# import required libraries
library(nortest)
library(tidyverse)
library(readxl)
library(car)

# COMBINE TO ONE DATASHEET 
getwd()
# Set your folder path
folder_path <- "video_datasheets"  # this will now be different as path changed. 

# Check the files in the folder
list.files(folder_path)

# Search specifically for .xlsx files
file_paths <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Print the file paths
print(file_paths)

# START NORMALITY TESTING ---------------------------------------------------

# view data
head(all_data)
tail(all_data)

# normality testing with anderson darling tets
ad.test(all_data$Length)

# create histogram for each 
ggplot(all_data, aes(x = Length)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_grid(Osculum ~ Camera) +
  labs(x = "Length", y = "Frequency")

# create q-q plots
ggplot(all_data, aes(sample = Length)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Osculum ~ Camera) +
  theme_minimal() +
  labs()

# log transform
all_data$log_length <- log(all_data$Length)

# histogram single 
hist(all_data$log_length, col = "blue", 
     main = "Histogram of Log-Transformed Length")

# histogram all
ggplot(all_data, aes(x = log_length, fill = Camera)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_grid(Osculum ~ Camera) +  
  labs(x = "Length", y = "Frequency") +
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"))
  
# qq plot
qqnorm(all_data$log_length)
qqline(all_data$log_length, col = "red")

# anderson darling test
ad.test(all_data$log_length)