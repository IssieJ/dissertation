## NORMALITY TESTING ON OSCULUM SIZE CHANGE MEASUREMENTS


library(tidyverse)
library(readxl)
library(car)

# COMBINE TO ONE DATASHEET -------------------------------------------------

# Set your folder path
folder_path <- "datasheets"

# Check the files in the folder
list.files(folder_path)

# Search specifically for .xlsx files
file_paths <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Print the file paths
print(file_paths)

# Combine all files into one dataframe
combined_data <- do.call(rbind, lapply(file_paths, function(file) {
  data <- read_excel(file)  # Read each Excel file
  
  # Add metadata columns based on file name
  data$Osculum <- gsub(".*osc(\\d+).*", "\\1", file)  # Extract osculum number from filename
  data$Camera <- gsub(".*cam(\\d+).*", "\\1", file)   # Extract camera number from filename
  
  return(data)
}))

# View the combined dataset
head(combined_data)

# Check the structure
str(combined_data)

write.csv(combined_data, "combined_data.csv", row.names = FALSE)


# START NORMALITY TESTING ---------------------------------------------------

# using all_data instead of combined_data because it has continuous ID rather 
# repeating each time the osc changes. 

head(all_data)
tail(all_data)

# Run Shapiro-Wilk test for each osculum-camera combination
normality_results <- all_data %>%
  group_by(Osculum, Camera) %>%
  summarise(p_value = shapiro.test(Length)$p.value)


leveneTest(Length ~ Sheet, data = all_data)


# View results
print(normality_results)

# create histogram for each 
ggplot(all_data, aes(x = Length)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_grid(Osculum ~ Camera) +  # Create a grid of histograms for each osculum and camera
  theme_minimal() +
  labs(title = "Histograms of Length by Osculum and Camera", x = "Length", y = "Frequency")

# create q-q plots
ggplot(all_data, aes(sample = Length)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Osculum ~ Camera) +
  theme_minimal() +
  labs(title = "Q-Q Plots of Length by Osculum and Camera")
