# 4: Threshold creation for rate of change graph
# January 2025

# import libraries
library(tidyverse)
library(readr)

#import dataset
all_repeats <- read_csv("video_datasheets/all_repeats.csv")
View(all_repeats)

#rename dataset
repeat_data <- all_repeats

# make osculum and camera a factor
repeat_data <- repeat_data %>%
  mutate(Camera = as.factor(Camera))

repeat_data <- repeat_data %>%
  mutate(Osculum = as.factor((Osculum)))

# Merge original and repeat data by Camera, Osculum, and Frame ID
merged_data <- full_join(all_data, repeat_data, 
                         by = c("Camera", "Osculum", "ID"), 
                         suffix = c("_original", "_repeat"))

# Check where there are missing values (frames missing in either dataset)
missing_rows <- merged_data %>% filter(is.na(Length_original) | is.na(Length_repeat))

# Select frames where both measurements exist, so filter out missing ones
aligned_data <- merged_data %>% drop_na(Length_original, Length_repeat)

# Compute rate of change in repeated measurements
repeat_data <- merged_data %>%
  arrange(Camera, Osculum, ID) %>%
  group_by(Camera, Osculum) %>%
  mutate(rate_of_change_repeat = abs((Length_repeat - lag(Length_repeat)) / (ID - lag(ID)))) %>%
  ungroup()

# check if distribution is normal
ggplot(repeat_data, aes(x = rate_of_change_repeat)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(color = "darkblue", size = 1) +
  facet_wrap(~ Osculum) +
  labs(title = "Distribution of Rate of Change (Repeat Measurements)",
       x = "Rate of Change per Frame",
       y = "Density") +
  theme_minimal()

# Not Normal
# Long tail - a few large rate changes occur, but they are rare.
# This kind of distribution is often seen in measurement noise, 
# where most variation is small and a few are big outliers.

# Compute a robust threshold for rate of change - 9oth percentile
rate_change_thresholds <- repeat_data %>%
  group_by(Camera, Osculum) %>%
  summarise(rate_change_threshold = quantile(rate_of_change_repeat, 0.90, na.rm = TRUE), .groups = "drop") 

# deriv_data has incorrect col names - fix them
deriv_data <- all_data %>%
  group_by(Osculum, Camera) %>%
  arrange(ID) %>%
  mutate(derivative = c(NA, diff(log(Length)) / diff(ID))) %>%
  ungroup()

# Merge with derivative data - join
deriv_data <- deriv_data %>%
  mutate(Camera = as.factor(Camera),
         Osculum = as.factor(Osculum)) %>%
  left_join(rate_change_thresholds, by = c("Camera", "Osculum"))

# Define upper and lower threshold values
deriv_data <- deriv_data %>%
  mutate(upper_threshold = rate_change_threshold,
         lower_threshold = -rate_change_threshold)

# Plot with new corrected thresholds
ggplot(deriv_data, aes(x = ID, y = derivative, color = as.factor(Osculum))) +
  geom_line(linewidth = 0.4) +
  
  # Add corrected threshold lines
  geom_hline(aes(yintercept = upper_threshold), linetype = "dashed", size = 0.6, color = "black") +
  geom_hline(aes(yintercept = lower_threshold), linetype = "dashed", size = 0.6, color = "black") +
  
  facet_grid(Osculum ~ Camera, scales = "free_x") +  
  labs(y = "Change in Length per Measured Frame", 
       x = "Measurement Number",
       color = "Osculum") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = NA),  
    legend.title = element_text("Osculum"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black"))