## error estimate with repeated measurements. 

library(tidyverse)

# combine all datasets into "all_repeats"

# set wd
setwd("C:/Users/issie/OneDrive - University of Edinburgh/uni year 4/dissertation/measurements_2")

# Read the repeated measurements
repeat_data <- read_csv("all_repeats.csv") # Adjust to your file name

# original means from FULL dataset
original_means <- all_data %>%
  group_by(Camera, Osculum) %>%
  summarise(mean_length = mean(Length), .groups = "drop")

# Merge the repeated measurements with the original dataset
paired_data <- original_means %>%
  inner_join(repeat_data, by = c("Camera", "Osculum"))

# Calculate absolute differences (errors)
paired_data <- paired_data %>%
  mutate(error = abs(mean_length - Length))

# Compute margin of error
mean_error <- mean(paired_data$error)  # Mean absolute error
std_error <- sd(paired_data$error)     # Standard deviation of error
margin_of_error <- 1.96 * std_error    # 95% confidence margin of error

# Add margin of error to the original means for the full dataset
plot_data <- original_means %>%
  mutate(
    lower_bound = mean_length - margin_of_error,
    upper_bound = mean_length + margin_of_error
  )

# Plot the full dataset with error ribbon
ggplot(plot_data, aes(x = factor(Camera), y = mean_length, group = Osculum)) +
  geom_line(aes(color = factor(Osculum))) +
  geom_point(aes(color = factor(Osculum))) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "blue", alpha = 0.2) +
  labs(
    title = "Oscula Length with Margin of Error",
    x = "Camera",
    y = "Mean Oscula Length"
  ) +
  theme_minimal()
