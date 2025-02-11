library(ggplot2)
library(dplyr)

repeat_data <- all_repeats # Adjust to your file name

# original means from FULL dataset
original_means <- all_data %>%
  group_by(Camera, Osculum) %>%
  summarise(mean_length = mean(Length), .groups = "drop")

# Merge the repeated measurements with the original dataset
paired_data <- original_means %>%
  inner_join(all_repeats, by = c("Camera", "Osculum"))

# Calculate absolute differences (errors)
paired_data <- paired_data %>%
  mutate(error = abs(mean_length - Length))

# Compute margin of error
mean_error <- mean(paired_data$error)  # Mean absolute error
std_error <- sd(paired_data$error)     # Standard deviation of error
margin_of_error <- 1.96 * std_error    # 95% confidence margin of error

# Camera and osculum
all_data <- all_data %>%
  mutate(Osculum_Camera = interaction(Osculum, Camera))

# Summarize full dataset by Osculum and Recording
summary_data2 <- all_data %>%
  group_by(Osculum, Camera) %>%
  summarise(
    mean_length = mean(length, na.rm = TRUE),
    lower_bound = mean_length - margin_of_error,
    upper_bound = mean_length + margin_of_error,
    .groups = "drop"
  )

# Ensure grouping and summary calculation
summary_data2 <- all_data %>%
  group_by(Osculum, Camera) %>%
  summarise(
    mean_length = mean(Length, na.rm = TRUE),  # Calculate mean
    sd_length = sd(Length, na.rm = TRUE),      # Calculate standard deviation
    n = n(),                                   # Count number of measurements
    .groups = "drop"
  ) %>%
  mutate(
    lower_bound = mean_length - 1.96 * (sd_length / sqrt(n)),  # 95% CI Lower
    upper_bound = mean_length + 1.96 * (sd_length / sqrt(n))   # 95% CI Upper
  )

ggplot(all_data, aes(x = interaction(Osculum, Camera), y = Length, color = factor(Camera))) +
  geom_boxplot(outlier.color = "red", alpha = 0.5) +
  geom_point(data = summary_data2, aes(y = mean_length), size = 3, shape = 21, fill = "white") +
  geom_errorbar(
    data = summary_data2,
    aes(y = mean_length, ymin = lower_bound, ymax = upper_bound),  # Correct columns here
    width = 0.2,
    size = 1.2,
    color = "black"
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Osculum and Camera",
    y = "Length",
    color = "Camera"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
