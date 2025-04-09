library(tidyverse)

# Log-transform the Length variable
all_data <- all_data %>%
  mutate(log_Length = log(Length))  # Use log10(Length) for base-10 log

# Create a continuous sequence index
all_data <- all_data %>%
  group_by(Camera) %>%
  mutate(Sequence_Index = row_number()) %>%  
  ungroup()

# Plot log-transformed data
ggplot(all_data, aes(x = Sequence_Index, y = log_Length, 
                     group = interaction(Camera, Osculum), color = factor(Osculum))) +
  geom_line(alpha = 0.7) +  
  facet_wrap(~ Camera, scales = "free_x") +  
  labs(x = "Measurement Index (Continuous)", y = "Log(Osculum Length)", color = "Osculum") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = NA),  
    legend.title = element_text("Osculum"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )

# Log-transform the repeated measurements data
repeat_data <- all_repeats %>%
  mutate(log_Length = log(Length))  # Use log10(Length) for base-10 log

# Check data types
str(original_means)
str(repeat_data)

# Calculate original means from FULL dataset (log-transformed)
original_means <- all_data %>%
  group_by(Camera, Osculum) %>%
  summarise(mean_log_Length = mean(log_Length), .groups = "drop")

# Convert Camera columns to the same type (e.g., both to factor)
original_means <- original_means %>%
  mutate(Camera = as.factor(Camera))

original_means <- original_means %>%
  mutate(Osculum = as.numeric(as.character(Osculum)))

repeat_data <- repeat_data %>%
  mutate(Camera = as.factor(Camera))

# Merge the repeated measurements with the original dataset
paired_data <- original_means %>%
  inner_join(repeat_data, by = c("Camera", "Osculum"))

# Calculate absolute differences (errors) for log-transformed data
paired_data <- paired_data %>%
  mutate(error = abs(mean_log_Length - log_Length))

# Compute margin of error
mean_error <- mean(paired_data$error)  # Mean absolute error
std_error <- sd(paired_data$error)     # Standard deviation of error
margin_of_error <- 1.96 * std_error    # 95% confidence margin of error

# Add margin of error to the original means for the full dataset
plot_data <- original_means %>%
  mutate(
    lower_bound = mean_log_Length - margin_of_error,
    upper_bound = mean_log_Length + margin_of_error
  )

# Plot the full dataset with error ribbon (log-transformed)
plot_data <- all_data %>%
  mutate(
    lower_bound = log_Length - margin_of_error,
    upper_bound = log_Length + margin_of_error
  )

# Plot with error shading (log-transformed)
ggplot(plot_data, aes(x = Sequence_Index, y = log_Length, 
                      group = interaction(Camera, Osculum), color = factor(Osculum))) +  
  geom_line(alpha = 0.7) +  
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = factor(Osculum)), alpha = 0.2) +  
  geom_line(color = "black", alpha = 0.8, size = 0.05) +
  facet_wrap(~ Camera, scales = "free_x") +  
  labs(x = "Measurement Index (Continuous)", y = "Log(Osculum Length)", color = "Osculum", fill = "Osculum") +
  theme(legend.key = element_blank(),  
        legend.background = element_rect(fill = NA),  
        legend.title = element_text("Osculum"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black")
  )


