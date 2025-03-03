## error estimate with repeated measurements. 

library(tidyverse)

# combine all datasets into "all_repeats"

# set wd
setwd("C:/Users/issie/OneDrive - University of Edinburgh/uni year 4/dissertation")

# import repeat data and original data

repeat_data <- all_repeats

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
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "darkgrey", alpha = 0.2) +
  labs(
    x = "Camera",
    y = "Mean Oscula Length", 
  ) +
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


# plot onto new graph

# new graph no error
ggplot(all_data, aes(x = Sequence_Index, y = Length, 
                     group = interaction(Camera, Osculum), color = factor(Osculum))) +
  geom_line(alpha = 0.7) +  
  facet_wrap(~ Camera, scales = "free_x") +  
  labs(x = "Measurement Index (Continuous)", y = "Osculum Length", color = "Osculum") +
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


# Plot with error shading
plot_data <- all_data %>%
  mutate(
    lower_bound = Length - margin_of_error,
    upper_bound = Length + margin_of_error
  )


# Plot with error shading
ggplot(plot_data, aes(x = Sequence_Index, y = Length, 
                      group = interaction(Camera, Osculum), color = factor(Osculum))) +
  geom_line(alpha = 0.7) +  
  facet_wrap(~ Camera, scales = "free_x") +  
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "darkgrey", alpha = 0.2) +  
  labs(x = "Measurement Index (Continuous)", y = "Osculum Length", color = "Osculum") +
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
  
  