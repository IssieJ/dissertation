# Plotting osculum length changes with and without error bars
# January 2025

# Import libraries
library(tidyverse)
library(readr)
library(RColorBrewer)

# Import data
all_data <- read_csv("video_datasheets/all_data.csv")

# View dataset
head(all_data)

# Count rows within each camera, create sequence index
all_data <- all_data %>%
  group_by(Camera) %>%
  mutate(Sequence_Index = row_number()) %>% 
  ungroup()

# Plot osculum length against measurement index, with no error
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

# Import Data
all_repeats <- read_csv("video_datasheets/all_repeats.csv")
View(all_repeats)

# Rename data
repeat_data <- all_repeats

# Original means from full dataset
original_means <- all_data %>%
  group_by(Camera, Osculum) %>%
  summarise(mean_length = mean(Length), .groups = "drop")

# Convert Camera columns to factor
original_means <- original_means %>%
  mutate(Camera = as.factor(Camera))

original_means <- original_means %>%
  mutate(Osculum = as.numeric(as.character(Osculum)))

# Merge the repeated measurements with the original dataset
paired_data <- original_means %>%
  inner_join(repeat_data, by = c("Osculum"))

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
    upper_bound = mean_length + margin_of_error)

# Add the error ribbon
plot_data <- all_data %>%
  mutate(
    lower_bound = Length - margin_of_error,
    upper_bound = Length + margin_of_error)

# Plot osculum length against measurement index, with  error
ggplot(plot_data %>% filter(Camera == 1), 
       aes(x = ID, y = Length, color = factor(Osculum))) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, 
                  fill = factor(Osculum)), alpha = 0.2, color = NA) +  
  geom_line() +
  facet_grid(Osculum ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Sponge 1",
       x = "Measurement Index (Continuous)", 
       y = "Osculum Length", color = "Osculum", fill = "Osculum") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),  # boxed legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )
# Log-transform the data and plot the same graphs with this

# Compute log-transformed means
log_means <- all_data %>%
  mutate(Log_Length = log(Length)) %>%  # Apply log transformation
  group_by(Camera, Osculum) %>%
  summarise(mean_log_length = mean(Log_Length), .groups = "drop")

# Compute errors on log-transformed data
paired_log_data <- log_means %>%
  inner_join(all_data %>% mutate(Log_Length = log(Length)), 
             by = c("Camera", "Osculum"))

paired_log_data <- paired_log_data %>%
  mutate(error = abs(mean_log_length - Log_Length))

# Calculate Margin of Error (log-scale)
mean_log_error <- mean(paired_log_data$error)
std_log_error <- sd(paired_log_data$error)
margin_log_error <- 1.96 * std_log_error  # 95% CI

# Apply error margin
plot_log_data <- all_data %>%
  mutate(
    Log_Length = log(Length),
    lower_bound = Log_Length - margin_log_error,
    upper_bound = Log_Length + margin_log_error
  )

# Plot osculum log-transformed length against measurement index, with error
ggplot(plot_log_data %>% filter(Camera == 2),  
       aes(x = ID, y = Log_Length, color = factor(Osculum))) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, 
                  fill = factor(Osculum)), alpha = 0.5, color = NA) + 
  geom_line(color = "black", linewidth = 0.5) +
  facet_grid(Osculum ~ ., scales = "free_y", space = "free_y") +
  scale_color_brewer(palette = "Dark2") +  # for line
  scale_fill_brewer(palette = "Dark2") +   # for ribbon
  labs(
    title = "Sponge 2",
    x = "Measurement Number", 
    y = "Log(Osculum Length)",
    color = "Osculum",
    fill = "Osculum"
  ) +
  theme(
    legend.key = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),  # boxed legend
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )
# change the camera number and graph title for each sponge. 