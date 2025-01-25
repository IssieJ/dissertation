# Summarize data for the ribbon: overall mean, lower bound, and upper bound across all Osculum and Camera
overall_summary <- summary_data2 %>%
  group_by(Osculum, Camera) %>%
  summarize(
    mean_length = mean(mean_length, na.rm = TRUE),
    lower_bound = mean(lower_bound, na.rm = TRUE),
    upper_bound = mean(upper_bound, na.rm = TRUE)
  ) %>%
  mutate(group = interaction(Osculum, Camera))

# Plot with ribbon
ggplot(all_data, aes(x = interaction(Osculum, Camera), y = Length, color = factor(Camera))) +
  geom_boxplot(outlier.color = "red", alpha = 0.5) +
  geom_point(data = summary_data2, aes(y = mean_length), size = 3, shape = 21, fill = "white") +
  geom_ribbon(
    data = overall_summary,
    aes(
      x = as.numeric(group),  # Ensure x matches ribbon data
      ymin = lower_bound,
      ymax = upper_bound,
      group = 1
    ),
    fill = "blue", alpha = 0.2, inherit.aes = FALSE
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Boxplot of Length with Ribbon (Grouped by Osculum and Camera)",
    x = "Osculum and Camera",
    y = "Length",
    color = "Camera"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
