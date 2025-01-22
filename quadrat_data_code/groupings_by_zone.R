## GROUP ALL DATA BY ZONE - FORGET SECTION

library(tidyverse)

# Summarising data by zone
groupedzone <- all_sections %>%
  group_by(zone) %>%
  summarize(
    mean_percent_cover_aerial = mean(percent_cover_aerial, na.rm = TRUE),
    mean_percent_cover_crosssec = mean(percent_cover_crosssec, na.rm = TRUE),
    mean_total_percent_sponge_cover = mean(total_percent_sponge_cover, na.rm = TRUE),
    mean_percent_cover_veg = mean(percent_cover_veg, na.rm = TRUE),
    mean_percent_cover_barnacle = mean(percent_cover_barnacle, na.rm = TRUE)
  )

# create graphs

# mean total sponge cover (%)
ggplot(groupedzone, aes(x = zone, y = mean_total_percent_sponge_cover, fill = zone)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean Total Sponge Cover (%)", x = "Zone", y = "Mean Percent Cover")

# total sponge cover (%)
ggplot(allsections_data, aes(x = zone, y = total_percent_sponge_cover, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  theme_minimal() +
  labs(
    title = "Total Sponge Cover by Zone",
    x = "Zone",
    y = "Total Percent Sponge Cover"
  )

# total veg cover
ggplot(all_sections, aes(x = zone, y = percent_cover_veg, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  theme_minimal() +
  labs(
    title = "Total Vegetation Cover by Zone",
    x = "Zone",
    y = "Percent Vegetation Cover"
  )

# barnacle cover by zone
ggplot(allsections_data, aes(x = zone, y = percent_cover_barnacle, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  theme_minimal() +
  labs(
    title = "Total Barnacle Cover by Zone",
    x = "Zone",
    y = "Percent Barnacle Cover"
  )

# sponge and veg cover on same plot
ggplot(allsections_data, aes(x = zone)) +
  geom_bar(aes(y = total_percent_sponge_cover, fill = "Sponge Cover"), stat = "identity", position = "dodge", alpha = 0.7) +
    geom_point(aes(y = percent_cover_veg / 10, color = "Vegetation Cover"), size = 2) +
  scale_y_continuous(
    name = "Sponge Cover (%)",
    sec.axis = sec_axis(~ . * 10, name = "Vegetation Cover (%)")  # Reverse scaling for vegetation
  ) +
  scale_fill_manual(values = "blue", name = "") +
  scale_color_manual(values = "darkgreen", name = "") +
  theme_minimal() +
  labs(
    title = "Sponge and Vegetation Cover by Zone",
    x = "Zone"
  ) +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "darkgreen")
  )
geom_point(aes(y = percent_cover_veg / 10, color = "Vegetation Cover"), size = 2)

# ------------ ANALYSIS OF DISTRIBUTIONS ------------------

# Calculate correlation within each zone
all_sections %>%
  group_by(zone) %>%
  summarise(correlation = cor(percent_cover_veg, total_percent_sponge_cover, use = "complete.obs"))

ggplot(all_sections, aes(x = percent_cover_veg, y = total_percent_sponge_cover, color = zone)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship Between Vegetation and Sponge Cover",
       x = "Vegetation Cover",
       y = "Sponge Cover")

# zone specific analysis: lower
lower_zone <- filter(all_sections, zone == "lower")
lm_lower <- lm(total_percent_sponge_cover ~ percent_cover_veg, data = lower_zone)
summary(lm_lower)

# middle
middle_zone <- filter(all_sections, zone == "middle")
lm_middle <- lm(total_percent_sponge_cover ~ percent_cover_veg, data = middle_zone)
summary(lm_middle)

# upper
upper_zone <- filter(all_sections, zone == "upper")
lm_upper <- lm(total_percent_sponge_cover ~ percent_cover_veg, data = upper_zone)
summary(lm_upper)

# linear model
model <- lm(total_percent_sponge_cover ~ percent_cover_veg * zone, data = all_sections)
summary(model)

# anova
anova <- aov(total_percent_sponge_cover ~ zone, data = all_sections)
summary(anova)

# checking for non-linear relationships
lm_nonlinear <- lm(total_percent_sponge_cover ~ poly(percent_cover_veg, 2) * zone, data = all_sections)
summary(lm_nonlinear)
