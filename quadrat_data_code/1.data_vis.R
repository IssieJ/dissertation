## SHEET ONE: DATA VISUALISATION ##

# data by zone


# import required libraries
library(tidyverse)

# view data

head(all_data_quadrats)
view(all_data_quadrats)

# make zone and section a factor
#all_data_quadrats$zone <- as.factor(all_data_quadrats$zone)

# Convert percent sponge cover data to proportions (0-1) from percentages (0-100)
#all_data_quadrats$total_percent_sponge_cover <- all_data_quadrats$total_percent_sponge_cover*100


# Summarising data by zone - creating mean values
groupedzone <- all_data_quadrats %>%
  group_by(zone) %>%
  summarize(
    mean_total_percent_sponge_cover = mean(total_percent_sponge_cover, na.rm = TRUE),
    mean_percent_cover_veg = mean(percent_cover_veg, na.rm = TRUE),
    mean_percent_cover_barnacle = mean(percent_cover_barnacle, na.rm = TRUE)
  )

# create graphs

# total sponge cover (%)
ggplot(all_data_quadrats, aes(x = zone, y = total_percent_sponge_cover, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  ylim(0, 0.5) +
  labs(
    x = "Zone",
    y = "Total Percent Sponge Cover"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    legend.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA),  # White outer background
    axis.line = element_line(colour = "black")
  )


# total veg cover
ggplot(all_data_quadrats, aes(x = zone, y = total_percent_sponge_cover, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  theme_minimal() +
  ylim(0,80) +
  labs(
    x = "Zone",
    y = "Percent Vegetation Cover")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    legend.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )


# ------------------------------------------
# note on the outlier in middle -  
# Natural Variation – 
# not removing it because:
# outlier is a real observation and part of natural variability, 
# it is included to reflect the true range of data.

# Scientific Integrity –
# removing data can lead to biased results or misinterpretation.
# ------------------------------------------
  
# barnacle cover by zone
ggplot(all_data_quadrats, aes(x = zone, y = percent_cover_barnacle, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  theme_minimal() +
  ylim(0, 100) +
  labs(
    x = "Zone",
    y = "Percent Barnacle Cover"
  )

# Sponge and Veg Cover on same plot
ggplot(all_data_quadrats, aes(x = zone)) +
  geom_col(aes(y = total_percent_sponge_cover, fill = "Sponge Cover"), alpha = 0.3) +
  geom_point(aes(y = percent_cover_veg * 0.1, color = "Vegetation Cover"), size = 2) +  
  scale_y_continuous(
    name = "Sponge Cover (%)", 
    sec.axis = sec_axis(~ . * 10, name = "Vegetation Cover (%)")
  ) +
  scale_fill_manual(values = c("Sponge Cover" = "blue"), name = NULL) +  # Removes heading
  scale_color_manual(values = c("Vegetation Cover" = "darkgreen"), name = NULL) +  # Removes heading
  labs(x = "Zone") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = NA),  
    legend.title = element_blank(),  # Another way to remove legend titles
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )

