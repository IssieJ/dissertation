## SHEET ONE: DATA VISUALISATION
# January 2025

# import required libraries
library(tidyverse)
library(readxl)
library(RColorBrewer)

# import data
all_data_quadrats <- read_excel("quadrat_data_sheets/all_data_quadrats.xlsx")


# view data
head(all_data_quadrats)
view(all_data_quadrats)

# Summarising data by zone - creating mean values
groupedzone <- all_data_quadrats %>%
  group_by(zone) %>%
  summarize(
    mean_total_percent_sponge_cover = mean(total_percent_sponge_cover, na.rm = TRUE),
    mean_percent_cover_veg = mean(percent_cover_veg, na.rm = TRUE),
    mean_percent_cover_barnacle = mean(percent_cover_barnacle, na.rm = TRUE)
  )

# plot total sponge cover
ggplot(all_data_quadrats, aes(x = zone, y = total_percent_sponge_cover, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  ylim(0,60) +
  labs(
    x = "Zone",
    y = "Percent Sponge Cover"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),  
    legend.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )

# plot total veg cover
ggplot(all_data_quadrats, aes(x = zone, y = percent_cover_veg, color = zone)) +
  geom_jitter(width = 0.3, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  ylim(0,100) +
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

# veg cover boxplot 
ggplot(all_data_quadrats, aes(x = zone, y = percent_cover_veg, fill = zone)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.4) +
  scale_fill_brewer(palette = "Dark2") +  # <- this line adds the colorblind-friendly palette
  theme_minimal() +
  ylim(0, 100) +
  labs(
    x = "Zone",
    y = "Percent Vegetation Cover"
  ) +
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
# ------------------------------------------
