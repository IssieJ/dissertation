## SHEET 2: NORMALITY TESTING AND ANALYSIS 
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

# Calculate correlation within each zone
all_data_quadrats %>%
  group_by(zone) %>%
  summarise(correlation = cor(percent_cover_veg, total_percent_sponge_cover, 
                              use = "complete.obs"))

# relationship between sponge cover and vegetation cover
ggplot(all_data_quadrats, aes(x = percent_cover_veg, 
                              y = total_percent_sponge_cover, color = zone)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Dark2") +  # <- colorblind-friendly palette
  ylim(0, 60) +
  xlim(0, 100) +
  labs(x = "Vegetation Cover",
       y = "Sponge Cover") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(anova))

# not normally distributed. 

# Kruskal-Wallis test
kruskal.test(total_percent_sponge_cover ~ zone, data = all_data_quadrats)
# shows significant difference between groups. 

# linear model
model <- lm(total_percent_sponge_cover ~ percent_cover_veg * zone, data = all_data_quadrats)
summary(model)