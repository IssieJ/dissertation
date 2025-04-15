# SHEET 4: RUGOSITY
# January 2025 

# import required libraries
library(tidyverse)
library(car)
library(readxl)
library(RColorBrewer)

# import datasets
rugosity <- read_excel("datasheets and graphs/quadrat_data_sheets/rugosity.xlsx")
View(rugosity)

# Summary of rugosity index by section and zone
summary(rugosity)
aggregate(rugosity_index ~ section + zone, data = rugosity, FUN = mean)

# BOX PLOTS

# Rugosity by zone
ggplot(rugosity, aes(x = zone, y = rugosity_index, fill = zone)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +  # colorblind-friendly
  labs(
    x = "Zone", 
    y = "Rugosity Index",
    fill = "zone"  # legend title
  ) +
  scale_y_continuous(limits = c(1, 1.3)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),  # legend box
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )

# anova
anova_rugosity <- aov(rugosity_index ~ zone, data = rugosity)
summary(anova_rugosity)

# check assumptions
shapiro.test(residuals(anova_rugosity))

leveneTest(rugosity_index ~ zone, data = rugosity)
# box plot shows some variation but ANOVA insignificant. 

