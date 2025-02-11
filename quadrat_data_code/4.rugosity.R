# SHEET 4: RUGOSITY

library(tidyverse)
library(readr)
library(car)

# Summary of rugosity index by section and zone
summary(rugosity)
aggregate(rugosity_index ~ section + zone, data = rugosity, FUN = mean)


# Box plots

# Rugosity by zone
ggplot(rugosity, aes(x = zone, y = rugosity_index, fill = zone)) +
  geom_boxplot() +
  labs(x = "Zone", y = "Rugosity Index") +
  scale_y_continuous(limits = c(1, 1.3)) +
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

# anova
anova_rugosity <- aov(rugosity_index ~ zone, data = rugosity)
summary(anova_rugosity)

# check assumptions
shapiro.test(residuals(anova_rugosity))
# w=0.95231 p-value=0.7154 - W close to 1 and p-value insignificant 
# = normal distribution

# box plot shows some variation but ANOVA insignificant. 

