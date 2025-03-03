## SHEET 2: NORMALITY TESTING AND ANALYSIS 

# load required libraries 
library(tidyverse)

# view data
view(all_data_quadrats)

# Calculate correlation within each zone
all_data_quadrats %>%
  group_by(zone) %>%
  summarise(correlation = cor(percent_cover_veg, total_percent_sponge_cover, use = "complete.obs"))

# relationship between sponge cover and vegetation cover
ggplot(all_data_quadrats, aes(x = percent_cover_veg, y = total_percent_sponge_cover, color = zone)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylim(0,80) +
  xlim(0,100) +
  labs(x = "Vegetation Cover",
       y = "Sponge Cover") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = NA),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )

# maybe should look at outliers for this

# anova
anova <- aov(total_percent_sponge_cover ~ zone, data = all_data_quadrats)
summary(anova)

# histogram of residuals
hist(residuals(anova), main="Histogram of Residuals", xlab="Residuals", breaks=20, col="lightblue", border="black")

# Q-Q plot to check normality of residuals
qqnorm(residuals(anova))
qqline(residuals(anova), col = "red")  # Adding reference line

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(anova))

# extremely not normally distributed. 
# can't use anova. 

# Kruskal-Wallis test
kruskal.test(total_percent_sponge_cover ~ zone, data = all_data_quadrats)
# shows significant difference between groups. 


# linear model
model <- lm(total_percent_sponge_cover ~ percent_cover_veg * zone, data = all_data_quadrats)
summary(model)

# analysis in notes
# doesnt matter that not normal distribution because sample size is big enough. 


