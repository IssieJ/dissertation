# rugosity indeces

library(tidyverse)
library(readr)
library(car)

# Summary of rugosity index by section and zone
summary(rugosity)
aggregate(rugosity_index ~ section + zone, data = rugosity, FUN = mean)


# Boxplots:

# Rugosity by zone
ggplot(rugosity, aes(x = zone, y = rugosity_index, fill = zone)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rugosity by Zone", x = "Zone", y = "Rugosity Index")

# Rugosity by section
ggplot(rugosity, aes(x = as.factor(section), y = rugosity_index, fill = as.factor(section))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rugosity by Section", x = "Section", y = "Rugosity Index")

# both on one boxplot
ggplot(rugosity, aes(x = zone, y = rugosity_index, fill = zone)) +
  geom_boxplot() +
  facet_wrap(~ section, labeller = label_both) + # Create a facet for each section
  theme_minimal() +
  labs(title = "Rugosity Index by Zone and Section",
       x = "Zone",
       y = "Rugosity Index",
       fill = "Zone") +
  theme(strip.text = element_text(size = 12, face = "bold")) # Customize facet labels


# 2-way ANOVA
anova_model <- aov(rugosity_index ~ zone + section, data = rugosity)
summary(anova_model)

anova2 <- aov(rugosity_index ~ zone, data = rugosity)
summary(anova2)

# check assumptions
shapiro.test(residuals(anova_model))
# w=0.8798 p-value=0.156 - W close to 1 and p-value insignificant 
# = normal distribution

# more info if there were more observations. 3 d.f. isnt really enough. 
# box plot shows some variation but ANOVA insignificant. 

