## STATISICAL ANALYSIS 

# testing to see if there is a change in osculum size
library(lme4)
library(tidyverse)

# anova
result <- aov(Length ~ Sheet, data = all_data)
summary(result)

# kruskal wallis test
kruskal.test(Length ~ Osculum_Camera, data = all_data)
kruskal.test(Length ~ Sheet, data = all_data)


lengthonly = subset(all_data, select = -c(Area, Mean, Angle) )

# Reorder levels of Sheet
lengthonly$Sheet <- factor(lengthonly$Sheet, levels = c(
  paste0("osc1_rec", 1:11),
  paste0("osc2_rec", 1:11),
  paste0("osc3_rec", 1:11)
))


# Create the boxplot with reordered levels
ggplot(lengthonly, aes(x = Sheet, y = Length)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Boxplot of Length by Sheet",
       x = "Osculum And Recording",
       y = "Length") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# LMERs
lmm <- lmer(Length ~ Sheet + (1 | Camera), data = lengthonly)
summary(lmm)

print(lmm, correlation = TRUE)
# this one has better REML convergence


lmm2 <- lmer(Length ~ Sheet + (1 | Osculum), data = lengthonly)
summary(lmm2)

vcov(lmm2, correlation = TRUE)


# Residual plot
plot(resid(lmm), main = "Residuals of the Model")
plot(resid(lmm2), main ="Residuals of the Model")

