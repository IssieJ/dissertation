# 2: STATISICAL ANALYSIS - testing for osculum size changes
# January 2025

# import required libraries
library(fitdistrplus)
library(lme4)
library(tidyverse)
library(car)
library(mgcv)
library(gratia)
library(RColorBrewer)

# make sure length data is log transformed
all_data$log_length <- log(all_data$Length)

# checking homoscedasticity 
all_data$Osculum <- as.factor(all_data$Osculum)
all_data$Camera <- as.factor(all_data$Camera)

# checking distributions
hist(all_data$log_length, breaks = 50, main = "Distribution of Osculum Length")

descdist(all_data$log_length, discrete = FALSE)

# testing for homogeneity of variance
leveneTest(log_length ~ Osculum * Camera, data = all_data)

# MODELLING

# General Additive Models

# model 1 - smooth function and camera and osculum as fixed effects. 
gam_model <- gam(log_length ~ s(ID, bs = "cs") + Camera + Osculum, 
                 family = Gamma(link = "log"), data = all_data)
summary(gam_model)

# model 2 - smooth function for each osculum individually
gam_model2 <- gam(log_length ~ s(ID, by = interaction(Camera, Osculum), bs = "cs"), 
                 family = Gamma(link = "log"), data = all_data)
summary(gam_model2)

# DERIVATIVE ANALYSIS

# Compute derivatives
deriv_data <- derivatives(gam_model2, term = "ID", partial_match = TRUE)

deriv_data <- all_data %>%
  group_by(Osculum, Camera) %>%
  arrange(ID) %>% 
  mutate(derivative = c(NA, diff(log(Length)) / diff(ID))) %>%
  ungroup()

# Plot rate of change with no thresholds 

ggplot(deriv_data, aes(x = ID, y = derivative, color = as.factor(Osculum))) +
  geom_line(linewidth = 0.4) +
  facet_grid(Osculum ~ Camera, scales = "free_x") + 
  scale_color_brewer(palette = "Dark2") +  # colorblind-friendly
  labs(
    y = "Change in Length per Measured Frame", 
    x = "Measurement Number",
    color = "Osculum"  # legend title
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key = element_blank(),  
    legend.background = element_rect(fill = "white", color = "black"),  # boxed legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line = element_line(colour = "black")
  )