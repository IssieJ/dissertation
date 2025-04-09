# SHEET THREE: GLMS WITH BETA DISTRIBUTION 
# January 20205

# import required libraries
library(tidyverse)
library(readxl)
library(lme4)
library(glmmTMB)
library(betareg)

# import data
all_data_quadrats <- read_excel("quadrat_data_sheets/all_data_quadrats.xlsx")

# view data
head(all_data_quadrats)

# GLM MODELS WITH BETA DISTRIBUTION

# convert percentages to proportions
all_data_quadrats$total_percent_sponge_cover <- 
  all_data_quadrats$total_percent_sponge_cover / 100
all_data_quadrats$percent_cover_veg <- 
  all_data_quadrats$percent_cover_veg / 100

# Small offset to avoid 0 and 1
epsilon <- 0.0001
all_data_quadrats$total_percent_sponge_cover <- 
  pmin(pmax(all_data_quadrats$total_percent_sponge_cover, epsilon), 1 - epsilon)

# SPONGE COVER
# model 1
sponge_model <- betareg(total_percent_sponge_cover ~ zone, 
                          data = all_data_quadrats)
summary(sponge_model)

# VEGETATION COVER
# model 1
all_data_quadrats$percent_cover_veg <- all_data_quadrats$percent_cover_veg / 100

epsilon <- 0.0001
all_data_quadrats$percent_cover_veg <- 
  pmin(pmax(all_data_quadrats$percent_cover_veg, epsilon), 1 - epsilon)

veg_model <- betareg(percent_cover_veg ~ zone, 
                              data = all_data_quadrats)
summary(veg_model)

# second veg model 
all_data_quadrats$zone <- relevel(as.factor(all_data_quadrats$zone), ref = "upper")

veg_model2 <- betareg_model_relevel <- betareg(percent_cover_veg ~ zone, 
                                               data = all_data_quadrats)
summary(veg_model2)

AIC(veg_model, veg_model2)