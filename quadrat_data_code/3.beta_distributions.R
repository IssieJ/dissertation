# SHEET THREE: GLMS WITH BETA DISTRIBUTION 

library(readxl)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(betareg)

# GLM MODELS WITH BETA DISTRIBUTION

# convert percentages to proportions
all_data_quadrats$total_percent_sponge_cover <- all_data_quadrats$total_percent_sponge_cover / 100

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
# model 2
epsilon <- 0.0001
all_data_quadrats$percent_cover_veg <- 
  pmin(pmax(all_data_quadrats$percent_cover_veg, epsilon), 1 - epsilon)

veg_model <- betareg(percent_cover_veg ~ zone, 
                              data = all_data_quadrats)
summary(veg_model)


# BARNACLE COVER

# model 3
epsilon <- 0.0001
all_data_quadrats$percent_cover_barnacle <- 
  pmin(pmax(all_data_quadrats$percent_cover_barnacle, epsilon), 1 - epsilon)

barnacle_model<- betareg(percent_cover_barnacle ~ zone, 
                               data = all_data_quadrats)
summary(barnacle_model)


# AICs
AIC(sponge_model, veg_model, barnacle_model)
