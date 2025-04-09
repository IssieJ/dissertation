# SHEET THREE: GLMS WITH BETA DISTRIBUTION 

library(readxl)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(betareg)
library(stargazer)

# GLM MODELS WITH BETA DISTRIBUTION

# convert percentages to proportions
all_data_quadrats$total_percent_sponge_cover <- all_data_quadrats$total_percent_sponge_cover / 100
all_data_quadrats$percent_cover_veg <- all_data_quadrats$percent_cover_veg / 100
# Small offset to avoid 0 and 1
epsilon <- 0.0001
all_data_quadrats$total_percent_sponge_cover <- 
  pmin(pmax(all_data_quadrats$total_percent_sponge_cover, epsilon), 1 - epsilon)


# SPONGE COVER
# model 1
sponge_model <- betareg(total_percent_sponge_cover ~ zone, 
                          data = all_data_quadrats)
summary(sponge_model)

# create table
stargazer(sponge_model, type = "text")  # Replace 'model' with your beta regression model

# VEGETATION COVER
# model 2
all_data_quadrats$percent_cover_veg <- all_data_quadrats$percent_cover_veg / 100

epsilon <- 0.0001
all_data_quadrats$percent_cover_veg <- 
  pmin(pmax(all_data_quadrats$percent_cover_veg, epsilon), 1 - epsilon)

veg_model <- betareg(percent_cover_veg ~ zone, 
                              data = all_data_quadrats)
summary(veg_model)

# second veg model 

veg_model2 <- betareg(percent_cover_veg ~ zone, data = all_data_quadrats, link = "probit")
summary(veg_model2)

AIC(veg_model, veg_model2)

# exactly the same - model2 is better because it aligns more with visual changes
# and model fits slightly better. 

# third veg model 
all_data_quadrats$zone <- relevel(as.factor(all_data_quadrats$zone), ref = "upper")

veg_model3 <- betareg_model_relevel <- betareg(percent_cover_veg ~ zone, data = all_data_quadrats)
summary(betareg_model_relevel)

AIC(veg_model2, veg_model3)

# mod3 is better becuase it better fits the trends seen in the graph. 



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
