library(readxl)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(betareg)

# DATA MANIPULATION 

# import section 1
view(section1_all)
# import section 2
view(section2_all)
# import section 3
view(section3_all)

# Load each dataset and add a 'section' column
section1_all$section <- "section1"
section2_all$section <- "section2"
section3_all$section <- "section3"

# Combine all sections into a single data frame
allsections_data <- bind_rows(section1_all, section2_all, section3_all)

# make zone and section a factor
allsections_data$zone <- as.factor(allsections_data$zone)
allsections_data$section <- as.factor(allsections_data$section)
------------------------------------------------------------------------------

# GLM MODELS WITH BETA DISTRIBUTION

# convert percentages to proportions
allsections_data$total_percent_sponge_cover <- allsections_data$total_percent_sponge_cover / 100

# Small offset to avoid 0 and 1
epsilon <- 0.0001
allsections_data$total_percent_sponge_cover <- 
  pmin(pmax(allsections_data$total_percent_sponge_cover, epsilon), 1 - epsilon)


# SPONGE COVER
# model 1
beta_glm_model <- betareg(total_percent_sponge_cover ~ zone, 
                          data = allsections_data)
summary(beta_glm_model)

# model 2
beta_glm_model2 <- betareg(total_percent_sponge_cover ~ zone+section, 
                           data = allsections_data)
summary(beta_glm_model2)

beta_glm_model2i <- betareg(total_percent_sponge_cover ~zone*section, 
                            data = allsections_data)

summary(beta_glm_model2i)

# AICs
AIC(beta_glm_model, beta_glm_model2)

# VEGETATION COVER
# model 3
epsilon <- 0.0001
allsections_data$percent_cover_veg <- 
  pmin(pmax(allsections_data$percent_cover_veg, epsilon), 1 - epsilon)

beta_glm_model_veg <- betareg(percent_cover_veg ~ zone, 
                              data = allsections_data)
summary(beta_glm_model_veg)

# model 4 
beta_glm_model_veg2 <- betareg(percent_cover_veg ~ zone+section, 
                               data = allsections_data)
summary(beta_glm_model_veg2)

# AICs
AIC(beta_glm_model_veg, beta_glm_model_veg2)

# BARNACLE COVER

# model 5
epsilon <- 0.0001
allsections_data$percent_cover_barnacle <- 
  pmin(pmax(allsections_data$percent_cover_barnacle, epsilon), 1 - epsilon)

beta_glm_model_barn <- betareg(percent_cover_barnacle ~ zone, 
                               data = allsections_data)
summary(beta_glm_model_barn)

# model 6
beta_glm_model_barn2 <- betareg(percent_cover_barnacle ~ zone+section, 
                               data = allsections_data)

summary(beta_glm_model_barn2)

# AICs
AIC(beta_glm_model_barn, beta_glm_model_barn2)