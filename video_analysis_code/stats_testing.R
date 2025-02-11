
## STATISICAL ANALYSIS 

# testing to see if there is a change in osculum size
install.packages("FSA")
library(lme4)
library(tidyverse)
library(FSA)
library(car)

# checking homoscedasticity 
all_data$Osculum <- as.factor(all_data$Osculum)
all_data$Camera <- as.factor(all_data$Camera)

leveneTest(log_length ~ Osculum * Camera, data = all_data)
# variances are very different - need to be the same for anova. 

# anova - can't use - too many assumptions violated. 
anova1 <- aov(log_length ~ Osculum + Camera, data = all_data)
summary(anova1)

# check distributions
qqnorm(residuals(anova1))
qqline(residuals(anova1), col = "red")

# kruskal wallis test
kruskal.test(log_length ~ interaction(Osculum, Camera), data = all_data)

# effect size test to tell how strong the difference is

kruskal_effsize <- function(stat, n, df) {
  eta_squared <- stat / (n - 1)
  return(eta_squared)
}

kruskal_effsize(4371, 5494, 17)  # Using your test's chi-squared, sample size, and df
# large effect size - close to 1 is larger 
# this is = 0.796



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


