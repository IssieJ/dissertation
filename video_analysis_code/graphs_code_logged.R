# Load necessary packages
library(ggplot2)
library(dplyr)

# Assuming your dataset is named 'df' and has columns: Length, Osculum, Camera
all_loglength <- all_data %>%
  mutate(Logged_Length = log(Length))  # Natural log transformation

# Compute summary stats: Mean, Standard Error (SE), and Confidence Interval (CI)
summary_all <- all_loglength%>%
  group_by(Osculum, Camera) %>%
  summarise(
    Mean_Logged_Length = mean(Logged_Length, na.rm = TRUE),
    SE = sd(Logged_Length, na.rm = TRUE) / sqrt(n()),  # Standard Error
    CI_Upper = Mean_Logged_Length + 1.96 * SE,  # 95% CI Upper
    CI_Lower = Mean_Logged_Length - 1.96 * SE   # 95% CI Lower
  )

# line plot with mean lengths (logged)
ggplot(summary_all, aes(x = factor(Camera), y = Mean_Logged_Length, 
                       color = factor(Osculum), group = factor(Osculum))) + 
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), 
              alpha = 0.2, color = "lightgrey") +  # Add confidence interval ribbon
  labs(x = "Camera", y = "Mean Logged Oscula Length", 
       color = "Osculum", fill = "Osculum") + 
    theme_minimal() +
      theme(
        panel.grid.major = element_blank(),  # Removes major gridlines
        panel.grid.minor = element_blank(),  # Removes minor gridlines
        axis.line = element_line(colour = "black")
      )

# boxplots with logged length data

ggplot(all_loglength, aes(x = interaction(Osculum, Camera), y = Logged_Length, fill = factor(Camera))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 22, size = 3, fill = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.3) +  # Bootstrapped error bars
  labs(x = "Osculum and Camera", y = "Logged Length", fill = "Camera") +
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")  
    ) +
  ylim(NA, 4) 

    