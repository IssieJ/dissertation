library(tidyverse)

head(all_data)

ggplot(all_data, aes(x = as.numeric(factor(ID)), y = Length, 
                 group = interaction(Camera, Osculum), color = factor(Osculum))) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ Camera) +
  labs(x = "Measurement Sequence", y = "Osculum Length", color = "Osculum") +
  theme_minimal()


all_data <- all_data %>%
  group_by(Camera) %>%
  mutate(Sequence_Index = row_number()) %>%  # Create a continuous sequence
  ungroup()

ggplot(all_data, aes(x = Sequence_Index, y = Length, 
                 group = interaction(Camera, Osculum), color = factor(Osculum))) +
  geom_line(alpha = 0.7) +  
  facet_wrap(~ Camera, scales = "free_x") +  
  labs(x = "Measurement Index (Continuous)", y = "Osculum Length", color = "Osculum") +
  theme_minimal()
