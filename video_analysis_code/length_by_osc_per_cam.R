library(tidyverse)

ggplot(cam6_allosc, aes(x = ID, y = Length, color = factor(Osculum), group = Camera)) +
  geom_line() +
  labs(title = "Camera 6 Length Comparison Across Oscula",
       x = "ID",
       y = "Length in Pixels") +
  theme_minimal()

  

