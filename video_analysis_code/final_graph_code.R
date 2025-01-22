library(tidyverse)

ggplot(cam6osc3_allrecs, aes(x = ID, y = Length, color = Sheet, group = Sheet)) +
  geom_line(size = 0.5) +
  scale_y_continuous(limits = c(5,20)) +
  scale_x_continuous(limits = c(0,500))+
  labs(title = "Osc 3 Length Comparison Across Recordings",
       x = "ID",
       y = "Length in Pixels") +
  theme_classic()
  
   