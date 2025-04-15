# PLOTTING VEGETATION BY SPECIES
# January 2025

# Import required libraries
library(tidyverse)
library(readxl)
library(RColorBrewer)

# Import data
all_data_quadrats <- read_excel("quadrat_data_sheets/all_data_quadrats.xlsx")
View(all_data_quadrats)

# Expand species list into long format
species_long <- all_data_quadrats %>%
  separate_rows(veg_species_list, sep = ", ") %>%  # Split species into separate rows
  mutate(Presence = 1) %>%
  pivot_wider(names_from = veg_species_list, values_from = Presence, values_fill = 0)  # Convert to wide format

print(species_long)
View(species_long)

#Convert veg_species_list into long format
species_long <- all_data_quadrats %>%
  separate_rows(veg_species_list, sep = ", ") %>%  # Split species into rows
  rename(species_name = veg_species_list) %>%      # Rename column for clarity
  mutate(Presence = 1)                             # Add presence indicator

# species by zone
species_by_zone <- species_long %>%
  group_by(zone, species_name) %>%
  summarise(Frequency = sum(Presence), .groups = "drop")

print(species_by_zone)

#plot graph
ggplot(species_long, aes(x = species_name, fill = zone)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Dark2") +  # colorblind-friendly
  theme_classic() +  
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black"),  # boxed legend
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  labs(x = "Species", y = "Frequency", fill = "zone")

ggsave("veg_by_species.png", width = 10, height = 5, dpi = 100)
