## correlation between sponges and vegetation by species

library(tidyverse)

# Load data
View(data_with_veg_id)

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

# plot the vegetation species across zones
ggplot(species_by_zone, aes(x=species_name, y=Frequency, fill=zone)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Species", y="Frequency")

# correlation matrix
species_wide <- species_long %>%
  pivot_wider(names_from = species_name, values_from = Presence, values_fill = 0)

cor_results <- cor(species_wide[, c("total_percent_sponge_cover", names(species_wide)[8:ncol(species_wide)])], 
                   use = "pairwise.complete.obs")

print(cor_results)

# save file
write.csv(species_long, file = "species_long.csv", row.names = FALSE)

# plot in wide format - better graph
ggplot(species_long, aes(x = species_name, fill = zone)) +
  geom_bar(position = "dodge") +  
  theme_classic() +  
  theme(
    axis.line = element_line(colour = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Species", y = "Frequency", fill = "Zone")

#plot tidy graph

test_species <- species_long %>%
  mutate(species_name = factor(species_name, levels = c(unique(species_name[species_name != "none"]), "none")))

ggplot(species_long, aes(x = species_name, fill = zone)) +
  geom_bar(position = "dodge", width = 0.7) +
  theme_classic() +  
  theme(
    axis.line = element_line(colour = "black"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Species", y = "Frequency", fill = "Zone") +
  scale_x_discrete(limits = levels(test_species$species_name))


# run a regression

species_wide$sponge_presence <- ifelse(species_wide$total_percent_sponge_cover > 0, 1, 0)

logit_model <- glm(sponge_presence ~ ., data = species_wide[, c("sponge_presence", names(species_wide)[8:ncol(species_wide)])], family = "binomial")
summary(logit_model)


table(species_wide$sponge_presence, species_wide$`fucus vesiculosus`)
table(species_wide$sponge_presence, species_wide$`ascophyllum nodosum`)


