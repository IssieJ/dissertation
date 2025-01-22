# STATS ANALYSIS WITH ALL SECTIONS COMBINED

# Load necessary libraries
library(readxl)
library(tidyverse)
library(lme4)
library(glmmTMB)

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
# allsections_data <- bind_rows(section1_all, section2_all, section3_all)

library(readr)

# Write to CSV file
write_csv(allsections_data, "all_sections.csv")


# file_path <- file.choose(new = TRUE)  # This opens a file picker to select a save location
# write.csv(allsections_data, file = file_path)

# make zone and section a factor
allsections_data$zone <- as.factor(allsections_data$zone)

allsections_data$section <- as.factor(allsections_data$section)

# As total_percent_sponge_cover is in percentages (0-100), convert it to proportions (0-1)
allsections_data$total_percent_sponge_cover <- allsections_data$total_percent_sponge_cover / 100

# try some plots with all the data

# Summarize average sponge cover by zone and section
summary_data <- allsections_data %>%
  group_by(zone, section) %>%
  summarize(avg_sponge_cover = mean(total_percent_sponge_cover, na.rm = TRUE))

ggplot(summary_data, aes(x = zone, y = avg_sponge_cover, fill = section)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Zone",
       y = "Average Sponge Cover (%)") +
  scale_y_continuous(limits = c(0, 0.15), breaks = c(0, 0.05, 0.1, 0.15)) +  # Set y-axis limit and breaks
  theme(panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank()) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2")  # Optional: set color palette

# barnacle cover all zones all sections
# Summarize average barnacle cover by zone and section
summary_data_barnacle <- allsections_data %>%
  group_by(zone, section) %>%
  summarize(avg_barnacle_cover = mean(percent_cover_barnacle, na.rm = TRUE),
            sd_barnacle_cover = sd(percent_cover_barnacle, na.rm = TRUE))

ggplot(summary_data_barnacle, aes(x = zone, y = avg_barnacle_cover, fill = section)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Zone",
       y = "Average Barnacle Cover (%)") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")  # Optional: set color palette

# vegetation cover all zones all sections
summary_data_vegetation <- allsections_data %>%
  group_by(zone, section) %>%
  summarize(avg_veg_cover = mean(percent_cover_veg, na.rm = TRUE),
            sd_veg_cover = sd(percent_cover_veg, na.rm = TRUE))

ggplot(summary_data_vegetation, aes(x = zone, y = avg_veg_cover, fill = section)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Zone",
       y = "Average Vegetation Cover (%)") +
  theme_classic()
  #scale_fill_brewer(palette = "Set4") # Optional: set color palette

