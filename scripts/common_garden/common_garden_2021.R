# Packages
library(tidyverse)
library(viridis)
library(gridExtra)

# Data
key <-
  read.csv('data/common_garden_data_2021/all_key_2021.csv')

survival <-
  read.csv('data/common_garden_data_2021/all_survival_2021.csv')

growth <-
  read.csv('data/common_garden_data_2021/all_growth_2021.csv')

sampleID <-
  read.csv('data/common_garden_data_2021/sample_ID.csv')

bedkey <-
  read.csv('data/common_garden_data_2021/bed_key.csv')

# test <- full_join(growth, bedkey)
write.csv(test, 'data/common_garden_data_2021/full_join_2021.csv')

data <- growth %>% 
  filter(Species != "unknown" & Species!="Betula nana" & Species!="Betula glandulosa") %>% 
  mutate(Sample_age = Year - Year_planted) %>%
  filter(Month == "8") %>%
  filter(Day != 2) %>% 
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3))

data_traits <- growth %>% 
  filter(Species != "unknown" & Species!="Betula nana" & Species!="Betula glandulosa") %>% 
  mutate(Sample_age = Year - Year_planted) %>%
  filter(!is.na(LA))

head(data)
str(data)
write.csv(data, 'scripts/common_garden/common_garden_data_2021/all_merged_data_2021.csv')

field_data <- 
  read.csv('data/source_pops/Salix_field_trait_data.csv')

##### Figures

#### Growth Figure

# Canopy height

(plot_canopy <- ggplot(data) +
    geom_smooth(aes(x= Sample_age, y= Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x= Sample_age, y= Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Canopy Height (cm)") +
    xlab("\nAge (years)") +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Stem elongation

(plot_stem <- ggplot(data) +
    geom_smooth(aes(x= Sample_age, y= mean_stem_elong, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x= Sample_age, y= mean_stem_elong, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Stem Elongation (mm)") +
    xlab("\nAge (years)") +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Shrub width

(plot_width <- ggplot(data_leaf) +
    geom_boxplot(aes(x= Site, y= mean_width, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Width (cm)") +
    xlab("") +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Leaf Length

data_leaf <- data %>% filter(Year == 2021) %>% mutate(mean_width = (Width_cm + Width_2_cm)/2)

(plot_leaf <- ggplot(data_leaf) +
    geom_boxplot(aes(x= Site, y= mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Leaf Length (mm)") +
    xlab("") +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

growth_plot <- grid.arrange(plot_canopy, plot_stem, plot_width, plot_leaf, ncol=2)

ggsave("scripts/common_garden/common_garden_figures/common_garden_growth_data.png", width = 28, height = 20, units = "cm", dpi = 100, plot = growth_plot)

#### Trait Figure

# Height traits

data2017 <- data %>% filter(Year == 2017)

(plot_height <- ggplot() +
    geom_boxplot(data = field_data, aes(x= Site, y= Plant_height_veg_m, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
    geom_boxplot(data = data2017, aes(x= Site, y= (Canopy_Height_cm/100), fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
    facet_grid(cols = vars(Species)) +
    ylab("Canopy height (m)") +
    xlab("") +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    ylim(-0.01, 2.0) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Leaf Area

(plot_LA <- ggplot() +
    geom_boxplot(data = data_traits, aes(x= Site, y= LA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    geom_boxplot(data = field_data, aes(x= Site, y= Leaf_area_mm2, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
    geom_boxplot(data = data_traits, aes(x= Site, y= LA, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
    facet_grid(cols = vars(Species)) +
    ylab(expression(paste("Leaf Area (",mm^{2},")"))) +
    xlab("") +
    ylim(-1, 2500) +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Leaf Mass

(plot_Leaf_mass <- ggplot() +
    geom_boxplot(data = field_data, aes(x= Site, y= Leaf_dry_mass_mg, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
    geom_boxplot(data = data_traits, aes(x= Site, y= (Dry_mass*1000), fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
    facet_grid(cols = vars(Species)) +
    ylab("Leaf Dry Mass (mg)") +
    xlab("") +
    ylim(-0.1, 250) +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Specific Leaf Area

(plot_SLA <- ggplot() +
    geom_boxplot(data = field_data, aes(x= Site, y= SLA_mm2_mg, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
    geom_boxplot(data = data_traits, aes(x= Site, y= SLA, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
    facet_grid(cols = vars(Species)) +
    ylab(expression(paste("Specific Leaf Area (",mm^{2}," ",mg^{-1},")"))) +
    xlab("") +
    ylim(-0.01, 30) +
    scale_colour_viridis_d(begin = 0.85, end = 0.4) +
    scale_fill_viridis_d(begin = 0.85, end = 0.4) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

trait_plot <- grid.arrange(plot_height, plot_LA, plot_Leaf_mass, plot_SLA, ncol=2)

ggsave("scripts/common_garden/common_garden_figures/common_garden_trait_data.png", width = 28, height = 20, units = "cm", dpi = 100, plot = trait_plot)
