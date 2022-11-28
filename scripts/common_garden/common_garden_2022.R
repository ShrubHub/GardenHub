#### COMMON GARDEN 2022 SCRIPT
### Data wrangling and visualisation script
### By Erica Zaja, created on 30/09/2022
## Adapted from Madelaine Anderson's common_garden_2021.R 
## Last updated: 07/10/2022

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)

# 2. LOADING DATA ----

# Data collected in the common garden in summer 2022 (June-July-August)
growth_2022 <- read_csv("scripts/common_garden/common_garden_data_2022/wrangled_ALL_combined_Common_Garden_Measurements_2022.csv")

# Common garden data collected between 2013-2021
growth <- read.csv("scripts/common_garden/common_garden_data_2021/all_growth_2021.csv")

# Data collected from source locations (Kluane and QHI) between 2013 – 2017
field_data <- read.csv('data/source_pops/Salix_field_trait_data.csv')

# Data collected from source locations (Kluane and QHI) in 2022
all_source_pop_2022 <- read_csv("data/source_pops/all_source_pop_2022.csv")

# Dataset with mother data (2013-2017)
Common_garden_2017 <- read_csv("data/common_garden_data_2017/Common_garden_2017.csv")

# 3. DATA WRANGLING ----

# Keeping only relevant columns of 2022 data
growth_2022 <- dplyr::select(growth_2022, Bed, SampleID, Year_planted, Species, Site, Sample_Date,
                             Month, Day, Year, Canopy_Height_cm, Width_cm, Width_2_cm, Stem_diameter,
                             Stem_Elongation_1_mm, Stem_Elongation_2_mm, Stem_Elongation_3_mm, 
                             Length_1_mm, Length_2_mm, Length_3_mm)

# Subsetting data to remove NA columns
growth_2022 <- growth_2022[1:780, ]

# Keeping only relevant columns of 2013-2021 data
growth <- dplyr::select(growth, Bed, SampleID, Year_planted, Species, Site, Sample_Date,
                        Month, Day, Year, Canopy_Height_cm, Width_cm, Width_2_cm, Stem_diameter,
                        Stem_Elongation_1_mm, Stem_Elongation_2_mm, Stem_Elongation_3_mm, 
                        Length_1_mm, Length_2_mm, Length_3_mm)

# Merging 2022 data with 2013-2021 data 
all_growth_2022 <- rbind(growth, growth_2022)

# Saving 2013-2022 data as csv file
#write.csv(all_growth_2022, 'scripts/common_garden/common_garden_data_2022/all_growth_2022.csv')

# Making variables into the right format
all_growth_2022$Stem_Elongation_1_mm <- as.numeric(all_growth_2022$Stem_Elongation_1_mm)
all_growth_2022$Stem_Elongation_2_mm <- as.numeric(all_growth_2022$Stem_Elongation_2_mm)
all_growth_2022$Stem_Elongation_3_mm <- as.numeric(all_growth_2022$Stem_Elongation_3_mm)
all_growth_2022$Length_1_mm <- as.numeric(all_growth_2022$Length_1_mm)
all_growth_2022$Length_2_mm <- as.numeric(all_growth_2022$Length_2_mm)
all_growth_2022$Length_3_mm <- as.numeric(all_growth_2022$Length_3_mm)
all_growth_2022$Width_cm <- as.numeric(all_growth_2022$Width_cm)
all_growth_2022$Width_2_cm <- as.numeric(all_growth_2022$Width_2_cm)
all_growth_2022$Canopy_Height_cm <- as.numeric(all_growth_2022$Canopy_Height_cm)

# Removing Betula nana and Betula glandulosa, keeping only month of August for consistency,
# Creating mean stem elongation, mean leaf length and mean width columns
all_merged_data_2022 <- all_growth_2022 %>% 
  filter(Species != "unknown" & Species!="Betula nana" & Species!="Betula glandulosa") %>% 
  mutate(Sample_age = Year - Year_planted) %>%
  filter(Month == "8") %>%
  filter(Day != 2) %>% 
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2),
         biovolume = (Width_cm*Width_2_cm*Canopy_Height_cm))

# Saving all merged and formatted 2013-2022 data as csv file
write.csv(all_merged_data_2022, 'scripts/common_garden/common_garden_data_2022/all_merged_data_2022.csv')

# Making variables into the right format
all_merged_data_2022$Bed <- as.factor(as.character(all_merged_data_2022$Bed))
all_merged_data_2022$Species <- as.factor(as.character(all_merged_data_2022$Species))
all_merged_data_2022$Site <- as.factor(as.character(all_merged_data_2022$Site))
all_merged_data_2022$Site <- as.factor(as.character(all_merged_data_2022$Site))
all_merged_data_2022$Month <- as.numeric(all_merged_data_2022$Month)
all_merged_data_2022$Day <- as.numeric(all_merged_data_2022$Day)
all_merged_data_2022$Canopy_Height_cm <- as.numeric(all_merged_data_2022$Canopy_Height_cm)

# Checking all variables are right format
str(all_merged_data_2022)

# Keeping only relevant columns
field_data <- dplyr::select(field_data, Species, Plant_height_veg_m, Lat, Lon, 
                            Elevation_m, Site, date, Year_measured)

# Making variables into the right format
field_data$Species <- as.factor(field_data$Species)
field_data$Site <- as.factor(field_data$Site)

# Merging field height data from 2017-2022 datasets
heights_2022_source_pop <- all_source_pop_2022 %>%
  select(Species, Site, Canopy_Height_cm)

heights_2017_source_pop <- field_data %>%
  select(Species, Plant_height_veg_m, Site) %>%
  mutate(Canopy_Height_cm = Plant_height_veg_m*100) %>%
  select(-Plant_height_veg_m) %>% 
  na.omit()

heights_all_source_pop <- rbind(heights_2017_source_pop, heights_2022_source_pop)

heights_all_source_pop$Site <- as.factor(heights_all_source_pop$Site)
heights_all_source_pop$Species <- as.factor(heights_all_source_pop$Species)
str(heights_all_source_pop)
# Saving all source population heights 2017-2022 data as csv file
write.csv(heights_all_source_pop, 'data/source_pops/heights_all_source_pop.csv')

# only keeping relevant columns of mother data
mother_data <- Common_garden_2017 %>%
  select(Sample_ID, Match, Sample_location, Date_sampled,
         Date_propagated, Date_planted, Year_planted, Mother_height,
         Mother_CW_1, Mother_CW_2, Mother_LS, Mother_LL1, Mother_LL2, 
         Mother_LL3, Mother_SE1, Mother_SE2, Mother_SE3, Cutting_length, 
         Cutting_diameter) %>%
  rename("SampleID" = "Sample_ID", 
         "Canopy_Height_cm" = "Mother_height",
         "Length_1_mm" = "Mother_LL1",
         "Length_2_mm" = "Mother_LL2",
         "Length_3_mm" = "Mother_LL3",
         "Stem_Elongation_1_mm" = "Mother_SE1",
         "Stem_Elongation_2_mm" = "Mother_SE2",
         "Stem_Elongation_3_mm" = "Mother_SE3",
         "Width_cm" = "Mother_CW_1",
         "Width_2_cm" = "Mother_CW_2", 
         "Site" = "Sample_location")

str(mother_data)

# Making variables right format
mother_data$Width_cm <- as.numeric(mother_data$Width_cm)
mother_data$Width_2_cm <- as.numeric(mother_data$Width_2_cm)
mother_data$Stem_Elongation_3_mm <- as.numeric(mother_data$Stem_Elongation_3_mm)
mother_data$Site <- as.factor(mother_data$Site)

# Merging wrangled versions of salix_field_data, all_source_pop_2022, common_garden_2017
all_source_pop_plus_mother <- bind_rows(heights_2017_source_pop, all_source_pop_2022,
                        mother_data)

# Saving all source population heights 2017-2022 data as csv file
write.csv(all_source_pop_plus_mother, 'data/source_pops/all_source_pop_plus_mother.csv')


# 3.1. DATA EXPLORE: Sample size ----
# Need to figure out how to remove NA rows of DEAD shurbs, not fully sen shrubs

# How many shrubs of each type (Arctic vs Alpine?)
count_alpine_shrubs <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane")
# n = 134

count_arctic_shrubs <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk")
# n = 126
# total n = 260

# How many Salix richardsonii from Kluane?
count_salric_kluane <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane",
         Species == "Salix richardsonii")
# n = 44 -3 NA rows = 41

# How many Salix richardsonii from QHI?
count_salric_QHI <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk",
         Species == "Salix richardsonii")
# n = 39 -6 NA rows = 33

# How many Salix pulchra from Kluane?
count_salpul_kluane <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane",
         Species == "Salix pulchra")
# n = 60

# How many Salix pulchra from QHI?
count_salpul_QHI <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk",
         Species == "Salix pulchra")
# n = 56

# How many Salix arctica from QHI?
count_salarct_QHI <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk",
         Species == "Salix arctica")
# n = 31

# How many Salix arctica from Kluane?
count_salarct_kluane <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane",
         Species == "Salix arctica")
# n = 30


# 4. DATA VISUALISATION ----

# 4.1. COMMON GARDEN ----

# a. Canopy height(2013-2022) ----
(plot_canopy_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
    xlab("\nAge (years)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 15, color = "black", face = "italic"),
          legend.title = element_text(size=15), #change legend title font size
          legend.text = element_text(size=12),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# b. Stem elongation (2013-2022) ----
(plot_stem_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y= mean_stem_elong, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= mean_stem_elong, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Elongation (mm)") +
    xlab("\nAge (years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))

# c. Stem diameter (2013-2022) ----
(plot_diameter_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y = Stem_diameter, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= Stem_diameter, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Stem diameter (mm)") +
    xlab("\nAge (years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# d. Shrub width (2013-2022) ----
(plot_width_2022 <- ggplot(all_merged_data_2022) +
    geom_boxplot(aes(x= Site, y = mean_width, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Width (cm)") +
    xlab("") +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# Facet
facet_traits <- grid.arrange(plot_canopy_2022, plot_stem_2022, plot_diameter_2022, plot_width_2022, ncol=2)

# e. Leaf length (2013-2022) boxplot ----

(plot_leaf_2022 <- ggplot(all_merged_data_2022) +
    geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Leaf Length (mm)") +
    xlab("") +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# e.1. Leaf length scatter (2013-2022) -----
(plot_leaf_scatter_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y = mean_leaf_length, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= mean_leaf_length, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Leaf Length (mm)") +
    xlab("\nAge (years)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 15, color = "black", face = "italic"),
          legend.title = element_text(size=15), #change legend title font size
          legend.text = element_text(size=12),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))
  
# f. Leaf length (2022) ----
data_leaf_2022 <- all_merged_data_2022 %>% filter(Year == 2022)

(plot_leaf_2022_only <- ggplot(data_leaf_2022) +
    geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Leaf Length (mm)") +
    xlab("") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# ggsave("scripts/common_garden/common_garden_figures/plot_leaf_2022_only.png", width = 28, height = 20, units = "cm", dpi = 100, plot = plot_leaf_2022_only)

# g. Biovolume (2013-2022) ----
(plot_biovol_2022 <- ggplot(all_merged_data_2022) +
   geom_boxplot(aes(x = Site, y = biovolume, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Biovolume (cm3)") +
   xlab("") +
   scale_colour_viridis_d(begin = 0.3, end = 0.9) +
   scale_fill_viridis_d(begin = 0.3, end = 0.9) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# 4.2. FIELD VS GARDEN ----

# a. Canopy height (m) 
(plot_height_compare <- ggplot() +
   geom_boxplot(data = heights_all_source_pop, aes(x = Site, y = Canopy_Height_cm, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
   #geom_boxplot(data = all_merged_data_2022, aes(x = Site, y = (Canopy_Height_cm), fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy height (cm)") +
   xlab("") +
   scale_colour_viridis_d(begin = 0.85, end = 0.4) +
   scale_fill_viridis_d(begin = 0.85, end = 0.4) +
  # ylim(-0.01, 2.0) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))




