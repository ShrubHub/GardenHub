#### COMMON GARDEN vs KLUANE WEEKLY SUBSETS 2022
#### Script by Erica Zaja, created 07/10/22
### Last updated: 09/08/2022

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)
library(gridExtra)

# 2. LOADING DATA ----

# Data collected in the common garden in summer 2022 (weekly throughout June-July-August)
weekly_garden_2706 <- read_excel("data/common_garden_shrub_data/weekly_subsets/270622_EZ_weekly_garden_2022.xlsx")
weekly_garden_0307 <- read_excel("data/common_garden_shrub_data/weekly_subsets/030722_EZ_weekly_garden_2022.xlsx")
weekly_garden_1007 <- read_excel("data/common_garden_shrub_data/weekly_subsets/100722_EZ_weekly_garden_2022.xlsx")
weekly_garden_1707 <- read_excel("data/common_garden_shrub_data/weekly_subsets/170722_EZ_weekly_garden_2022.xlsx")
weekly_garden_2307 <- read_excel("data/common_garden_shrub_data/weekly_subsets/230722_EZ_weekly_garden_2022.xlsx")
weekly_garden_3107 <- read_excel("data/common_garden_shrub_data/weekly_subsets/310722_EZ_weekly_garden_2022.xlsx")
weekly_garden_1208 <- read_excel("data/common_garden_shrub_data/weekly_subsets/120822_EZ_weekly_garden_2022.xlsx")

# Data collected on Kluane Plateau in summer 2022 (weekly throughout July-August)
weekly_kluane_0108 <- read_excel("data/source_pop_Kluane_shrub_data/weekly_subsets/010822_EZ_weekly_source_pop_Kluane_2022.xlsx")
weekly_kluane_0907 <- read_excel("data/source_pop_Kluane_shrub_data/weekly_subsets/090722_EZ_weekly_source_pop_Kluane_2022.xlsx")
weekly_kluane_1308 <- read_excel("data/source_pop_Kluane_shrub_data/weekly_subsets/130822_EZ_weekly_source_pop_Kluane_2022.xlsx")
weekly_kluane_1607 <- read_excel("data/source_pop_Kluane_shrub_data/weekly_subsets/160722_EZ_weekly_source_pop_Kluane_2022.xlsx")
weekly_kluane_2407 <- read_excel("data/source_pop_Kluane_shrub_data/weekly_subsets/240722_EZ_weekly_source_pop_Kluane_2022.xlsx")


# Data collected in QHI in summer 2022 (weekly throughout July-August)
weekly_QHI_0208 <- read_excel("data/source_pop_Qiki_shrub_data/weekly_subsets/02082022_EZ_weekly_source_pop_Qiki_2022.xlsx")
weekly_QHI_0808 <- read_excel("data/source_pop_Qiki_shrub_data/weekly_subsets/08082022_EZ_weekly_source_pop_Qiki_2022.xlsx")
weekly_QHI_1907 <- read_excel("data/source_pop_Qiki_shrub_data/weekly_subsets/19072022_EZ_weekly_source_pop_Qiki_2022.xlsx")
weekly_QHI_2407 <- read_excel("data/source_pop_Qiki_shrub_data/weekly_subsets/24072022_EZ_weekly_source_pop_Qiki_2022.xlsx")

# 3. DATA WRANGLING ----

# Merging all common garden weekly subsets
all_weekly_garden_2022 <- rbind(weekly_garden_0307, weekly_garden_1007, weekly_garden_1707,
                                weekly_garden_2307, weekly_garden_3107, weekly_garden_1208,
                                weekly_garden_2706)

# Putting variables in right format
all_weekly_garden_2022$Species <- as.factor(all_weekly_garden_2022$Species)
all_weekly_garden_2022$Site <- as.factor(all_weekly_garden_2022$Site)
all_weekly_garden_2022$Month <- as.numeric(all_weekly_garden_2022$Month)
all_weekly_garden_2022$Day <- as.numeric(all_weekly_garden_2022$Day)
all_weekly_garden_2022$Year_planted <- as.numeric(all_weekly_garden_2022$Year_planted)
all_weekly_garden_2022$Year <- as.numeric(all_weekly_garden_2022$Year)
all_weekly_garden_2022$Canopy_Height_cm <- as.numeric(all_weekly_garden_2022$Canopy_Height_cm)
all_weekly_garden_2022$Width_cm <- as.numeric(all_weekly_garden_2022$Width_cm)
all_weekly_garden_2022$Width_2_cm <- as.numeric(all_weekly_garden_2022$Width_2_cm)
all_weekly_garden_2022$Stem_Elongation_1_mm <- as.numeric(all_weekly_garden_2022$Stem_Elongation_1_mm)
all_weekly_garden_2022$Stem_Elongation_2_mm <- as.numeric(all_weekly_garden_2022$Stem_Elongation_2_mm)
all_weekly_garden_2022$Stem_Elongation_3_mm <- as.numeric(all_weekly_garden_2022$Stem_Elongation_3_mm)
all_weekly_garden_2022$Length_1_mm <- as.numeric(all_weekly_garden_2022$Length_1_mm)
all_weekly_garden_2022$Length_2_mm <- as.numeric(all_weekly_garden_2022$Length_2_mm)
all_weekly_garden_2022$Length_3_mm <- as.numeric(all_weekly_garden_2022$Length_3_mm)

str(all_weekly_garden_2022) # checking variable format

# Creating mean stem elongation, mean leaf length and mean width columns
all_weekly_garden_2022 <- all_weekly_garden_2022 %>% 
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2), 
         biovolume = (Width_cm*Width_2_cm*Canopy_Height_cm))

# Saving dataframe as csv
write.csv(all_weekly_garden_2022, 'data/common_garden_shrub_data/weekly_subsets/all_weekly_garden_2022.csv')

# Merging all Kluane Plateau weekly subsets
all_weekly_kluane_2022 <- rbind(weekly_kluane_0108, weekly_kluane_0907,
                                weekly_kluane_1308, weekly_kluane_1607,
                                weekly_kluane_2407)

str(all_weekly_kluane_2022) # checking variable types

# Reformatting date
all_weekly_kluane_2022$SampleDate <- as.POSIXct(all_weekly_kluane_2022$SampleDate, format = "%d/%m/%Y")

# Creating mean stem elongation, mean leaf length and mean width columns
all_weekly_kluane_2022 <- all_weekly_kluane_2022 %>% 
  filter(Species != "Salix reticulata") %>% 
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2),
         biovolume = (Width_cm*Width_2_cm*Canopy_Height_cm))

all_weekly_kluane_2022 <- all_weekly_kluane_2022[, -c(18:19)] # removing extra columns

# Save as csv
write.csv(all_weekly_kluane_2022, 'data/source_pop_Kluane_shrub_data/weekly_subsets/all_weekly_kluane_2022.csv')

# Merging all QHI weekly subsets
all_weekly_QHI_2022 <- rbind(weekly_QHI_0208, weekly_QHI_0808,
                             weekly_QHI_1907, weekly_QHI_2407)

# Creating mean stem elongation, mean leaf length and mean width columns
all_weekly_QHI_2022 <- all_weekly_QHI_2022 %>% 
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2), 
         biovolume = (Width_cm*Width_2_cm*Canopy_Height_cm))

all_weekly_QHI_2022 <- all_weekly_QHI_2022[-c(67:70), ] # removing NA rows

# Save as csv
write.csv(all_weekly_QHI_2022, 'data/source_pop_Qiki_shrub_data/weekly_subsets/all_weekly_QHI_2022.csv')

# Merging Kluane and QHI data
all_source_pop_2022 <- rbind(all_weekly_kluane_2022, all_weekly_QHI_2022)

# Check variable type
str(all_source_pop_2022)

# Putting variables in right format
all_source_pop_2022$Species <- as.factor(all_source_pop_2022$Species)
all_source_pop_2022$Site <- as.factor(all_source_pop_2022$Site)
all_source_pop_2022$Stem_diameter <- as.numeric(all_source_pop_2022$Stem_diameter)

# Saving as csv
write.csv(all_source_pop_2022, 'data/all_source_pop_2022.csv')


# 4. DATA VISUALISATION summer 2022 ----

# 4.1. COMMON GARDEN ----

# a. Canopy height(2022) ----
(plot_weekly_canopy_2022 <- ggplot(all_weekly_garden_2022)+
   geom_smooth(aes(x = Sample_Date, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_Date, y = Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   facet_grid(cols = vars(Species)) +
   ylab("Canopy Height (cm)") +
   xlab("\nDate") +
   scale_colour_viridis_d(begin = 0.3, end = 0.9) +
   scale_fill_viridis_d(begin = 0.3, end = 0.9) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# b. Stem elongation (2022) ----
(plot_weekly_stem_2022 <- ggplot(all_weekly_garden_2022) +
   geom_smooth(aes(x = Sample_Date, y= mean_stem_elong, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_Date, y= mean_stem_elong, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   facet_grid(cols = vars(Species)) +
   ylab("Stem Elongation (mm)") +
   xlab("\nDate") +
   scale_colour_viridis_d(begin = 0.3, end = 0.9) +
   scale_fill_viridis_d(begin = 0.3, end = 0.9) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# c. Stem diameter (2022) ----
(plot_weekly_diameter_2022 <- ggplot(all_weekly_garden_2022) +
   geom_smooth(aes(x = Sample_Date, y = Stem_diameter, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_Date, y= Stem_diameter, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   facet_grid(cols = vars(Species)) +
   ylab("Stem diameter (mm)") +
   xlab("\nDate") +
   scale_colour_viridis_d(begin = 0.3, end = 0.9) +
   scale_fill_viridis_d(begin = 0.3, end = 0.9) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# d. Shrub width (2022) ----
(plot_weekly_width_2022 <- ggplot(all_weekly_garden_2022) +
   geom_boxplot(aes(x = Site, y= mean_width, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   facet_grid(cols = vars(Species)) +
   ylab("Width (cm)") +
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

# e. Leaf length (2022) ----

(plot_weekly_leaf_2022 <- ggplot(all_weekly_garden_2022) +
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

# f. Biovolume (2022)-----

(plot_weekly_biovol_2022 <- ggplot(all_weekly_garden_2022) +
   geom_boxplot(aes(x = Site, y = biovolume, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   facet_wrap(~Species, scales = "free_y" ) +
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

# facet plot
facet_weekly_traits_2022 <- grid.arrange(plot_weekly_canopy_2022, plot_weekly_stem_2022, 
                                         plot_weekly_diameter_2022, plot_weekly_width_2022, 
                                         ncol=2)

# 4.2. SOURCE POP. VS GARDEN ----
str(all_source_pop_2022)

# a. Canopy height ----
(plot_height_compare_2022 <- ggplot() +
   geom_boxplot(data = all_source_pop_2022, aes(x = Site, y = Canopy_Height_cm, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
   geom_boxplot(data = all_weekly_garden_2022, aes(x = Site, y = Canopy_Height_cm, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
   #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales="free_y")+
   ylab("Canopy height (cm)") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# b. Stem elongation ----
(plot_stem_elong_compare_2022 <- ggplot() +
    geom_boxplot(data = all_source_pop_2022, aes(x = Site, y = mean_stem_elong, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
    geom_boxplot(data = all_weekly_garden_2022, aes(x = Site, y = mean_stem_elong, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
    #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales="free_y")+
    ylab("Stem elongation (mm)") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))


# c. Stem diameter ----

(plot_diameter_compare_2022 <- ggplot() +
   geom_boxplot(data = all_source_pop_2022, aes(x = Site, y = Stem_diameter, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
   geom_boxplot(data = all_weekly_garden_2022, aes(x = Site, y = Stem_diameter, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
   # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales="free_y")+
    ylab("Stem diameter (mm)") +
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

# d. Shrub width ----

(plot_width_compare_2022 <- ggplot() +
   geom_boxplot(data = all_source_pop_2022, aes(x = Site, y = mean_width, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
   geom_boxplot(data = all_weekly_garden_2022, aes(x = Site, y = mean_width, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
   facet_grid(cols = vars(Species)) +
   ylab("Shrub width (m)") +
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

# e. Leaf length -----

(plot_leaf_compare_2022 <- ggplot() +
   geom_boxplot(data = all_source_pop_2022, aes(x = Site, y = mean_leaf_length, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
   geom_boxplot(data = all_weekly_garden_2022, aes(x = Site, y = mean_leaf_length, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
   #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales="free_y")+
   ylab("Leaf length (mm)") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# f. Biovolume ----
(plot_biovol_compare_2022 <- ggplot() +
   geom_boxplot(data = all_source_pop_2022, aes(x = Site, y = biovolume, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
   geom_boxplot(data = all_weekly_garden_2022, aes(x = Site, y = biovolume, fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Biovoume (cm3)") +
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


# facet plot
facet_weekly_compare_2022 <- grid.arrange(plot_leaf_compare_2022, plot_width_compare_2022, 
                                         plot_diameter_compare_2022, plot_height_compare_2022,
                                         ncol=2)

facet_weekly_compare_2022_b <- grid.arrange(plot_diameter_compare_2022, plot_stem_elong_compare_2022, plot_height_compare_2022,
                                            ncol=1)
                                          

# TO DO ----
# Questions: do patterns focused on 2022 match long-term data?
## Make MEAN MONTHLY or WEEKLY TEMP etc. to compare to monthly shrub measurements?
## Need to extract climate data for longterm shrub measurements OR do I not need this because we know that
# Kluane is warmer than QHI, so no relationships with climate needed
