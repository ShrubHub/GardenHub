#### COMMON GARDEN vs ENVIRONMENTAL VARIABLES 2022
#### Script by Erica Zaja, created 12/10/22
### Last updated: 12/10/2022 

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)

# 2. LOADING DATA ----

# Environmental variables (TOMST data for summer 2022)
CG_FullTOMST_2022 <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_FullTOMST_2022.csv")
CG_mean_daily_temp <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_temp.csv")
CG_mean_daily_top_sensor <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_top_sensor.csv")
CG_mean_daily_soil_temp <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_soil_temp.csv")

# ALL common garden subsets from summer 2022
all_weekly_garden_2022 <- read_csv("data/common_garden_shrub_data/weekly_subsets/all_weekly_garden_2022.csv")

# 3. DATA WRANGLING -----
# Renaming columns so they match with Common garden data
CG_mean_daily_temp <- CG_mean_daily_temp %>%
  # mutate(Month = format(Date, "%m"), 
    #     Day = format(Date,"%d"),
     #    Year = format(Date, "%Y")) %>%
  rename(Sample_Date = Date)

CG_mean_daily_top_sensor <- CG_mean_daily_top_sensor %>%
  rename(Sample_Date = Date, 
         mean_top_temp = mean_temp)

CG_mean_daily_soil_temp <- CG_mean_daily_soil_temp %>%
  rename(Sample_Date = Date,
         mean_soil_temp = mean_temp)

# Merging common garden data with TOMST data, by date
all_garden_env_2022 <- merge(all_weekly_garden_2022, CG_mean_daily_temp, by ="Sample_Date")
all_garden_env_2022 <- merge(all_garden_env_2022, CG_mean_daily_top_sensor, by ="Sample_Date")
all_garden_env_2022 <- merge(all_garden_env_2022, CG_mean_daily_soil_temp, by ="Sample_Date")

unique(all_garden_env_2022$mean_temp)

# 4. DATA VISUALISATION -----

# Trying to plot differnet figures of traits vs env variables
(plot_canopy_vs_temp_2022 <- ggplot(all_garden_env_2022) +
    geom_smooth(aes(x = mean_temp, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "lm")) +
    geom_point(aes(x = mean_temp, y = Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Canopy Height (cm)") +
    xlab("\nDaily mean air temperature") +
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

(plot_mean_leaf_vs_temp_2022 <- ggplot(all_garden_env_2022) +
    geom_smooth(aes(x = mean_soil_temp, y = mean_leaf_length, colour = Site, fill = Site, group = Site, method = "lm")) +
    geom_point(aes(x = mean_soil_temp, y = mean_leaf_length, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Leaf length (cm)") +
    xlab("\nDaily mean soil temperature") +
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

(plot_mean_stem_vs_temp_2022 <- ggplot(all_garden_env_2022) +
    geom_smooth(aes(x = mean_soil_temp, y = mean_leaf_length, colour = Site, fill = Site, group = Site, method = "lm")) +
    geom_point(aes(x = mean_soil_temp, y = mean_leaf_length, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("Leaf length (cm)") +
    xlab("\nDaily mean air temperature") +
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


# create month day year column on the full tomst, then filter for each env variable
# then join with the all weekly garden

lm1 <- lm(mean_leaf_length~mean_temp, data =all_garden_env_2022)
summary(lm1)
