#### PHENOCAMS: common garden, Kluane and QHI
#### Script by Erica Zaja, created 14/12/22
### Last updated: 14/12/22

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)
library(gridExtra)

# 2. LOADING DATA ----
KP_phenocams_2021 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2021.csv")
QHI_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/QHI_phenocams_2022.csv")
KP_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2022.csv")
Phenocam_Datasheet_QHI <- read_excel("data/phenology/phenocam_pics/Phenocam_Datasheet_QHI.xlsx")
# add common garden datasheet when available


# 3. DATA WRANGLING-----

KP_phenocams_2022 <-KP_phenocams_2022[1:9,] # removing loads of NAs
KP_phenocams_2021 <- KP_phenocams_2021[1:9,]

# renaming columns 
KP_phenocams_2022_wrangle <- KP_phenocams_2022 %>%
  rename("Plot" = "PLOT", "Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "First_greening" = "First signs of greening",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "First_leaf_bud_burst" = "First leaf bud burst",
         "Half_leaves_green" ="50% Leaves Green",
         "All_leaves_green" = "100% Leaves Green",
         "First_leaf_yellow"= "First leaf yellow",
         "Half_leaves_yellow" = "50% Leaves Yellow",
         "All_leaves_yellow" = "100% Leaves Yellow",
         "Salix_pulchra_bud_burst" = "Salix pulchra First Leaf Bud Burst",
         "Salix_pulchra_first_yellow"= "Salix pulchra First Yellowing of Leaves",
         "Salix_pulchra_last_yellow" = "Salix pulchra Last Leaf Turns Yellow",
         "Salix_rich_bud_burst" = "Salix richardsonii First Leaf Bud Burst",
         "Salix_rich_first_yellow" = "Salix richardsonii First Yellowing of Leaves",
         "Salix_rich_last_yellow" = "Salix richardsonii Last Leaf Turns Yellow") %>%
  select(Plot, Year, Viewshed, NOTES, Snow_melt, All_snow_free, First_greening, Snow_return_EoS,
         Half_snow_cover_EoS, Full_snow_cover_EoS, First_leaf_bud_burst, 
         Half_leaves_green, All_leaves_green, First_leaf_yellow, Half_leaves_yellow, 
         All_leaves_yellow, Salix_pulchra_bud_burst, Salix_pulchra_first_yellow, 
         Salix_pulchra_last_yellow, Salix_rich_bud_burst, Salix_rich_first_yellow, 
         Salix_rich_last_yellow)

KP_phenocams_2021_wrangle <- KP_phenocams_2021 %>%
  rename("Plot" = "PLOT", "Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "First_leaf_bud_burst" = "First leaf bud burst",
         "Half_leaves_green" ="50% Leaves Green",
         "All_leaves_green" = "100% Leaves Green",
         "First_leaf_yellow"= "First leaf yellow",
         "Half_leaves_yellow" = "50% Leaves Yellow",
         "All_leaves_yellow" = "100% Leaves Yellow",
         "Salix_pulchra_bud_burst" = "Salix pulchra First Leaf Bud Burst",
         "Salix_pulchra_first_yellow"= "Salix pulchra First Yellowing of Leaves",
         "Salix_pulchra_last_yellow" = "Salix pulchra Last Leaf Turns Yellow",
         "Salix_rich_bud_burst" = "Salix richardsonii First Leaf Bud Burst",
         "Salix_rich_first_yellow" = "Salix richardsonii First Yellowing of Leaves",
         "Salix_rich_last_yellow" = "Salix richardsonii Last Leaf Turns Yellow") %>%
  select(Plot, Year, Viewshed, NOTES, Snow_melt, All_snow_free, Snow_return_EoS,
         Half_snow_cover_EoS, Full_snow_cover_EoS, First_leaf_bud_burst, 
         Half_leaves_green, All_leaves_green, First_leaf_yellow, Half_leaves_yellow, 
         All_leaves_yellow, Salix_pulchra_bud_burst, Salix_pulchra_first_yellow, 
         Salix_pulchra_last_yellow, Salix_rich_bud_burst, Salix_rich_first_yellow, 
         Salix_rich_last_yellow)

# merge 2021 and 2022 kp datasets?
KP_phenocams_2021_2022 <- bind_rows(KP_phenocams_2021_wrangle, KP_phenocams_2022_wrangle)

