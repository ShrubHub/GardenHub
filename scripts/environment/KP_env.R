# KP environmental script - HOBO 
# Script by Erica Zaja, created on 08/12/2022
# Last updated: 23/01/2023

# 90% snow free plateau from phenocams 2022
# June 18, 15, 21, 17, 18, 19, 26, 21 --> mean = June 18th
# setting start of summer on KP: June 18th
# End of summer: ??? look at snow return date 

# Workflow
# 1. Explore elevation, filter anything below treeline 
# i.e. keep anything between 1300-2000m (range where cuttings taken)
# 2. Explore date ranges, filter dates to: JULY 
# 4. Make means for each year for july

# LIBRARIES ----
library(MazamaCoreUtils)
library(arsenal)
library(readxl)
library(tidyverse)

# LOADING DATA ----

# soil temp 2015-2017 (1 july 2015- 1 January 2017)
# Formatted_temps <- read_csv("data/hobo/Formatted_temps.csv")

# data HOBO station summarized by Haydn Thomas 
Haydn_summary <- read.csv("data/environment/Formatted_temps_shrubhub.csv")

# DATA WRANGLING -------
unique(Haydn_summary$Plotcode)
unique(Haydn_summary$Year) # [1] 2015 2016 2017
# filter only KP data 
str(Haydn_summary)
alpine <- c("Kluane_Plateau_1", "Kluane_Plateau_2", 
            "PC16_1","PC16_2", "PP16_1", "PP16_2", 
            "Kluane_10", "Kluane_5", "Kluane_6",
            "Kluane_7", "Kluane_8")

KP_hobo_pheno_2017 <- Haydn_summary %>% 
  filter(Plotcode %in% alpine )  %>% 
  dplyr::select(-X) # drop sort column 

unique(KP_hobo_pheno_2017$Plotcode)
unique(KP_hobo_pheno_2017$Year)

range(KP_FullTOMST_2022$Datetime_UTC) # 1 june 2022 - 15 aug 2022

# Monthly means 

# Daily mean surface temperature
KP_mean_monthly_temp_hobo <- KP_hobo_pheno_2017  %>%
  group_by(Year, Month) %>% 
  summarise(mean_temp = mean(Temperature))

KP_july_temp_hobo <- KP_mean_monthly_temp_hobo %>%
  filter(Month == 7) 

# DONE ! 
# 2015 = 4.568919
# 2016 = 8.252268
# 2017 = 4.763441

# SUMMARY MEANS with TOMST -----
# 2022
KP_FullTOMST_2022 <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv")


