# KP environmental script - HOBO 
# Script by Erica Zaja, created on 08/12/2022
# Last updated: 23/01/2023

# 90% snow free plateau from phenocams 2022
# June 18, 15, 21, 17, 18, 19, 26, 21 --> mean = June 18th
# setting start of summer on KP: June 18th
# End of summer: August 28th 
# Growing season length =71 days

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

# 1. SURFACE TEMP -----
# data HOBO station summarized by Haydn Thomas  NB THIS IS SOIL DATA
Haydn_summary <- read.csv("data/environment/Formatted_temps_shrubhub.csv")

# DATA WRANGLING -------
# NB THIS IS ALL SOIL DATA!
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
# July mean temperatures (HOBO):
# 2015 = 4.568919
# 2016 = 8.252268
# 2017 = 4.763441

# SUMMARY MEANS with TOMST -----
# FIX THESE. SOME IS SOIL DATA
# July mean temperatures (average of top sensor, surface temp) (TOMST):
# 2021 = mean(11.20757, 8.504422) = 9.855996
# 2022 = mean(9.465119, 7.805064) = 8.635091

# Overall mean (2015-2022):
# mean(4.568919, 8.252268, 4.763441, 9.855996, 8.635091)
# 7.215143  

# DONE 

