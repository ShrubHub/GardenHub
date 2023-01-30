### QHI environmental conditions
# Script by Madi 08/12/2022
# last updated: 13/01/2023

# LIBRARIES ---- 

library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(readr)
library(tidyverse)

# TOMST ----
# from 2022 27 July - 16 August 2022 

# data largely wrangled already 
# tomst_QHI_mean_daily_temp <- read.csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_temp.csv")

# HOBO ---- 
# data from 2022 
# data collection and processing by Elise Gallois 
# script adapted from Elise Gallois truly she did it all
hobo_QHI_2022 <- read.csv("data/hobo/QHI_FullHOBO_2022.csv")
hobo_QHI_2022$Serial_No <- substr(hobo$SerialID, 54, 61)

# drop unnecessary columns and keep only temp data
# make separate date and hour column 
# keep only observations post July 27 2022 aka when they were set up 
hobo_clean_2022 <- hobo_QHI_2022 %>% 
  filter(Variable == "Temperature") %>% 
  select(-c(End.of.File, Button.Down, Host.Connected, Button.Up, Index)) %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  mutate(hour = lubridate::hour(Datetime_UTC)) %>% 
  filter(Date > lubridate::ymd("2022-07-27"))
# how many sensors 
length(unique(hobo_clean_2022$Serial_No))
  
# summarize by day, group all sensors together 
hobo_mean_temp_2022 <- hobo_clean_2022 %>%
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE)) %>% 
  ungroup()

mean(hobo_mean_temp_2022$mean_temp) # 9.853633

# QHI soil temp 2015-16 ----
# data from phenology ridge HOBO station 
# summarized by Haydn Thomas 

Haydn_summary <- read.csv("data/environment/Formatted_temps_shrubhub.csv") # NB THIS IS SOIL DATA
metadata_summary <-  read.csv("data/environment/metadata_shrubhub.csv")

# match metadata & enviro data
summary <- full_join(Haydn_summary, metadata_summary, by = "Plotcode")
# filter only QHI data 
unique(summary$Plotcode)
# mix of HOBO and iButton - combine for now 
QHI_enviro_summary <- summary %>% 
  dplyr::filter(Plotcode %in% c("QHI_phenology_plot", "HH_1", "HH_2", "HK_1", "HK_2", 
                                "HR_1", "HR_2", "HS_1", "HS_2", "HV_1", "HV_2")) %>% 
  dplyr::select(-X) # drop sort column 

july_QHI_enviro <- QHI_enviro_summary %>% 
  filter(Month == "7")
july_QHI_enviro_mean <- july_QHI_enviro  %>%
  group_by(Year) %>% 
  summarise(mean_temp = mean(Temperature))

# 2015: 9.05
# 2016: 6.79

# SUMMARY MEANS with TOMST -----

# TOMST 2022 mean soil temp 
# 2022: 3.323774

# HOBO + TOMST mean soil temp july 
soil_temp_QHI <- c(9.05,6.79, 3.323774)
mean(soil_temp_QHI)# 6.387925

# phenology ridge hobo data from 2017 ----
pheno_hobo <- read_csv("data/environment/QHI/HOBO_Phenology_ridge_11th_Aug_2017.csv")
str(pheno_hobo)

# clean up 
pheno_hobo_working <- pheno_hobo %>% 
  rename("Air_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Air Temp)",
         "Canopy_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Canopy Temp)",
         "Ground_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Ground Temp)",
         "Soil_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Soil Temp)", 
         "Date_time" = "Date Time, GMT-06:00") %>% 
  dplyr::select(-c("Good Battery (LGR S/N: 10737643)", "Host Connected (LGR S/N: 10737643)", 
            "End Of File (LGR S/N: 10737643)"))
# date issue 
# normal dates 
pheno_hobo_dates_good <- pheno_hobo_working %>% 
  filter(Date_time <= "08/01/17" )

# wrong dates 
pheno_hobo_dates_bad <- pheno_hobo_working %>% 
  filter(Date_time >= "08/01/17" )

pheno_hobo_dates_bad$Date_time <- ymd_hm(pheno_hobo_dates_bad$Date_time)
pheno_hobo_dates_bad$day <- month(as.POSIXlt(pheno_hobo_dates_bad$Date_time, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_bad$month_wrong <- year(as.POSIXlt(pheno_hobo_dates_bad$Date_time, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_bad$year <- day(as.POSIXlt(pheno_hobo_dates_bad$Date_time, format="%Y-%m-%d %H:%M"))
# subtract 2000 from this value to get correct month value 
pheno_hobo_dates_bad_1  <- pheno_hobo_dates_bad %>% 
  mutate(Month = month_wrong - 2000) %>% 
  select(-c(month_wrong)) %>% 
  mutate(year = year + 2000)

pheno_hobo_dates_bad_1$date <- paste(pheno_hobo_dates_bad_1$Month,
                                     pheno_hobo_dates_bad_1$day, 
                                     pheno_hobo_dates_bad_1$year,
                                                    sep="/") %>% mdy() %>% 
  as.Date()

pheno_hobo_dates_bad_1$date_new <- format(as.Date(pheno_hobo_dates_bad_1$date, format = "%m-%d-%Y"), "%m/%d/%y")
# drop confusing old columns
pheno_hobo_dates_bad_1 <- pheno_hobo_dates_bad_1 %>% 
  select(-c(Date_time))

# reclass date to drop H:M and rename good dataframe 
pheno_hobo_dates_good$Date_time_1 <- ymd_hm(pheno_hobo_dates_good$new_date_1)
pheno_hobo_dates_good$new_date_1 <-  parse_date_time(pheno_hobo_dates_good$Date_time, "%m/%d/%y %I:%M:%S %p")
pheno_hobo_dates_good$Month <- month(as.POSIXlt(pheno_hobo_dates_good$new_date_1, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_good$year <- year(as.POSIXlt(pheno_hobo_dates_good$new_date_1, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_good$day <- day(as.POSIXlt(pheno_hobo_dates_good$new_date_1, format="%Y-%m-%d %H:%M"))

pheno_hobo_dates_good$date_new <- format(as.Date(pheno_hobo_dates_good$new_date_1, format = "%m-%d-%Y %I:%M:%S %p"), "%m/%d/%y")

pheno_hobo_dates_good_1 <- pheno_hobo_dates_good %>% 
  select(-c(Date_time))

pheno_hobo_dates_good_1$day <- as.numeric(pheno_hobo_dates_good_1$day)
# merge good and bad dates
pheno_hobo_all <- full_join(pheno_hobo_dates_good_1, pheno_hobo_dates_bad_1, 
                            by = c("#", 
                            "Air_temp_C", 
                            "Canopy_temp_C", 
                            "Ground_temp_C", 
                            "Soil_temp_C", 
                            "Month", 
                            "day", 
                            "year", 
                            "date_new"))
# messy but it works for this 

# filter july observations 
july_QHI_pheno <- pheno_hobo_all %>% 
  filter(Month == "7")
july_QHI_pheno_mean <- july_QHI_pheno  %>%
  group_by(year) %>% 
  summarise(mean_air_temp = mean(Air_temp_C),
            mean_surface_temp = mean(Canopy_temp_C), 
            mean_canopy_temp = mean(Ground_temp_C), 
            mean_ground_temp = mean(Soil_temp_C))



