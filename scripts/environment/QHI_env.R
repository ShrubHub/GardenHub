### QHI environmental conditions
# Script by Madi 08/12/2022
# last updated: 14/12/2022

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


