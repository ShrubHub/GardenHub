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

tomst_QHI_mean_daily_temp <- read.csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_temp.csv")


# HOBO ---- 
# data from 2022 
# data collection and processing by Elise Gallois 
hobo_QHI_2022 <- read.csv("data/hobo/QHI_FullHOBO_2022.csv")

hobo_summary <- ggplot(hobo_QHI_2022, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic()

hobo_summary + theme(legend.position = "none")

# historgram of variables
hist_hobo <- ggplot(hobo_QHI_2022) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free")

hobo_sort <- hobo_QHI_2022 %>% 
  mutate(Date = lubridate::parse_date_time(Datetime_UTC, orders = c("%m/%d/%Y")))


str(hobo_QHI_2022)

# iButtons ---- 
# ignore for now 
# eclectic mix of data  