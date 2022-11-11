### Processing TOMST logger data from Qikiqtaruk-Hershel Island (QHI) (2022)
### Script by Erica Zaja, based on script by Elise Gallois
### Last updated: 11/10/2022

### 1. LOADING LIBRARIES -----
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)

### 2. READING DATA ----

# TOMST data (40 loggers in total) from Common Garden collected on August 17th 2022
tomst_qhi <- read_csv("data/tomst/QHI_TOMST_August2022/QHI_FullTOMST_2022.csv") # data by Elise Gallois

### 2. DATA VISUALISATION ----

(qhi_tomst_summary <- ggplot(tomst_qhi, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic() + 
  theme(legend.position = "none"))

# Historgram of variables
(qhi_hist_tomst <- ggplot(tomst_qhi) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free") +
  theme(legend.position = "none"))


### 3. EXPLORING VARIABLES ----

# a. Surface temperature (T2: Surface sensor) ----

# get date column
qhi_data <- tomst_qhi %>% 
  mutate(Date = lubridate::date(Datetime_UTC))

str(qhi_data)

# see top 5 warmest days
QHI_mean_daily_temp <- qhi_data %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  
  glimpse()

range(QHI_mean_daily_temp$mean_temp) # with filter above running
#  2.488281 18.893772
# warmest: 5th August, coldest: 29th July

# Saving as csv
write.csv(QHI_mean_daily_temp, file = "data/tomst/QHI_TOMST_August2022/QHI_mean_daily_temp.csv", row.names = FALSE)

# Plot daily mean temp over summer 2022
(qhi_mean_daily_temp <- ggplot(QHI_mean_daily_temp, aes(x = Date, y = mean_temp)) +
    geom_line()+ 
    ylab("Daily mean surface temperature (°C)") +
    xlab("Date (2022)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

#### b. Above-surface temperature (T3: Top sensor) ----

# Daily mean top sensor temperature (10cm above surface)
QHI_mean_daily_top_sensor <- qhi_data  %>%
  filter(Variable %in% "T3: Top sensor") %>% 
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(QHI_mean_daily_top_sensor$mean_temp)
# 2.002572 18.769251
# warmest: 22st Jult , coldest: 29th July

# Save as csv
write.csv(QHI_mean_daily_top_sensor, file = "data/tomst/QHI_TOMST_August2022/QHI_mean_daily_top_sensor.csv", row.names = FALSE)

# Plot daily mean top sensor temp over summer 2022
(qhi_mean_daily_top_sensor <- ggplot(QHI_mean_daily_top_sensor, aes(x = Date, y = mean_temp)) +
    geom_line()+ 
    ylab("Daily mean above-surface temperature (°C)") +
    xlab("Date (2022)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

#### c. Soil temperature (T1: Soil sensor) ----

# Daily mean soil temperature 
QHI_mean_daily_soil_temp <- qhi_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(QHI_mean_daily_soil_temp$mean_temp)
# 2.543413 17.772561
# warmest: 22nd July, coldest: 29th July

# Save as csv
write.csv(QHI_mean_daily_soil_temp, file = "data/tomst/QHI_TOMST_August2022/QHI_mean_daily_soil_temp.csv", row.names = FALSE)

# Plot daily mean soil temp over summer 2022
(qhi_mean_daily_soil_temp <- ggplot(QHI_mean_daily_soil_temp, aes(x = Date, y = mean_temp)) +
    geom_line()+ 
    ylab("Daily mean soil temperature (°C)") +
    xlab("Date (2022)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

#### d. Soil moisture (SoilMoistureCount) ----

# Daily mean soil moisture
QHI_mean_daily_soil_moist <- qhi_data  %>%
  filter(Variable %in% "SoilMoistureCount") %>% 
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_moist = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_moist) %>%  # see top 5 warmest days
  glimpse()

range(QHI_mean_daily_soil_moist$mean_moist)
# 930.2683 2340.4636
# driest:24th July, wettest: 9th August

# Save as csv
write.csv(QHI_mean_daily_soil_moist, file = "data/tomst/QHI_TOMST_August2022/QHI_mean_daily_soil_moist.csv", row.names = FALSE)

# Plot daily mean soil moisture over summer 2022
(qhi_mean_daily_soil_moist <- ggplot(QHI_mean_daily_soil_moist, aes(x = Date, y = mean_moist)) +
    geom_line()+ 
    ylab("Daily mean soil moisture") +
    xlab("Date (2022)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Panel of env variables
facet_env_QHI <- grid.arrange(qhi_mean_daily_soil_moist, qhi_mean_daily_soil_temp, 
                             qhi_mean_daily_temp, qhi_mean_daily_top_sensor, ncol=2)

