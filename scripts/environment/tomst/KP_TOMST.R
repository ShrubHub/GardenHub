### Processing TOMST logger data from Kluane Plateau (KP) (2022)
### Script by Erica Zaja, based on script by Elise Gallois
### Last updated: 10/10/2022

### 1. LOADING LIBRARIES -----
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)

# Loading dataset

tomst_kp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv") 

### 2. READING DATA ----
# NB no need to run this anymore, data already wrangled
# Using function created by Elise Gallois to read the TOMST raw data 
read_tms4 <- function(file) {
  
  # Extract serial number from filename
  serial <- file 
  print(file)
  
  # Read the data file
  data <- read_delim(file, delim = ";",
                     col_names = F, 
                     locale=locale(decimal_mark = ",")) 
  
  
  # Check file has contents. Empty files due to bad data download only have "File is empty" as text. 
  if (ncol(data) > 1) {
    # Create vector of column names
    vars <- c("Index", "Datetime_UTC", "TimeZone", "T1: Soil sensor", "T2: Surface sensor", "T3: Top sensor", "SoilMoistureCount", "shake",
              "errFlag", "empty")
    
    # Format data for output
    names(data) <- vars
    
    data_with_ID <- data  %>% 
      mutate(SerialID = serial) %>% 
      select(SerialID, everything()) %>% 
      mutate(Datetime_UTC = lubridate::parse_date_time(Datetime_UTC,orders = c("%Y.%m.%d %H:%M")))
    
  } else {
    print("empty file")
    data_with_ID <- NULL
  }
  
  
  return(data_with_ID)
}

# Read-in data files
# TOMST data (12 loggers in total) from Kluane plateau collected on August 15th 2022
tomst <- "data/tomst/Kluane_Plateau_TOMST_15August2022" 
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
kp_data <- map_dfr(files, read_tms4)

# change date (GMT to newty time) - 6 hours time difference 
kp_data$Datetime_UTC <- kp_data$Datetime_UTC - hours(6)


### 3. DATA MANIPULATION ----

tomst_kp <-  kp_data %>% 
filter(Datetime_UTC > lubridate::ymd_hm("2022-06-01 15:00")) %>% # keeping summer 2022 values only 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

# Saving as csv
write.csv(tomst_kp, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv", row.names = FALSE)

# Reading in file 
tomst_kp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv")

### 4. DATA VISUALISATION ----

(kp_tomst_summary <- ggplot(tomst_kp, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "none"))

# Historgram of variables
(kp_hist_tomst <- ggplot(tomst_kp) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free") + 
  theme(legend.position = "none"))

### 5. EXPLORING VARIABLES ----

# a. Surface temperature (T2: Surface sensor) ----

# get date column
kp_data <- tomst_kp %>% 
  mutate(Date = lubridate::date(Datetime_UTC))

# Daily mean surface temperature
KP_mean_daily_temp <- kp_data  %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_temp$mean_temp)
# -0.3047619 11.9635417
# warmest: 4th July, coldest: 1st June

# subsetting to start the season on June 18th (post snow melt)
season_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-15")

mean(season_surface_temp$mean_temp)
# 7.188471

# Save as csv
write.csv(KP_mean_daily_temp, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_temp.csv", row.names = FALSE)

# Monthly means
# filter out june
june_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-30")

mean(june_surface_temp$mean_temp) # 6.274342

# filter out july
july_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_surface_temp$mean_temp) # 7.805064

# filter out august
aug_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_surface_temp$mean_temp) # 6.706426

# Plot daily mean temp over summer 2022
(kp_mean_daily_temp <- ggplot(KP_mean_daily_temp, aes(x = Date, y = mean_temp)) +
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

# histogram
(kp_mean_daily_temp_hist <- ggplot(KP_mean_daily_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean surface temperature (°C) (2022)") +
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
KP_mean_daily_top_sensor <- kp_data  %>%
  filter(Variable %in% "T3: Top sensor") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_top_sensor$mean_temp)
# -0.2578869 16.6310764
# warmest: 4th July, coldest: 1st June

# subsetting to start the season on June 18th (post snow melt)
season_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-15")

mean(season_topsensor_temp$mean_temp)
# 8.943557

# Save as csv
write.csv(KP_mean_daily_top_sensor, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_top_sensor.csv", row.names = FALSE)

# Monthly means
# filter out june
june_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-30")

mean(june_topsensor_temp$mean_temp) # 9.330219

# filter out july
july_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_topsensor_temp$mean_temp) # 9.465119

# filter out august
aug_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_topsensor_temp$mean_temp) # 7.530555

# Plot daily mean top sensor temp over summer 2022
# time series
(kp_mean_daily_top_sensor <- ggplot(KP_mean_daily_top_sensor, aes(x = Date, y = mean_temp)) +
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

# histogram
(kp_mean_daily_top_sensor_hist <- ggplot(KP_mean_daily_top_sensor, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean above-surface temperature (°C) (2022)") +
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
KP_mean_daily_soil_temp <- kp_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_soil_temp$mean_temp)
# -0.4803571  5.4093424
# warmest: 13th August, coldest: 1st June 

# subsetting to start the season on June 18th (post snow melt)
season_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-15")

mean(season_soil_temp$mean_temp)
# 3.556209


# Save as csv
write.csv(KP_mean_daily_soil_temp, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_soil_temp.csv", row.names = FALSE)

# Monthly means
# filter out june
june_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-30")

mean(june_soil_temp$mean_temp) # 1.361704

# filter out july
july_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp$mean_temp) # 3.925377

# filter out aug
aug_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_soil_temp$mean_temp) #  4.695168

# Plot daily mean soil temp over summer 2022
# timeseries
(kp_mean_daily_soil_temp <- ggplot(KP_mean_daily_soil_temp, aes(x = Date, y = mean_temp)) +
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

# histogram
(kp_mean_daily_soil_temp_hist <- ggplot(KP_mean_daily_soil_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean soil temperature (°C) (2022)") +
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
KP_mean_daily_soil_moist <- kp_data  %>%
  filter(Variable %in% "SoilMoistureCount") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_moist = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_moist) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_soil_moist$mean_moist)
# 1762.584 2533.140
# driest:8th June, wettest: 2nd August
mean(KP_mean_daily_soil_moist$mean_moist)
# 1983.971 ? no clue about moisture unit

# Monthly means
# filter out june
june_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_soil_moist$mean_moist) # 2048.208

# filter out july
july_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_moist$mean_moist) # 1948.594

# filter out august
aug_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_soil_moist$mean_moist) # 1928.611

# Save as csv
write.csv(KP_mean_daily_soil_moist, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_soil_moist.csv", row.names = FALSE)


# Plot daily mean soil moisture over summer 2022
# time series
(kp_mean_daily_soil_moist <- ggplot(KP_mean_daily_soil_moist, aes(x = Date, y = mean_moist)) +
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

# histogram
(kp_mean_daily_soil_moist_hist <- ggplot(KP_mean_daily_soil_moist, aes(x = mean_moist)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean soil moisture (2022)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Panel of env variables
# timeseries panel
facet_env_kp <- grid.arrange(kp_mean_daily_soil_moist, kp_mean_daily_soil_temp, 
                             kp_mean_daily_temp, kp_mean_daily_top_sensor, ncol=2)

# hist panel
facet_env_kp_hist <- grid.arrange(kp_mean_daily_soil_moist_hist, kp_mean_daily_soil_temp_hist, 
                             kp_mean_daily_temp_hist, kp_mean_daily_top_sensor_hist, ncol=2)


