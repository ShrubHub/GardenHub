### Processing TOMST logger data from Kluane Plateau (KP) (2022)
### Script by Erica Zaja, based on script by Elise Gallois
### Last updated: 19/12/2022 by Madi 

### 1. LOADING LIBRARIES -----
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)

# Loading dataset

tomst_kp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv") 

### 2. READING DATA ----
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
#filter(Datetime_UTC > lubridate::ymd_hm("2022-06-01 15:00")) %>% # keeping summer 2022 values only 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value") 
# Saving as csv
write.csv(tomst_kp, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv", row.names = FALSE)

# Reading in file 
# NOTE THIS FILE ONLY HAS 2022 DATA, MUST LOAD ABOVE, FILE TOO LARGE TO COMMIT 
# tomst_kp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv")
  
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
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  filter(Date >= '2021-07-29') # only keep dates after sensors were deployed

# Daily mean surface temperature
KP_mean_daily_temp <- kp_data  %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2021-07-29")) %>% # when they were deployed
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_temp$mean_temp)
# -3.802463 11.828776

# subsetting to start the season on June 18th 2022 (post 2022 snow melt)
season_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-15")

mean(season_surface_temp_2022$mean_temp)
# 7.188471

# subsetting to growing seasons 2021 and 2022 
season_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2021-07-28" & Date <= "2021-08-31" | # 2021 growing season
         Date >= "2022-06-18" & Date <= "2022-08-15") # 2022 growing season

mean(season_surface_temp$mean_temp)
# 6.695886

# Save as csv
write.csv(KP_mean_daily_temp, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_temp.csv", row.names = FALSE)
KP_mean_daily_temp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_temp.csv")

# Monthly means
# filter out june 2022, only 2022 because tomst wasnt in place in June 2021
june_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-30")

mean(june_surface_temp_2022$mean_temp) # 6.274342

# filter out july 2022
july_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_surface_temp_2022$mean_temp) # 7.805064

july_surface_temp_2021 <- KP_mean_daily_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_surface_temp_2021$mean_temp) # 8.504422

july_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31" | # sensor deployed midway through month
    Date >= "2022-07-01" & Date <= "2022-07-31") 

mean(july_surface_temp$mean_temp) # 7.847449

# filter out august 2022
aug_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_surface_temp_2022$mean_temp) # 6.706426
# august 2021 and 2022
aug_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
    Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_surface_temp$mean_temp) # 5.988896

# Plot daily mean temp over full year
(kp_mean_daily_temp <- ggplot(KP_mean_daily_temp, aes(x = Date, y = mean_temp)) +
    geom_line()+ 
    ylab("Daily mean surface temperature (°C)") +
    xlab("Date") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# over summer 2022
(kp_mean_daily_temp_summer <- ggplot(season_surface_temp_2022, aes(x = Date, y = mean_temp)) +
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

# histogram of summer temperatures 
(kp_mean_daily_temp_hist <- ggplot(season_surface_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean surface temperature (°C)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# b. Above-surface temperature (T3: Top sensor) ----

# Daily mean top sensor temperature (10cm above surface)
KP_mean_daily_top_sensor <- kp_data  %>%
  filter(Variable %in% "T3: Top sensor") %>% 
  filter(Date > lubridate::ymd("2021-07-29"))   %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_top_sensor$mean_temp)
# for all TOMST
# -7.625651 16.4200304

# subsetting to start the season on June 18th (post snow melt)
season_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-15")
range(season_topsensor_temp_2022$mean_temp) # for summer 2022: 
# 3.703179 16.420030
mean(season_topsensor_temp_2022$mean_temp)
# 8.943557

# 2021 and 2021 data 
season_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-29" & Date <= "2021-08-31" | # 2021 growing season
           Date >= "2022-06-18" & Date <= "2022-08-15") # 2022 growing season
mean(season_topsensor_temp$mean_temp)
# 8.203

# Save as csv
write.csv(KP_mean_daily_top_sensor, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_top_sensor.csv", row.names = FALSE)
KP_mean_daily_top_sensor <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_top_sensor.csv")

# Monthly means
# filter out june 2022
june_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-30")

mean(june_topsensor_temp_2022$mean_temp) # 9.330219

# filter out july 2022
july_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_topsensor_temp_2022$mean_temp) # 9.465119

july_topsensor_temp_2021 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_topsensor_temp_2021$mean_temp) # 11.20757

july_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31" |
    Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_topsensor_temp$mean_temp) # 9.570722

# filter out august 2022
aug_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_topsensor_temp_2022$mean_temp) # 7.530555
# 2021 and 2022 august 
aug_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
           Date >= "2022-08-01" & Date <= "2022-08-15")
mean(aug_topsensor_temp$mean_temp) # 6.815827

# Plot daily mean top sensor temp over 
# time series
(kp_mean_daily_top_sensor <- ggplot(KP_mean_daily_top_sensor, aes(x = Date, y = mean_temp)) +
    geom_line()+ 
    ylab("Daily mean above-surface temperature (°C)") +
    xlab("Date") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# over summer 2022
(kp_mean_daily_top_sensor_2022 <- ggplot(season_topsensor_temp_2022, aes(x = Date, y = mean_temp)) +
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
(kp_mean_daily_top_sensor_hist <- ggplot(season_topsensor_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean above-surface temperature (°C)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# c. Soil temperature (T1: Soil sensor) ----

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
# for all dates 
# -2.632107 13.279976


# subsetting to start the season on June 18th (post snow melt)
season_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-15")
# summer 2022:
# -0.4803571  5.4093424
# warmest: 13th August, coldest: 1st June 
mean(season_soil_temp_2022$mean_temp)
# 3.556209

# subsetting growing seasons 2021 and 2022 
season_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-08-31" |
    Date >= "2022-06-18" & Date <= "2022-08-15")
mean(season_soil_temp$mean_temp)
# 3.826141

# Save as csv
write.csv(KP_mean_daily_soil_temp, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_soil_temp.csv", row.names = FALSE)

# Monthly means
# filter out june 2022
june_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-30")

mean(june_soil_temp_2022$mean_temp) # 1.361704

# filter out july 2022 
july_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp_2022$mean_temp) # 3.925377

# 2021 and 2022 july 
july_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31" |
    Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp$mean_temp) # 4.107419

# filter aug
aug_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_soil_temp_2022$mean_temp) #  4.695168

# filter 2022 and 2021 aug
aug_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
    Date >= "2022-08-01" & Date <= "2022-08-15")
mean(aug_soil_temp$mean_temp) #  4.314712

# Plot daily mean soil temp over full year
# timeseries
(kp_mean_daily_soil_temp <- ggplot(KP_mean_daily_soil_temp, aes(x = Date, y = mean_temp)) +
    geom_line()+ 
    ylab("Daily mean soil temperature (°C)") +
    xlab("Date") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# histogram
(kp_mean_daily_soil_temp_hist <- ggplot(season_soil_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean soil temperature (°C)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# d. Soil moisture (SoilMoistureCount) ----

# filter the full dataset to start from date of water bath
KP_moist <- tomst_kp %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>%  
  filter(Date >= '2021-07-23') %>% # date of water bath 23rd July 2021
  filter(Variable %in% "SoilMoistureCount")

max(KP_moist$Value) #3698
min(KP_moist$Value) # 301
mean(KP_moist$Value) # 1535.597

# making moisture into a percentage
# formula: ((input - min) * 100) / (max - min)
KP_moist_percent <- KP_moist %>%
  mutate(moisture_percent = (Value - 301)*100/(3698-301))

max(KP_moist_percent$moisture_percent) # 100%
min(KP_moist_percent$moisture_percent) # 0%
mean(KP_moist_percent$moisture_percent) # 36.34373 %

# Daily mean soil moisture for all year
KP_mean_daily_soil_moist <- KP_moist_percent  %>%
  filter(Date > lubridate::ymd("2021-07-29")) %>% # date when tomst deployed
  group_by(Date) %>% 
  summarise(mean_moist = mean(moisture_percent)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_moist) %>%  # see top 5 warmest days
 glimpse() 
  #filter(Date >= "2021-07-29" & Date <= "2021-08-31" |
          # Date >= "2022-06-18" & Date <= "2022-08-15") 
  
range(KP_mean_daily_soil_moist$mean_moist)
# 25.60355 65.80822
# driest: 20 march 2022, wettest: 30th june 2022
mean(KP_mean_daily_soil_moist$mean_moist)
# 36.84612

# subsetting growing seasons 2021 and 2022 
season_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2021-07-29" & Date <= "2021-08-31" |
           Date >= "2022-06-18" & Date <= "2022-08-15")

mean(season_soil_moist$mean_moist) # 50.54856

# Monthly means
# filter june 2022
june_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_soil_moist$mean_moist) #  51.43146

# filter july 2022
july_soil_moist_2022 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_moist$mean_moist) # 48.50145

# 2021 and 2022 july 
july_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_moist$mean_moist)

# filter august 2022
aug_soil_moist_2022 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_soil_moist_2022$mean_moist) # 47.91319

# filter 2022 and 2021 aug
aug_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
           Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_soil_moist$mean_moist) #  49.41268

# Save as csv
write.csv(KP_mean_daily_soil_moist, file = "data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_soil_moist.csv", row.names = FALSE)

# Plot daily mean soil moisture over full year
# time series
(kp_mean_daily_soil_moist <- ggplot(KP_mean_daily_soil_moist, aes(x = Date, y = mean_moist)) +
    geom_line()+ 
    ylab("Daily mean soil moisture (%)") +
    xlab("Date") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# histogram
(kp_mean_daily_soil_moist_hist <- ggplot(season_soil_moist, aes(x = mean_moist)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean soil moisture (%)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# PANELS ----
# Panel of env variables
# timeseries panel
facet_env_kp <- grid.arrange(kp_mean_daily_soil_moist, kp_mean_daily_soil_temp, 
                             kp_mean_daily_temp, kp_mean_daily_top_sensor, ncol=2)

# hist panel
facet_env_kp_hist <- grid.arrange(kp_mean_daily_soil_moist_hist, kp_mean_daily_soil_temp_hist, 
                             kp_mean_daily_temp_hist, kp_mean_daily_top_sensor_hist, ncol=2)


