### Processing TOMST logger data from common garden (CG) (2022)
### Script by Erica Zaja, based on script by Elise Gallois
### Last updated: 11/10/2022

### 1. LOADING LIBRARIES -----
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)

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
# TOMST data (3 loggers in total) from Common Garden collected on August 17th 2022
tomst <- "data/tomst/Common_garden_TOMST_17August2022"
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
cg_data <- map_dfr(files, read_tms4)

# change date (GMT to newty time) - 6 hours time difference 
cg_data$Datetime_UTC <- cg_data$Datetime_UTC - hours(6)
str(cg_data)

### 3. DATA MANIPULATION ----

tomst_cg <-  cg_data %>%  
  #filter(Datetime_UTC > lubridate::ymd_hm("2022-06-01 15:00")) %>% # keeping summer 2022 values only 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

str(tomst_cg)

# Saving as csv
write.csv(tomst_cg, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_FullTOMST_2022.csv", row.names = FALSE)

# Reading in file
tomst_cg <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_FullTOMST_2022.csv")

### 4. DATA VISUALISATION ----

(cg_tomst_summary <- ggplot(tomst_cg, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color=SerialID)) + 
  facet_wrap(~ Variable, scales = "free_y") +
  theme_classic() + 
  theme(legend.position = "none"))

# Historgram of variables
(cg_hist_tomst <- ggplot(tomst_cg) +
  aes(x = Value, fill = SerialID) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Variable), scales = "free") +
  theme(legend.position = "none"))


### 5. EXPLORING VARIABLES ----

# a. Surface temperature (T2: Surface sensor) ----

# get date column
cg_data <- tomst_cg %>% 
  mutate(Date = lubridate::date(Datetime_UTC))

str(cg_data)

# see top 5 warmest days
CG_mean_daily_temp <- cg_data %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2021-07-29")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  
  glimpse()

range(CG_mean_daily_temp$mean_temp)
# -14.46593  19.01866
# warmest: 1st June, coldest: 12th July
mean(CG_mean_daily_temp$mean_temp)
# 3.968781

# Saving as csv
write.csv(CG_mean_daily_temp, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_temp.csv", row.names = FALSE)
CG_mean_daily_temp <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_temp.csv")

# Monthly means
# filter out june 2022
june_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_surface_temp$mean_temp) # 13.47703

# filter out july 2022
july_surface_temp_2022 <- CG_mean_daily_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_surface_temp_2022$mean_temp) # 14.54956

july_surface_temp_2021 <- CG_mean_daily_temp %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31")

mean(july_surface_temp_2021$mean_temp) # 17.97439

# 2021 and 2022 july 
july_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31")
mean(july_surface_temp$mean_temp) # 14.75712

# filter out august 2022
aug_surface_temp_2022 <- CG_mean_daily_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_surface_temp_2022$mean_temp) # 12.94198

# 2021 and 2022 august
aug_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
           Date >= "2022-08-01" & Date <= "2022-08-17")
mean(aug_surface_temp$mean_temp) # 12.29057

# subsetting growing seasons 2021 and 2022 
season_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2021-07-30" & Date <= "2021-08-31" | # 2021 growing season
           Date >= "2022-06-01" & Date <= "2022-08-17") # 2022 growing season
mean(season_surface_temp$mean_temp)
# 13.34453

# Plot daily mean temp over full year
(cg_mean_daily_temp <- ggplot(CG_mean_daily_temp, aes(x = Date, y = mean_temp)) +
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

# histogram of season temps (2021-22)
(cg_mean_daily_temp_hist <- ggplot(season_surface_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean surface temperature (°C) (2021-22)") +
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
CG_mean_daily_top_sensor <- cg_data  %>%
  filter(Variable %in% "T3: Top sensor") %>% 
  filter(Date > lubridate::ymd("2021-07-29")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(CG_mean_daily_top_sensor$mean_temp)
#  -31.73025  19.67491
# warmest: 1st June , coldest: 12th July
mean(CG_mean_daily_top_sensor$mean_temp)
# 13.42857

# Monthly means
# filter out june
june_topsensor_temp <- CG_mean_daily_top_sensor%>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_topsensor_temp$mean_temp) # 12.8446

# filter out july
july_topsensor_temp_2022 <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_topsensor_temp$mean_temp) # 9.570722

july_topsensor_temp_2021 <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31")

mean(july_topsensor_temp_2021$mean_temp) # 18.29926

# 2021 and 2022 july
july_topsensor_temp <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31")
mean(july_topsensor_temp$mean_temp) # 14.43845

# filter out august
aug_topsensor_temp_2022 <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_topsensor_temp$mean_temp) # 12.56167

# 2021 and 2022 august
aug_topsensor_temp <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
           Date >= "2022-08-01" & Date <= "2022-08-17")
mean(aug_topsensor_temp$mean_temp) # 11.90226

# subsetting growing seasons 2021 and 2022 
season_topsensor_temp <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-30" & Date <= "2021-08-31" | # 2021 growing season 
           Date >= "2022-06-01" & Date <= "2022-08-17") # 2022 growing season
mean(season_topsensor_temp$mean_temp) # 12.91095

# Plot daily mean top sensor temp over full year
(cg_mean_daily_top_sensor <- ggplot(CG_mean_daily_top_sensor, aes(x = Date, y = mean_temp)) +
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

# histogram of season temps (2021-22)
(cg_mean_daily_top_sensor_hist <- ggplot(season_topsensor_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean above-surface temperature (°C) (2021-22)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Saving as csv
write.csv(CG_mean_daily_top_sensor, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_top_sensor.csv", row.names = FALSE)

# c. Soil temperature (T1: Soil sensor) ----

# Daily mean soil temperature 
CG_mean_daily_soil_temp <- cg_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(CG_mean_daily_soil_temp$mean_temp)
# 9.492622 16.236979
# warmest: 5th July, coldest: 14th June 

mean(CG_mean_daily_soil_temp$mean_temp) 
# 12.94467

# Monthly means
# filter out june
june_soil_temp <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_soil_temp$mean_temp) # 12.33335

# filter out july
july_soil_temp_2022 <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp_2022$mean_temp) # 13.63691

# 2021 and 2022 july
july_soil_temp <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31")
mean(july_soil_temp$mean_temp) # 13.80938

# filter out august 2022
aug_soil_temp_2022 <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_soil_temp_2022$mean_temp) # 12.76118

# 2021 and 2022 august
aug_soil_temp <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
           Date >= "2022-08-01" & Date <= "2022-08-17")
mean(aug_soil_temp$mean_temp) # 11.90226

# subsetting growing seasons 2021 and 2022 
season_soil_temp <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2021-07-30" & Date <= "2021-08-31" | # 2021 growing season 
           Date >= "2022-06-01" & Date <= "2022-08-17") # 2022 growing season
mean(season_soil_temp$mean_temp) # 12.75037

# Plot daily mean soil temp over full year
(cg_mean_daily_soil_temp <- ggplot(CG_mean_daily_soil_temp, aes(x = Date, y = mean_temp)) +
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

# histogram of growing seasons (2021-22)
(cg_mean_daily_soil_temp_hist <- ggplot(season_soil_temp, aes(x = mean_temp)) +
    geom_histogram(fill = "#CC9145", colour = "black", bins = 10)+ 
    ylab("Frequency") +
    xlab("Daily mean soil temperature (°C) (2021-22)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Saving as csv
write.csv(CG_mean_daily_soil_temp, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_soil_temp.csv", row.names = FALSE)

# d. Soil moisture (SoilMoistureCount) ----

# filter the full dataset to start from date of water bath
CG_moist <- tomst_cg %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  filter(Date >= '2021-07-23') %>% # date of water bath 23rd July 2021
  filter(Variable %in% "SoilMoistureCount")

max(CG_moist$Value) #3522
min(CG_moist$Value) # 339
mean(CG_moist$Value) # 1438.297

# making moisture into a percentage
# formula: ((input - min) * 100) / (max - min)
CG_moist_percent <- CG_moist %>%
  mutate(moisture_percent = (Value - 339)*100/(3522-339))

max(CG_moist_percent$moisture_percent) # 100%
min(CG_moist_percent$moisture_percent) # 0%
mean(CG_moist_percent$moisture_percent) # 34.53652 %

# Daily mean soil moisture
CG_mean_daily_soil_moist <- CG_moist_percent  %>%
  filter(Date > lubridate::ymd("2021-07-29")) %>% # date when tomst deployed 
  group_by(Date) %>% 
  summarise(mean_moist = mean(moisture_percent))

range(CG_mean_daily_soil_moist$mean_moist)
# 19.92748 60.06235
# driest:30th July 2021, wettest: 29th april 2022
mean(CG_mean_daily_soil_moist$mean_moist)
# 34.84954

# Monthly means
# filter out june
june_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_soil_moist$mean_moist) # 38.66966

# filter out july
july_soil_moist_2022 <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_moist_2022$mean_moist) # 40.04226

# 2021 and 2022 july
july_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2021-07-30" & Date <= "2021-07-31" |
           Date >= "2022-07-01" & Date <= "2022-07-31")
mean(july_soil_moist$mean_moist) #  39.10862

# filter out august
aug_soil_moist_2022 <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_soil_moist_2022$mean_moist) #  45.96839

# 2021 and 2022 august
aug_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2021-08-01" & Date <= "2021-08-31" |
           Date >= "2022-08-01" & Date <= "2022-08-17")
mean(aug_soil_moist$mean_moist) # 46.43083

# subsetting growing seasons 2021 and 2022 
season_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2021-07-30" & Date <= "2021-08-31" | # 2021 growing season 
           Date >= "2022-06-01" & Date <= "2022-08-17") # 2022 growing season
mean(season_soil_moist$mean_moist) # 42.15634

# Saving as csv
write.csv(CG_mean_daily_soil_moist, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_soil_moist.csv", row.names = FALSE)

# Plot daily mean soil moisture over full year
(cg_mean_daily_soil_moist <- ggplot(CG_mean_daily_soil_moist, aes(x = Date, y = mean_moist)) +
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

# histogram of growing seasons
(cg_mean_daily_soil_moist_hist <- ggplot(season_soil_moist, aes(x = mean_moist)) +
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

# PANELS -----
# Panel of env variables
# timeseries panel
facet_env_KP <- grid.arrange(cg_mean_daily_soil_moist, cg_mean_daily_soil_temp, 
                             cg_mean_daily_temp, cg_mean_daily_top_sensor, ncol=2)

# hist panel
facet_env_kp_hist <- grid.arrange(cg_mean_daily_soil_moist_hist, cg_mean_daily_soil_temp_hist, 
                                  cg_mean_daily_temp_hist, cg_mean_daily_top_sensor_hist, ncol=2)

