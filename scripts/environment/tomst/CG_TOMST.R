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
  # filter(Date > lubridate::ymd("2022-06-01")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  
  glimpse()

range(CG_mean_daily_temp$mean_temp)
# 8.883898 21.582143
# warmest: 1st June, coldest: 12th July
mean(CG_mean_daily_temp$mean_temp)
# 13.89118

# Saving as csv
write.csv(CG_mean_daily_temp, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_temp.csv", row.names = FALSE)

# Monthly means
# filter out june
june_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_surface_temp$mean_temp) # 13.74875

# filter out july
july_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_surface_temp$mean_temp) # 14.54956

# filter out august
aug_surface_temp <- CG_mean_daily_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_surface_temp$mean_temp) # 12.94198

# Plot daily mean temp over summer 2022
(cg_mean_daily_temp <- ggplot(CG_mean_daily_temp, aes(x = Date, y = mean_temp)) +
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
(cg_mean_daily_temp_hist <- ggplot(CG_mean_daily_temp, aes(x = mean_temp)) +
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


# b. Above-surface temperature (T3: Top sensor) ----

# Daily mean top sensor temperature (10cm above surface)
CG_mean_daily_top_sensor <- cg_data  %>%
  filter(Variable %in% "T3: Top sensor") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(CG_mean_daily_top_sensor$mean_temp)
# 7.551866 21.325000
# warmest: 1st June , coldest: 12th July
mean(CG_mean_daily_top_sensor$mean_temp)
# 13.42857

# Monthly means
# filter out june
june_topsensor_temp <- CG_mean_daily_top_sensor%>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_topsensor_temp$mean_temp) # 13.13366

# filter out july
july_topsensor_temp <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_topsensor_temp$mean_temp) # 14.18936

# filter out august
aug_topsensor_temp <- CG_mean_daily_top_sensor %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_topsensor_temp$mean_temp) # 12.56167

# Plot daily mean top sensor temp over summer 2022
(cg_mean_daily_top_sensor <- ggplot(CG_mean_daily_top_sensor, aes(x = Date, y = mean_temp)) +
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
(cg_mean_daily_top_sensor_hist <- ggplot(CG_mean_daily_top_sensor, aes(x = mean_temp)) +
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
july_soil_temp <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp$mean_temp) # 13.63691

# filter out august
aug_soil_temp <- CG_mean_daily_soil_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_soil_temp$mean_temp) # 12.76118

# Plot daily mean soil temp over summer 2022
(cg_mean_daily_soil_temp <- ggplot(CG_mean_daily_soil_temp, aes(x = Date, y = mean_temp)) +
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
(cg_mean_daily_soil_temp_hist <- ggplot(CG_mean_daily_soil_temp, aes(x = mean_temp)) +
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

# Saving as csv
write.csv(CG_mean_daily_soil_temp, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_soil_temp.csv", row.names = FALSE)

# d. Soil moisture (SoilMoistureCount) ----
# N.B max and min values to be changed when we figure out day of water bath
# filter to change units
CG_moist <- cg_data  %>%
  filter(Variable %in% "SoilMoistureCount") 

max(CG_moist$Value) #2138
min(CG_moist$Value) # 1198
mean(CG_moist$Value) # 1636.912

# making moisture into a percentage
# formula: ((input - min) * 100) / (max - min)
CG_moist_percent <- CG_moist %>%
  mutate(moisture_percent = (Value - 1198)*100/(2138-1198))

# Daily mean soil moisture
CG_mean_daily_soil_moist <- CG_moist_percent  %>%
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_moist = mean(moisture_percent)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_moist) %>%  # see top 5 warmest days
  glimpse()

range(CG_mean_daily_soil_moist$mean_moist)
# 26.16763 67.06006
# driest:10th July, wettest: 2nd Aug
mean(CG_mean_daily_soil_moist$mean_moist)
# 46.76717 %

# Monthly means
# filter out june
june_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_soil_moist$mean_moist) # 39.49235

# filter out july
july_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_moist$mean_moist) # 44.20692

# filter out august
aug_soil_moist <- CG_mean_daily_soil_moist %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-17")

mean(aug_soil_moist$mean_moist) # 64.2738

# Saving as csv
write.csv(CG_mean_daily_soil_moist, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_soil_moist.csv", row.names = FALSE)

# Plot daily mean soil moisture over summer 2022
(cg_mean_daily_soil_moist <- ggplot(CG_mean_daily_soil_moist, aes(x = Date, y = mean_moist)) +
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
(cg_mean_daily_soil_moist_hist <- ggplot(CG_mean_daily_soil_moist, aes(x = mean_moist)) +
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
facet_env_KP <- grid.arrange(cg_mean_daily_soil_moist, cg_mean_daily_soil_temp, 
                             cg_mean_daily_temp, cg_mean_daily_top_sensor, ncol=2)

# hist panel
facet_env_kp_hist <- grid.arrange(cg_mean_daily_soil_moist_hist, cg_mean_daily_soil_temp_hist, 
                                  cg_mean_daily_temp_hist, cg_mean_daily_top_sensor_hist, ncol=2)

# Making overall means using hobo and tomst ----
mean_air_temp <- mean( 12.98, 14.80, 11.40) # N.B only using HOBO, 12.98
mean_surface_temp <- mean(12.65, 13.25, 10.03, 13.75, 14.55, 12.94) 
mean_soil_temp <- mean( 11.91, 14.17, 11.32,  12.33, 13.46, 12.76)
# missing soil moisture and top sensor