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
  filter(Datetime_UTC > lubridate::ymd_hm("2022-06-01 15:00")) %>% # keeping summer 2022 values only 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

str(tomst_cg)

# Saving as csv
write.csv(tomst_cg, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_FullTOMST_2022.csv", row.names = FALSE)

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

# Saving as csv
write.csv(CG_mean_daily_temp, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_temp.csv", row.names = FALSE)

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

# Saving as csv
write.csv(CG_mean_daily_soil_temp, file = "data/tomst/Common_Garden_TOMST_17August2022/CG_mean_daily_soil_temp.csv", row.names = FALSE)

# d. Soil moisture (SoilMoistureCount) ----

# Daily mean soil moisture
CG_mean_daily_soil_moist <- cg_data  %>%
  filter(Variable %in% "SoilMoistureCount") %>% 
  #filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_moist = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_moist) %>%  # see top 5 warmest days
  glimpse()

range(CG_mean_daily_soil_moist$mean_moist)
# 1443.976 1828.365
# driest:10th July, wettest: 29th June 

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

# Panel of env variables
facet_env_KP <- grid.arrange(cg_mean_daily_soil_moist, cg_mean_daily_soil_temp, 
                             cg_mean_daily_temp, cg_mean_daily_top_sensor, ncol=2)

