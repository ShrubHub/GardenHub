### Processing TOMST logger data from Kluane Plateau (KP) (2022)
### Script by Erica Zaja, based on script by Elise Gallois
### Last updated: 02/10/2023 by Madi 

### 1. LOADING LIBRARIES -----
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)


# Loading dataset

#tomst_kp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv") 

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

tomst <- "data/tomst/KLU_TOMST_backup_2023/KP_data_files_aug2023" 
files <- list.files(path = tomst, pattern = "^data_*", full.names = T)
kp_data <- map_dfr(files, read_tms4)

# change date (GMT to newty time) - 6 hours time difference 
kp_data$Datetime_UTC <- kp_data$Datetime_UTC - hours(6)

tomst_kp_data <-  kp_data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2021-07-29 15:00")) %>% 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

# make doy, month and year columns 
tomst_kp_data <- tomst_kp_data %>% 
  mutate(Date = lubridate::date(Datetime_UTC))
tomst_kp_data$doy <- yday(tomst_kp_data$Date) # make DOY column
tomst_kp_data$year <- format(as.Date(tomst_kp_data$Date, format="%Y-%m-%d"),"%Y") # make year column
tomst_kp_data$month <- format(as.Date(tomst_kp_data$Date, format="%Y-%m-%d"),"%m") # make month column

# Saving as csv
saveRDS(tomst_kp_data, "data/tomst/2023/tomst_KP_2023_data.rds") # save as Rdata bc too big for a csv 

# Reading in file 
load("data/tomst/2023/tomst_KP_2023_data.rds")


### 4. DATA VISUALISATION ----

(kp_tomst_summary <- ggplot(tomst_kp_data, aes(x = Datetime_UTC, y = Value)) +
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

# Daily mean surface temperature
KP_mean_daily_temp <- tomst_kp_data  %>%
  filter(Variable %in% "T2: Surface sensor") %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

range(KP_mean_daily_temp$mean_temp)
# -4.00148 to 13.20301

# subsetting to start the season on June 18th 2022 (post 2022 snow melt)
season_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-31")

mean(season_surface_temp_2022$mean_temp)
# 6.963005

season_surface_temp_2023 <- KP_mean_daily_temp %>%
  subset(Date >= "2023-06-18" & Date <= "2023-08-22")

mean(season_surface_temp_2023$mean_temp)
# 8.823751

# subsetting to growing seasons 2021 and 2022 
season_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2021-07-28" & Date <= "2021-08-31" | # 2021 growing season
         Date >= "2022-06-18" & Date <= "2022-08-31" | # 2022 growing season
        Date >= "2023-06-18" & Date <= "2023-08-22"  ) # 2023 growing season

mean(season_surface_temp$mean_temp)
# 7.477066 

# Save as csv
write.csv(KP_mean_daily_temp, file = "data/tomst/2023/KP_mean_daily_temp_2023.csv", row.names = FALSE)
KP_mean_daily_temp <- read_csv("data/tomst/2023/KP_mean_daily_temp_2023.csv")

# Monthly means
# filter out june 2022, only 2022 because tomst wasnt in place in June 2021
KP_mean_daily_temp$month <- format(as.Date(KP_mean_daily_temp$Date, format="%Y-%m-%d"),"%m") # make month column

june_surface_temp_2023 <- KP_mean_daily_temp %>%
  subset(month == "06")

mean(june_surface_temp_2023$mean_temp) # 4.063052

# filter out july 2021, 2022, 2023

july_surface_temp <- KP_mean_daily_temp %>%
  subset(month == "07")

mean(july_surface_temp_2023$mean_temp) # 8.885591
sd(july_surface_temp_2023$mean_temp) # 2.276723

july_surface_temp_2021 <- KP_mean_daily_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_surface_temp_2021$mean_temp) # 9.838716
sd(july_surface_temp_2021$mean_temp) # 2.444737

july_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_surface_temp_2022$mean_temp) # 7.776408
sd(july_surface_temp_2022$mean_temp) # 2.175941

july_surface_temp_2023 <- KP_mean_daily_temp %>%
  subset(Date >= "2023-07-01" & Date <= "2023-07-31")

mean(july_surface_temp_2023$mean_temp) # 9.902535
sd(july_surface_temp_2023$mean_temp) # 1.869253

july_surface_temp <- KP_mean_daily_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31" | # sensor deployed midway through month
    Date >= "2022-07-01" & Date <= "2022-07-31") 

mean(july_surface_temp$mean_temp) # 7.847449

# filter out august 2022
aug_surface_temp_2022 <- KP_mean_daily_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-31")

mean(aug_surface_temp_2022$mean_temp) # 6.398539

# august 2021, 2022, 2023
aug_surface_temp <- KP_mean_daily_temp %>%
  subset(month == "08")

mean(aug_surface_temp$mean_temp) # 6.513912

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
KP_mean_daily_top_sensor <- tomst_kp_data  %>%
  filter(Variable %in% "T3: Top sensor") %>% 
  filter(Date > lubridate::ymd("2021-07-29"))   %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

KP_mean_daily_top_sensor$month <- format(as.Date(KP_mean_daily_top_sensor$Date, format="%Y-%m-%d"),"%m") # make month column

range(KP_mean_daily_top_sensor$mean_temp)
# for all TOMST
# -8.17081 17.87311

# subsetting to start the season on June 18th (post snow melt) to end of August 
season_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-31")
range(season_topsensor_temp_2022$mean_temp) # for summer 2022: 
# 3.703179 16.419152
mean(season_topsensor_temp_2022$mean_temp)
# 8.51564

# 2021 and 2021 data 
season_topsensor_temp <- KP_mean_daily_top_sensor %>% 
  subset(Date >= "2021-07-29" & Date <= "2021-08-31" | # 2021 growing season
           Date >= "2022-06-18" & Date <= "2022-08-15") # 2022 growing season
mean(season_topsensor_temp$mean_temp)
# 8.203

# Save as csv
write.csv(KP_mean_daily_top_sensor, file = "data/tomst/2023/KP_mean_daily_top_sensor.csv", row.names = FALSE)
KP_mean_daily_top_sensor <- read.csv("data/tomst/2023/KP_mean_daily_top_sensor.csv")

# Monthly means
# filter out june 2022
june_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-06-18" & Date <= "2022-06-31")

mean(june_topsensor_temp_2022$mean_temp) # 9.411094

# filter out july 2021
july_topsensor_temp_2021 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_topsensor_temp_2021$mean_temp) # 11.15368
sd(july_topsensor_temp_2021$mean_temp) # 1.475441

# filter out july 2022
july_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_topsensor_temp_2022$mean_temp) # 9.440068
sd(july_topsensor_temp_2022$mean_temp) # 3.546498

# filter out july 2023
july_topsensor_temp_2023 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2023-07-01" & Date <= "2023-07-31")

mean(july_topsensor_temp_2023$mean_temp) # 11.7165
sd(july_topsensor_temp_2023$mean_temp) # 2.704851

july_topsensor_temp_2021 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_topsensor_temp_2021$mean_temp) # 11.15368
sd(july_topsensor_temp_2021$mean_temp) # 1.475441

july_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(month == "07")

mean(july_topsensor_temp$mean_temp) # 10.59626

# filter out august 2022
aug_topsensor_temp_2022 <- KP_mean_daily_top_sensor %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-15")

mean(aug_topsensor_temp_2022$mean_temp) # 7.56839

# 2021 and 2022 2023 august 
aug_topsensor_temp <- KP_mean_daily_top_sensor %>%
  subset(month == "08")
mean(aug_topsensor_temp$mean_temp) # 7.317902

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
KP_mean_daily_soil_temp <- tomst_kp_data  %>%
  filter(Variable %in% "T1: Soil sensor") %>% 
  filter(Date > lubridate::ymd("2021-07-29"))   %>% 
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_temp) %>%  # see top 5 warmest days
  glimpse()

KP_mean_daily_soil_temp$month <- format(as.Date(KP_mean_daily_soil_temp$Date, format="%Y-%m-%d"),"%m") # make month column

range(KP_mean_daily_soil_temp$mean_temp)
# for all dates 
# -3.105054  8.271958

# subsetting to start the season on June 18th (post snow melt)
season_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-06-18" & Date <= "2022-08-31")
# summer 2022:
# -0.4803571  5.4093424
# warmest: 13th August, coldest: 1st June 
mean(season_soil_temp_2022$mean_temp)
# 3.792667

# subsetting growing seasons 2021 and 2022 2023
season_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-08-31" |
    Date >= "2022-06-18" & Date <= "2022-08-31" |
      Date >= "2023-06-18" & Date <= "2023-08-22")
mean(season_soil_temp$mean_temp)
# 4.482087

# Save as csv
write.csv(KP_mean_daily_soil_temp, file = "data/tomst/2023/KP_mean_daily_soil_temp.csv", row.names = FALSE)

# Monthly means
# filter out june 
june_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(month == "06")

mean(june_soil_temp$mean_temp) # 0.8666971

# filter out july 2022 
july_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp_2022$mean_temp) # 3.980058
sd(july_soil_temp_2022$mean_temp) # 0.5259824

# filter out july 2023
july_soil_temp_2023 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2023-07-01" & Date <= "2023-07-31")

mean(july_soil_temp_2023$mean_temp) # 6.028433
sd(july_soil_temp_2023$mean_temp) # 1.698395
   
july_soil_temp_2021 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_soil_temp_2021$mean_temp) # 4.623698
sd(july_soil_temp_2021$mean_temp) # 0.09449853

# 2021 and 2022 2023 july 
july_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(month == "07")

mean(july_soil_temp$mean_temp) # 4.992354

# filter aug
aug_soil_temp_2022 <- KP_mean_daily_soil_temp %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-31")

mean(aug_soil_temp_2022$mean_temp) # 4.588177

# filter 2022 and 2021 aug
aug_soil_temp <- KP_mean_daily_soil_temp %>%
  subset(month == "08")
mean(aug_soil_temp$mean_temp) # 4.836923

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
KP_moist <- tomst_kp_data %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>%  
  filter(Date >= '2021-07-23') %>% # date of water bath 23rd July 2021
  filter(Variable %in% "SoilMoistureCount")

KP_moist$month <- format(as.Date(KP_moist$Date, format="%Y-%m-%d"),"%m") # make month column

max(KP_moist$Value) # 3700
min(KP_moist$Value) # 337
mean(KP_moist$Value) # 1543.042

# making moisture into a percentage
# formula: ((input - min) * 100) / (max - min)
KP_moist_percent <- KP_moist %>%
  mutate(moisture_percent = (Value - 337)*100/(3700-337))

max(KP_moist_percent$moisture_percent) # 100%
min(KP_moist_percent$moisture_percent) # 0%
mean(KP_moist_percent$moisture_percent) # 35.8621 %

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

KP_mean_daily_soil_moist$month <- format(as.Date(KP_mean_daily_soil_moist$Date, format="%Y-%m-%d"),"%m") # make month column

range(KP_mean_daily_soil_moist$mean_moist)
# 24.87957 65.93007
# driest: 20 march 2022, wettest: 30th june 2022
mean(KP_mean_daily_soil_moist$mean_moist)
# 35.86221

# subsetting growing seasons 2021 and 2022 
season_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2021-07-29" & Date <= "2021-08-31" |
           Date >= "2022-06-18" & Date <= "2022-08-31" |
           Date >= "2023-06-18" & Date <= "2023-08-22" )

mean(season_soil_moist$mean_moist) # 47.06433

# Monthly means
# filter june 2022
june_soil_moist_2022 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-06-01" & Date <= "2022-06-30")

mean(june_soil_moist_2022$mean_moist) # 50.8985

# filter july 2023
july_soil_moist_2023 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2023-07-01" & Date <= "2023-07-31")

mean(july_soil_moist_2023$mean_moist) # 42.11462
sd(july_soil_moist_2023$mean_moist) # 6.501787

# filter july 2022
july_soil_moist_2022 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_moist_2022$mean_moist) # 48.42088
sd(july_soil_moist_2022$mean_moist) # 4.523732

# july 2021 
july_soil_moist_2021 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2021-07-29" & Date <= "2021-07-31")

mean(july_soil_moist_2021$mean_moist) #  45.55465
sd(july_soil_moist_2021$mean_moist) # 0.2655338

# 2021 and 2022 july 
july_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(month == "07")

mean(july_soil_moist$mean_moist) # 45.27671
sd(july_soil_moist$mean_moist)
# filter august 2022
aug_soil_moist_2022 <- KP_mean_daily_soil_moist %>%
  subset(Date >= "2022-08-01" & Date <= "2022-08-31")

mean(aug_soil_moist_2022$mean_moist) # 44.10483

# filter 2022 and 2021 aug
aug_soil_moist <- KP_mean_daily_soil_moist %>%
  subset(month == "08")

mean(aug_soil_moist$mean_moist) # 45.18861

# Save as csv
write.csv(KP_mean_daily_soil_moist, file = "data/tomst/2023/KP_mean_daily_soil_moist.csv", row.names = FALSE)

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


# SUMMARY DATA -----
# Surface + top sensor -----
# July mean temperatures (average of top sensor, surface temp) (TOMST):
mean(july_surface_temp_2021$mean_temp) # 9.838716
sd(july_surface_temp_2021$mean_temp) # 2.444737
mean(july_topsensor_temp_2021$mean_temp) # 11.15368
sd(july_topsensor_temp_2021$mean_temp) # 1.475441
# 2021 = mean(9.838716, 11.15368) = 9.838716
# 2021 sd = mean(2.444737,  1.475441) = 2.444737

mean(july_surface_temp_2022$mean_temp) # 7.776408
sd(july_surface_temp_2022$mean_temp) # 2.175941
mean(july_topsensor_temp_2022$mean_temp) # 9.440068
sd(july_topsensor_temp_2022$mean_temp) # 3.546498
# 2022 = mean(7.776408, 9.440068) = 7.776408
# 2022 sd = mean(2.175941, 3.546498) =  2.175941

mean(july_surface_temp_2023$mean_temp) # 9.902535
sd(july_surface_temp_2023$mean_temp) # 1.869253
mean(july_topsensor_temp_2023$mean_temp) # 11.7165
sd(july_topsensor_temp_2023$mean_temp) # 2.704851

# 2023 mean(9.902535, 11.7165) = 9.902535
# 2023 mean(1.869253, 2.704851) =  1.869253

# overall mean & sd (OF THE MEAN)
# overall SD 
mean_kp_temp <- c(9.838716, 7.776408, 9.902535) # 9.172553
mean(mean_kp_temp)
sd(mean_kp_temp) # 1.209518

# Soil temp (2021-22 TOMST + (other years HOBO, see KP_env.R script)-----
# 2021 = # 4.623698, 2021 sd = 0.09449853
# 2022 = 3.980058, 2022 sd = 0.5259824
# 2023 = 6.028433, sd = 1.698395

# 2015 = 4.568919, 2015 sd = 1.385542
# 2016 = 8.252268, 2016 sd =2.993852
# 2017 = 4.763441, 2017 sd = 1.545860
# overall mean: 
mean_soiltemp_kp <- c(4.623698, 3.980058, 6.028433, 4.568919, 8.252268, 4.763441)
mean(mean_soiltemp_kp) #  5.36947
sd(mean_soiltemp_kp) #  1.56458

# Soil moisture -----
# 2021: 45.55465
# 2022:48.42088
# 2023: 42.11462
mean_soilmoist_kp <- c(45.55465, 48.42088, 42.11462)
mean(mean_soilmoist_kp) # 45.36338
sd(mean_soilmoist_kp) # 3.157478
