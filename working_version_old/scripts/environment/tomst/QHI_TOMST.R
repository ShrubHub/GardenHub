### Processing TOMST logger data from Qikiqtaruk-Hershel Island (QHI) (2022)
### Script by Erica Zaja, based on script by Elise Gallois
### Last updated: 27/02/2023 by Madi 

### 1. LOADING LIBRARIES -----
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)

### 2. READING DATA ----

# TOMST data (40 loggers in total) from Common Garden collected on August 17th 2022
# tomst_qhi <- read_csv("data/tomst/QHI_TOMST_August2022/QHI_FullTOMST_2022.csv") # data by Elise Gallois

load("data/tomst/2023/tomst_qhi_2023_data.rds")

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

str(tomst_qhi_2023_data)
tomst_qhi_2023_data$year <- year(as.POSIXlt(tomst_qhi_2023_data$Date, format="%Y-%m-%d"))
tomst_qhi_2023_data$month <- format(as.Date(tomst_qhi_2023_data$Date, format="%Y-%m-%d"),"%m") # make month column

# see top 5 warmest days
QHI_mean_daily_temp <- tomst_qhi_2023_data %>%
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(month, year, Variable) %>% 
  summarise(mean = mean(Value), 
            sd = sd(Value)) 

QHI_mean_daily_temp$month <- format(as.Date(QHI_mean_daily_temp$Date, format="%Y-%m-%d"),"%m") # make month column
QHI_mean_daily_temp$year <- year(as.POSIXlt(QHI_mean_daily_temp$Date, format="%Y-%m-%d"))

range(QHI_mean_daily_temp$mean_temp) # with filter above running
#  -17.63166 16.75613121
# warmest: 5th August, coldest: 29th July
mean(QHI_mean_daily_temp$mean_temp)
# -2.962292

# filter july 
july_surface_temp <- QHI_mean_daily_temp %>%
  subset(month == "07")

mean(july_surface_temp$mean_temp) # 5.230957
sd(july_surface_temp$mean_temp) # 2.716989

# JULY surface temp
# 2022: 5.230957, sd = 3.432733
# 2023: 13.209166

# JULY above ground temp
# 2022: 5.174015 sd = 3.679054
# 2023: 14.332443

# JULY soil temp
# 2022: 3.200155
# 2023: 8.517848

# mean july air temp 
# 2022
mean(5.230957, 5.174015) # 5.230957
# 2023 
air_2023 <- c(13.209166, 8.517848) # 10.86351 sd = 3.317263
mean(air_2023)
sd(air_2023)
# Saving as csv
write.csv(QHI_mean_daily_temp, file = "data/tomst/2023/QHI_mean_month_year_data.csv", row.names = FALSE)

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
QHI_mean_daily_top_sensor <- tomst_qhi_2023_data  %>%
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
mean(QHI_mean_daily_top_sensor$mean_temp)
# 7.567246 

# filter july 2022
july_top_sensor_temp <- QHI_mean_daily_top_sensor%>%
  subset(Date >= "2022-07-27" & Date <= "2022-07-31")

mean(july_top_sensor_temp$mean_temp) #  4.912379

# mean of top sensor and surface temp
# (4.912379+5.01368)/2 = 4.963029

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
QHI_mean_daily_soil_temp <- tomst_qhi_2023_data  %>%
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
mean(QHI_mean_daily_soil_temp$mean_temp)
# 4.320375 

# filter out july 2022 
july_soil_temp_2022 <- QHI_mean_daily_soil_temp %>%
  subset(Date >= "2022-07-01" & Date <= "2022-07-31")

mean(july_soil_temp_2022$mean_temp) # 3.323774
sd(july_soil_temp_2022$mean_temp) # 0.7855326

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
# filter to change units
QHI_moist <- tomst_qhi_2023_data  %>%
  filter(Variable %in% "SoilMoistureCount") 

max(QHI_moist$Value) #3747
min(QHI_moist$Value) # 3
mean(QHI_moist$Value) # 1380.094

# making moisture into a percentage
# formula: ((input - min) * 100) / (max - min)
QHI_moist_percent <- QHI_moist %>%
  mutate(moisture_percent = (Value - 3)*100/(3747-3))

# Daily mean soil moisture
QHI_mean_daily_soil_moist <- QHI_moist_percent  %>%
  #filter(Variable %in% "SoilMoistureCount") %>% 
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date) %>% 
  summarise(mean_moist = mean(moisture_percent), 
            sd_moist = sd(moisture_percent)) %>% 
  group_by(Date) %>% 
  top_n(-5, mean_moist) %>%  # see top 5 warmest days
  glimpse()

range(QHI_mean_daily_soil_moist$mean_moist)
# 24.53367 66.23640

# filter july 2022
july_soil_moist_2022 <- QHI_mean_daily_soil_moist %>%
  subset(Date >= "2022-07-27" & Date <= "2022-07-31")

mean(july_soil_moist_2022$mean_moist) # 57.09273
sd(july_soil_moist_2022$mean_moist) # 0.9446492

july_soil_moist_2023 <- QHI_mean_daily_soil_moist %>%
  subset(Date >= "2023-07-01" & Date <= "2023-07-31")

mean(july_soil_moist_2023$mean_moist) # 54.09433
sd(july_soil_moist_2023$mean_moist) # 2.647204

# mean soil moisture 
mean_soil_moist <-  c(54.09433, 57.09273)
mean(mean_soil_moist) # 55.59353
sd(mean_soil_moist) # 2.120189

# Save as csv
write.csv(QHI_mean_daily_soil_moist, file = "data/tomst/2023/QHI_mean_daily_soil_moist.csv", row.names = FALSE)

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

