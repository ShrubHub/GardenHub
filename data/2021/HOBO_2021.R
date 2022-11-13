#### HOBO 2021 SCRIPT
### Data wrangling and visualisation script
### By Erica Zaja and Madi Anderson, created on 20/10/2022
## Last updated: 20/10/2022 by Madelaine 

# 1. LOADING LIBRARIES -----
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)

# 2. LOADING DATA ----
HOBO_Common_garden_12Aug2021 <- read.csv(file = "scripts/common_garden/common_garden_data_2021/HOBO_Common_garden_12Aug2021.csv")

# 3. DATA WRANGLING ----

CG_HOBO <- HOBO_Common_garden_12Aug2021 %>%
  rename("Soil_moist" = "Water.Content_.m..m..",
         "Ground_temp" = "Ground_temp_..C.",
         "Air_temp" ="Air_temp_..C.",
         "Soil_temp" = "Soil_Temp_..C.", 
         "Date_time_GMT" = "Date.Time..GMT.07.00") 

# convert date/time variation to 24h format aka drop AM & PM 
CG_HOBO_date <- CG_HOBO %>% 
  mutate(date = parse_date_time(CG_HOBO$Date_time_GMT,    #  make new column called date with no AM/PM
                                "%m/%d/%y %I:%M:%S %p")) 

CG_HOBO_date_2 <- CG_HOBO_date %>% 
  separate(date, into = c('year', 'month', 'day'))

CG_HOBO_date_3 <- full_join(CG_HOBO_date, CG_HOBO_date_2)
         
str(CG_HOBO_date)  
# I didn't remove / overwrite existing date column to check to make sure they were the same 

# Make MONTHLY means 
CG_HOBO_monthly_means <- CG_HOBO_date_3 %>%
  group_by(year, month) %>%
  mutate(mean_soil_moist = mean(Soil_moist),
         mean_ground_temp = mean(Ground_temp),
         mean_soil_temp = mean(Soil_temp),
         mean_air_temp = mean(Air_temp)) %>%
  select(date, year, month, mean_soil_moist, mean_ground_temp, mean_soil_temp,
         mean_air_temp) %>%
  group_by(year, month) %>%
  slice(1) %>% # keeping only one observation from each set of year/month
  ungroup()
  
# 4. DATA VISUALISATION -----

# 4.1 Time series -----

# a. Ground temperature ----
(plot_HOBO_ground_temp <- ggplot(CG_HOBO_date, aes(x = date, y = Ground_temp)) +
   geom_line() + 
   #geom_smooth(method = "lm", colour = "black") +
   ylab("Ground temperature (°C)") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# b. Soil moisture ----
(plot_HOBO_soil_moist <- ggplot(CG_HOBO_date, aes(x = date, y = Soil_moist)) +
   geom_line() + 
   #geom_smooth(method="lm", colour = "black")+
   ylab("Soil moisture ()") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# c. Soil temperature ----
(plot_HOBO_soil_temp <- ggplot(CG_HOBO_date, aes(x = date, y = Soil_temp)) +
   geom_line()+
   #geom_smooth(method="lm", colour = "black") + 
   ylab("Soil temperature (°C)") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# d. Air temperature -----
(plot_HOBO_air_temp <- ggplot(CG_HOBO_date, aes(x = date, y = Air_temp)) +
   geom_line() + 
    #geom_smooth(method = "lm", colour = "black")+
    ylab("Air temperature (°C)") +
    xlab("Date") +
    theme_bw() +
    #scale_color_manual(values = c("dark green", "blue", "red")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Make facet plot of the timeseries
facet_HOBO_timeseries <- grid.arrange(plot_HOBO_air_temp,plot_HOBO_ground_temp,
                                      plot_HOBO_soil_moist, plot_HOBO_soil_temp)

# 4.2. Monthly time series ----

# a. Monthly ground temperature ----
(plot_HOBO_ground_temp_monthly <- ggplot(CG_HOBO_monthly_means, aes(x = date, y = mean_ground_temp)) +
   geom_line() + 
   #geom_smooth(method = "lm", colour = "black") +
   ylab("Monthly ground temperature (°C)") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# b. Monthly soil moisture ----
(plot_HOBO_soil_moist_monthly <- ggplot(CG_HOBO_monthly_means, aes(x = date, y = mean_soil_moist)) +
   geom_line() + 
   #geom_smooth(method="lm", colour = "black")+
   ylab("Monthly soil moisture ()") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# c. Monthly soil temperature ----
(plot_HOBO_soil_temp_monthly <- ggplot(CG_HOBO_monthly_means, aes(x = date, y = mean_soil_temp)) +
   geom_line()+
   #geom_smooth(method="lm", colour = "black") + 
   ylab("Monthly soil temperature (°C)") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# d. Monthly air temperature ----
(plot_HOBO_air_temp_monthly <- ggplot(CG_HOBO_monthly_means, aes(x = date, y = mean_air_temp)) +
    geom_line() + 
    #geom_smooth(method = "lm", colour = "black")+
    ylab("Air temperature (°C)") +
    xlab("Date") +
    theme_bw() +
    #scale_color_manual(values = c("dark green", "blue", "red")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# Make facet plot of the monthly timeseries
facet_HOBO_timeseries_monthly <- grid.arrange(plot_HOBO_air_temp_monthly,plot_HOBO_ground_temp_monthly,
                                      plot_HOBO_soil_moist_monthly, plot_HOBO_soil_temp_monthly)





