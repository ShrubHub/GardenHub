#### Common garden HOBO 2021 SCRIPT
### Data wrangling and visualisation script
### By Erica Zaja and Madi Anderson, created on 20/10/2022
## Last updated: 26/01/2023 by Madelaine 

# 1. LOADING LIBRARIES -----
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(readr)

# 2. LOADING DATA ----
HOBO_Common_garden_12Aug2021 <- read.csv(file = "data/hobo/HOBO_Common_garden_12Aug2021.csv") # 2018-21
HOBO_Common_garden_2017 <-  read.csv2("data/hobo/common_garden/Kluane_hobo_Common_garden_2017.csv", sep=",") #2015-17
str(HOBO_Common_garden_2017)

# 3. DATA WRANGLING ----
# 2017 
str(HOBO_Common_garden_2017)
# normal dates 
HOBO_Common_garden_2017_wrangle_good <- HOBO_Common_garden_2017 %>% 
  filter(Date.Time..GMT.06.00 <= "12/31/16" )
# wrong dates 
HOBO_Common_garden_2017_wrangle_bad <- HOBO_Common_garden_2017 %>% 
  filter(Date.Time..GMT.06.00 >= "12/31/16" ) 

HOBO_Common_garden_2017_wrangle_bad$Date.Time..GMT.06.00 <- ymd_hm(HOBO_Common_garden_2017_wrangle_bad$Date.Time..GMT.06.00)
HOBO_Common_garden_2017_wrangle_bad$day <- month(as.POSIXlt(HOBO_Common_garden_2017_wrangle_bad$Date.Time..GMT.06.00, format="%Y-%m-%d %H:%M"))
HOBO_Common_garden_2017_wrangle_bad$month_wrong <- year(as.POSIXlt(HOBO_Common_garden_2017_wrangle_bad$Date.Time..GMT.06.00, format="%Y-%m-%d %H:%M"))
HOBO_Common_garden_2017_wrangle_bad$year <- day(as.POSIXlt(HOBO_Common_garden_2017_wrangle_bad$Date.Time..GMT.06.00, format="%Y-%m-%d %H:%M"))
# subtract 2000 from this value to get correct month value 
HOBO_Common_garden_2017_wrangle_bad_1  <- HOBO_Common_garden_2017_wrangle_bad %>% 
  mutate(Month = month_wrong - 2000) %>% 
  select(-c(X., month_wrong)) %>% 
  mutate(year = year + 2000)

HOBO_Common_garden_2017_wrangle_bad_1$date <- as.Date(with(HOBO_Common_garden_2017_wrangle_bad_1, paste(year, Month, day,sep="/")), "%m/%d/%Y")

HOBO_Common_garden_2017_wrangle_bad_1$date <- paste(HOBO_Common_garden_2017_wrangle_bad_1$Month,
                                                    HOBO_Common_garden_2017_wrangle_bad_1$day, 
                                                    HOBO_Common_garden_2017_wrangle_bad_1$year,
                                                    sep="/") %>% mdy() %>% 
  as.Date()

HOBO_Common_garden_2017_wrangle_bad_1$date_new <- format(as.Date(HOBO_Common_garden_2017_wrangle_bad_1$date, format = "%m-%d-%Y"), "%m/%d/%y")
# drop confusing old columns
HOBO_Common_garden_2017_wrangle_bad_2 <- HOBO_Common_garden_2017_wrangle_bad_1 %>% 
  select(-c(Date.Time..GMT.06.00, date))

#FINALLY 
# reclass date to drop H:M and rename good dataframe 
HOBO_Common_garden_2017_wrangle_good$new_date_1 <-  parse_date_time(HOBO_Common_garden_2017_wrangle_good$Date.Time..GMT.06.00, "%m/%d/%y %I:%M:%S %p")
         
HOBO_Common_garden_2017_wrangle_good$date_new <- format(as.Date(HOBO_Common_garden_2017_wrangle_good$new_date_1, format = "%m-%d-%Y %I:%M:%S %p"), "%m/%d/%y")

HOBO_Common_garden_2017_wrangle_good_1 <- HOBO_Common_garden_2017_wrangle_good %>% 
  select(-c(X., Date.Time..GMT.06.00, new_date_1))

# merge good and reformatted 
HOBO_Common_garden_2017_wrangle_solve <- left_join(HOBO_Common_garden_2017_wrangle_bad_2, 
                                                   HOBO_Common_garden_2017_wrangle_good_1, 
                                                   by = c("date_new", 
                                                          "Water.Content..m..m...LGR.S.N..10742708..SEN.S.N..10736284..LBL..Soil.moisture.", 
                                                          "Temp...C..LGR.S.N..10742708..SEN.S.N..10736450..LBL..Ground.temp.", 
                                                          "Temp...C..LGR.S.N..10742708..SEN.S.N..10736453..LBL..Soil.temp.", 
                                                          "Temp...C..LGR.S.N..10742708..SEN.S.N..10736452..LBL..Air.temp."))

CG_HOBO_2017 <- HOBO_Common_garden_2017_wrangle_solve %>%
  rename("Soil_moist" = "Water.Content..m..m...LGR.S.N..10742708..SEN.S.N..10736284..LBL..Soil.moisture.",
         "Ground_temp" = "Temp...C..LGR.S.N..10742708..SEN.S.N..10736450..LBL..Ground.temp.",
         "Air_temp" ="Temp...C..LGR.S.N..10742708..SEN.S.N..10736452..LBL..Air.temp.",
         "Soil_temp" = "Temp...C..LGR.S.N..10742708..SEN.S.N..10736453..LBL..Soil.temp.", 
         "Date" = "date_new") 

# 2021 
CG_HOBO_2021 <- HOBO_Common_garden_12Aug2021 %>%
  rename("Soil_moist" = "Water.Content_.m..m..",
         "Ground_temp" = "Ground_temp_..C.",
         "Air_temp" ="Air_temp_..C.",
         "Soil_temp" = "Soil_Temp_..C.", 
         "Date_time_GMT" = "Date.Time..GMT.07.00") %>% 
  dplyr::select(-X.)
CG_HOBO_2021$date <- mdy_hms(CG_HOBO_2021$Date_time_GMT)

CG_HOBO_2021$day <- day(as.POSIXlt(CG_HOBO_2021$date, format="%Y-%m-%d %H:%M:S"))
CG_HOBO_2021$Month <- month(as.POSIXlt(CG_HOBO_2021$date, format="%Y-%m-%d %H:%M:S"))
CG_HOBO_2021$year <- year(as.POSIXlt(CG_HOBO_2021$date, format="%Y-%m-%d %H:%M:S"))

CG_HOBO_2021_test <- CG_HOBO_2021 %>% 
  select(-Date_time_GMT)

CG_HOBO_2021_test$Date <- format(as.POSIXct(CG_HOBO_2021_test$date,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%y')

# reclass variables 
str(CG_HOBO_2017)
CG_HOBO_2017$Soil_moist <- as.numeric(CG_HOBO_2017$Soil_moist)
CG_HOBO_2017$Ground_temp <- as.numeric(CG_HOBO_2017$Ground_temp)
CG_HOBO_2017$Air_temp <- as.numeric(CG_HOBO_2017$Air_temp)
CG_HOBO_2017$Soil_temp <- as.numeric(CG_HOBO_2017$Soil_temp)
str(CG_HOBO_2021)
# drop old date column sorry this is so convoluted 
CG_HOBO_2021_test_1 <- CG_HOBO_2021_test %>% 
  select(-date)
  
# merge dataframes together 
CG_HOBO <- rbind(CG_HOBO_2017, CG_HOBO_2021_test_1)

str(CG_HOBO)  

CG_HOBO$Date <- mdy(CG_HOBO$Date)

# save all CG hobo merged together 
write.csv(CG_HOBO, "data/hobo/all_CG_HOBO.csv")

str(CG_HOBO)

# Make DAILY means (one value per day)
CG_HOBO_daily_means <- CG_HOBO %>%
  group_by(year, Month, day) %>%
  mutate(mean_soil_moist = mean(Soil_moist),
         mean_ground_temp = mean(Ground_temp),
         mean_soil_temp = mean(Soil_temp),
         mean_air_temp = mean(Air_temp)) %>%
  dplyr::select(Date, year, Month, day, mean_soil_moist, mean_ground_temp, mean_soil_temp,
         mean_air_temp) %>%
  group_by(year, Month, day) %>%
  slice(1) %>% # keeping only one observation from each set of year/month
  ungroup()

max(CG_HOBO_daily_means$Date)
range(CG_HOBO_daily_means$mean_soil_moist) # -0.06062708  0.40506250

# filtering for months of interest only 
# july and august
CG_HOBO_daily_means_season <- CG_HOBO_daily_means %>%
  filter(Month %in% c("6", "7", "8")) 

range(CG_HOBO_daily_means_season$mean_soil_moist) # -0.01660833  0.15310833

# Make MONTHLY means 
CG_HOBO_monthly_means_season <- CG_HOBO_daily_means_season %>%
  group_by(year, Month) %>%
  mutate(mean_soil_moist_month = mean(mean_soil_moist),
         mean_ground_temp_month = mean(mean_ground_temp),
         mean_soil_temp_month = mean(mean_soil_temp),
         mean_air_temp_month = mean(mean_air_temp)) %>%
  dplyr::select(year, Month, mean_soil_moist_month, mean_ground_temp_month, mean_soil_temp_month,
         mean_air_temp_month) %>%
  group_by(year, Month) %>%
  slice(1) %>% # keeping only one observation from each set of year/month
  ungroup() %>%
  rename("month"="Month")

range(CG_HOBO_monthly_means_season$mean_soil_moist_month) #  -0.004076823  0.105578542

# NB 2016 only has sept-oct-nov months so it gets filtered out when only
# keeping months of interest

# saving as csv
write.csv(CG_HOBO_monthly_means_season, "data/hobo/CG_HOBO_monthly_means_season.csv")
CG_HOBO_monthly_means_season <- read_csv("data/hobo/CG_HOBO_monthly_means_season.csv")

# ONE June mean value per variable
CG_HOBO_monthly_means_june <- CG_HOBO_monthly_means_season %>%
  filter(month == "6")

mean(CG_HOBO_monthly_means_june$mean_soil_moist_month) # 0.05757273
mean(CG_HOBO_monthly_means_june$mean_ground_temp_month) # 12.53025
mean(CG_HOBO_monthly_means_june$mean_soil_temp_month) # 11.20712
mean(CG_HOBO_monthly_means_june$mean_air_temp_month) # 12.54286

# ONE July mean value per variable
CG_HOBO_monthly_means_july <- CG_HOBO_monthly_means_season %>%
  filter(month == "7")

mean(CG_HOBO_monthly_means_july$mean_soil_moist_month) # 0.05968028
mean(CG_HOBO_monthly_means_july$mean_ground_temp_month) # 13.11494
mean(CG_HOBO_monthly_means_july$mean_soil_temp_month) # 13.64918
mean(CG_HOBO_monthly_means_july$mean_air_temp_month) # 14.01731

# ONE August mean value per variable
CG_HOBO_monthly_means_aug <- CG_HOBO_monthly_means_season %>%
  filter(month == "8")

mean(CG_HOBO_monthly_means_aug$mean_soil_moist_month) # 0.06137545
mean(CG_HOBO_monthly_means_aug$mean_ground_temp_month) # 11.57838
mean(CG_HOBO_monthly_means_aug$mean_soil_temp_month) # 11.98366
mean(CG_HOBO_monthly_means_aug$mean_air_temp_month) # 12.42648

# 4. DATA VISUALISATION -----

# 4.1 Time series -----

# a. Ground temperature ----
(plot_HOBO_ground_temp <- ggplot(CG_HOBO, aes(x = Date, y = Ground_temp)) +
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
(plot_HOBO_soil_moist <- ggplot(CG_HOBO, aes(x = Date, y = Soil_moist)) +
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
(plot_HOBO_soil_temp <- ggplot(CG_HOBO, aes(x = Date, y = Soil_temp)) +
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
(plot_HOBO_air_temp <- ggplot(CG_HOBO, aes(x = Date, y = Air_temp)) +
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
(plot_HOBO_ground_temp_monthly <- ggplot(CG_HOBO_monthly_means_season, aes(x = date, y = mean_ground_temp_month)) +
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
(plot_HOBO_soil_moist_monthly <- ggplot(CG_HOBO_monthly_means_july, aes(x = year, y = mean_soil_moist_month)) +
   geom_point() + 
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





