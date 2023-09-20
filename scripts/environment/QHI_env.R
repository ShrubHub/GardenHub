### QHI environmental conditions
# Script by Madi 08/12/2022
# last updated: 27/02/2023

# LIBRARIES ---- 

library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)


#### functio to extract TOMST data 

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

tomst_qhi <- "data/tomst/QHI_TOMST_Aug2023/data_files_only"
files_qhi <- list.files(path = tomst_qhi, pattern = "^data_*", full.names = T)
qhi_data <- map_dfr(files_qhi, read_tms4)

tomst_kp <- "data/tomst/KLU_TOMST_backup_2023/KP_data_files_aug2023"
files_kp <- list.files(path = tomst_kp, pattern = "^data_*", full.names = T)
kp_data <- map_dfr(files_kp, read_tms4)

tomst_cg <- "data/tomst/KLU_TOMST_backup_2023/Common_garden/data_files"
files_cg <- list.files(path = tomst_cg, pattern = "^data_*", full.names = T)
cg_data <- map_dfr(files_kp, read_tms4)

# change date (GMT to NWT time) - 7 hours time difference
qhi_data$Datetime_UTC <- qhi_data$Datetime_UTC - hours(7)

tomst_qhi_data <-  qhi_data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2022-07-27 15:00")) %>% 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

# save as .csv
write.csv(tomst_qhi_data, "data/tomst/2023/tomst_qhi_2023_data.csv")

# change date (GMT to NWT time) - 7 hours time difference
kp_data$Datetime_UTC <- kp_data$Datetime_UTC - hours(7)

tomst_kp_data <-  kp_data %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2022-07-27 15:00")) %>% 
  pivot_longer(cols = 5:8,
               names_to = "Variable",
               values_to = "Value")

# TOMST ----
# from 2022 27 July - 16 August 2022 

# data largely wrangled already 
# tomst_QHI_mean_daily_temp <- read.csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_temp.csv")

# HOBO ---- 
# data from 2022 
# data collection and processing by Elise Gallois 
# script adapted from Elise Gallois truly she did it all
hobo_QHI_2022 <- read.csv("data/hobo/QHI_FullHOBO_2022.csv")
hobo_QHI_2022$Serial_No <- substr(hobo$SerialID, 54, 61)

# drop unnecessary columns and keep only temp data
# make separate date and hour column 
# keep only observations post July 27 2022 aka when they were set up 
hobo_clean_2022 <- hobo_QHI_2022 %>% 
  filter(Variable == "Temperature") %>% 
  select(-c(End.of.File, Button.Down, Host.Connected, Button.Up, Index)) %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  mutate(hour = lubridate::hour(Datetime_UTC)) %>% 
  filter(Date > lubridate::ymd("2022-07-27"))
# how many sensors 
length(unique(hobo_clean_2022$Serial_No))
  
# summarize by day, group all sensors together 
hobo_mean_temp_2022 <- hobo_clean_2022 %>%
  group_by(Date) %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE), 
            sd_temp = sd(Value, na.rm = T)) %>% 
  ungroup()

mean(hobo_mean_temp_2022$mean_temp) # 9.853633
mean(hobo_mean_temp_2022$sd_temp) # 6.236422

hobo_clean_2022$month <- month(as.POSIXlt(hobo_clean_2022$Date, format="%Y-%m-%d"))
hobo_clean_2022$year <- year(as.POSIXlt(hobo_clean_2022$Date, format="%Y-%m-%d"))
hobo_clean_2022$day <- day(as.POSIXlt(hobo_clean_2022$Date, format="%Y-%m-%d"))

hobo_mean_july_2022 <- hobo_clean_2022 %>%
  group_by(month) %>% 
  summarise(mean_temp = mean(Value, na.rm = TRUE), 
            sd_temp = sd(Value, na.rm = T)) %>% 
  ungroup()

# july 2022 8.018875

# QHI soil temp 2015-16 ----
# data from phenology ridge HOBO station 
# summarized by Haydn Thomas 

Haydn_summary <- read.csv("data/environment/Formatted_temps_shrubhub.csv") # NB THIS IS SOIL DATA
metadata_summary <-  read.csv("data/environment/metadata_shrubhub.csv")

# match metadata & enviro data
summary <- full_join(Haydn_summary, metadata_summary, by = "Plotcode")
# filter only QHI data 
unique(summary$Plotcode)
# mix of HOBO and iButton - combine for now 
QHI_soil_summary <- summary %>% 
  dplyr::filter(Plotcode %in% c("QHI_phenology_plot", "HH_1", "HH_2", "HK_1", "HK_2", 
                                "HR_1", "HR_2", "HS_1", "HS_2", "HV_1", "HV_2")) %>% 
  dplyr::select(-X) # drop sort column 

july_QHI_enviro <- QHI_soil_summary %>% 
  filter(Month == "7")
july_QHI_enviro_mean <- july_QHI_enviro  %>%
  group_by(Year) %>% 
  summarise(mean_temp = mean(Temperature), 
            sd_temp = sd(Temperature))

# 2015: 9.05
# 2016: 6.79
mean(july_QHI_enviro$Temperature)
sd(july_QHI_enviro$Temperature)



# phenology ridge hobo data from 2017 ----
pheno_hobo <- read_csv("data/environment/QHI/HOBO_Phenology_ridge_11th_Aug_2017.csv")
str(pheno_hobo)

# clean up 
pheno_hobo_working <- pheno_hobo %>% 
  rename("Air_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Air Temp)",
         "Canopy_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Canopy Temp)",
         "Ground_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Ground Temp)",
         "Soil_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Soil Temp)", 
         "Date_time" = "Date Time, GMT-06:00") %>% 
  dplyr::select(-c("Good Battery (LGR S/N: 10737643)", "Host Connected (LGR S/N: 10737643)", 
            "End Of File (LGR S/N: 10737643)"))
# date issue 
# normal dates 
pheno_hobo_dates_good <- pheno_hobo_working %>% 
  filter(Date_time <= "08/01/17" )

# wrong dates 
pheno_hobo_dates_bad <- pheno_hobo_working %>% 
  filter(Date_time >= "08/01/17" )

pheno_hobo_dates_bad$Date_time <- ymd_hm(pheno_hobo_dates_bad$Date_time)
pheno_hobo_dates_bad$day <- month(as.POSIXlt(pheno_hobo_dates_bad$Date_time, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_bad$month_wrong <- year(as.POSIXlt(pheno_hobo_dates_bad$Date_time, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_bad$year <- day(as.POSIXlt(pheno_hobo_dates_bad$Date_time, format="%Y-%m-%d %H:%M"))
# subtract 2000 from this value to get correct month value 
pheno_hobo_dates_bad_1  <- pheno_hobo_dates_bad %>% 
  mutate(Month = month_wrong - 2000) %>% 
  select(-c(month_wrong)) %>% 
  mutate(year = year + 2000)

pheno_hobo_dates_bad_1$date <- paste(pheno_hobo_dates_bad_1$Month,
                                     pheno_hobo_dates_bad_1$day, 
                                     pheno_hobo_dates_bad_1$year,
                                                    sep="/") %>% mdy() %>% 
  as.Date()

pheno_hobo_dates_bad_1$date_new <- format(as.Date(pheno_hobo_dates_bad_1$date, format = "%m-%d-%Y"), "%m/%d/%y")
# drop confusing old columns
pheno_hobo_dates_bad_1 <- pheno_hobo_dates_bad_1 %>% 
  select(-c(Date_time))

# reclass date to drop H:M and rename good dataframe 
pheno_hobo_dates_good$Date_time_1 <- ymd_hm(pheno_hobo_dates_good$new_date_1)
pheno_hobo_dates_good$new_date_1 <-  parse_date_time(pheno_hobo_dates_good$Date_time, "%m/%d/%y %I:%M:%S %p")
pheno_hobo_dates_good$Month <- month(as.POSIXlt(pheno_hobo_dates_good$new_date_1, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_good$year <- year(as.POSIXlt(pheno_hobo_dates_good$new_date_1, format="%Y-%m-%d %H:%M"))
pheno_hobo_dates_good$day <- day(as.POSIXlt(pheno_hobo_dates_good$new_date_1, format="%Y-%m-%d %H:%M"))

pheno_hobo_dates_good$date_new <- format(as.Date(pheno_hobo_dates_good$new_date_1, format = "%m-%d-%Y %I:%M:%S %p"), "%m/%d/%y")

pheno_hobo_dates_good_1 <- pheno_hobo_dates_good %>% 
  select(-c(Date_time))

pheno_hobo_dates_good_1$day <- as.numeric(pheno_hobo_dates_good_1$day)
# merge good and bad dates
pheno_hobo_all <- full_join(pheno_hobo_dates_good_1, pheno_hobo_dates_bad_1, 
                            by = c("#", 
                            "Air_temp_C", 
                            "Canopy_temp_C", 
                            "Ground_temp_C", 
                            "Soil_temp_C", 
                            "Month", 
                            "day", 
                            "year", 
                            "date_new"))
# messy but it works for this 

pheno_hobo_all$Month <- as.factor(pheno_hobo_all$Month)

# means by month 
monthly_QHI_pheno_mean <- pheno_hobo_all  %>%
  group_by(Month) %>% 
  summarise(mean_air_temp = mean(Air_temp_C),
            mean_canopy_temp = mean(Canopy_temp_C), 
            mean_ground_temp = mean(Ground_temp_C), 
            mean_soil_temp = mean(Soil_temp_C))

QHI_pheno_mean <- pheno_hobo_all  %>%
  summarise(mean_air_temp = mean(Air_temp_C, na.rm = TRUE),
            mean_canopy_temp = mean(Canopy_temp_C, na.rm = TRUE), 
            mean_ground_temp = mean(Ground_temp_C, na.rm = TRUE), 
            mean_soil_temp = mean(Soil_temp_C, na.rm = TRUE))

aug_QHI_pheno <- pheno_hobo_all %>% 
  filter(Month == "8")
aug_QHI_pheno_mean <- aug_QHI_pheno  %>%
  summarise(mean_air_temp = mean(Air_temp_C, na.rm = TRUE),
            mean_canopy_temp = mean(Canopy_temp_C, na.rm = TRUE), 
            mean_ground_temp = mean(Ground_temp_C, na.rm = TRUE), 
            mean_soil_temp = mean(Soil_temp_C, na.rm = TRUE))

# filter july observations 
july_QHI_pheno <- pheno_hobo_all %>% 
  filter(Month == "7")
july_QHI_pheno_mean <- july_QHI_pheno  %>%
  group_by(year) %>% 
  summarise(mean_air_temp = mean(Air_temp_C),
            mean_canopy_temp = mean(Canopy_temp_C), 
            mean_ground_temp = mean(Ground_temp_C), 
            mean_soil_temp = mean(Soil_temp_C))

# mean temp hobo 2022 8.018875
# mean air temp hobo 2017 13.19234
# mean air temp TOMST 2022 5.01368

#use tomst and hobo 2017 bc overlap from 2022 of tomst and hobo 
temp_july <- c(5.01368, 13.19234)
mean(temp_july)
# 9.10301

# HOBO 2017 ----
# Isla found these files - 

pheno_hobo_raw <- read.csv("data/hobo/QHI_2017/Phenology_Ridge_HOBO_11th_Aug_2017.csv", header = F)
pheno_hobo_raw <- pheno_hobo_raw[-1,] # drop 
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
pheno_hobo_1 <- header.true(pheno_hobo_raw) # make first row column names 
str(pheno_hobo_1)

# clean up 
pheno_hobo_working <- pheno_hobo_1 %>% 
  rename("Air_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Air Temp)",
         "Canopy_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Canopy Temp)",
         "Ground_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Ground Temp)",
         "Soil_temp_C" = "Temp, °C (LGR S/N: 10737643, SEN S/N: 10737643, LBL: Soil Temp)", 
         "Date_time" = "Date Time, GMT-06:00") %>% 
  dplyr::select(-c("Bad Battery (LGR S/N: 10737643)", "Host Connected (LGR S/N: 10737643)", 
                   "End Of File (LGR S/N: 10737643)", "#")) %>% 
  separate(Date_time, c("month", "day", "year"), "/") %>% 
  separate(year, c("year_1", "time"), " ")

# reclass variables 
pheno_hobo_working$Air_temp_C <- as.numeric(pheno_hobo_working$Air_temp_C)
pheno_hobo_working$Canopy_temp_C <- as.numeric(pheno_hobo_working$Canopy_temp_C)
pheno_hobo_working$Ground_temp_C <- as.numeric(pheno_hobo_working$Ground_temp_C)
pheno_hobo_working$Soil_temp_C <- as.numeric(pheno_hobo_working$Soil_temp_C)

# means by month 
monthly_QHI_pheno_mean <- pheno_hobo_working  %>%
  group_by(month) %>% 
  summarise(mean_air_temp = mean(Air_temp_C, na.rm = T),
            sd_air_temp = sd(Air_temp_C, na.rm = T),
            mean_canopy_temp = mean(Canopy_temp_C, na.rm = T), 
            sd_canopy_temp = sd(Canopy_temp_C, na.rm = T),
            mean_ground_temp = mean(Ground_temp_C, na.rm = T),
            sd_ground_temp = sd(Ground_temp_C, na.rm = T),
            mean_soil_temp = mean(Soil_temp_C, na.rm = T), 
            sd_soil_temp = sd(Soil_temp_C, na.rm = T))

# same as above for colinson head 
colhead_hobo_raw <- read.csv("data/hobo/QHI_2017/Collinson_Head_HOBO_15th_Aug_2017.csv", header = T)

str(colhead_hobo_raw)
# clean up 
colhead_hobo_working <- colhead_hobo_raw %>% 
  rename("Air_temp_C" = "Temp...C..LGR.S.N..10742709..SEN.S.N..10736454..LBL..Air.temperature.",
         "Soil_moisture" = "Water.Content..m..m...LGR.S.N..10742709..SEN.S.N..10736285..LBL..Soil.moisture.",
         "Surface_temp_C" = "Temp...C..LGR.S.N..10742709..SEN.S.N..10736451..LBL..Surface.temperature.",
         "Soil_temp_C" = "Temp...C..LGR.S.N..10742709..SEN.S.N..10736449..LBL..Soil.temperature.", 
         "Date_time" = "Date.Time..GMT.06.00") %>% 
  dplyr::select(-c("X.", "Plot.Title..Herschel...Haydn")) 

# dates are messed up, separate good and bad dates 
pheno_hobo_dates_good <- colhead_hobo_working %>% 
    filter(Date_time <= "12/31/16 12:30:01 PM" )
  
# wrong dates 
pheno_hobo_dates_bad <- colhead_hobo_working %>% 
    filter(Date_time > "12/31/16 12:30:01 PM" )
# separate by month date year for good dates 
colhead_hobo_good_1 <- pheno_hobo_dates_good %>% 
separate(Date_time, c("month", "day", "year"), "/") %>% 
  separate(year, c("year_1", "time"), " ")
colhead_hobo_good_1$month <- as.numeric(colhead_hobo_good_1$month)

colhead_hobo_bad_1 <- pheno_hobo_dates_bad %>% 
  separate(Date_time, c("month_part", "day", "year"), "-") %>% 
  separate(year, c("year_1", "time"), " ") 
colhead_hobo_bad_1$month_part <- as.numeric(colhead_hobo_bad_1$month_part)
colhead_hobo_bad_2 <- colhead_hobo_bad_1 %>% 
  mutate(month = (month_part - 2000)) %>% 
  dplyr::select(-month_part)

colhead_hobo_all <- full_join(colhead_hobo_bad_2, colhead_hobo_good_1, 
                              by = c("month", 
                                     "year_1", 
                                     "Soil_moisture", 
                                     "Soil_temp_C", "Surface_temp_C", 
                                     "Air_temp_C", 
                                     "day", 
                                     "time"))

str(colhead_hobo_all)

monthly_QHI_colhead_mean <- colhead_hobo_all  %>%
  group_by(month) %>% 
  summarise(mean_air_temp = mean(Air_temp_C, na.rm = T),
            sd_air_temp = sd(Air_temp_C, na.rm = T), 
            mean_soil_moist = mean(Soil_moisture, na.rm = T),
            sd_soil_moist = sd(Soil_moisture, na.rm = T), 
            mean_surface_temp = mean(Surface_temp_C, na.rm = T), 
            sd_surface_temp = sd(Surface_temp_C, na.rm = T), 
            mean_soil_temp = mean(Soil_temp_C, na.rm = T), 
            sd_soil_temp = sd(Soil_temp_C, na.rm = T))

# SUMMARY MEANS with TOMST -----



# mean temp colhobo 2022 8.018875 +/- 7.130952 
# mean air temp hobo col head 2016-17: 10.7161888 +/- 4.995366
# mean air temp hobo pheno ridge 2016-17: 11.532801 +/- 5.270756
# mean air temp TOMST 2022 5.01368 +/- 2.42599
# mean of QHI for 2016-17: 
july_qhi_16_17 <- c(10.7161888, 11.532801)
mean(july_qhi_16_17) 
#11.12449
july_qhi_16_17_sd <- c(4.995366, 5.270756)
mean(july_qhi_16_17_sd) # 5.133061 mean sd 

#use tomst and hobo 2017 bc overlap from 2022 of tomst and hobo 
air_temp_july <- c(5.01368, 13.19234, 11.12449)
mean(air_temp_july) # 9.776837
sd(air_temp_july) #4.252616

# mean standard deviation 
air_sd_july <- c(2.42599, 5.133061, 7.130952)
mean(air_sd_july)

# mean soil temp colhobo 2022  
# mean soil temp hobo col head 2016-17: 3.6884819 +/- 1.0665079
# mean soil temp hobo pheno ridge 2016-17: clearly wrong....
# mean soil temp TOMST 2022:  3.323774 sd: 0.7855326
# mean of QHI july soil temp from Haydn summary: 7.45063 sd: 4.195172

#use tomst and hobo 2017 bc overlap from 2022 of tomst and hobo 
soil_temp_july <- c(3.6884819, 3.323774, 7.45063)
mean(soil_temp_july)
# 4.820962
sd(soil_temp_july)

# mean standard deviation 
soil_temp_sd_july <- c(1.0665079, 0.7855326, 4.195172)
mean(soil_temp_sd_july)
# 2.015738


            