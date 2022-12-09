# KP environmental 
# Script by Erica Zaja, created on 08/12/2022
# Last updated: 08/12/2022

# 90% snow free plateau from phenocams 2022
# June 18, 15, 21, 17, 18, 19, 26, 21 --> mean = June 18th
# setting start of summer on KP: June 18th
# End of summer: August 31st.

# Workflow
# 1. Explore elevation, filter anything below treeline (what is the elevation of treeline on KP?)
# 2. Explore date ranges, filter dates to: june 18-aug 31
# 3. Merge datasets
# 4. Make means for each year 

# LIBRARIES ----
library(MazamaCoreUtils)
library(arsenal)

# LOADING DATA ----

# 2016-2017 ibuttons: record air temperature at ground level 
# need to check elevations with John Godlee
PC_2016_ibuttons <- read_excel("data/environment/Kluane/PC_2016_ibuttons.xlsx")
KP_2016_ibuttons <- read_excel("data/environment/Kluane/KP_2016_ibuttons.xlsx")

# surface temp 2016-2017 ?? Need to check with Haydn what these are
KP_tea_surface_1 <- read.csv("data/environment/Kluane/KP_tea_surface_1.csv", skip = 14)
KP_tea_surface_2 <- read.csv("data/environment/Kluane/KP_tea_surface_2.csv", skip = 14)

# soil temp 2015-2017 (1 july 2015- 1 January 2017)
Formatted_temps <- read_csv("data/hobo/Formatted_temps.csv")

# soil temp 2016-2017 Need to check with Haydn what these are
KP_tea_soil_1 <- read.csv("data/environment/Kluane/KP_tea_soil_1.csv",  skip = 14)
KP_tea_soil_2 <- read.csv("data/environment/Kluane/KP_tea_soil_2.csv",  skip = 14)

# surface temp 2017 - need to exclude lower elevation site 1 (site 1 = 1050, site 2: 1305, site 3: 1530)
# KP_2017_herb_site1_1 <- read.csv("data/environment/Kluane/KP_2017_herb_site1_1.csv", skip = 14)
# KP_2017_herb_site1_2 <- read.csv("data/environment/Kluane/KP_2017_herb_site1_2.csv", skip = 14) # there were 2 ibuttons at site 1
KP_2017_herb_low <- read.csv("data/environment/Kluane/KP_2017_herb_site2.csv", skip = 14)
KP_2017_herb_mid <- read.csv("data/environment/Kluane/KP_2017_herb_site3.csv", skip = 14)

# Don't need these below, they are integrated in the data above
# KP_herb_site3_2017 <- read.csv("data/environment/Kluane/KP_herb_site3_2017.csv", skip = 14)
# KP_herb_site2_2017 <- read.csv("data/environment/Kluane/KP_herb_site2_2017.csv", skip = 14)
# KP_herb_site1_2017 <- read.csv("data/environment/Kluane/KP_herb_site1_2017.csv", skip = 14)

# 2018 (different elevations)
# exclude KP_2018_1 and KP_2018_2 because they are in the boreal forest (1049m)
# KP_2018_1 <- read.csv("data/environment/Kluane/Canada.Kluane.1049m.2018.08.05 11h20_a.csv", skip = 14)
# KP_2018_2 <- read_csv("data/environment/Kluane/Canada.Kluane.1049m.2018.08.05 11h20_b.csv", skip = 14)
KP_2018_low <- read_csv("data/environment/Kluane/Canada.Kluane.1305m.2018.08.05 15h20_a.csv", skip = 14)
KP_2018_low2 <- read_csv("data/environment/Kluane/Canada.Kluane.1305m.2018.08.05 15h20_b.csv", skip = 14)
KP_2018_mid <- read_csv("data/environment/Kluane/Canada.Kluane.1527m.2018.08.05 17h43_a.csv", skip = 14)
KP_2018_mid2 <- read_csv("data/environment/Kluane/Canada.Kluane.1527m.2018.08.05 17h43_b.csv", skip = 14)
KP_2018_high <- read_csv("data/environment/Kluane/Canada.Kluane.1816m.2018.08.05 20h30_a.csv", skip = 14)
KP_2018_high2 <- read_csv("data/environment/Kluane/Canada.Kluane.1816m.2018.08.05 20h30_b.csv", skip = 14)

# 2022
KP_FullTOMST_2022 <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv")

# DATA WRANGLING -------

# Exploring 2016 datasets
str(KP_2016_ibuttons)

# exploring date ranges
range(KP_2016_ibuttons$Time) # 4 July 2016 - 6 July 2016
range(PC_2016_ibuttons$Time) # 28th June 2016 - 2 July 2016
# need these

# Exploring 2016-2017 datasets 
str(KP_tea_surface_1)

# converting to date format
KP_tea_surface_1$Date.Time <- as.POSIXct(KP_tea_surface_1$Date.Time, format = "%d/%m/%Y")
KP_tea_surface_2$Date.Time <- as.POSIXct(KP_tea_surface_2$Date.Time, format = "%d/%m/%Y")
KP_tea_soil_1$Date.Time <- as.POSIXct(KP_tea_soil_1$Date.Time, format = "%d/%m/%Y")
KP_tea_soil_2$Date.Time <- as.POSIXct(KP_tea_soil_2$Date.Time, format = "%d/%m/%Y")

# function to convert "0017" to "2017"
two_dig_year_cnvt <- function(z, year=2013){
  y <- as.numeric(format(z, '%Y'))
  range <- 2013 - 2000
  year(z) <- ifelse(y >= 0 & y <= range, 2000 + y, 
                    ifelse(y > range & y <= 200, 2000 + y, y))
  z
}

# converting 00 to 20 
KP_tea_surface_1$Date.Time <- two_dig_year_cnvt(KP_tea_surface_1$Date.Time) 
KP_tea_surface_2$Date.Time <- two_dig_year_cnvt(KP_tea_surface_2$Date.Time) 
KP_tea_soil_1$Date.Time <- two_dig_year_cnvt(KP_tea_soil_1$Date.Time) 
KP_tea_soil_2$Date.Time <- two_dig_year_cnvt(KP_tea_soil_2$Date.Time) 

# dates ranges
range(KP_tea_surface_1$Date.Time) # 3 september 2016 - 10th october 2017
range(KP_tea_surface_2$Date.Time) # 4 July 2016 -  10 June 2017
range(KP_tea_soil_1$Date.Time) # 4 july 2016 - 10 june 2017
range(KP_tea_soil_2$Date.Time) # 3 september 2016 - 10th october 2017

# Exploring datasets 2017-2018
str(KP_2017_herb_site1_1)
KP_2017_herb_site1_1$Date.Time <- as.POSIXct(KP_2017_herb_site1_1$Date.Time, format = "%d/%m/%Y")
KP_2017_herb_site1_2$Date.Time <- as.POSIXct(KP_2017_herb_site1_1$Date.Time, format = "%d/%m/%Y")
KP_2017_herb_site2$Date.Time <- as.POSIXct(KP_2017_herb_site2$Date.Time, format = "%d/%m/%Y")
KP_2017_herb_site3$Date.Time <- as.POSIXct(KP_2017_herb_site3$Date.Time, format = "%d/%m/%Y")

# KP_herb_site1_2017$Date.Time <- as.POSIXct(KP_herb_site1_2017$Date.Time, format = "%d/%m/%Y")
KP_herb_site2_2017$Date.Time <- as.POSIXct(KP_herb_site2_2017$Date.Time, format = "%d/%m/%Y")
KP_herb_site3_2017$Date.Time <- as.POSIXct(KP_herb_site3_2017$Date.Time, format = "%d/%m/%Y")

# converting 00 to 20 in years
KP_2017_herb_site1_1$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site1_1$Date.Time) 
KP_2017_herb_site1_2$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site1_2$Date.Time) 
KP_2017_herb_site2$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site2$Date.Time) 
KP_2017_herb_site3$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site3$Date.Time) 
KP_herb_site1_2017$Date.Time <- two_dig_year_cnvt(KP_herb_site1_2017$Date.Time) 
KP_herb_site2_2017$Date.Time <- two_dig_year_cnvt(KP_herb_site2_2017$Date.Time) 
KP_herb_site3_2017$Date.Time <- two_dig_year_cnvt(KP_herb_site3_2017$Date.Time) 

range(KP_2017_herb_site1_1$Date.Time) # 2 august 2017- 5 august2017
range(KP_2017_herb_site1_2$Date.Time) # 2 august 2017- 5 august 2017
range(KP_2017_herb_site2$Date.Time) # 2 august 2017- 5 august 2017
range(KP_2017_herb_site3$Date.Time) # 2 august 2017- 5 august 2017
range(KP_herb_site1_2017$Date.Time) # 3 august 2017- 4 august 2017
range(KP_herb_site2_2017$Date.Time) # 3 august 2017- 4 august 2017
range(KP_herb_site3_2017$Date.Time) # 3 august 2017- 4 august 2017

identical(KP_2017_herb_site1_1, KP_herb_site1_2017)
summary(comparedf(KP_2017_herb_site1_1, KP_herb_site1_2017))

# Exploring 2018 data
str(KP_2018_3)

KP_2018_3 <- KP_2018_3 %>%
  rename("Date.Time" = "Date/Time")

KP_2018_4 <- KP_2018_4 %>%
  rename("Date.Time" = "Date/Time")

KP_2018_5 <- KP_2018_5 %>%
  rename("Date.Time" = "Date/Time")

KP_2018_6 <- KP_2018_6 %>%
  rename("Date.Time" = "Date/Time")

KP_2018_7 <- KP_2018_7 %>%
  rename("Date.Time" = "Date/Time")

KP_2018_8 <- KP_2018_8 %>%
  rename("Date.Time" = "Date/Time")

KP_2018_3$Date.Time  <- as.POSIXct(KP_2018_$Date.Time, format = "%d/%m/%Y")
KP_2018_4$Date.Time  <- as.POSIXct(KP_2018_4$Date.Time, format = "%d/%m/%Y")
KP_2018_5$Date.Time  <- as.POSIXct(KP_2018_5$Date.Time, format = "%d/%m/%Y")
KP_2018_6$Date.Time  <- as.POSIXct(KP_2018_6$Date.Time, format = "%d/%m/%Y")
KP_2018_7$Date.Time  <- as.POSIXct(KP_2018_7$Date.Time, format = "%d/%m/%Y")
KP_2018_8$Date.Time  <- as.POSIXct(KP_2018_8$Date.Time, format = "%d/%m/%Y")

KP_2018_3$Date.Time  <- two_dig_year_cnvt(KP_2018_3$Date.Time ) 
KP_2018_4$Date.Time  <- two_dig_year_cnvt(KP_2018_4$Date.Time ) 
KP_2018_5$Date.Time  <- two_dig_year_cnvt(KP_2018_5$Date.Time ) 
KP_2018_6$Date.Time  <- two_dig_year_cnvt(KP_2018_6$Date.Time ) 
KP_2018_7$Date.Time  <- two_dig_year_cnvt(KP_2018_7$Date.Time ) 
KP_2018_8$Date.Time  <- two_dig_year_cnvt(KP_2018_8$Date.Time ) 

range(KP_2018_3$Date.Time) # 5 july 2018- 7 july 2018
range(KP_2018_4$Date.Time) # 5 july 2018- 7 july 2018
range(KP_2018_5$Date.Time) # 5 july 2018- 7 july 2018
range(KP_2018_6$Date.Time) # 5 july 2018- 7 july 2018
range(KP_2018_7$Date.Time) # 5 july 2018- 7 july 2018
range(KP_2018_8$Date.Time) # 5 july 2018- 7 july 2018

range(KP_FullTOMST_2022$Datetime_UTC) # 1 june 2022 - 15 aug 2022

# Merging datasets
KP_ibuttons_all_surface_temps <- rbind(KP_tea_surface_1, KP_tea_surface_2, 
                              KP_2017_herb_site1_1, KP_2017_herb_site1_2, 
                              KP_2017_herb_site2, KP_2017_herb_site3,
                              KP_herb_site1_2017, KP_herb_site2_2017,
                              KP_herb_site3_2017,KP_2018_3, KP_2018_4,
                              KP_2018_5, KP_2018_6, KP_2018_7, KP_2018_8)

# make a year column, then group by year and calculate mean of the value
