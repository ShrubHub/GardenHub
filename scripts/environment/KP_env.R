# KP environmental 
# Script by Erica Zaja, created on 08/12/2022
# Last updated: 08/12/2022

# LIBRARIES ----
library(MazamaCoreUtils)

# LOADING DATA ----

# 2016-2017
PC_2016_ibuttons <- read_excel("data/environment/Kluane/PC_2016_ibuttons.xlsx")
KP_2016_ibuttons <- read_excel("data/environment/Kluane/KP_2016_ibuttons.xlsx")

# surface temp 2016-2017
KP_tea_surface_1 <- read.csv("data/environment/Kluane/KP_tea_surface_1.csv", skip = 14)
KP_tea_surface_2 <- read.csv("data/environment/Kluane/KP_tea_surface_2.csv", skip = 14)

# soil temp 2016-2017
KP_tea_soil_1 <- read.csv("data/environment/Kluane/KP_tea_soil_1.csv",  skip = 14)
KP_tea_soil_2 <- read.csv("data/environment/Kluane/KP_tea_soil_2.csv",  skip = 14)

# surface temp 2017-2018
KP_2017_herb_site1_1 <- read.csv("data/environment/Kluane/KP_2017_herb_site1_1.csv", skip = 14)
KP_2017_herb_site1_2 <- read.csv("data/environment/Kluane/KP_2017_herb_site1_2.csv", skip = 14)
KP_2017_herb_site2 <- read.csv("data/environment/Kluane/KP_2017_herb_site2.csv", skip = 14)
KP_2017_herb_site3 <- read.csv("data/environment/Kluane/KP_2017_herb_site3.csv", skip = 14)
KP_herb_site3_2017 <- read.csv("data/environment/Kluane/KP_herb_site3_2017.csv", skip = 14)
KP_herb_site2_2017 <- read.csv("data/environment/Kluane/KP_herb_site2_2017.csv", skip = 14)
KP_herb_site1_2017 <- read.csv("data/environment/Kluane/KP_herb_site1_2017.csv", skip = 14)
 
# 2022
KP_FullTOMST_2022 <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv")

# DATA WRANGLING -------

# 90% snow free plateau from phenocams 2022
# June 18, 15, 21, 17, 18, 19, 26, 21 --> mean = June 18th
# setting start of summer on KP: June 18th
# End of summer: August 31st.

# Workflow
# 1. Explore date ranges 
# 2. filter dates: june 18-aug 31
# 3. merge datasets
# 4. make means

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
KP_herb_site1_2017$Date.Time <- as.POSIXct(KP_herb_site1_2017$Date.Time, format = "%d/%m/%Y")


# converting 00 to 20 in years
KP_2017_herb_site1_1$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site1_1$Date.Time) 
KP_2017_herb_site1_2$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site1_2$Date.Time) 
KP_2017_herb_site2$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site2$Date.Time) 
KP_2017_herb_site3$Date.Time <- two_dig_year_cnvt(KP_2017_herb_site3$Date.Time) 

range(KP_2017_herb_site1_1$Date.Time) # 2 august 2017- 5 august2017
range(KP_2017_herb_site1_2$Date.Time) # 2 august 2017- 5 august 2017
range(KP_2017_herb_site2$Date.Time) # 2 august 2017- 5 august 2017
range(KP_2017_herb_site3$Date.Time) # 2 august 2017- 5 august 2017
