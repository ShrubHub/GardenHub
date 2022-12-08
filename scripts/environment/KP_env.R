# KP environmental 
# Script by Erica Zaja, created on 08/12/2022
# Last updated: 08/12/2022

# LIBRARIES ----

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

# workflow
# 1. filter dates: june 18-aug 31
# 2. make means
# 


