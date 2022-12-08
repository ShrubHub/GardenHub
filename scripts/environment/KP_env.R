# KP environmental 
# Script by Erica Zaja, created on 08/12/2022
# Last updated: 08/12/2022

# LIBRARIES ----

# LOADING DATA ----

# 2016-2017
PC_2016_ibuttons <- read_excel("data/environment/Kluane/PC_2016_ibuttons.xlsx")
KP_2016_ibuttons <- read_excel("data/environment/Kluane/KP_2016_ibuttons.xlsx")

# surface temp 2016-2017
KP_tea_surface_1 <- read_csv("data/environment/Kluane/KP_tea_surface_1.csv")
KP_tea_surface_2 <- read_csv("data/environment/Kluane/KP_tea_surface_2.csv")
# soil temp 2016-2017-2018
KP_tea_soil_1 <- read_csv("data/environment/Kluane/KP_tea_soil_1.csv")
KP_tea_soil_2 <- read_csv("data/environment/Kluane/KP_tea_soil_2.csv")
KP_2017_herb_site1_1 <- read_csv("data/environment/Kluane/KP_2017_herb_site1_1.csv")
KP_2017_herb_site1_2 <- read_csv("data/environment/Kluane/KP_2017_herb_site1_2.csv")
KP_2017_herb_site2 <- read_csv("data/environment/Kluane/KP_2017_herb_site2.csv")
KP_2017_herb_site3 <- read_csv("data/environment/Kluane/KP_2017_herb_site3.csv")
KP_herb_site3_2017 <- read_csv("data/environment/Kluane/KP_herb_site3_2017.csv")
KP_herb_site2_2017 <- read_csv("data/environment/Kluane/KP_herb_site2_2017.csv")
KP_herb_site1_2017 <- read_csv("data/environment/Kluane/KP_herb_site1_2017.csv")

# 2022
KP_FullTOMST_2022 <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_FullTOMST_2022.csv")

# DATA WRANGLING

