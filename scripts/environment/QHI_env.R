### QHI environmental conditions
# Script by Madi 08/12/2022
# last updated: 14/12/2022

# LIBRARIES ---- 

library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)

# TOMST ----
# from 2022 27 July - 16 August 2022 

# data largely wrangled already 

tomst_QHI_mean_daily_temp <- read.csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_temp.csv")


# HOBO ---- 
# data from 2022 
# data from 2017-18 

# iButtons ---- 
# ignore for now 
# eclectic mix of data  