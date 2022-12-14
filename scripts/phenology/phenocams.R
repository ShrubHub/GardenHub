#### PHENOCAMS: common garden, Kluane and QHI
#### Script by Erica Zaja, created 14/12/22
### Last updated: 14/12/22

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)
library(gridExtra)

# 2. LOADING DATA ----
KP_phenocams_2021 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2021.csv")
QHI_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/QHI_phenocams_2022.csv")
KP_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2022.csv")
Phenocam_Datasheet_QHI <- read_excel("data/phenology/phenocam_pics/Phenocam_Datasheet_QHI.xlsx")
# add common garden datasheet when available


# 3. DATA WRANGLING-----

KP_phenocams_2022 <-KP_phenocams_2022[1:9,] # removing loads of NAs
KP_phenocams_2021 <- KP_phenocams_2021[1:9,]

# merge 2021 and 2022 kp datasets?

