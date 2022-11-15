#### COMMON GARDEN PHENOLOGY GROUND TRUTHING 2022
#### Script by Erica Zaja, created 15/11/22
### Last updated: 15/11/22

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)
library(gridExtra)

# 2. LOADING DATA ----

# Data collected in the common garden in summer 2022 (weekly throughout June-July-August)
CG_ground_truth <- read_excel("data/phenology/groundtruthing_2022/FINAL_phenocam_groundtruthing_common_garden22_copy5.xlsx")

# 3. DATA WRANGLING ----

# Putting variables in right format
str(CG_ground_truth)

CG_ground_truth$Species <- as.factor(CG_ground_truth$Species)
# CG_ground_truth$P5_first_yellowing<- as.POSIXct(CG_ground_truth$P5_first_yellowing, format = "%Y/%m/%d")

