#### CLIMATE DATA EXTRACTION - CHELSA
### extraction script
### By Erica Zaja, created on 26/01/2023
## Last updated: 26/01/2023 by Erica
# Script credit: adapted from Joseph Everest and Mariana Garcia Criado

# LOADING LIBRARIES -----
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(lme4)
library(sjPlot)
library(gridExtra)
library(ggpubr)
library(corrplot)
library(Hmisc)

# Climate data from CHELSA 2022
temp <- raster("datasets/climate_data/CHELSA_bio10_10.tif") 
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15

precip <- raster("datasets/climate_data/CHELSA_bio10_18.tif")
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

