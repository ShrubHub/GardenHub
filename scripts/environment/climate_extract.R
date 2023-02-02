#### CLIMATE DATA EXTRACTION - CHELSA
### extraction script
### By Erica Zaja, created on 26/01/2023
## Last updated: 02/02/2023 by Madi
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
library(dplyr)
library(gridExtra)
library(ggpubr)
library(corrplot)
library(Hmisc)

# set wd to external hard drive where data is stored (too big for github)
setwd("/Volumes/Spectral_21/CHELSA_data_1999-2019")

# Climate data from CHELSA 2022
temp <- raster("data/environment/CHELSA/CHELSA_bio10_10.tif") 
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15

# Madi testing something out 
# files stored on external hard drive because quite large 
temp_2018 <- raster("data/CHELSA_tas_07_2018_V.2.1.tif") 
temp_1999 <- raster("data/CHELSA_tas_07_1999_V.2.1.tif") 


precip_2018 <- raster("data/CHELSA_pr_07_2018_V.2.1.tif")
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

# DATA EXPLORATION -----
# checking resolution of rasters
res(temp)
res(temp_2018)

res(precip_2018)
# [1] 0.008333333 0.008333333
# The spatial resolution of a raster refers the size of each cell in meters. 
# This size in turn relates to the area on the ground that the pixel represents.
# The higher the resolution for the same extent the crisper the image (and the larger the file size) 

# Visualising climate rasters
plot(temp, main = "Mean daily mean air temperatures of the warmest quarter (°C)")
plot(temp_2018, main = "Mean daily mean air temperatures of 2018 (°C)")
plot(temp_1999, main = "Mean daily mean air temperatures of 1999 (°C)")



plot(precip_2018, main = "Mean monthly precipitation of the warmest quarter (kg m-2)")
precip_2018_raster <- levelplot(precip_2018)
temp_raster <- levelplot(temp)

temp_2018_raster <- levelplot(temp_2018)

# EXTRACTION (to be filled in) ------
# Loading the coordinates of the sites of interest
coords <- read.csv("lat_long_chelsa.csv") %>% 
  dplyr::select(longitude, latitude) # keeping lat and long

# Creating SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

# creating raster stack
chelsa.stack <- stack(precip_2018, temp_2018)

# Extracting variables values for each pair of coordinates
chelsa.extract <- raster::extract(chelsa.stack, coords_sp, df = TRUE) # extract coords 

tea_climate <- extract(temp_2018, coords)


# Combining dataframes:
# Converting the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassigning the 'ID' to the coordinates dataframe
coord.df$ID <- row.names(coord.df)
coord.df$ID <- as.numeric(coord.df$ID) # Make numeric

# Merging the two dataframes: extracted CHELSA variables and the coordinates
coord.chelsa.combo.a <- left_join(chelsa.extract, coord.df, by = c("ID" = "ID"))

# Modifying some of the variables to more useful values
coord.chelsa.combo.b <- coord.chelsa.combo.a %>% 
  mutate((CHELSA_tas_07_2018_V.2.1 = CHELSA_tas_07_2018_V.2.1/10 - 273.15)) # Divide by 10 to get to degC
# subtract 273.15 to adjust for Kelvin to C conversion 

# Renaming the variables to shorter column headings
coord.chelsa.combo.c <- coord.chelsa.combo.b %>% 
  rename(CH_TempMeanSummer = CHELSA_tas_07_2018_V.2.1,
         CH_PrecipMeanSummer = CHELSA_pr_07_2018_V.2.1) %>% na.omit()

unique(coord.chelsa.combo.c$CH_TempMeanSummer)

# Exporting the dataframe to csv
# write.csv(coord.chelsa.combo.c, "data/environment/CHELSA/coord_chelsa_combo_new.csv")

################################################################## ÉND -----
