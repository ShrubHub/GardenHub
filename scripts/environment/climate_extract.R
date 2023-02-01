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
temp <- raster("data/environment/CHELSA/CHELSA_bio10_10.tif") 
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15

# Madi testing something out 
# files stored on external hard drive because quite large 
temp_2019 <- raster("CHELSA_data_1999-2019/data/CHELSA_tas_07_2019_V.2.1.tif") 


precip <- raster("data/environment/CHELSA/CHELSA_bio10_18.tif")
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

# DATA EXPLORATION -----
# checking resolution of rasters
res(temp)
res(temp_2019)

res(precip)
# [1] 0.008333333 0.008333333
# The spatial resolution of a raster refers the size of each cell in meters. 
# This size in turn relates to the area on the ground that the pixel represents.
# The higher the resolution for the same extent the crisper the image (and the larger the file size) 

# Visualising climate rasters
plot(temp, main = "Mean daily mean air temperatures of the warmest quarter (°C)")
plot(temp_2019, main = "Mean daily mean air temperatures of 2019 (°C)")

plot(precip, main = "Mean monthly precipitation of the warmest quarter (kg m-2)")
precip_raster <- levelplot(precip)
temp_raster <- levelplot(temp)

temp_raster <- levelplot(temp_2019)

# EXTRACTION (to be filled in) ------
# Loading the coordinates of the sites of interest
coords <- read.csv("CHELSA_data_1999-2019/lat_long_chelsa.csv") %>% 
  dplyr::select(longitude, latitude) # keeping lat and long

# Creating SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

# creating raster stack
chelsa.stack <- stack(precip, temp)

# Extracting variables values for each pair of coordinates
chelsa.extract <- raster::extract(chelsa.stack, coords_sp, df = TRUE) # extract coords 

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
  mutate(CHELSA_bio10_10 = CHELSA_bio10_10/10) # Divide by 10 to get to degC

# Renaming the variables to shorter column headings
coord.chelsa.combo.c <- coord.chelsa.combo.b %>% 
  rename(CH_TempMeanSummer = CHELSA_bio10_10,
         CH_PrecipMeanSummer = CHELSA_bio10_18) %>% na.omit()

unique(coord.chelsa.combo.c$CH_TempMeanSummer)

# Exporting the dataframe to csv
# write.csv(coord.chelsa.combo.c, "data/environment/CHELSA/coord_chelsa_combo_new.csv")

################################################################## ÉND -----