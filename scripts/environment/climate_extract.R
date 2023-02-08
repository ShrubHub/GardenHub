#### CLIMATE DATA EXTRACTION - CHELSA
### extraction script
### By Erica Zaja, created on 26/01/2023
## Last updated: 03/02/2023 by Madi
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
library(tidyverse)
library(corrplot)
library(Hmisc)

# set wd to external hard drive where data is stored (too big for github)
setwd("/Volumes/Spectral_21/CHELSA_data_1999-2019")

# Climate data from CHELSA 2022
temp <- raster("data/environment/CHELSA/CHELSA_bio10_10.tif") 
# Temperature climatologies: mean daily mean air temperatures of the warmest quarter (bio10) (°C). Offset -273.15

# Madi testing extractions 
temp_2018 <- raster("data/temperature/CHELSA_tas_07_2018_V.2.1.tif") 
temp_1999 <- raster("data/temperature/CHELSA_tas_07_1999_V.2.1.tif") 

precip_2018 <- raster("data/precipitation/CHELSA_pr_07_2018_V.2.1.tif")
# Precipitation climatologies: mean monthly precipitation amount of the warmest quarter (bio18) (kg m-2)

# DATA EXPLORATION -----
# checking resolution of rasters
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

# madi taking slightly diff approach 
# Loading the coordinates of the sites of interest
coords_full <- read.csv("lat_long_chelsa.csv")

coords <-  coords_full %>% 
  dplyr::select(longitude, latitude) # keeping lat and long

# Creating SpatialPoints (sp) object of unique coordinates
coords_sp <- SpatialPoints(coords)

temp_rasters <- list.files(path="data/temperature/", pattern = "*.tif", full.names = TRUE)
temp_stack <- raster::stack(temp_rasters)
mean_temp <- raster::extract(temp_stack, coords, df = T)


# Combining dataframes:
# Converting the SpatialPoints (sp) object into a dataframe 
coord.df <- as.data.frame(coords_sp)

# Reassigning the 'ID' to the coordinates dataframe
coords_full$ID <- row.names(coords_full)
coords_full$ID <- as.numeric(coords_full$ID) # Make numeric

# Reassigning the 'ID' to the coordinates dataframe
mean_temp$ID <- row.names(mean_temp)
mean_temp$ID <- as.numeric(mean_temp$ID) # Make numeric

mean_temp_a <- left_join(mean_temp, coords_full, by = c("ID" = "ID"))

# make long form of data 
mean_temp_long  <- gather(mean_temp_a, year, mean_temp, 
                                         c(2:22))

mean_temp_data <- mean_temp_long %>% 
  mutate((mean_temp_C = (mean_temp/10 - 273.15))) # Divide by 10 to get to degC and subtract 273.15 
# change year column to just year 

mean_temp_data$year <- gsub("^.*?07_","",mean_temp_data$year)
mean_temp_data$year <- gsub("\\_V.2.1*","",mean_temp_data$year)
         
# same for precipitation 

prec_rasters <- list.files(path="data/precipitation/", pattern = "*.tif", full.names = TRUE)
prec_stack <- raster::stack(prec_rasters)
prec_temp <- raster::extract(prec_stack, coords, df = TRUE)

# Reassigning the 'ID' to the coordinates dataframe
prec_temp$ID <- row.names(prec_temp)
prec_temp$ID <- as.numeric(prec_temp$ID) # Make numeric

mean_prec_a <- left_join(prec_temp, coords_full, by = c("ID" = "ID"))

mean_prec_long  <- gather(mean_prec_a, year, PrecipMeanJuly, 
                          c(2:22))
# change year column to just year 
mean_prec_long$year <- gsub("^.*?07_","",mean_prec_long$year)
mean_prec_long$year <- gsub("\\_V.2.1*","",mean_prec_long$year)


# merge 
july_enviro <- full_join(mean_prec_long, mean_temp_data, 
                         by = c("ID",
                                "site",
                                "latitude", 
                                "longitude", 
                                "year"))

write.csv(july_enviro, "july_enviro_chelsa.csv")

# Loading chelsa temperature and precipitation data -----
july_enviro_chelsa <- read_csv("data/environment/july_enviro_chelsa.csv")


# DATA WRANGLE ------
unique(july_enviro_chelsa$site)

#rename column
july_enviro_chelsa <- july_enviro_chelsa %>%
  rename ("mean_temp_C" ="(mean_temp_C = (mean_temp/10 - 273.15))")
str(july_enviro_chelsa)

# QHI july mean temp and precip
QHI_july_temp <- july_enviro_chelsa %>%
  filter(site == "QHI")%>%
  select(site, year, mean_temp_C)

mean(QHI_july_temp$mean_temp_C, na.rm=TRUE) # 6.15
# QHI july mean surface temp: 9.10 °C 
# based on 2022 TOMST and 2017 HOBO data (both of those data sources are biased towards end of July for what it's worth)

QHI_july_precip <- july_enviro_chelsa %>%
  filter(site == "QHI")%>%
  select(site, year, PrecipMeanJuly)

mean(QHI_july_precip$PrecipMeanJuly, na.rm=TRUE) # 9586.81
# QHI july mean surface temp: 9.10 °C 

# KP july mean temp and precip
KP_july_temp <- july_enviro_chelsa %>%
  filter(site == "Kluane_plateau")%>%
  select(site, year, mean_temp_C)

mean(KP_july_temp$mean_temp_C, na.rm=TRUE) #7.311905

KP_july_precip <- july_enviro_chelsa %>%
  filter(site == "Kluane_plateau")%>%
  select(site, year, PrecipMeanJuly)

mean(KP_july_precip$PrecipMeanJuly, na.rm=TRUE) # 7123.429

# CG july mean temp and precip
CG_july_temp <- july_enviro_chelsa %>%
  filter(site == "Common_garden") %>%
  select(site, year, mean_temp_C)

mean(CG_july_temp$mean_temp_C, na.rm=TRUE) #13.67857

# Common garden july mean surface temp: 17.97°C 
# Based on 6 years data (TOMST+ HOBO)

CG_july_precip <- july_enviro_chelsa %>%
  filter(site == "Common_garden") %>%
  select(site, year, PrecipMeanJuly)

mean(CG_july_precip$PrecipMeanJuly, na.rm=TRUE) # 5287.333

# ORDINATION ------

library(BiodiversityR)
library(ggsci)
library(ggrepel)
library(ggforce)
library(vegan)

# I think we need to put the dataframe into wide format? 
# i.e. column names must be: "KP","QHI", "CG" and their temperatures
july_enviro_chelsa_temp_wide <- july_enviro_chelsa %>% 
  dplyr::select(site, year, mean_temp_C) %>%
  pivot_wider(names_from=site, values_from=mean_temp_C) %>% 
  na.omit()


Ordination.model1 <- metaMDS(july_enviro_chelsa_temp_wide, distance='bray',  k=2, trymax=1, 
                             autotransform=TRUE, noshare=0.1, expand=TRUE, trace=1, plot=FALSE)

plot1 <- ordiplot(Ordination.model1, choices=c(1,2))

three_site_chelsa <-  july_enviro_chelsa %>% 
  filter(site %in% c("QHI", "Kluane_plateau", "Common_garden"))

(climate_space <- ggplot(three_site_chelsa, 
                         aes(x = mean_temp_C, y = PrecipMeanJuly, color = site, shape =site)) +
  geom_point(size = 2.5)+
    stat_ellipse() +
    ylab("Mean July precipitation (kg/m2 month-1/100)") +
    xlab("\nMean July temperature (°C)") +
    scale_colour_viridis_d(begin = 0.2, end = 0.85) +
    scale_fill_viridis_d(begin = 0.2, end = 0.85) +
    theme_bw()+
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 15, color = "black", face = "italic"),
          legend.title = element_text(size=15), #change legend title font size
          legend.text = element_text(size=12),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))



