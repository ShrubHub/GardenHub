# BAYESIAN traits results models -----
# Script by Madi
# Last update: 22/02/2023

# libraries ----
library(plyr) # load before dplyr aka tidyverse 
library(tidyverse)
library(brms)

# DATA ----
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") # most traits
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") # leaf length

str(all_CG_source_traits)
str(all_CG_source_growth)

# reclass variables 
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

all_CG_source_traits$Species <- as.factor(all_CG_source_traits$Species)
all_CG_source_traits$plant_tag_id <- as.factor(all_CG_source_traits$plant_tag_id)
all_CG_source_traits$population <- as.factor(all_CG_source_traits$population)
all_CG_source_traits$date_sampled <- as.POSIXct(all_CG_source_traits$date_sampled, format = '%Y-%m-%d')
all_CG_source_traits$year <- as.factor(all_CG_source_traits$year)
# currently doesn't exist! all_CG_source_traits$Sample_age <- as.factor(all_CG_source_traits$Sample_age)

# filter out two extreme LDMC values from 2014 
# filter out one extreme LMA value from 2021 
all_CG_source_traits <- all_CG_source_traits %>% 
  filter(LDMC_g_g < 1 | is.na(LDMC_g_g)) %>% 
  filter(leaf_mass_per_area_g_m2 < 130 | is.na(leaf_mass_per_area_g_m2)) %>% 
  filter(SLA > 2 | is.na(SLA))

# to run separate models per species filter out species: 
arctica_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix arctica")

pulchra_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix pulchra")

richardsonii_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix richardsonii")