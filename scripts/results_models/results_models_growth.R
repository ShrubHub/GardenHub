# Results models for growth differences comparing northern and southern willows
# in the garden to their source populations
# by Erica Zaja, created on 13/12/2022
# Last update: 14/12/2022

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# model structure: 
# lmer(growth_variable ~ population*site + sample_age +(1|sample_year/species/sample_ID))
# N.B Site is (a) common garden, (b) Kluane, (c) QHI. 
# population: is (a) common garden northern, (b) common garden southern, (c) source 

# Libraries ----
library(lme4)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)

# data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# Wrangling 
str(all_CG_source_growth)
# reclassing variables
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

# Modelling

# 1. Canopy height -----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_growth_mod_1 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_growth_mod_2 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year/Species), data = all_CG_source_growth)
# model 3 runs but: fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients
height_growth_mod_3 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)
tab_model(height_growth_mod_3) 

# 1.1. Canopy height only in garden 
# filter dataset to retain only population "northern" and "southern"
all_CG_source_growth_garden_only <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))

# model 1 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_garden_growth_mod_1 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model 2 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_garden_growth_mod_2 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species), data = all_CG_source_growth_garden_only)
# model 3 below does converge
height_garden_growth_mod_3 <- lmer(Canopy_Height_cm ~ population + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
tab_model(height_garden_growth_mod_3)
# canopy height significatly taller for southern populations

