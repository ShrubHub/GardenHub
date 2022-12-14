# Results models for trait differences comparing northern and southern willows
# Mad 13/12/2022
# Last updated: 13/12/2022

# traits: SLA, LDMC, LA? (currently not possible), leaf length  

# model, where Site is Kluane or QHI 
# lmer(TRAIT ~ population*site + sample_age + (1|sample_year/species/sample_ID))
# where population = northern or southern pop in garden and site = source QHI or KLU
# need to make sample_year column, although not critical for leaf traits? 
# separate nested random effects if it doesn't converge 

# LIBRARIES ---- 

library(dplyr)
library(ggplot2)
library(lme4)
library(sjPlot)
library(ggpubr)

# DATA ----
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") # most traits
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") # leaf length

str(all_CG_source_traits)
str(all_CG_source_growth)

# reclass variables 
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

all_CG_source_traits$Species <- as.factor(all_CG_source_traits$Species)
all_CG_source_traits$plant_tag_id <- as.factor(all_CG_source_traits$plant_tag_id)
all_CG_source_traits$population <- as.factor(all_CG_source_traits$population)
all_CG_source_traits$Site <- as.factor(all_CG_source_traits$Site)
all_CG_source_traits$date_sampled <- as.POSIXct(all_CG_source_traits$date_sampled, format = '%Y-%m-%d')
all_CG_source_traits$year <- as.factor(all_CG_source_traits$year)
# currently doesn't exist! all_CG_source_traits$Sample_age <- as.factor(all_CG_source_traits$Sample_age)

# SLA ----

# LDMC ---- 

# LA ---- 

# LEAF LENGTH ---- 

# LMA ---- 


