# Results models for trait differences comparing northern and southern willows
# by Mad 13/12/2022
# Last updated: 17/12/2022

# traits: SLA, LDMC, LA, leaf length  

# model, where population is CG northern, CG southern, source northern (QHI), source southern (Kluane alpine)
# lmer(TRAIT ~ population + (1|sample_year/species/sample_ID))
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

# SLA ----
SLA_mod_1 <- lmer(SLA ~ population + (1|year/Species/plant_tag_id), 
                 data = all_CG_source_traits)
summary(SLA_mod_1)
tab_model(SLA_mod_1) 

# dropping plant_tag_id because shrubs weren't repeatedly measured either in source pop or garden
# in 2021 and 2022, same shrubs were sampled in garden, but also diff than in 2017 
SLA_mod_2 <- lmer(SLA ~ population + (1|year/Species), 
                  data = all_CG_source_traits)
# fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients
summary(SLA_mod_2)
tab_model(SLA_mod_2)

# LDMC ---- 
# same as SLA so not including plant_tag_id because shrubs weren't repeatedly measured either in source pop or garden
# in 2021 and 2022, same shrubs were sampled in garden, but also diff than in 2017 
LDMC_mod_1 <- lmer(LDMC_g_g ~ population + (1|year/Species), 
                  data = all_CG_source_traits)
summary(LDMC_mod_1)
tab_model(LDMC_mod_1)

# LA ---- 

# LEAF LENGTH ---- 
# not including sample age as fixed effect (or random) for leaf traits 
ll_mod_1 <- lmer(mean_leaf_length ~ population + (1|Year/Species/SampleID_standard), 
               data = all_CG_source_growth)
summary(ll_mod_1)
tab_model(ll_mod_1)

# LEAF AREA 
# data is a mess because of inconsistent unit reporting, 
# I'm going to try to sort it out but here are just data I collected in 2021 and 2022 
# in the common garden and source populations 
all_CG_source_traits_2022 <- all_CG_source_traits %>% 
  dplyr::filter(year %in% c("2021", "2022")) 

# same model srtucture as above. No sample age or ID 
# removing year as random effect because only two years worth of data 
LA_mod_1 <- lmer(LA ~ population + (1|Species), 
                   data = all_CG_source_traits_2022)
# small sample size 
summary(LA_mod_1)
tab_model(LA_mod_1)

# LMA ---- 
# same model as SLA, LDMC, leaf length  
LMA_mod_1 <- lmer(leaf_mass_per_area_g_m2 ~ population + (1|year/Species), 
                 data = all_CG_source_traits)
# boundary (singular) fit: see help('isSingular')

# removing year as nested random effect:
LMA_mod_2 <- lmer(leaf_mass_per_area_g_m2 ~ population + (1|Species) + (1|year), 
                  data = all_CG_source_traits)
summary(LMA_mod_2)
tab_model(LMA_mod_2)

