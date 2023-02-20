# BAYESIAN growth results models -----

# Loading libraries ----
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# reclassing variables
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

# filter dataset to retain only population "northern" and "southern"
all_CG_source_growth_garden_only <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))
all_CG_source_growth_garden_only$population <- as.factor(all_CG_source_growth_garden_only$population)

# MODELLING -------
# one model per species
# 1. Canopy height-----

#Richardsonii -----
all_CG_source_growth_garden_rich_height <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix richardsonii") %>%
  dplyr::select(Canopy_Height_cm, Year, population, Sample_age)%>%
  na.omit()

unique(all_CG_source_growth_garden_rich_height$Year)
range(all_CG_source_growth_garden_rich_height$Canopy_Height_cm)

hist(all_CG_source_growth_garden_rich_height$Canopy_Height_cm) # not gaussian, not poisson?
# scaling 
all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale <- scale(all_CG_source_growth_garden_rich_height$Canopy_Height_cm, center = T)  # scaling time
hist(all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale ) # still not gaussian?

# model
garden_rich_height <- brms::brm(Canopy_Height_cm_scale ~ population + (1|Sample_age) + (1|Year),
                                data = all_CG_source_growth_garden_rich_height, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000)

summary(garden_rich_height)
plot(garden_rich_height)
pp_check(garden_rich_height) # i think maybe the family () is wrong 

# 2. Stem elongation ------
# Richardsonii
all_CG_source_growth_garden_rich <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix richardsonii") %>%
  dplyr::select(mean_stem_elong, Year, population, Sample_age)%>%
  na.omit()

unique(all_CG_source_growth_garden_rich$Year)
range(all_CG_source_growth_garden_rich$mean_stem_elong)

hist(all_CG_source_growth_garden_rich$mean_stem_elong) # not gaussian, not poisson?
# scaling 
all_CG_source_growth_garden_rich$mean_stem_elong_scale <- scale(all_CG_source_growth_garden_rich$mean_stem_elong, center = T)  # scaling time
hist(all_CG_source_growth_garden_rich$mean_stem_elong_scale) # still not gaussian?

# model
garden_rich_elong <- brms::brm(mean_stem_elong ~ population + (1|Sample_age) + (1|Year),
                               data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)

summary(garden_rich_elong)
plot(garden_rich_elong)
pp_check(garden_rich_elong) # i think maybe the family () is wrong 

