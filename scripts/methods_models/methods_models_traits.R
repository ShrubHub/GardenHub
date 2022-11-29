# methods models for trait differences comparing northern and southern willows
# Created 29 Nov 2022 Madelaine 
# last updated: 29/11/22

# traits: SLA, LDMC, LA, leaf length, stem diameter, canopy height, shrub width
# model structure: 
#  lmer(SLA ~ site*species +  (1|sample_year/field_sample_ID))
# where site is Kluane or Qikiqtaruk 


# libraries ----
library(lme4)


# data ---- 
# SLA, LDMC, LA: 
all_source_area_traits <- read.csv("data/source_pops/all_source_area_traits.csv")


# SLA ----
str(all_source_area_traits)

SLA_method_mod <- lmer(SLA ~ Site*Species + (1|year/))


# LDMC ---- 

# LA ---- 

