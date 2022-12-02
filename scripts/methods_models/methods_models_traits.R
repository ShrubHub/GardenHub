# methods models for trait differences comparing northern and southern willows
# Created 29 Nov 2022 Madelaine 
# last updated: 29/11/22

# traits: SLA, LDMC, LA, leaf length, stem diameter, shrub width
# model structure: 
#  lmer(SLA ~ site*species +  (1|sample_year/field_sample_ID))
#  OR, having species as a random effect: 
# lmer(SLA ~ site + (1|sample_year) + (1|species))
# where site is Kluane or Qikiqtaruk 


# libraries ----
library(lme4)
library(dplyr)

# data ---- 
# SLA, LDMC, LA: 
all_source_area_traits <- read.csv("data/source_pops/all_source_area_traits.csv")
# leaf length, stem diameter, width 
unique_source_mother <-  read.csv("data/source_pops/unique_source_mother.csv")

str(all_source_area_traits)
# reclass variables 
all_source_area_traits$Species <- as.factor(all_source_area_traits$Species)
all_source_area_traits$Site <- as.factor(all_source_area_traits$Site)
all_source_area_traits$year <- as.factor(all_source_area_traits$year)

str(unique_source_mother) 
# reclass variables 
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique_source_mother$SampleYear <- as.factor(unique_source_mother$SampleYear)

# SLA ----
# not including field_sample_id because those don't exist for any values prior to 2021 ? and not repeated measures anyways 
SLA_method_mod <- lmer(SLA ~ Site*Species + (1|year), data = all_source_area_traits) 
summary(SLA_method_mod)
plot(SLA_method_mod)
qqnorm(resid(SLA_method_mod))
qqline(resid(SLA_method_mod)) 

# okay, do we want species as an interaction term? not our main q and our other models aren't including it
# here, as random effect: 
SLA_method_mod_spp <- lmer(SLA ~ Site + (1|year) + (1|Species), data = all_source_area_traits) 
summary(SLA_method_mod_spp)
plot(SLA_method_mod_spp)
qqnorm(resid(SLA_method_mod_spp))
qqline(resid(SLA_method_mod_spp))

# LDMC ---- 
# same comments as above (SLA)
LDMC_method_mod <- lmer(LDMC_g_g ~ Site*Species + (1|year), data = all_source_area_traits) 
summary(LDMC_method_mod)
plot(LDMC_method_mod)
qqnorm(resid(LDMC_method_mod))
qqline(resid(LDMC_method_mod)) 

# species as random effect 
LDMC_method_mod_spp <- lmer(LDMC_g_g ~ Site + (1|year) + (1|Species), data = all_source_area_traits) 
summary(LDMC_method_mod_spp)
plot(LDMC_method_mod_spp)
qqnorm(resid(LDMC_method_mod_spp))
qqline(resid(LDMC_method_mod_spp)) 

# LA ---- 
LA_method_mod <- lmer(LA ~ Site*Species + (1|year), data = all_source_area_traits) 
summary(LA_method_mod)
plot(LA_method_mod)
qqnorm(resid(LA_method_mod))
qqline(resid(LA_method_mod)) 

# species as random effect 
LA_method_mod_spp <- lmer(LA ~ Site + (1|year) + (1|Species), data = all_source_area_traits) 
summary(LA_method_mod_spp)
plot(LA_method_mod_spp)
qqnorm(resid(LA_method_mod_spp))
qqline(resid(LA_method_mod_spp)) 

# leaf length (LL) ----
# use mean value from 3 leaf lengths 
LL_method_mod <- lmer(mean_leaf_length ~ Site*Species + (1|SampleYear), data = unique_source_mother) 
# note: fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
summary(LL_method_mod) 
plot(LL_method_mod)
qqnorm(resid(LL_method_mod))
qqline(resid(LL_method_mod))

# species as random effect 
LL_method_mod_spp <- lmer(mean_leaf_length ~ Site + (1|Species) + (1|SampleYear), data = unique_source_mother) 
summary(LL_method_mod_spp) 
plot(LL_method_mod_spp)
qqnorm(resid(LL_method_mod_spp))
qqline(resid(LL_method_mod_spp))


# shrub width ---- 
# oh it seems as though Erica has run this and stem diameter 
# 


