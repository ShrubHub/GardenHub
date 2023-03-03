# BAYESIAN traits results models -----
# Script by Madi
# Last update: 22/02/2023

# libraries ----
library(plyr) # load before dplyr aka tidyverse 
library(tidyverse)
library(brms)
library(ggplot2)
library(tidybayes)

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

# rename levels of variables for easier interpretation 
all_CG_source_traits$population <- plyr::revalue(all_CG_source_traits$population, 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden",
                                                   "source_south"="Southern Source",
                                                   "source_north"="Northern Source"))
all_CG_source_traits$population <- ordered(all_CG_source_traits$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden", 
                                                      "Southern Source",
                                                      "Southern Garden"))
all_CG_source_growth$population <- plyr::revalue(all_CG_source_growth$population, 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden",
                                                   "source_south"="Southern Source",
                                                   "source_north"="Northern Source"))
all_CG_source_growth$population <- ordered(all_CG_source_growth$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden",
                                                      "Southern Source", 
                                                      "Southern Garden"))

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
# to run separate models per species filter out species for leaf length: 
arctica_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix arctica")
pulchra_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix pulchra")
richardsonii_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix richardsonii")

# look at distributions ----
# SLA
hist(arctica_all_traits$SLA) # mild right skew
arctica_all_traits$SLA_log <- log(arctica_all_traits$SLA)
hist(pulchra_all_traits$SLA) # mild right skew
pulchra_all_traits$SLA_log <- log(pulchra_all_traits$SLA)
hist(pulchra_all_traits$SLA_log) # mildly better right skew
hist(richardsonii_all_traits$SLA)# mild right skew
richardsonii_all_traits$SLA_log <- log(richardsonii_all_traits$SLA)
hist(richardsonii_all_traits$SLA_log) # better 

# LDMC
hist(arctica_all_traits$LDMC_g_g) # very mild right skew
hist(pulchra_all_traits$LDMC_g_g) # mild right skew
hist(richardsonii_all_traits$LDMC_g_g)# decent
# LA - compare with log version 
hist(arctica_all_traits$LA) #  right skew
arctica_all_traits$LA_log <- log(arctica_all_traits$LA)
hist(arctica_all_traits$LA_log) #  better 
hist(pulchra_all_traits$LA) # right skew
pulchra_all_traits$LA_log <- log(pulchra_all_traits$LA)
hist(pulchra_all_traits$LA_log) #  better 
hist(richardsonii_all_traits$LA) # right skew 
richardsonii_all_traits$LA_log <- log(richardsonii_all_traits$LA)
hist(richardsonii_all_traits$LA_log) # better 
# LMA
hist(arctica_all_traits$leaf_mass_per_area_g_m2) # decent 
hist(pulchra_all_traits$leaf_mass_per_area_g_m2) # decent 
hist(richardsonii_all_traits$leaf_mass_per_area_g_m2) # mild bimodal  
richardsonii_all_traits$leaf_mass_per_area_g_m2_log <- log(richardsonii_all_traits$leaf_mass_per_area_g_m2)
hist(richardsonii_all_traits$leaf_mass_per_area_g_m2_log) # kind of bimodal  

# leaf length 
hist(arctica_all_growth$mean_leaf_length) # decent
hist(pulchra_all_growth$mean_leaf_length) # mild right skew
hist(richardsonii_all_growth$mean_leaf_length) # pretty decent 

# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# MODELS ----
# model structure : 
# mod <- brms::brm(trait ~ population + (1|Year),
# data = spp_traits, family = gaussian(), chains = 3,
# iter = 3000 - 5000, warmup = 1000)

# SLA ----
# S. arctica 
arctica_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_SLA) #There were 3 divergent transitions after warmup
plot(arctica_SLA)
pp_check(arctica_SLA, type = "dens_overlay", ndraws = 100) # pretty good 

# S. pulchra 
pulchra_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_SLA) # There were 1 divergent transitions after warmup
plot(pulchra_SLA)
pp_check(pulchra_SLA, type = "dens_overlay", ndraws = 100) # pretty good 

# S. richardsonii 
rich_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                      iter = 3000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_SLA) # there were 1 divergent transitions after warmup
plot(rich_SLA)
pp_check(rich_SLA, type = "dens_overlay", ndraws = 100) # pretty good 

# LDMC ----
# S. arctica 
arctica_LDMC <- brms::brm(LDMC_g_g ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99)) #207 divergent transitions after warmup
summary(arctica_LDMC)
plot(arctica_LDMC)
pp_check(arctica_LDMC) 
# S. arctica log transformed 
arctica_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) #There were 18 divergent transitions after warmup.
summary(arctica_LDMC_log)
plot(arctica_LDMC_log)
pp_check(arctica_LDMC_log) 

# S. pulchra 
pulchra_LDMC <- brms::brm(LDMC_g_g ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99)) # 4 divergent transitions after warmup
summary(pulchra_LDMC) 
plot(pulchra_LDMC)
pp_check(pulchra_LDMC) 

pulchra_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                          iter = 5000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) # There were 6 divergent transitions after warmup
summary(pulchra_LDMC_log) 
plot(pulchra_LDMC_log)
pp_check(pulchra_LDMC_log) 

# S. richardsonii 
rich_LDMC <- brms::brm(LDMC_g_g ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99)) # 11 divergent transitions after warmup
summary(rich_LDMC)
plot(rich_LDMC)
pp_check(rich_LDMC) 

rich_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99)) # 13 divergent transitions after warmup
summary(rich_LDMC_log)
plot(rich_LDMC_log)
pp_check(rich_LDMC_log) 

# LA ----
# not including year bc only 2 years worth of data 2021 and 2022 
# S. arctica 
arctica_LA <- brms::brm(log(LA) ~ population, data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000)
summary(arctica_LA) 
plot(arctica_LA)
pp_check(arctica_LA) 

# S. pulchra
pulchra_LA <- brms::brm(log(LA) ~ population, data = pulchra_all_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(pulchra_LA) 
plot(pulchra_LA)
pp_check(pulchra_LA) 

# S. richardsonii 
rich_LA <- brms::brm(log(LA) ~ population, data = richardsonii_all_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(rich_LA) 
plot(rich_LA)
pp_check(rich_LA)

# LMA ----
# S. arcitca 
arctica_LMA <- brms::brm(leaf_mass_per_area_g_m2 ~ population, data = arctica_2022_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(arctica_LMA) 
plot(arctica_LMA)
pp_check(arctica_LMA) 

# S. pulchra
pulchra_LMA <- brms::brm(leaf_mass_per_area_g_m2 ~ population, data = pulchra_2022_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(pulchra_LMA) 
plot(pulchra_LMA)
pp_check(pulchra_LMA) 

# S. richardsonii 
rich_LMA <- brms::brm(leaf_mass_per_area_g_m2 ~ population, data = richardsonii_2022_traits, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000)
summary(rich_LMA) 
plot(rich_LMA)
pp_check(rich_LMA)

# LEAF LENGTH ----
# no leaf length for S. arctic from source pop
# S. arctica 
arctica_LL <- brms::brm(mean_leaf_length ~ population + (1|Year), data = arctica_all_growth, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000)
# 2 divergent transitions after warmup
# The largest R-hat is 1.66, indicating chains have not mixed
summary(arctica_LL)
plot(arctica_LL)
pp_check(arctica_LL) 

arctica_LL_log <- brms::brm(log(mean_leaf_length) ~ population + (1|Year), data = arctica_all_growth, family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000)
summary(arctica_LL_log)
plot(arctica_LL_log)
pp_check(arctica_LL_log) 

# S. pulchra
pulchra_LL <- brms::brm(mean_leaf_length ~ population + (1|Year), data = pulchra_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000) # There were 1 divergent transitions after warmup
summary(pulchra_LL)
plot(pulchra_LL)
pp_check(pulchra_LL) 
# S. richardsonii 
rich_LL <- brms::brm(mean_leaf_length ~ population + (1|Year), data = richardsonii_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(rich_LL)
plot(rich_LL)
pp_check(rich_LL) 


# SLA
# LMDC
# LA
# LMA
# LEAF LENGTH 

