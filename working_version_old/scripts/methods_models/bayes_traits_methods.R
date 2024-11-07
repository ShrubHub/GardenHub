# BAYESIAN traits methods models -----
# Script by Madi
# Last update: 27/02/2023

# libraries ----
library(plyr) # load before dplyr aka tidyverse 
library(tidyverse)
library(brms)
library(ggplot2)
library(tidybayes)

# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# data ---- 
# SLA, LDMC, LA: 
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") 
# leaf length: 
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") 

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

# filter out CG observations: 
all_source_traits <- all_CG_source_traits %>% 
  dplyr::filter(population %in% c("Southern Source", "Northern Source"))
# filter out CG observations: 
all_source_growth <- all_CG_source_growth %>% 
  dplyr::filter(population %in% c("Southern Source", "Northern Source"))

# to run separate models per species filter out species: 
arctica_source_traits <- all_source_traits %>% 
  filter(Species == "Salix arctica")
pulchra_source_traits <- all_source_traits %>% 
  filter(Species == "Salix pulchra")
richardsonii_source_traits <- all_source_traits %>% 
  filter(Species == "Salix richardsonii")
# to run separate models per species filter out species for leaf length: 
arctica_source_growth <- all_source_growth %>% 
  filter(Species == "Salix arctica")
pulchra_source_growth <- all_source_growth %>% 
  filter(Species == "Salix pulchra")
richardsonii_source_growth <- all_source_growth %>% 
  filter(Species == "Salix richardsonii")

# look at distributions ----
# SLA
hist(arctica_source_traits$SLA) # mild right skew
hist(pulchra_source_traits$SLA) # mildly better right skew
hist(richardsonii_source_traits$SLA)# mild right skew

# LDMC
hist(arctica_source_traits$LDMC_g_g) 
hist(pulchra_source_traits$LDMC_g_g) # right skew
hist(richardsonii_source_traits$LDMC_g_g) # very off 
# LA
arctica_source_traits$LA_log <- log(arctica_source_traits$LA)
pulchra_source_traits$LA_log <- log(pulchra_source_traits$LA)
richardsonii_source_traits$LA_log <- log(richardsonii_source_traits$LA)
hist(arctica_source_traits$LA_log) #  good
hist(pulchra_source_traits$LA_log) # decent
hist(richardsonii_source_traits$LA_log)# decent
# LMA
arctica_source_traits$LMA_log <- log(arctica_source_traits$leaf_mass_per_area_g_m2)
hist(arctica_source_traits$LMA_log) 
hist(arctica_source_traits$leaf_mass_per_area_g_m2) 
hist(pulchra_source_traits$leaf_mass_per_area_g_m2) # not great 
hist(richardsonii_source_traits$leaf_mass_per_area_g_m2) # not great

# leaf length 
hist(arctica_source_growth$mean_leaf_length) # decent
hist(pulchra_source_growth$mean_leaf_length) # mild right skew
hist(richardsonii_source_growth$mean_leaf_length) # pretty decent 

# SLA ---- 
# S. arctica 51 observations 
arctica_SLA_meth_mod <- brms::brm(log(SLA) ~ Site + (1|year), data = arctica_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99)) # There were 3 divergent transitions after warmup
summary(arctica_SLA_meth_mod) # crosses 0
plot(arctica_SLA_meth_mod)
pp_check(arctica_SLA_meth_mod, type = "dens_overlay", ndraws = 100)

# compare to with scaled data 
arctica_source_traits$SLA_scaled <- center_scale(arctica_source_traits$SLA)

arctica_SLA_meth_mod_scaled <- brms::brm(log(SLA_scaled) ~ Site + (1|year), data = arctica_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000,
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))# There were 1 divergent transitions after warmup
summary(arctica_SLA_meth_mod_scaled) # crosses 0
plot(arctiac_SLA_meth_mod)
pp_check(arctiac_SLA_meth_mod, type = "dens_overlay", ndraws = 100)

# S. pulchra 65 observations 
pulchra_SLA_meth_mod <- brms::brm(log(SLA) ~ Site + (1|year), data = pulchra_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(pulchra_SLA_meth_mod) 
plot(pulchra_SLA_meth_mod)
pp_check(pulchra_SLA_meth_mod, type = "dens_overlay", ndraws = 100)

# S. richardsonii 80 observations 
rich_SLA_meth_mod <- brms::brm(log(SLA) ~ Site + (1|year), data = richardsonii_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_SLA_meth_mod) # There were 2 divergent transitions after warmup.
plot(rich_SLA_meth_mod)
pp_check(rich_SLA_meth_mod, type = "dens_overlay", ndraws = 100)

# LMDC ----
# S. arctica - 29 observations 
arctiac_LDMC_meth <- brms::brm(LDMC_g_g ~ Site, data = arctica_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000)
# model didn't run with year as random effect, works without 
summary(arctiac_LDMC_meth) # 
plot(arctiac_LDMC_meth)
pp_check(arctiac_LDMC_meth, type = "dens_overlay", ndraws = 100) # decent 

# S. pulchra 45 observations 
pulchra_LDMC_meth <- brms::brm(LDMC_g_g ~ Site, data = pulchra_source_traits, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)
summary(pulchra_LDMC_meth) # significant diff
plot(pulchra_LDMC_meth)
pp_check(pulchra_LDMC_meth, type = "dens_overlay", ndraws = 100) # pretty good  
# S. richardsonii 61 observations 
rich_LDMC_meth <- brms::brm(LDMC_g_g ~ Site, data = richardsonii_source_traits, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)
# model didn't run with year as random effect, works without 
summary(rich_LDMC_meth) # significant diff
plot(rich_LDMC_meth)
pp_check(rich_LDMC_meth, type = "dens_overlay", ndraws = 100) # decent 


# LA ----

# S. arctica 93 observations 
arctica_LA_meth <- brms::brm(log(LA) ~ population, data = arctica_source_traits, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000) # runs without issue 
summary(arctica_LA_meth) # sig diff 
plot(arctica_LA_meth)
pp_check(arctica_LA_meth, type = "dens_overlay", ndraws = 100) # good  

# S. pulchra  104 observations 
pulchra_LA_meth <- brms::brm(log(LA) ~ population, data = pulchra_source_traits, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000) # runs without issue 
summary(pulchra_LA_meth) # sig diff 
plot(pulchra_LA_meth)
pp_check(pulchra_LA_meth, type = "dens_overlay", ndraws = 100) # oh weird bi modal?   

# S. richardsonii  120 observations 
rich_LA_meth <- brms::brm(log(LA) ~ population, data = richardsonii_source_traits, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000) # runs without issue 
summary(rich_LA_meth) # sig diff 
plot(rich_LA_meth)
pp_check(rich_LA_meth, type = "dens_overlay", ndraws = 100) # same bimodal swoop - is this okay?  

# LMA ----- 
# S. arctica 9 observations only - so few data - worth keeping?
arctica_LMA_meth <- brms::brm(log(leaf_mass_per_area_g_m2) ~ population, data = arctica_source_traits, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000) # runs without issue 
summary(arctica_LMA_meth) # doesn't cross 0 
plot(arctica_LMA_meth)
pp_check(arctica_LMA_meth, type = "dens_overlay", ndraws = 100) # not great?

# S. pulchra 25 observations 
pulchra_LMA_meth <- brms::brm(log(leaf_mass_per_area_g_m2) ~ population, data = pulchra_source_traits, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000) # runs without issue 
summary(pulchra_LMA_meth) # not sig diff, crosses 0
plot(pulchra_LMA_meth)
pp_check(pulchra_LMA_meth, type = "dens_overlay", ndraws = 100) # not great 
# S. richardsonii 41 observations 
rich_LMA_meth <- brms::brm(log(leaf_mass_per_area_g_m2) ~ population, data = richardsonii_source_traits, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000) # runs without issue 
summary(rich_LMA_meth) # not sig diff, crosses 0
plot(rich_LMA_meth)
pp_check(rich_LMA_meth, type = "dens_overlay", ndraws = 100) # weird?  

# leaf length -----
# not including year as random effect bc only 2 years worth of data 
# S. arctica - don't have data 
# S. pulchra - 61 observations 
pulchra_LL_meth <- brms::brm(mean_leaf_length ~ population, data = pulchra_source_growth, 
                        family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000) # no issues running 
summary(pulchra_LL_meth) # significant diff 
plot(pulchra_LL_meth)
pp_check(pulchra_LL_meth, type = "dens_overlay", ndraws = 100) 

# S. richardsonii 76 observations 
rich_LL_meth <- brms::brm(mean_leaf_length ~ population, data = richardsonii_source_growth, 
                             family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000) # no issues running
summary(rich_LL_meth) # significant diff 
plot(rich_LL_meth)
pp_check(rich_LL_meth, type = "dens_overlay", ndraws = 100) 

