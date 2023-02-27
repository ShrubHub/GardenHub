# BAYESIAN traits methods models -----
# Script by Madi
# Last update: 27/02/2023

# libraries ---- 
library(brms)
library(tidyverse)

# data ---- 
# SLA, LDMC, LA: 
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") 
# leaf length: 
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") 

# filter out CG observations: 
all_source_traits <- all_CG_source_traits %>% 
  dplyr::filter(population %in% c("source_south", "source_north"))
# filter out CG observations: 
all_source_growth <- all_CG_source_growth %>% 
  dplyr::filter(population %in% c("source_south", "source_north"))

str(all_source_traits)
# reclass variables 
all_source_traits$Species <- as.factor(all_source_traits$Species)
all_source_traits$Site <- as.factor(all_source_traits$Site)
all_source_traits$year <- as.factor(all_source_traits$year)

str(all_CG_source_growth) 
# reclass variables 
all_source_growth$Species <- as.factor(all_source_growth$Species)
all_source_growth$population <- as.factor(all_source_growth$population)
all_source_growth$Year <- as.factor(all_source_growth$Year)

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

# leaf area using 2021 and 2022 data only: 
all_source_traits_2022 <- all_source_traits %>% 
  dplyr::filter(year %in% c("2021", "2022")) 
# now filter by spp
arctica_source_2022_traits <- all_source_traits_2022 %>% 
  filter(Species == "Salix arctica")
pulchra_source_2022_traits <- all_source_traits_2022 %>% 
  filter(Species == "Salix pulchra")
richardsonii_source_2022_traits <- all_source_traits_2022 %>% 
  filter(Species == "Salix richardsonii")

# look at distributions ----
# SLA
hist(arctica_source_traits$SLA) # mild right skew
hist(pulchra_source_traits$SLA) # mildly better right skew
hist(richardsonii_source_traits$SLA)# mild right skew

# LDMC
hist(arctica_source_traits$LDMC_g_g) 
hist(pulchra_source_traits$LDMC_g_g) # very right skew
hist(richardsonii_source_traits$LDMC_g_g) # very off 
# LA
hist(arctica_source_2022_traits$LA) #  right skew
hist(pulchra_source_2022_traits$LA) # mild right skew
hist(richardsonii_source_2022_traits$LA)# decent
# LMA
hist(arctica_source_2022_traits$leaf_mass_per_area_g_m2) 
hist(pulchra_source_2022_traits$leaf_mass_per_area_g_m2) 
hist(richardsonii_source_2022_traits$leaf_mass_per_area_g_m2) 
# leaf length 
hist(arctica_source_growth$mean_leaf_length) # decent
hist(pulchra_source_growth$mean_leaf_length) # mild right skew
hist(richardsonii_source_growth$mean_leaf_length) # pretty decent 

# SLA ---- 

# S. arctica 51 observations 
arctiac_SLA_meth_mod <- brms::brm(SLA ~ Site + (1|year), data = arctica_source_traits, family = gaussian(), chains = 3,
                                  iter = 5000, warmup = 1000)
summary(arctiac_SLA_meth_mod) # There were 9 divergent transitions after warmup with 5000 iterations, 71 with 3000 iterations
plot(arctiac_SLA_meth_mod)
pp_check(arctiac_SLA_meth_mod)

# S. pulchra 65 observations 
pulchra_SLA_meth_mod <- brms::brm(SLA ~ Site + (1|year), data = pulchra_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000)
summary(pulchra_SLA_meth_mod) # There were 320 divergent transitions after warmup with 5000 iterations omg 
# only 9 with 3000 iterations - sample size is small 
plot(pulchra_SLA_meth_mod)
pp_check(pulchra_SLA_meth_mod)

# S. richardsonii 80 observations 
rich_SLA_meth_mod <- brms::brm(SLA ~ Site + (1|year), data = richardsonii_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000)
summary(rich_SLA_meth_mod) # There were 16 divergent transitions after warmup
plot(rich_SLA_meth_mod)
pp_check(rich_SLA_meth_mod)


# LMDC ----

# S. arctica - 29 observations 
arctiac_LDMC_meth <- brms::brm(LDMC_g_g ~ Site, data = arctica_source_traits, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000)
# model didn't run with year as random effect, works without 
summary(arctiac_LDMC_meth) # 
plot(arctiac_LDMC_meth)
pp_check(arctiac_LDMC_meth, type = "dens_overlay", ndraws = 100) # decent but slightly askew? 

# S. pulchra 45 observations 
pulchra_LDMC_meth <- brms::brm(LDMC_g_g ~ Site, data = pulchra_source_traits, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)
# model didn't run with year as random effect, works without 
summary(pulchra_LDMC_meth) # significant diff
plot(pulchra_LDMC_meth)
pp_check(pulchra_LDMC_meth, type = "dens_overlay", ndraws = 100) # not great 
# S. richardsonii 61 observations 
rich_LDMC_meth <- brms::brm(LDMC_g_g ~ Site, data = richardsonii_source_traits, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)
# model didn't run with year as random effect, works without 
summary(rich_LDMC_meth) # significant diff
plot(rich_LDMC_meth)
pp_check(rich_LDMC_meth, type = "dens_overlay", ndraws = 100) # decent 


# LA ----

# not including year because only 2 years worth of data 
# S. arctica, 9 observations - is this worth it? 
arctiac_LA_meth <- brms::brm(LA ~ population, data = arctica_source_2022_traits, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000) # runs without issue 
summary(arctiac_LA_meth) # not sig diff, crosses 0
plot(arctiac_LA_meth)
pp_check(arctiac_LA_meth, type = "dens_overlay", ndraws = 100) # oh this is not good 

# S. pulchra, 25 observations 

# S. richardsonii, 42 observations 

# LMA ----- 
# so few data - worth keeping? 
# S. arctica 
# S. pulchra 
# S. richardsonii 

# leaf length -----
# not including year as random effect bc only 2 years worth of data 
# S. arctica - don't have data 
# S. pulchra - 61 observations 
pulchra_LL_meth <- brms::brm(mean_leaf_length ~ population, data = pulchra_source_growth, 
                        family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000) # no issues running 
summary(pulchra_LL_meth) # significant diff 
plot(pulchra_LL_meth)
pp_check(pulchra_LL_meth, type = "dens_overlay", ndraws = 100) 

# S. richardsonii 76 observations 
rich_LL_meth <- brms::brm(mean_leaf_length ~ population, data = richardsonii_source_growth, 
                             family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000) # no issues running
summary(rich_LL_meth) # significant diff 
plot(rich_LL_meth)
pp_check(rich_LL_meth, type = "dens_overlay", ndraws = 100) 

