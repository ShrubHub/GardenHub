# BAYESIAN phenology script ----
# BY Erica and Madi 
# Last update: 01/03/2023

# Libraries----
library(plyr)
library(tidyverse)
library(brms)

# Load data ----
all_phenocam_data_salix <- read_csv("data/phenology/phenocam_pics/all_phenocam_data_salix.csv")
all_growing_season <- read_csv("data/phenology/phenocam_pics/all_growing_season.csv")

# Wrangle data ------
# ordering levels so source and garden populations side by side
all_phenocam_data_salix$population <- plyr::revalue(all_phenocam_data_salix$population, 
                                                    c("QHI"="Northern Garden",
                                                      "Kluane"="Southern Garden",
                                                      "Southern_source"="Southern Source",
                                                      "Northern_source"="Northern Source"))

all_phenocam_data_salix$population <- ordered(all_phenocam_data_salix$population, 
                                              levels = c("Northern Source", 
                                                         "Northern Garden", 
                                                         "Southern Source",
                                                         "Southern Garden"))

all_phenocam_data_salix$Year <- as.factor(all_phenocam_data_salix$Year)

all_growing_season$population <- plyr::revalue(all_growing_season$population, 
                                               c("Northern"="Northern Garden",
                                                 "Southern"="Southern Garden",
                                                 "KP"="Southern Source",
                                                 "QHI"="Northern Source"))

all_growing_season$population <- ordered(all_growing_season$population, 
                                         levels = c("Northern Source", 
                                                    "Northern Garden", 
                                                    "Southern Source",
                                                    "Southern Garden"))
# SPECIES SPECIFIC datasets: CG + Sources -----
all_phenocam_rich <- all_phenocam_data_salix %>%
  filter(Species == "Salix richardsonii")

all_phenocam_pulchra <- all_phenocam_data_salix %>%
  filter(Species == "Salix pulchra")

all_phenocam_arctica <- all_phenocam_data_salix %>%
  filter(Species == "Salix arctica")

# SOURCE POP ONLY species specific datasets -----
all_phenocam_rich_source <- all_phenocam_rich %>%
  filter(population %in% c("Northern Source", "Southern Source"))

all_phenocam_pul_source <- all_phenocam_pulchra %>%
  filter(population %in% c("Northern Source", "Southern Source"))

all_phenocam_arc_source <- all_phenocam_arctica %>%
  filter(population %in% c("Northern Source", "Southern Source"))

# CG only species specific data -----
all_phenocam_rich_garden <- all_phenocam_rich %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))

all_phenocam_pul_garden <- all_phenocam_pulchra %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))

all_phenocam_arc_garden <- all_phenocam_arctica %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))

# Sp specific growing season data
all_growing_season_rich <- all_growing_season %>%
  filter(Species == "Salix richardsonii")

all_growing_season_pul <- all_growing_season %>%
  filter(Species == "Salix pulchra")

all_growing_season_arc <- all_growing_season %>%
  filter(Species == "Salix arctica")

# exploring variables ------
hist(all_phenocam_pul_source$First_bud_burst_DOY, breaks=30) # defo not normal
hist(all_phenocam_rich_source$First_bud_burst_DOY, breaks=30) # defo not normal
hist(all_phenocam_arc_source$First_bud_burst_DOY, breaks=30) # defo not normal

hist(all_phenocam_pul_source$First_leaf_yellow_DOY, breaks=30) # defo not normal
hist(all_phenocam_rich_source$First_leaf_yellow_DOY, breaks=30) # defo not normal
hist(all_phenocam_arc_source$First_leaf_yellow_DOY, breaks=30) # defo not normal

# MODELLING ------
# 1. LEAF EMERGENCE (only source pops) -------

# Salix richardsonii -------
source_rich_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

bud_burst_mod <- lmer(First_bud_burst_DOY ~ population  + (1|Year), data = all_phenocam_pul_source)
tab_model(bud_burst_mod)

summary(source_rich_emerg) # not significant diff. 
plot(source_rich_emerg)
pp_check(source_rich_emerg , type = "dens_overlay", nsamples = 100) # fine??

# Salix pulchra -------
source_pul_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_pul_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_emerg) # not significant
plot(source_pul_emerg)
pp_check(source_pul_emerg, type = "dens_overlay", nsamples = 100) # not too happy with that....

# Salix arctica -------
# MISSING KLUANE PLATEAU DATA so cannot run 

# 1.1. LEAF EMERGENCE (CG vs SOURCES)  ------
# Salix richardsonii -----
garden_rich_emerg_compare <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_emerg_compare)
plot(garden_rich_emerg_compare)
pp_check(garden_rich_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good

# Salix pulchra -----
garden_pul_emerg_compare <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                                       data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000, 
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_emerg_compare)
plot(garden_pul_emerg_compare)
pp_check(garden_pul_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good

# Salix arctica -----
garden_arc_emerg_compare <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                                      data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                      iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_emerg_compare)
plot(garden_arc_emerg_compare)
pp_check(garden_arc_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good


# 1.2. LEAF EMERGENCE (CG ONLY MODELS) ------
# Salix richardsonii -----
garden_rich_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                                       data = all_phenocam_rich_garden, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))



summary(garden_rich_emerg) # no significant difference
plot(garden_rich_emerg)
pp_check(garden_rich_emerg,type = "dens_overlay", nsamples = 100) # looks ok

# Salix pulchra -----
garden_pul_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_pul_garden, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))



summary(garden_pul_emerg) # yes significant. (southern emergence later?)
plot(garden_pul_emerg)
pp_check(garden_pul_emerg,type = "dens_overlay", nsamples = 100) # looks ok

# Salix arctica -----
garden_arc_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                              data = all_phenocam_arc_garden, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))



summary(garden_arc_emerg) # YEs significant (southern emergence later?)
plot(garden_arc_emerg)
pp_check(garden_arc_emerg,type = "dens_overlay", nsamples = 100) # looks ok


# 2. LEAF YELLOWING (only source pops) -----
# Salix richardsonii -------
source_rich_yellow <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_yellow) # no significant diff
plot(source_rich_yellow)
pp_check(source_rich_yellow, type = "dens_overlay", nsamples = 100) # looks ok

# Salix pulchra -------
source_pul_yellow <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                data = all_phenocam_pul_source, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_yellow) # no significant diff
plot(source_pul_yellow)
pp_check(source_pul_yellow, type = "dens_overlay", nsamples = 100) # looks ok

# Salix arctica -------
# Missing KP data so cannot run

# 2.1. LEAF YELLOWING (source vs garden) -------
# Salix richardsonii ------
garden_rich_yellow_compare <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                        data = all_phenocam_rich, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))



summary(garden_rich_yellow_compare)
plot(garden_rich_yellow_compare)
pp_check(garden_rich_yellow_compare, type = "dens_overlay", nsamples = 100) # looks good

# Salix pulchra ------
garden_pul_yellow_compare <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                        data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))



summary(garden_pul_yellow_compare)
plot(garden_pul_yellow_compare)
pp_check(garden_pul_yellow_compare, type = "dens_overlay", nsamples = 100) # looks good

# Salix arctica ------
garden_arc_yellow_compare <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                       data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_yellow_compare)
plot(garden_arc_yellow_compare)
pp_check(garden_arc_yellow_compare, type = "dens_overlay", nsamples = 100) # looks good

# 2.2.  LEAF YELLOWING (only CG) -----
# Salix richardsonii -------
garden_rich_yellow <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                        data = all_phenocam_rich_garden, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_yellow) # significant (later for southern shrubs?)
plot(garden_rich_yellow) 
pp_check(garden_rich_yellow, type = "dens_overlay", nsamples = 100) # looks good

# Salix pulchra -------
garden_pul_yellow <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                                data = all_phenocam_pul_garden, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_yellow) # significant (later for southern shrubs?)
plot(garden_pul_yellow)
pp_check(garden_rich_yellow, type = "dens_overlay", nsamples = 100) # looks good

# Salix arctica -------
garden_arc_yellow <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                               data = all_phenocam_arc_garden, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000,
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_yellow) # significant (later for southern shrubs?)
plot(garden_arc_yellow)
pp_check(garden_arc_yellow, type = "dens_overlay", nsamples = 100) # looks good


# 3. GROWING SEASON LENGTH -----
# Salix richardsonii ------
growing_season_rich <- brms::brm(growing_season ~ population, 
                                data = all_growing_season_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))



summary(growing_season_rich) # all different
plot(growing_season_rich)
pp_check(growing_season_rich, type = "dens_overlay", nsamples = 100) # looks good

# Salix pulchra ------
growing_season_pul <- brms::brm(growing_season ~ population, 
                                 data = all_growing_season_pul, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000,
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(growing_season_pul) # 
plot(growing_season_pul)
pp_check(growing_season_pul, type = "dens_overlay", nsamples = 100) # looks good

# Salix arctica ------
growing_season_arc <- brms::brm(growing_season ~ population,
                                data = all_growing_season_arc, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_arc)
plot(growing_season_arc)
pp_check(growing_season_arc, type = "dens_overlay", nsamples = 100) # looks good
