# BAYESIAN growth methods models -----
# Script by Erica
# Last update: 21/02/2023

# 1. Loading libraries ----
library(brms)
library(tidyverse)

# 2. Loading data ---- 
unique_source_mother <- read_csv("data/source_pops/unique_source_mother.csv")

# 3. Wrangling ----
# variables in right format
str(unique_source_mother)
unique_source_mother$SampleYear <- as.factor(unique_source_mother$SampleYear)
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique_source_mother$year <-  format(as.Date(unique_source_mother$SampleDate, format="%d/%m/%Y"),"%Y")
unique(unique_source_mother$SampleYear)

# Species specific datasets
unique_source_mother_rich <- unique_source_mother %>%
  filter(Species == "Salix richardsonii")

unique_source_mother_pulchra <- unique_source_mother %>%
  filter(Species == "Salix pulchra")

unique_source_mother_arctica <- unique_source_mother %>%
  filter(Species == "Salix arctica")

# exploring variables distribution
hist(unique_source_mother_arctica$Canopy_Height_cm) # right skew
hist(unique_source_mother_arctica$mean_width) # right skew
hist(unique_source_mother_arctica$mean_stem_elong) # right skew
hist(unique_source_mother_arctica$Stem_diameter, breaks = 30) #  right skew - so weird
hist(unique_source_mother_arctica$biovolume,  breaks = 30)#  right skew

# 4. Modelling -----

# a. CANOPY HEIGHT -----
# Salix richardsonii -------
source_rich_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_height) # lower heights for QHi
plot(source_rich_height)
pp_check(source_rich_height, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra ------
source_pul_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_height) # lower heights for QHi
plot(source_pul_height)
pp_check(source_pul_height, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica -----
source_arc_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_height) # no significant difference
plot(source_arc_height)
pp_check(source_arc_height, type = "dens_overlay", nsamples = 100)  # good) 

# b. STEM ELONGATION ---- 
# Salix richardsonii -------
source_rich_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|SampleYear),
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_elong)# shorter elongations for QHi
plot(source_rich_elong)
pp_check(source_rich_elong,  type = "dens_overlay", nsamples = 100)

# Salix pulchra -------
source_pul_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|SampleYear),
                               data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_elong)# shorter elongations for QHi
plot(source_pul_elong)
pp_check(source_pul_elong,  type = "dens_overlay", nsamples = 100)

# Salix arctica: NOT CONVERGED------
source_arc_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|SampleYear),
                               data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 20, adapt_delta = 0.99))

summary(source_arc_elong)# shorter elongations for QHi
plot(source_arc_elong)
pp_check(source_arc_elong,  type = "dens_overlay", nsamples = 100)

# c. WIDTH ----
# Salix richardsonii -------
source_rich_width<- brms::brm(log(mean_width) ~ Site + (1|SampleYear),
                               data = unique_source_mother_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_width) # significantly LARGER width for QHI shrubs
plot(source_rich_width)
pp_check(source_rich_width, type = "dens_overlay", nsamples = 100)  # fine

# Salix pulchra -----
source_pul_width<- brms::brm(log(mean_width) ~ Site + (1|SampleYear),
                              data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_width) # significantly lower width for QHI shrubs
plot(source_pul_width)
pp_check(source_pul_width, type = "dens_overlay", nsamples = 100)  # fine


# Salix arctica -------
source_arc_width<- brms::brm(log(mean_width) ~ Site + (1|SampleYear),
                             data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_width) # no significant diff
plot(source_arc_width)
pp_check(source_arc_width, type = "dens_overlay", nsamples = 100)  # fine


# d. STEM DIAMETER -----
# Salix richardsonii -------
source_rich_diam <- brms::brm(log(Stem_diameter) ~ Site + (1|SampleYear),
                              data = unique_source_mother_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_diam) # lower stem diameter for QHi shrubs
plot(source_rich_diam)
pp_check(source_rich_diam, type = "dens_overlay", nsamples = 100) 

# Salix pulchra ------
source_pul_diam <- brms::brm(log(Stem_diameter) ~ Site + (1|SampleYear),
                              data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_diam) # lower stem diameter for QHi shrubs
plot(source_pul_diam)
pp_check(source_pul_diam, type = "dens_overlay", nsamples = 100) 

# Salix arctica :NOT CONVERGED------
source_arc_diam <- brms::brm(log(Stem_diameter) ~ Site + (1|SampleYear),
                              data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_diam) # lower stem diameter for QHi shrubs
plot(source_arc_diam)
pp_check(source_arc_diam, type = "dens_overlay", nsamples = 100) 

# d.BIOVOLUME----
# Salix richardsonii -------
source_rich_biovol <- brms::brm(log(biovolume) ~ Site + (1|SampleYear),
                             data = unique_source_mother_rich, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(source_rich_biovol) # no significant difference
plot(source_rich_biovol)
pp_check(source_rich_biovol, type = "dens_overlay", nsamples = 100) 

# Salix pulchra -------
source_pul_biovol <- brms::brm(log(biovolume) ~ Site + (1|SampleYear),
                                data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(source_pul_biovol) # smaller biovolume for QHI shrubs
plot(source_pul_biovol)
pp_check(source_pul_biovol, type = "dens_overlay", nsamples = 100) 

# Salix arctica -------
source_arc_biovol <- brms::brm(log(biovolume) ~ Site + (1|SampleYear),
                               data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(source_arc_biovol) # no significant difference
plot(source_arc_biovol)
pp_check(source_arc_biovol, type = "dens_overlay", nsamples = 100) 

