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
unique(unique_source_mother$year)

# Species specific datasets
unique_source_mother_rich <- unique_source_mother %>%
  filter(Species == "Salix richardsonii")

unique_source_mother_pulchra <- unique_source_mother %>%
  filter(Species == "Salix pulchra")

unique_source_mother_arctica <- unique_source_mother %>%
  filter(Species == "Salix arctica")

# 4. Modelling -----


# a. Canopy height -----
# Salix richardsonii -------
source_rich_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|year),
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000)

summary(source_rich_height)
plot(source_rich_height)
pp_check(source_rich_height) 


# b. Stem elongation ---- 
# Salix richardsonii -------
source_rich_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|year),
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000)

summary(source_rich_elong)
plot(source_rich_elong)
pp_check(source_rich_elong)

# c. Width----
# Salix richardsonii -------
source_rich_width<- brms::brm(log(mean_width) ~ Site,
                               data = unique_source_mother_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)

summary(source_rich_elong)
plot(source_rich_elong)
pp_check(source_rich_elong) 
conditional_effects(source_rich_elong)


# d. Stem diameter -----
# Salix richardsonii -------
source_rich_diam <- brms::brm(log(Stem_diameter) ~ Site,
                              data = unique_source_mother_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000)

summary(source_rich_diam)
plot(source_rich_diam)
pp_check(source_rich_diam) 

# d. Biovolume ----
# Salix richardsonii -------
source_rich_biovol <- brms::brm(log(biovolume) ~ Site + (1|year),
                             data = unique_source_mother_rich, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000)

summary(source_rich_biovol)
plot(source_rich_biovol)
pp_check(source_rich_biovol) 

