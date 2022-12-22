# Results models for growth differences comparing northern and southern willows
# in the garden to their source populations
# by Erica Zaja, created on 13/12/2022
# Last update: 19/12/2022

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# attempted model structure: 
# lmer(growth_variable ~ population*site + sample_age + (1|sample_year/species/sample_ID))
# N.B Site is (a) common garden, (b) Kluane, (c) QHI. 
# population: is (a) common garden northern, (b) common garden southern, (c) source 

# if triple nested structure doesn't work, next attempted structure: 
# lmer(growth_variable ~ population*site + sample_age + (1|sample_year)+(1|species)+ (1|sample_ID))

# Loading libraries ----
library(lme4)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
library(corrplot)

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# Data wrangling -----
str(all_CG_source_growth)
# reclassing variables
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)
unique(all_CG_source_growth$population)
unique(all_CG_source_growth$Site)

# Modelling -----

# 1. Canopy height (compare all) -----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_growth_mod_1 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_growth_mod_2 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year/Species), data = all_CG_source_growth)
# model 3 runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficient
height_growth_mod_3 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year/SampleID_standard)+ (1|Species), data = all_CG_source_growth)
# model 4 runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
height_growth_mod_4 <- lmer(Canopy_Height_cm ~ population*Site + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)
tab_model(height_growth_mod_4) 

# 1.1. Canopy height only in garden -----
# filter dataset to retain only population "northern" and "southern"
all_CG_source_growth_garden_only <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))

# model 1 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_garden_growth_mod_1 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model 2 below doesn't converge: boundary (singular) fit: see help('isSingular')
height_garden_growth_mod_2 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species), data = all_CG_source_growth_garden_only)
# model 3 below does converge
height_garden_growth_mod_3 <- lmer(Canopy_Height_cm ~ population + Sample_age + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
tab_model(height_garden_growth_mod_3)
# canopy height significatly taller for southern population

# 2. Width (compare all)-----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
width_growth_mod_1 <- lmer(mean_width ~ population*Site + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
width_growth_mod_2 <- lmer(mean_width~ population*Site + (1|Year/SampleID_standard)+ (1|Species), data = all_CG_source_growth)
# model 3 runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
width_growth_mod_3 <- lmer(mean_width ~ population*Site +  (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)
tab_model(width_growth_mod_3)

# 2.1. Width only in garden -----
# model 1 below doesnt converge
height_garden_width_mod_1 <- lmer(mean_width ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model below runs with no warning
height_garden_width_mod_2 <- lmer(mean_width ~ population + Sample_age +(1|Year)+(1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
tab_model(height_garden_width_mod_2)
# southern shrubs in the garden have wider canopies

# 3. Stem elongation (compare all) ----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
elong_growth_mod_1 <- lmer(mean_stem_elong ~ population*Site + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 below runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
elong_growth_mod_2 <- lmer(mean_stem_elong ~ population*Site + (1|Year/SampleID_standard)+ (1|Species), data = all_CG_source_growth)
# model 3 below runs but: fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
elong_growth_mod_3 <- lmer(mean_stem_elong ~ population*Site + (1|Year)+ (1|Species) + (1|SampleID_standard) , data = all_CG_source_growth)
tab_model(elong_growth_mod_3)

# 3.1. Stem elongation only in garden -----
# model below converges
elong_garden_growth_mod_1 <- lmer(mean_stem_elong ~ population + Sample_age + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
tab_model(elong_garden_growth_mod_1)
# higher stem elongation for southern shrubs
# model below converges too
elong_garden_growth_mod_2 <- lmer(mean_stem_elong ~ population + Sample_age + (1|Year/Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)


# 4. Stem diameter (compare all) ----
# models 1 and 2 below doesnt converge: boundary (singular) fit: see help('isSingular')
diam_growth_mod_1 <- lmer(Stem_diameter ~ population*Site + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
diam_growth_mod_2 <- lmer(Stem_diameter ~ population*Site + (1|Year/Species) + (1|SampleID_standard), data = all_CG_source_growth)
# model 2 below does converge but fixed-effect model matrix is rank deficient so dropping 8 columns / coefficients
diam_growth_mod_3 <- lmer(Stem_diameter ~ population*Site + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)
tab_model(diam_growth_mod_3)

# 4.1. Stem diameter only in garden ----
# model 1 below doesnt converge: boundary (singular) fit: see help('isSingular')
diam_garden_growth_mod_1 <- lmer(Stem_diameter ~ population + Sample_age + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model 2 below doesnt converge: boundary (singular) fit: see help('isSingular')
diam_garden_growth_mod_2 <- lmer(Stem_diameter ~ population + Sample_age + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
# model 3 below does converge (without year?)
diam_garden_growth_mod_3 <- lmer(Stem_diameter ~ population + Sample_age + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
tab_model(diam_garden_growth_mod_3)
# larger diameters for southern shrubs


# correlation matrix ----
# filter growth only (but maybe need to separate CG and source pops?)
growth_variables <- all_CG_source_growth %>%
  select(Canopy_Height_cm, Stem_diameter, mean_stem_elong,
         mean_leaf_length, mean_width)%>%
  na.omit()
  
res <- cor(growth_variables)
round(res, 2)

