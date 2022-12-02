# Methods models for growth differences comparing northern and southern willows
# by Erica Zaja, created on 29/11/2022
# Last update: 29/11/2022

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# model structure: 
# lmer(growth_variable  ~ site*species +  (1|sample_year/field_sample_ID))
# N.B Site is Kluane or QHI

# 1. Loading libraries ----
library(lme4)

# 2. Loading data ---- 
unique_source_mother <- read_csv("data/source_pops/unique_source_mother.csv")

# 3. Modelling ----
# a. Canopy height (cm) -----

# variables in right format
str(unique_source_mother)
unique_source_mother$SampleYear <- as.factor(july_source_pop_plus_mother$SampleYear)
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique(unique_source_mother$SampleYear)

# Running lmer
height_method_mod <- lmer(Canopy_Height_cm ~ Site*Species + (1|SampleYear), data = unique_source_mother)

summary(height_method_mod)
plot(height_method_mod)
qqnorm(resid(height_method_mod))
qqline(resid(height_method_mod)) 

# b. Stem elongation ---- 
#unique_source_mother_elong <- unique_source_mother %>%
 #select(mean_stem_elong, Site, Species, SampleYear, SampleID) %>%
  #drop_na(mean_stem_elong)

stem_elong_method_mod <- lmer(mean_stem_elong ~ Site*Species + (1|SampleYear), data = unique_source_mother)
# fixed-effect model matrix is rank deficient so dropping 1 column / coefficient?
summary(stem_elong_method_mod)
plot(stem_elong_method_mod)
qqnorm(resid(stem_elong_method_mod))
qqline(resid(stem_elong_method_mod)) 

# c. Width---- 
width_method_mod <- lmer(mean_width ~ Site*Species + (1|SampleYear), data = unique_source_mother)
summary(width_method_mod)
plot(width_method_mod)
qqnorm(resid(width_method_mod ))
qqline(resid(width_method_mod )) 

str(unique_source_mother)

# d. Stem diameter -----
unique_source_mother_diam <- unique_source_mother %>%
select(Stem_diameter, Site, Species, SampleYear, SampleID) %>%
drop_na(Stem_diameter)

unique(unique_source_mother_diam$Site)
# where did all the kluane stem diameters go?

diam_method_mod <- lm(Stem_diameter ~ Site*Species, data = unique_source_mother)
# removing sample year random effect because only data from 2022
