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

# 3. Wrangling ----
# variables in right format
str(unique_source_mother)
unique_source_mother$SampleYear <- as.factor(unique_source_mother$SampleYear)
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique(unique_source_mother$SampleYear)

# 4. Modelling ----

# a. Canopy height -----

# Height lmer with species interacting
height_method_mod <- lmer(Canopy_Height_cm ~ Site*Species + (1|SampleYear), data = unique_source_mother)

summary(height_method_mod)
plot(height_method_mod)
qqnorm(resid(height_method_mod))
qqline(resid(height_method_mod)) 
tab_model(height_method_mod)

# Height lmer with species random effect
height_method_mod_2 <- lmer(Canopy_Height_cm ~ Site + (1|Species) + (1|SampleYear), data = unique_source_mother)
summary(height_method_mod_2)
plot(height_method_mod_2)
qqnorm(resid(height_method_mod_2))
qqline(resid(height_method_mod_2)) 
tab_model(height_method_mod_2)

# b. Stem elongation ---- 

# Stem elongation lmer with species interacting
stem_elong_method_mod <- lm(mean_stem_elong ~ Site*Species, data = unique_source_mother)
# fixed-effect model matrix is rank deficient so dropping 1 column / coefficient?
summary(stem_elong_method_mod)
plot(stem_elong_method_mod)
qqnorm(resid(stem_elong_method_mod))
qqline(resid(stem_elong_method_mod)) 
tab_model(stem_elong_method_mod)

# Stem elongation lmer with species random effect
stem_elong_method_mod_2 <- lmer(mean_stem_elong ~ Site + (1|Species), data = unique_source_mother)
#no warning for this 
summary(stem_elong_method_mod_2)
plot(stem_elong_method_mod_2)
qqnorm(resid(stem_elong_method_mod_2))
qqline(resid(stem_elong_method_mod_2)) 
tab_model(stem_elong_method_mod_2)


# c. Width---- 

# Width lmer with species interacting
width_method_mod <- lmer(mean_width ~ Site*Species + (1|SampleYear), data = unique_source_mother)

summary(width_method_mod)
plot(width_method_mod)
qqnorm(resid(width_method_mod))
qqline(resid(width_method_mod)) 
tab_model(width_method_mod)

# Width lmer with species random effect
width_method_mod_2 <- lmer(mean_width ~ Site + (1|Species) + (1|SampleYear), data = unique_source_mother)

summary(width_method_mod_2)
plot(width_method_mod_2)
qqnorm(resid(width_method_mod_2))
qqline(resid(width_method_mod_2)) 
tab_model(width_method_mod_2 )

# d. Stem diameter -----

# Diameter lmer with species interacting
diam_method_mod <- lm(Stem_diameter ~ Site*Species, data = unique_source_mother)
# removing sample year random effect because only data from 2022

summary(diam_method_mod)
plot(diam_method_mod)
qqnorm(resid(diam_method_mod))
qqline(resid(diam_method_mod)) 
tab_model(diam_method_mod)

# Diameter lmer with species random effect
diam_method_mod_2 <- lmer(Stem_diameter ~ Site + (1|Species), data = unique_source_mother)
# removing sample year random effect because only data from 2022
# boundary (singular) fit: see help('isSingular')
summary(diam_method_mod_2)
plot(diam_method_mod_2)
qqnorm(resid(diam_method_mod_2))
qqline(resid(diam_method_mod_2)) 
tab_model(diam_method_mod_2)


