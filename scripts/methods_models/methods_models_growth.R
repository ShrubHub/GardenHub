# Methods models for growth differences comparing northern and southern willows
# by Erica Zaja, created on 29/11/2022
# Last update: 29/11/2022

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# model structure: 
# lmer(growth_variable  ~ site*species +  (1|sample_year/field_sample_ID))
# N.B Site is Kluane or QHI

# Loading libraries ----
library(lme4)

# Loading data ---- 
july_source_pop_plus_mother <- read.csv("data/source_pop/july_source_pops_plus_mother.csv")

# 1. Canopy height (cm) -----
str(july_source_pop_plus_mother)

july_source_pop_plus_mother$SampleYear <- as.factor(july_source_pop_plus_mother$SampleYear)
unique(july_source_pop_plus_mother$SampleYear)

height_method_mod <- lmer(Canopy_Height_cm ~ Site*Species + (1|SampleYear), data = july_source_pop_plus_mother)

summary(height_method_mod)
plot(height_method_mod)
qqnorm(resid(height_method_mod))
qqline(resid(height_method_mod)) 

# 2. Stem elongation ---- 
july_source_pop_plus_mother_elong <- july_source_pop_plus_mother %>%
  select(mean_stem_elong, Site, Species, SampleYear, SampleID) %>%
  drop_na(mean_stem_elong)
  
str(july_source_pop_plus_mother_elong)

stem_elong_method_mod <- lmer(mean_stem_elong ~ Site*Species + (1|SampleYear), data = july_source_pop_plus_mother_elong)
# doesnt work because we havent identified the species yet
summary(stem_elong_method_mod)
plot(stem_elong_method_mod)
qqnorm(resid(stem_elong_method_mod))
qqline(resid(stem_elong_method_mod)) 

# 3. Width---- 
july_source_pop_plus_mother_width <- july_source_pop_plus_mother %>%
  select(mean_width, Site, Species, SampleYear, SampleID) %>%
  drop_na(mean_width)

width_method_mod <- lmer(mean_width ~ Site*Species + (1|SampleYear), data = july_source_pop_plus_mother)

# 4. Stem diameter -----

diam_method_mod <- lmer(Stem_diameter ~ Site*Species + (1|SampleYear), data = july_source_pop_plus_mother)

