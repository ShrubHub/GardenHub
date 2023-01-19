# Methods models for growth differences comparing northern and southern willows
# by Erica Zaja, created on 29/11/2022
# Last update: 29/11/2022

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# model structure: 
# lmer(growth_variable  ~ site*species +  (1|sample_year/field_sample_ID))
# N.B Site is Kluane or QHI

# 1. Loading libraries ----
library(lme4)
library(dplyr)
library(sjPlot)
library(ggpubr)

# 2. Loading data ---- 
unique_source_mother <- read_csv("data/source_pops/unique_source_mother.csv")

# 3. Wrangling ----
# variables in right format
str(unique_source_mother)
unique_source_mother$SampleYear <- as.factor(unique_source_mother$SampleYear)
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique(unique_source_mother$SampleYear)
view(unique_source_mother)
# filter out strange arctica values 
unique_source_mother_edit_1 <- unique_source_mother %>%
  subset(Species != "Salix arctica") # remove all arctica from main dataset

unique_source_mother_edit_2 <- unique_source_mother %>%
  subset(Species == "Salix arctica" & Canopy_Height_cm <= 25.0) # filter out arcticas shorter than 25cm

# remerge datasets
unique_source_mother <- rbind(unique_source_mother_edit_1, unique_source_mother_edit_2)
view(unique_source_mother)
str(unique_source_mother)

# 4. Modelling ----

# a. Canopy height -----

# Height lmer with species interacting
height_method_mod <- lmer(Canopy_Height_cm ~ Site*Species + (1|SampleYear), data = unique_source_mother)

summary(height_method_mod)
plot(height_method_mod)
qqnorm(resid(height_method_mod))
qqline(resid(height_method_mod)) 
tab_model(height_method_mod)

(height_p <- ggplot(unique_source_mother, aes(Site, Canopy_Height_cm)) +
    geom_boxplot() +
    facet_wrap(vars(Species))+
    ylim (0,250)) # to remove outliers

(plot_canopy_height_source <- ggplot(unique_source_mother) +
    geom_boxplot(aes(x = Site, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Canopy Height (cm)") +
    xlab("\n") +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 15, color = "black", face = "italic"),
          legend.title = element_text(size=15), #change legend title font size
          legend.text = element_text(size=12),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

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
# N.B only comparing pulchra and richardsonii

summary(stem_elong_method_mod_2)
plot(stem_elong_method_mod_2)
qqnorm(resid(stem_elong_method_mod_2))
qqline(resid(stem_elong_method_mod_2)) 
tab_model(stem_elong_method_mod_2)

(elong_p <- ggplot(unique_source_mother, aes(Site, mean_stem_elong)) +
    geom_boxplot() +
    facet_wrap(vars(Species))) 
  
(plot_stem_elong_source <- ggplot(unique_source_mother) +
    geom_boxplot(aes(x = Site, y = mean_stem_elong, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Canopy Height (cm)") +
    xlab("\n") +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 15, color = "black", face = "italic"),
          legend.title = element_text(size=15), #change legend title font size
          legend.text = element_text(size=12),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# c. Width---- 

# Width lmer with species interacting
unique_source_mother_width <- unique_source_mother %>%
  select(Site, Species, SampleYear, SampleID, mean_width)

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

(width_p <- ggplot(unique_source_mother, aes(Site, mean_width)) +
    geom_boxplot() +
    facet_wrap(vars(Species))) 
    
# d. Stem diameter -----

# Diameter lmer with species interacting
diam_method_mod <- lm(Stem_diameter ~ Site*Species, data = unique_source_mother)
# removing sample year random effect because only data from 2022
# N.B only comparing pulchra and richardsonii

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

(diam_p <- ggplot(unique_source_mother, aes(Site, Stem_diameter)) +
    geom_boxplot() +
    facet_wrap(vars(Species))) 

# quick arrange 
(growth_plots <- ggarrange(height_p, width_p, elong_p, diam_p, nrow = 2, ncol = 2))


# visualize growth by year  ---- 
# should all be similar year to year
(height_p_year <- ggplot(unique_source_mother, aes(Site, Canopy_Height_cm)) + 
   geom_boxplot() +
   facet_wrap(vars(SampleYear))) # all similar, with Kluane higher canopies

(width_p_year <- ggplot(unique_source_mother, aes(Site, mean_width)) + 
    geom_boxplot() +
    facet_wrap(vars(SampleYear))) # 2022 different than other years: Kluane higher widths

(elong_p_year <- ggplot(unique_source_mother, aes(Site, mean_stem_elong)) + 
    geom_boxplot() +
    facet_wrap(vars(SampleYear))) # all similar with Kluane higher elongation

(diam_p_year <- ggplot(unique_source_mother, aes(Site, Stem_diameter)) + 
    geom_boxplot() +
    facet_wrap(vars(SampleYear))) # only 2022
