# BAYESIAN growth results models -----
# Script by Erica
# Last update: 21/02/2023
# Code adapted from coding club tutorial by Louise Litrico:
# https://ourcodingclub.github.io/tutorials/brms/ 

# 3 options for growth model structure: 
# 1) max height across all years, 
# 2) using the last three years of August height data as a repeated measures test (without the shrubID nesting in the random effects), 
# which assumes that the shrubs aren’t really getting taller any more (seems true from the data, but that isn’t a priori), or 
# 3) only using the August 2022 data (aka the last time point).

# Loading libraries ----
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# reclassing variables
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

# filter dataset to retain only population "northern" and "southern"
all_CG_source_growth_garden_only <- all_CG_source_growth %>%
  filter(Site == "Common_garden") %>%
  filter(population %in% c("Northern", "Southern"))

all_CG_source_growth_garden_only$population <- as.character(all_CG_source_growth_garden_only$population)
str(all_CG_source_growth_garden_only$population)
unique(all_CG_source_growth_garden_only$population)
all_CG_source_growth_garden_only$population <- as.factor(all_CG_source_growth_garden_only$population)


# ordering levels so source and garden populations side by side
all_CG_source_growth_garden_only$population <- plyr::revalue(all_CG_source_growth_garden_only$population , 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden"))


# Separating into 3 datasets: one per spp.
all_CG_source_growth_garden_rich <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix richardsonii")
# write.csv(all_CG_source_growth_garden_rich, "data/all_CG_source_growth_garden_rich.csv")

all_CG_source_growth_garden_pulchra <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix pulchra") 
# write.csv(all_CG_source_growth_garden_pulchra, "data/all_CG_source_growth_garden_pulchra.csv")

all_CG_source_growth_garden_arctica <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix arctica") 
# write.csv(all_CG_source_growth_garden_arctica, "data/all_CG_source_growth_garden_arctica.csv")

# exploring variables distribution
hist(all_CG_source_growth_garden_rich$Canopy_Height_cm) # right skew
hist(all_CG_source_growth_garden_pulchra$Canopy_Height_cm) #  right skew
hist(all_CG_source_growth_garden_arctica$Canopy_Height_cm)#  right skew

hist(all_CG_source_growth_garden_rich$mean_stem_elong) # right skew
hist(all_CG_source_growth_garden_pulchra$mean_stem_elong) #  right skew
hist(all_CG_source_growth_garden_arctica$mean_stem_elong)#  right skew

hist(all_CG_source_growth_garden_rich$biovolume) # right skew
hist(all_CG_source_growth_garden_pulchra$biovolume) #  right skew
hist(all_CG_source_growth_garden_arctica$biovolume)#  right skew

hist(all_CG_source_growth_garden_rich$mean_width) # right skew
hist(all_CG_source_growth_garden_pulchra$mean_width) #  right skew
hist(all_CG_source_growth_garden_arctica$mean_width)#  right skew

hist(all_CG_source_growth_garden_rich$Stem_diameter) # right skew
hist(all_CG_source_growth_garden_pulchra$Stem_diameter) #  right skew
hist(all_CG_source_growth_garden_arctica$Stem_diameter)#  right skew


# MODELLING -------
# NB one model per species

# 1. CANOPY HEIGHT -----

# S. Richardsonii ----
unique(all_CG_source_growth_garden_rich$Year)
hist(all_CG_source_growth_garden_rich$Canopy_Height_cm) # not gaussian, not poisson, need to log transform

# tried scaling but doesnt change anything, so not doing it
# all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale <- scale(all_CG_source_growth_garden_rich_height$Canopy_Height_cm, center = T)  # scaling time
# hist(all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale ) # still not gaussian?

# model
garden_rich_height <- brms::brm(log(Canopy_Height_cm) ~ population + (1|Sample_age) + (1|Year),
                                data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000)

summary(garden_rich_height)
plot(garden_rich_height)
pp_check(garden_rich_height) 

# S. Pulchra -----
# S. Arctica -----

# 2. STEM ELONGATION ------

# S. Richardsonii -----
unique(all_CG_source_growth_garden_rich$population)
range(all_CG_source_growth_garden_rich$mean_stem_elong)

hist(all_CG_source_growth_garden_rich$mean_stem_elong) # not gaussian, not poisson?
# scaling 
# all_CG_source_growth_garden_rich$mean_stem_elong_scale <- scale(all_CG_source_growth_garden_rich$mean_stem_elong, center = T)  # scaling time
# hist(all_CG_source_growth_garden_rich$mean_stem_elong_scale) # still not gaussian?
# all_CG_source_growth_garden_rich$population<-as.factor(all_CG_source_growth_garden_rich$population)

# model
garden_rich_elong <- brms::brm(log(mean_stem_elong) ~ population + (1|Sample_age) + (1|Year),
                               data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)


summary(garden_rich_elong)
plot(garden_rich_elong)
pp_check(garden_rich_elong) 

# S. Pulchra -----

# S. Arctica -----

# 3. BIOVOLUME ------
# S. Richardsonii -----
# model
garden_rich_biovol <- brms::brm(log(biovolume) ~ population + (1|Sample_age) + (1|Year),
                               data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol) # fine
pp_check(garden_rich_biovol,  type = "dens_overlay", nsamples = 100) # fine

# S. Pulchra -----
# S. Arctica -----

# 4. WIDTH ------
# omitting year random effect because only 2 years
# S. Richardsonii -----
garden_rich_width <- brms::brm(log(mean_width) ~ population + (1|Sample_age),
                                data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_width) # significantly larger widths for southern shrubs in garden
plot(garden_rich_width) # fine
pp_check(garden_rich_width,  type = "dens_overlay", nsamples = 100) # fine

# S. Pulchra -----
# S. Arctica -----

# 4. STEM DIAMETER ------
# omitting year random effect because only 2 years
# S. Richardsonii -----
# model
garden_rich_diam <- brms::brm(log(Stem_diameter) ~ population + (1|Sample_age),
                               data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_diam) # significantly larger stem diameters for southern shrubs in garden
plot(garden_rich_diam) # fine
pp_check(garden_rich_diam,  type = "dens_overlay", nsamples = 100) # fine

# S. Pulchra -----
# S. Arctica -----

# PLOTTING -----
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=18, colour = "black"), 
                                 axis.title.y = element_text(face="bold", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# plot model output

# CANOPY HEIGHT richardsonii ----
rich_heights <- (conditional_effects(garden_rich_height)) # extracting conditional effects from bayesian model
rich_height_data <- rich_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_height_plot <-ggplot(rich_height_data) +
    geom_point(data = all_CG_source_growth_garden_rich, aes(x = population, y = log(Canopy_Height_cm), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__), colour = "red", size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__),
               alpha = 1) +
    ylab("Canopy height (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# STEM ELONG richardsonii ----
rich_elong <- (conditional_effects(garden_rich_elong)) # extracting conditional effects from bayesian model
rich_elong_data <- rich_elong[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_height_plot <-ggplot(rich_elong_data) +
    geom_point(data = all_CG_source_growth_garden_rich, aes(x = population, y = log(mean_stem_elong), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__), colour = "red", size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__),
                  alpha = 1) +
    ylab("Mean stem elongation (log, mm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())


# stem elong -----
(model_elong <- all_CG_source_growth_garden_rich %>%
   add_predicted_draws(garden_rich_elong) %>%  # adding the posterior distribution
   ggplot(aes(x = population, y = mean_stem_elong)) +  
   stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                   alpha = 0.5, colour = "black") +
   geom_point(data = all_CG_source_growth_garden_rich, colour = "darkseagreen4", size = 3) +   # raw data
   scale_fill_brewer(palette = "Greys") +
   ylab("Mean stem elongation (mm)\n") +  # latin name for red knot
   xlab("\nPopulation") +
   theme_bw() +
   theme(legend.title = element_blank(),
         legend.position = c(0.15, 0.85)))



