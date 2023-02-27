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
# all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")
# Only using max growth variables values
max_widths_cg <- read_csv("data/common_garden_data_2022/max_widths_cg.csv")
max_heights_cg <- read_csv("data/common_garden_data_2022/max_heights_cg.csv")
max_biovol_cg <- read_csv("data/common_garden_data_2022/max_biovol_cg.csv")
max_elong_cg <- read_csv("data/common_garden_data_2022/max_elong_cg.csv")
max_diam_cg <- read_csv("data/common_garden_data_2022/max_diam_cg.csv")

# Wrangle ------
# max height  -----
max_heights_cg$Species <- as.factor(max_heights_cg$Species)
max_heights_cg$SampleID_standard <- as.factor(max_heights_cg$SampleID_standard)
max_heights_cg$population <- as.factor(max_heights_cg$population)
max_heights_cg$Site <- as.factor(max_heights_cg$Site)
max_heights_cg$Sample_Date <- as.POSIXct(max_heights_cg$Sample_Date, format = '%Y/%m/%d')
max_heights_cg$Year <- as.factor(max_heights_cg$Year)
max_heights_cg$Sample_age <- as.factor(max_heights_cg$Sample_age)

# ordering levels
max_heights_cg$population <- plyr::revalue(max_heights_cg$population , 
                                          c("Northern"="Northern Garden",
                                            "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_heights_cg_rich <- max_heights_cg %>%
  filter (Species == "Salix richardsonii")

max_heights_cg_pul <- max_heights_cg %>%
  filter (Species == "Salix pulchra") 

max_heights_cg_arc <- max_heights_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_heights_cg_rich$max_canopy_height_cm) # right skew
hist(max_heights_cg_pul$max_canopy_height_cm) #  right skew
hist(max_heights_cg_arc$max_canopy_height_cm)#  right skew/normal?

# max width -----
max_widths_cg$Species <- as.factor(max_widths_cg$Species)
max_widths_cg$SampleID_standard <- as.factor(max_widths_cg$SampleID_standard)
max_widths_cg$population <- as.factor(max_widths_cg$population)
max_widths_cg$Site <- as.factor(max_widths_cg$Site)
max_widths_cg$Sample_Date <- as.POSIXct(max_widths_cg$Sample_Date, format = '%Y/%m/%d')
max_widths_cg$Year <- as.factor(max_widths_cg$Year)
max_widths_cg$Sample_age <- as.factor(max_widths_cg$Sample_age)

# ordering levels
max_widths_cg$population <- plyr::revalue(max_widths_cg$population , 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_widths_cg_rich <- max_widths_cg %>%
  filter (Species == "Salix richardsonii")

max_widths_cg_pul <- max_widths_cg %>%
  filter (Species == "Salix pulchra") 

max_widths_cg_arc <- max_widths_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_widths_cg_rich$max_mean_width_cm) # right skew
hist(max_widths_cg_pul$max_mean_width_cm) #  right skew
hist(max_widths_cg_arc$max_mean_width_cm)#  right skew

# MODELLING -------
# NB one model per species

# 1. CANOPY HEIGHT -----

# S. Richardsonii ----
# all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale <- scale(all_CG_source_growth_garden_rich_height$Canopy_Height_cm, center = T)  # scaling time

# model
garden_rich_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data =max_heights_cg_rich, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_height) # significantly higher canopy heights for southern pop.
plot(garden_rich_height) # fine
pp_check(garden_rich_height,  type = "dens_overlay", nsamples = 100)  # good

# S. Pulchra -----
garden_pul_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data =max_heights_cg_pul, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_height) # significantly higher canopy heights for southern pop.
plot(garden_pul_height) # fine
pp_check(garden_pul_height, type = "dens_overlay", nsamples = 100)  # good) 

# S. Arctica -----
garden_arc_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data =max_heights_cg_arc, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_height)# NOT significant difference (again makes sense!)
plot(garden_arc_height) # fine
pp_check(garden_arc_height, type = "dens_overlay", nsamples = 100)# good

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
garden_rich_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
                                data = max_widths_cg_rich, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_width) # significantly larger widths for southern shrubs in garden
plot(garden_rich_width) # fine
pp_check(garden_rich_width,  type = "dens_overlay", nsamples = 100) # fine

# S. Pulchra -----
garden_pul_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
                               data = max_widths_cg_pul, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_width) # significantly larger widths for southern shrubs in garden
plot(garden_pul_width) # fine
pp_check(garden_pul_width,  type = "dens_overlay", nsamples = 100) # fine

# S. Arctica -----
garden_arc_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
                               data = max_widths_cg_arc, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_width) # NOT significantly larger widths for southern shrubs in garden (makes sense)
plot(garden_arc_width) # fine
pp_check(garden_arc_width,  type = "dens_overlay", nsamples = 100) # fine


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



