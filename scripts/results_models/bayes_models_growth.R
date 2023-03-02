# BAYESIAN growth results models -----
# Script by Erica
# Last update: 27/02/2023
# Code adapted from coding club tutorial by Louise Litrico:
# https://ourcodingclub.github.io/tutorials/brms/ 

# 3 available options for growth model structure: 
# 1) max height across all years (WE ARE DOING THIS ONE.)
# OR  2) using the last three years of August height data as a repeated measures test (without the shrubID nesting in the random effects), 
# which assumes that the shrubs aren’t really getting taller any more (seems true from the data, but that isn’t a priori), or 
# OR 3) only using the August 2022 data (aka the last time point).

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

# max stem elongation -----
max_elong_cg$Species <- as.factor(max_elong_cg$Species)
max_elong_cg$SampleID_standard <- as.factor(max_elong_cg$SampleID_standard)
max_elong_cg$population <- as.factor(max_elong_cg$population)
max_elong_cg$Site <- as.factor(max_elong_cg$Site)
max_elong_cg$Sample_Date <- as.POSIXct(max_elong_cg$Sample_Date, format = '%Y/%m/%d')
max_elong_cg$Year <- as.factor(max_elong_cg$Year)
max_elong_cg$Sample_age <- as.factor(max_elong_cg$Sample_age)

# ordering levels
max_elong_cg$population <- plyr::revalue(max_elong_cg$population , 
                                          c("Northern"="Northern Garden",
                                            "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_elong_cg_rich <- max_elong_cg %>%
  filter (Species == "Salix richardsonii")

max_elong_cg_pul <- max_elong_cg %>%
  filter (Species == "Salix pulchra") 

max_elong_cg_arc <- max_elong_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_elong_cg_rich$max_stem_elong) # right skew
hist(max_elong_cg_pul$max_stem_elong) #  right skew
hist(max_elong_cg_arc$max_stem_elong)#  right skew

# max biovolume -----
max_biovol_cg$Species <- as.factor(max_biovol_cg$Species)
max_biovol_cg$SampleID_standard <- as.factor(max_biovol_cg$SampleID_standard)
max_biovol_cg$population <- as.factor(max_biovol_cg$population)
max_biovol_cg$Site <- as.factor(max_biovol_cg$Site)
max_biovol_cg$Sample_Date <- as.POSIXct(max_biovol_cg$Sample_Date, format = '%Y/%m/%d')
max_biovol_cg$Year <- as.factor(max_biovol_cg$Year)
max_biovol_cg$Sample_age <- as.factor(max_biovol_cg$Sample_age)

# ordering levels
max_biovol_cg$population <- plyr::revalue(max_biovol_cg$population , 
                                         c("Northern"="Northern Garden",
                                           "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_biovol_cg_rich <- max_biovol_cg %>%
  filter (Species == "Salix richardsonii")

max_biovol_cg_pul <- max_biovol_cg %>%
  filter (Species == "Salix pulchra") 

max_biovol_cg_arc <- max_biovol_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_biovol_cg_rich$max_biovol) # right skew
hist(max_biovol_cg_pul$max_biovol, breaks = 30) #  right skew - so weird
hist(max_biovol_cg_arc$max_biovol)#  right skew

# max stem diameter -----
max_diam_cg$Species <- as.factor(max_diam_cg$Species)
max_diam_cg$SampleID_standard <- as.factor(max_diam_cg$SampleID_standard)
max_diam_cg$population <- as.factor(max_diam_cg$population)
max_diam_cg$Site <- as.factor(max_diam_cg$Site)
max_diam_cg$Sample_Date <- as.POSIXct(max_diam_cg$Sample_Date, format = '%Y/%m/%d')
max_diam_cg$Year <- as.factor(max_diam_cg$Year)
max_diam_cg$Sample_age <- as.factor(max_diam_cg$Sample_age)

# ordering levels
max_diam_cg$population <- plyr::revalue(max_diam_cg$population , 
                                          c("Northern"="Northern Garden",
                                            "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_diam_cg_rich <- max_diam_cg %>%
  filter (Species == "Salix richardsonii")

max_diam_cg_pul <- max_diam_cg %>%
  filter (Species == "Salix pulchra") 

max_diam_cg_arc <- max_diam_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_diam_cg_rich$max_stem_diam) # right skew
hist(max_diam_cg_pul$max_stem_diam, breaks = 30) #  right skew - so weird
hist(max_diam_cg_arc$max_stem_diam,  breaks = 30)#  right skew

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
# model
garden_rich_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                               data = max_elong_cg_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_elong) # southern pop significantly longer stem elong
plot(garden_rich_elong) # fine
pp_check(garden_rich_elong, type = "dens_overlay", nsamples = 100) # goood

# S. Pulchra -----
garden_pul_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                               data = max_elong_cg_pul, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_elong)# southern pop significantly longer stem elong
plot(garden_pul_elong) # fine
pp_check(garden_pul_elong, type = "dens_overlay", nsamples = 100)  # fine

# S. Arctica -----
garden_arc_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                               data = max_elong_cg_arc, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_elong) # southern shrubs SIGNIFICANTLY SHORTER elong! 
plot(garden_arc_elong) # fine
pp_check(garden_arc_elong,  type = "dens_overlay", nsamples = 100) # fine

# 3. BIOVOLUME ------
# S. Richardsonii -----
# model
garden_rich_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                               data = max_biovol_cg_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol) # fine
pp_check(garden_rich_biovol,  type = "dens_overlay", nsamples = 100) # fine

# S. Pulchra -----
garden_pul_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                                data = max_biovol_cg_pul, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_pul_biovol) # fine
pp_check(garden_pul_biovol,  type = "dens_overlay", nsamples = 100) # fine

# S. Arctica -----
garden_arc_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                                data = max_biovol_cg_arc, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_biovol) # NOT significant diff. 
plot(garden_arc_biovol) # fine
pp_check(garden_arc_biovol,  type = "dens_overlay", nsamples = 100) # fine


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
garden_rich_diam <- brms::brm(log(max_stem_diam) ~ population + (1|Sample_age),
                               data = max_diam_cg_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_diam) # significantly larger stem diameters for southern shrubs in garden
plot(garden_rich_diam) # fine
pp_check(garden_rich_diam,  type = "dens_overlay", nsamples = 100) # fine

# S. Pulchra -----
garden_pul_diam <- brms::brm(log(max_stem_diam) ~ population + (1|Sample_age),
                              data = max_diam_cg_pul, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_diam) # significantly larger stem diameters for southern shrubs in garden
plot(garden_pul_diam) # fine
pp_check(garden_pul_diam,  type = "dens_overlay", nsamples = 100) # fine

# S. Arctica -----
garden_arc_diam <- brms::brm(log(max_stem_diam) ~ population + (1|Sample_age),
                              data = max_diam_cg_arc, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_diam) # no significant diff.
plot(garden_arc_diam) # fine
pp_check(garden_arc_diam,  type = "dens_overlay", nsamples = 100) # fine


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

# CANOPY HEIGHT arctica ----
arc_heights <- (conditional_effects(garden_arc_height)) # extracting conditional effects from bayesian model
arc_height_data <- arc_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_height_plot <-ggplot(arc_height_data) +
    geom_point(data = max_heights_cg_arc, aes(x = population, y = log(max_canopy_height_cm), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
               alpha = 1) +
    ylab("Max. canopy height (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# STEM ELONG richardsonii ----
rich_elong <- (conditional_effects(garden_rich_elong)) # extracting conditional effects from bayesian model
rich_elong_data <- rich_elong[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_height_plot <-ggplot(rich_elong_data) +
    geom_point(data = max_elong_cg_rich, aes(x = population, y = log(max_stem_elong), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__), colour = "red", size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__),
                  alpha = 1) +
    ylab("Max. mean stem elongation (log, mm)\n") +
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

# Biovolume richardsonii ----
rich_biovol <- (conditional_effects(garden_rich_biovol)) # extracting conditional effects from bayesian model
rich_biovol_data <- rich_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_biovol_plot <-ggplot(rich_biovol_data) +
    geom_point(data = max_biovol_cg_rich, aes(x = population, y = log(max_biovol), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__), colour = "red", size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__),
                  alpha = 1) +
    ylab("Max. biovol (log, mm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# can do for all figures....

# try with ggpredict () ------
# Model predictions - get number of species per degree from here
ggpred_height <- ggpredict(garden_arc_height, terms = "population")
colnames(ggpred_height) = c('population', 'fit', 'lwr', 'upr', 'dunno')

(arc_height_plot <-ggplot(ggpred_height) +
    geom_point(data = max_heights_cg_arc, aes(x = population, y = log(max_canopy_height_cm), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = population , y = log(fit),colour = population), size = 4)+
    geom_errorbar(aes(x = population, ymin = log(lwr), ymax = log(upr),colour = population),
                  alpha = 1) +
    ylab("Max. canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) # if i log everything it's exactly the same plot as with conditional effects! 

