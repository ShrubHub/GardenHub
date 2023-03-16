# BAYESIAN traits results models -----
# Script by Madi
# Last update: 13/03/2023

# libraries ----
library(plyr) # load before dplyr aka tidyverse 
library(tidyverse)
library(brms)
library(ggplot2)
library(tidybayes)
library(gridExtra)
library(knitr) # For kable tables
library(kableExtra) # For kable tables

# DATA ----
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") # most traits
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") # leaf length

str(all_CG_source_traits)
str(all_CG_source_growth)

# reclass variables 
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

all_CG_source_traits$Species <- as.factor(all_CG_source_traits$Species)
all_CG_source_traits$plant_tag_id <- as.factor(all_CG_source_traits$plant_tag_id)
all_CG_source_traits$population <- as.factor(all_CG_source_traits$population)
all_CG_source_traits$date_sampled <- as.POSIXct(all_CG_source_traits$date_sampled, format = '%Y-%m-%d')
all_CG_source_traits$year <- as.factor(all_CG_source_traits$year)

# rename levels of variables for easier interpretation 
all_CG_source_traits$population <- plyr::revalue(all_CG_source_traits$population, 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden",
                                                   "source_south"="Southern Source",
                                                   "source_north"="Northern Source"))

all_CG_source_growth$population <- plyr::revalue(all_CG_source_growth$population, 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden",
                                                   "source_south"="Southern Source",
                                                   "source_north"="Northern Source"))

# to run separate models per species filter out species: 
arctica_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix arctica")
pulchra_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix pulchra")
richardsonii_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix richardsonii")
# to run separate models per species filter out species for leaf length: 
arctica_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix arctica")
pulchra_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix pulchra")
richardsonii_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix richardsonii")

# look at distributions ----
# SLA
hist(arctica_all_traits$SLA) # mild right skew
arctica_all_traits$SLA_log <- log(arctica_all_traits$SLA)
hist(pulchra_all_traits$SLA) # mild right skew
pulchra_all_traits$SLA_log <- log(pulchra_all_traits$SLA)
hist(pulchra_all_traits$SLA_log) # mildly better right skew
hist(richardsonii_all_traits$SLA)# mild right skew
richardsonii_all_traits$SLA_log <- log(richardsonii_all_traits$SLA)
hist(richardsonii_all_traits$SLA_log) # better 
# LDMC
hist(arctica_all_traits$LDMC_g_g) # very mild right skew
hist(pulchra_all_traits$LDMC_g_g) # mild right skew
hist(richardsonii_all_traits$LDMC_g_g)# decent
# LA - compare with log version 
hist(arctica_all_traits$LA) #  right skew
arctica_all_traits$LA_log <- log(arctica_all_traits$LA)
hist(arctica_all_traits$LA_log) #  better 
hist(pulchra_all_traits$LA) # right skew
pulchra_all_traits$LA_log <- log(pulchra_all_traits$LA)
hist(pulchra_all_traits$LA_log) #  better 
hist(richardsonii_all_traits$LA) # right skew 
richardsonii_all_traits$LA_log <- log(richardsonii_all_traits$LA)
hist(richardsonii_all_traits$LA_log) # better 
# LMA
hist(arctica_all_traits$leaf_mass_per_area_g_m2) # decent 
hist(pulchra_all_traits$leaf_mass_per_area_g_m2) # decent 
hist(richardsonii_all_traits$leaf_mass_per_area_g_m2) # mild bimodal  
richardsonii_all_traits$leaf_mass_per_area_g_m2_log <- log(richardsonii_all_traits$leaf_mass_per_area_g_m2)
hist(richardsonii_all_traits$leaf_mass_per_area_g_m2_log) # kind of bimodal  
# leaf length 
hist(arctica_all_growth$mean_leaf_length) # decent
hist(pulchra_all_growth$mean_leaf_length) # mild right skew
hist(richardsonii_all_growth$mean_leaf_length) # pretty decent 

# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# extract model result function =====

model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$year
  obs = sum$nobs

  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "year"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

# MODELS ----
# model structure : 
# mod <- brms::brm(trait ~ population + (1|Year), data = spp_traits, 
# family = gaussian(), chains = 3, iter = 3000 - 5000, warmup = 1000)

# SLA ----
# S. arctica ----
arctica_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
S <- summary(arctica_SLA) 
plot(arctica_SLA)
pp_check(arctica_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
arctica_SLA_results <- model_summ(arctica_SLA)

S$nobs

# S. pulchra ----
pulchra_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_SLA) # There were 1 divergent transitions after warmup
tab_model(pulchra_SLA)
plot(pulchra_SLA)
pp_check(pulchra_SLA, type = "dens_overlay", ndraws = 100) # pretty good 

# S. richardsonii ----
rich_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                      iter = 3000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_SLA) 
plot(rich_SLA)
pp_check(rich_SLA, type = "dens_overlay", ndraws = 100) # pretty good 

# LDMC ----
# S. arctica ----
arctica_LDMC <- brms::brm(LDMC_g_g ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(arctica_LDMC)
plot(arctica_LDMC)
pp_check(arctica_LDMC) 
# S. arctica log transformed 
arctica_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) #There were 5 divergent transitions after warmup.
summary(arctica_LDMC_log)
plot(arctica_LDMC_log)
pp_check(arctica_LDMC_log) 
arctica_LDMC_log_results <- fixef(arctica_LDMC_log, probs = c(0.05, 0.95))
arctica_LDMC_log_results_df <- as.data.frame(arctica_LA_results)

# S. pulchra ----
pulchra_LDMC <- brms::brm(LDMC_g_g ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99)) # 4 divergent transitions after warmup
summary(pulchra_LDMC) 
plot(pulchra_LDMC)
pp_check(pulchra_LDMC) 

pulchra_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                          iter = 5000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) # There were 6 divergent transitions after warmup
summary(pulchra_LDMC_log) 
plot(pulchra_LDMC_log)
pp_check(pulchra_LDMC_log) 

# S. richardsonii ----
rich_LDMC <- brms::brm(LDMC_g_g ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99)) # 11 divergent transitions after warmup
summary(rich_LDMC)
plot(rich_LDMC)
pp_check(rich_LDMC) 

rich_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99)) # 13 divergent transitions after warmup
summary(rich_LDMC_log)
plot(rich_LDMC_log)
pp_check(rich_LDMC_log) 

# LA ----
# S. arctica ----
arctica_LA <- brms::brm(log(LA)  ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LA) 
plot(arctica_LA)
pp_check(arctica_LA) 
arctica_LA_results <- fixef(arctica_LA, probs = c(0.05, 0.95))
arctica_LA_results_df <- as.data.frame(arctica_LA_results)

# S. pulchra ----
pulchra_LA <- brms::brm(log(LA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(pulchra_LA) 
plot(pulchra_LA)
pp_check(pulchra_LA) 

# S. richardsonii ----
rich_LA <- brms::brm(log(LA) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(rich_LA) 
plot(rich_LA)
pp_check(rich_LA)

# LMA ----
# S. arcitca ----
arctica_LMA <- brms::brm(leaf_mass_per_area_g_m2 ~ population + (1|year), data = arctica_2022_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(arctica_LMA) 
plot(arctica_LMA)
pp_check(arctica_LMA) 

# S. pulchra ----
pulchra_LMA <- brms::brm(leaf_mass_per_area_g_m2 ~ population + (1|year), data = pulchra_2022_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(pulchra_LMA) 
plot(pulchra_LMA)
pp_check(pulchra_LMA) 

# S. richardsonii ----
rich_LMA <- brms::brm(leaf_mass_per_area_g_m2 ~ population + (1|year), data = richardsonii_2022_traits, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000)
summary(rich_LMA) 
plot(rich_LMA)
pp_check(rich_LMA)

# LEAF LENGTH ----
# S. arctica ----
# no leaf length for S. arctic from source pop
# make common garden only model 
# removing year as random effect bc only two years worth of data 
arctica_cg_growth <- arctica_all_growth %>% 
  filter(population %in% c("Northern", "Southern"))

arctica_LL_CG <- brms::brm((mean_leaf_length) ~ population , data = arctica_cg_growth, family = gaussian(), chains = 3,
                            iter = 5000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LL_CG)
tab(arctica_LL_CG)
plot(arctica_LL_CG)
pp_check(arctica_LL_CG)

# S. pulchra ----
pulchra_LL <- brms::brm(mean_leaf_length ~ population + (1|Year), data = pulchra_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000) # There were 1 divergent transitions after warmup
summary(pulchra_LL)
plot(pulchra_LL)
pp_check(pulchra_LL) 
# S. richardsonii ----
rich_LL <- brms::brm(mean_leaf_length ~ population + (1|Year), data = richardsonii_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000)
summary(rich_LL)
plot(rich_LL)
pp_check(rich_LL) 

# PLOTS ---- 
all_CG_source_traits$population <- ordered(all_CG_source_traits$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden", 
                                                      "Southern Source",
                                                      "Southern Garden"))

all_CG_source_growth$population <- ordered(all_CG_source_growth$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden",
                                                      "Southern Source", 
                                                      "Southern Garden"))

all_CG_source_growth$Species <- ordered(all_CG_source_growth$Species, 
                                        levels = c("Salix richardsonii", 
                                                   "Salix pulchra",
                                                   "Salix arctica"))


theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=12),
                                 axis.text.x  = element_text(vjust=0.5, size=12, colour = "black", angle = 45), 
                                 axis.title.y = element_text(face="bold", size=12),
                                 axis.text.y  = element_text(vjust=0.5, size=12, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}
# SLA ---- 
# arctica ----
arc_sla <- (conditional_effects(arctica_SLA)) # extracting conditional effects from bayesian model
arc_sla_data <- arc_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population

(arc_sla_plot <-ggplot(arc_sla_data) +
    geom_point(data = arctica_all_traits, aes(x = population, y = log(SLA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1) +
    ylab("Salix arctica SLA (log, UNIT)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())
# pulchra ----
pul_sla <- (conditional_effects(pulchra_SLA)) # extracting conditional effects from bayesian model
pul_sla_data <- pul_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population

(pul_sla_plot <-ggplot(pul_sla_data) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = log(SLA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1) +
    ylab("Salix arctica SLA (log, UNIT)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())
# richardsonii ----
richard_sla <- (conditional_effects(rich_SLA)) # extracting conditional effects from bayesian model
richard_sla_data <- richard_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population

(rich_sla_plot <-ggplot(richard_sla_data) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = log(SLA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1) +
    ylab("Salix arctica SLA (log, UNIT)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

panel_sla_bayes <- grid.arrange(rich_sla_plot, pul_sla_plot, arc_sla_plot, nrow = 1)

(sla_panel <- ggarrange(rich_sla_plot, pul_sla_plot, arc_sla_plot, 
                           labels = c("A", "B", "C"), common.legend = TRUE, legend = "bottom",
                           ncol = 3, nrow = 1))

# LMDC ---- 
# LA ----
# LMA -----
# LEAF LENGTH -----

