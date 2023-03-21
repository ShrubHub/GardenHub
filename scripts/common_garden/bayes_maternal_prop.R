# BAYESIAN maternal effects / propagation effects ------
# script by Erica and Madi
# last update: 28/02/2023

# 1. Loading libraries ----
library(brms)
library(tidyverse)
# with random effect: 
model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Sample_age
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "Sample_age"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge it all together
}

# with NO random effects: 
model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  sigma$nobs <- obs
  
  modelTerms <- as.data.frame(bind_rows(fixed, sigma))  # merge it all together
}

# 2. Loading data ---- 
mother_cg <- read_csv("data/source_pops/mother_cg.csv")

# 3. Wrangle ------
# make species specific dfs
mother_cg_arctica <- mother_cg %>% 
  dplyr::filter(Species == "Salix arctica")

mother_cg_rich <- mother_cg %>% 
  dplyr::filter(Species == "Salix richardsonii")

mother_cg_pulchra <- mother_cg %>% 
  dplyr::filter(Species == "Salix pulchra")

# explore height distributions and compare with log transform of data 
hist(mother_cg_arctica$Mother_Canopy_Height_cm) # right skew
mother_cg_arctica$Mother_Canopy_Height_cm_log <- log(mother_cg_arctica$Mother_Canopy_Height_cm)
hist(mother_cg_arctica$Mother_Canopy_Height_cm_log) # a little better 

hist(mother_cg_pulchra$Mother_Canopy_Height_cm) # right skew
mother_cg_pulchra$Mother_Canopy_Height_cm_log <- log(mother_cg_pulchra$Mother_Canopy_Height_cm)
hist(mother_cg_pulchra$Mother_Canopy_Height_cm_log) # better

hist(mother_cg_rich$Mother_Canopy_Height_cm) # right skew mild 
mother_cg_rich$Mother_Canopy_Height_cm_log <- log(mother_cg_rich$Mother_Canopy_Height_cm)
hist(mother_cg_rich$Mother_Canopy_Height_cm_log)

# explore width distributions and compare with log transform
hist(mother_cg_arctica$Mother_mean_width) # right skew
hist(mother_cg_arctica$max_mean_width_cm) # slight right skew 
mother_cg_arctica$max_mean_width_cm_log <- log(mother_cg_arctica$max_mean_width_cm)
mother_cg_arctica$Mother_mean_width_log <- log(mother_cg_arctica$Mother_mean_width)
hist(mother_cg_arctica$Mother_mean_width_log) # a little better 
hist(mother_cg_arctica$max_mean_width_cm_log)

hist(mother_cg_pulchra$Mother_mean_width) # right skew
hist(mother_cg_pulchra$max_mean_width_cm) # right skew  
mother_cg_pulchra$max_mean_width_cm_log <- log(mother_cg_pulchra$max_mean_width_cm)
mother_cg_pulchra$Mother_mean_width_log <- log(mother_cg_pulchra$Mother_mean_width)
hist(mother_cg_pulchra$max_mean_width_cm_log) # better 
hist(mother_cg_pulchra$Mother_mean_width_log) # better

hist(mother_cg_rich$Mother_mean_width) # right skew mild 
hist(mother_cg_rich$max_mean_width_cm) # right skew  
mother_cg_rich$max_mean_width_cm_log <- log(mother_cg_rich$max_mean_width_cm)
mother_cg_rich$Mother_mean_width_log <- log(mother_cg_rich$Mother_mean_width)
hist(mother_cg_rich$max_mean_width_cm_log) # better not great 
hist(mother_cg_rich$Mother_mean_width_log) # not much better 


# 4. MODELLING ------

#4.1. MATERNAL EFFECTS 

# HEIGHT -------

# Salix richardsonii ------
# interactive site model without year 
maternal_rich_height_site <- brms::brm(max_canopy_height_cm ~ log(Mother_Canopy_Height_cm)* Site,
                                  data = mother_cg_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_height_site) # not significant
plot(maternal_rich_height_site)
pp_check(maternal_rich_height_site, type = "dens_overlay", nsamples = 100)  # good) 


#maternal_rich_height_site_year <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm)* Site +(1|SampleYear),
#                                       data = mother_cg_rich, family = gaussian(), chains = 3,
#                                       iter = 3000, warmup = 1000, 
#                                       control = list(max_treedepth = 15, adapt_delta = 0.99))
# summary(maternal_rich_height_site_year) # not significant
# plot(maternal_rich_height_site_year)
# pp_check(maternal_rich_height_site_year, type = "dens_overlay", nsamples = 100)  # good)

# maternal_rich_height <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm) + Site + (1|SampleYear),
#                                data = mother_cg_rich, family = gaussian(), chains = 3,
#                                iter = 3000, warmup = 1000, 
#                                control = list(max_treedepth = 15, adapt_delta = 0.99))
#summary(maternal_rich_height) # not significant
#plot(maternal_rich_height)
#pp_check(maternal_rich_height, type = "dens_overlay", nsamples = 100)  # good) 


# Salix pulchra -------
maternal_pul_height <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm)* Site,
                                  data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_height) # significant for QHI 
plot(maternal_pul_height)
pp_check(maternal_pul_height, type = "dens_overlay", nsamples = 100)  # good) 

#maternal_pul_height_old <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm) + Site,
                              #   data = mother_cg_pulchra, family = gaussian(), chains = 3,
                              ##   iter = 3000, warmup = 1000, 
                               #  control = list(max_treedepth = 15, adapt_delta = 0.99))
#summary(maternal_pul_height_old) # not significant
#plot(maternal_pul_height_old)
#pp_check(maternal_pul_height_old, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
maternal_arc_height <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm) * Site,
                                 data = mother_cg_arctica, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(maternal_arc_height) # significant effect of maternal heights on child heights
plot(maternal_arc_height)
pp_check(maternal_arc_height, type = "dens_overlay", nsamples = 100)  # good) 

# WIDTH ------
# Salix richardsonii ------
maternal_rich_width <- brms::brm(log(max_mean_width_cm) ~ log(Mother_mean_width) * Site,
                                  data = mother_cg_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_width) # not significant
plot(maternal_rich_width)
pp_check(maternal_rich_width, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
maternal_pul_width <- brms::brm(log(max_mean_width_cm) ~ log(Mother_mean_width) * Site,
                                 data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_width) # not significant but negative estimate 
plot(maternal_pul_width)
pp_check(maternal_pul_width, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
maternal_arc_width <- brms::brm(log(max_mean_width_cm) ~ log(Mother_mean_width) * Site,
                                data = mother_cg_arctica, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_arc_width) # not significant
plot(maternal_arc_width)
pp_check(maternal_arc_width, type = "dens_overlay", nsamples = 100)  # meh 


# BIOVOLUME -------
maternal_biovol_pulchra_mod <-  lm(mother_biovol_log ~ child_max_biovol_log*Site, data = mother_cg_edit_biovol_pulchra)

# Salix richardsonii ------
maternal_rich_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) * Site,
                                 data = mother_cg_rich, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_biovol) # not significant
plot(maternal_rich_biovol)
pp_check(maternal_rich_biovol, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
maternal_pul_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) * Site,
                                  data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_biovol) # not significant
plot(maternal_pul_biovol)
pp_check(maternal_pul_biovol, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
maternal_arc_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) * Site,
                                 data = mother_cg_arctica, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_arc_biovol) # not significant
plot(maternal_arc_biovol)
pp_check(maternal_arc_biovol, type = "dens_overlay", nsamples = 100)  # decent? 

# 4.2. PROPAGATION EFFECTS ------

# Height vs Cutting length ------
# Salix richardsonii ------
prop_rich <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) * Site,
                                  data = mother_cg_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_rich) # not significant
plot(prop_rich)
pp_check(prop_rich, type = "dens_overlay", ndraws = 100)  # good 

# Salix pulchra -------
prop_pul <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) * Site,
                       data = mother_cg_pulchra, family = gaussian(), chains = 3,
                       iter = 3000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_pul) # not significant
plot(prop_pul)
pp_check(prop_pul, type = "dens_overlay", ndraws = 100)  # good 

# Salix arctica --------
prop_arc <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) * Site,
                      data = mother_cg_arctica, family = gaussian(), chains = 3,
                      iter = 3000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_arc) # not significant
plot(prop_arc)
pp_check(prop_arc, type = "dens_overlay", ndraws = 100)  # okay? kind of wonky 

# Biovolume vs Cutting length ------
# Salix richardsonii ------
prop_biovol_rich <- brms::brm(log(max_biovol) ~ log(Cutting_length) * Site,
                       data = mother_cg_rich, family = gaussian(), chains = 3,
                       iter = 3000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_rich) # not significant
plot(prop_biovol_rich)
pp_check(prop_biovol_rich, type = "dens_overlay", ndraws = 100)  # kind of bimodal? 

# Salix pulchra -------
prop_biovol_pul <- brms::brm(log(max_biovol) ~ log(Cutting_length) * Site,
                              data = mother_cg_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_pul) # not significant
plot(prop_biovol_pul)
pp_check(prop_biovol_pul, type = "dens_overlay", ndraws = 100)  # good

# Salix arctica --------
prop_biovol_arc <- brms::brm(log(max_biovol) ~ log(Cutting_length) * Site,
                             data = mother_cg_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_arc) # not significant
plot(prop_biovol_arc)
pp_check(prop_biovol_arc, type = "dens_overlay", ndraws = 100)  # good) 

# Cutting length vs mother canopy height  ------

# Salix richardsonii ------
prop_cutting_rich <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) * Site,
                              data = mother_cg_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_rich) # YES. Taller mothers, longer cuttings 
plot(prop_cutting_rich)
pp_check(prop_cutting_rich, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
prop_cutting_pul <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) * Site,
                               data = mother_cg_pulchra, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_pul) # no
plot(prop_cutting_pul)
pp_check(prop_cutting_pul, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
prop_cutting_arc <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) * Site,
                              data = mother_cg_arctica, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_arc) #no 
plot(prop_cutting_arc)
pp_check(prop_cutting_arc, type = "dens_overlay", nsamples = 100)  # good)

# 5. DATA VISUALISATION --------

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

#Salix richardsonii child height vs mother height ----
rich_height_maternal <- (conditional_effects(maternal_rich_height_site)) # extracting conditional effects from bayesian model
rich_height_maternal_data <- rich_height_maternal[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_height_mat_plot <-ggplot(rich_height_maternal_data) +
    geom_point(data = mother_cg_rich, aes(x = log(Mother_Canopy_Height_cm), y = max_canopy_height_cm),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = effect1__, y = estimate__))+
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                  alpha = 0.2) +
    ylab("Child canopy height (log, cm) \n") +
    xlab("\nMother canopy height (log, cm) " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

#Salix pulchra child height vs mother height ----
pul_height_maternal <- (conditional_effects(maternal_pul_height)) # extracting conditional effects from bayesian model
pul_height_maternal_data <- pul_height_maternal[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_height_mat_plot <-ggplot(pul_height_maternal_data) +
    geom_point(data = mother_cg_pulchra, aes(x = log(Mother_Canopy_Height_cm), y = log(max_canopy_height_cm)),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = effect1__, y = estimate__))+
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
                alpha = 0.2) +
    ylab("Child canopy height (log, cm) \n") +
    xlab("\nMother canopy height (log, cm) " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())


