# BAYESIAN maternal effects / propagation effects ------
# script by Erica and Madi
# last update: 28/02/2023

# 1. Loading libraries ----
library(brms)
library(tidyverse)

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

# explore 
hist(mother_cg_arctica$Mother_Canopy_Height_cm) # right skew
mother_cg_arctica$Mother_Canopy_Height_cm_log <- log(mother_cg_arctica$Mother_Canopy_Height_cm)
hist(mother_cg_arctica$Mother_Canopy_Height_cm_log) # a little better 

hist(mother_cg_pulchra$Mother_Canopy_Height_cm) # right skew
mother_cg_pulchra$Mother_Canopy_Height_cm_log <- log(mother_cg_pulchra$Mother_Canopy_Height_cm)
hist(mother_cg_pulchra$Mother_Canopy_Height_cm_log) # better

hist(mother_cg_rich$Mother_Canopy_Height_cm) # right skew mild 
mother_cg_rich$Mother_Canopy_Height_cm_log <- log(mother_cg_rich$Mother_Canopy_Height_cm)
hist(mother_cg_rich$Mother_Canopy_Height_cm_log)

# 4. MODELLING ------

#4.1. MATERNAL EFFECTS 

# HEIGHT -------

# Salix richardsonii ------
# interactive site model without year 
maternal_rich_height_site <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm)* Site,
                                  data = mother_cg_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_height_site) # not significant
plot(maternal_rich_height_site)
pp_check(maternal_rich_height_site, type = "dens_overlay", nsamples = 100)  # good) 


# maternal_rich_height_site_year <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm)* Site +(1|SampleYear),
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
                                 data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
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
maternal_rich_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) + Site + (1|SampleYear),
                                 data = mother_cg_rich, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_biovol) # not significant
plot(maternal_rich_biovol)
pp_check(maternal_rich_biovol, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
maternal_pul_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) + Site + (1|SampleYear),
                                  data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_biovol) # not significant
plot(maternal_pul_biovol)
pp_check(maternal_pul_biovol, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
maternal_arc_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) + Site + (1|SampleYear),
                                 data = mother_cg_arctica, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_arc_biovol) # not significant
plot(maternal_arc_biovol)
pp_check(maternal_arc_biovol, type = "dens_overlay", nsamples = 100)  # good) 

# 4.2. PROPAGATION EFFECTS ------

# Height vs Cutting length ------
# Salix richardsonii ------
prop_rich <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) + Site + (1|SampleYear),
                                  data = mother_cg_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_rich) # not significant
plot(prop_rich)
pp_check(prop_rich, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
prop_pul <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) + Site + (1|SampleYear),
                       data = mother_cg_pulchra, family = gaussian(), chains = 3,
                       iter = 3000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_pul) # not significant
plot(prop_pul)
pp_check(prop_pul, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------

prop_arc <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) + Site + (1|SampleYear),
                      data = mother_cg_arctica, family = gaussian(), chains = 3,
                      iter = 3000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_arc) # not significant
plot(prop_arc)
pp_check(prop_arc, type = "dens_overlay", nsamples = 100)  # good) 

# Biovolume vs Cutting length ------
# Salix richardsonii ------
prop_biovol_rich <- brms::brm(log(max_biovol) ~ log(Cutting_length) + Site + (1|SampleYear),
                       data = mother_cg_rich, family = gaussian(), chains = 3,
                       iter = 3000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_rich) # not significant
plot(prop_biovol_rich)
pp_check(prop_biovol_rich, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
prop_biovol_pul <- brms::brm(log(max_biovol) ~ log(Cutting_length) + Site + (1|SampleYear),
                              data = mother_cg_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_pul) # not significant
plot(prop_biovol_pul)
pp_check(prop_biovol_pul, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
prop_biovol_arc <- brms::brm(log(max_biovol) ~ log(Cutting_length) + Site + (1|SampleYear),
                             data = mother_cg_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_arc) # not significant
plot(prop_biovol_arc)
pp_check(prop_biovol_arc, type = "dens_overlay", nsamples = 100)  # good) 

# Cutting length vs mother canopy height  ------

# Salix richardsonii ------
prop_cutting_rich <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) + Site + (1|SampleYear),
                              data = mother_cg_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_rich) # YES. Taller mothers, longer cuttings 
plot(prop_cutting_rich)
pp_check(prop_cutting_rich, type = "dens_overlay", nsamples = 100)  # good) 

# Salix pulchra -------
prop_cutting_pul <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) + Site + (1|SampleYear),
                               data = mother_cg_pulchra, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_pul) # no
plot(prop_cutting_pul)
pp_check(prop_cutting_pul, type = "dens_overlay", nsamples = 100)  # good) 

# Salix arctica --------
prop_cutting_arc <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) + Site + (1|SampleYear),
                              data = mother_cg_arctica, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_arc) #no 
plot(prop_cutting_arc)
pp_check(prop_cutting_arc, type = "dens_overlay", nsamples = 100)  # good) 
