# BAYESIAN traits results models -----
# Script by Madi
# Last update: 03/04/2023

# libraries ----
library(plyr) # load before dplyr aka tidyverse 
library(tidyverse)
library(brms)
library(ggplot2)
library(tidybayes)
library(ggpubr)
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
all_CG_source_growth$year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

all_CG_source_traits$Species <- as.factor(all_CG_source_traits$Species)
all_CG_source_traits$plant_tag_id <- as.factor(all_CG_source_traits$plant_tag_id)
all_CG_source_traits$population <- as.factor(all_CG_source_traits$population)
all_CG_source_traits$date_sampled <- as.POSIXct(all_CG_source_traits$date_sampled, format = '%Y-%m-%d')
all_CG_source_traits$year <- as.factor(all_CG_source_traits$year)

# rename levels of variables for easier interpretation 
all_CG_source_traits$population <- plyr::revalue(all_CG_source_traits$population, 
                                                 c("Northern"="N. Garden",
                                                   "Southern"="S. Garden",
                                                   "source_south"="S. Source",
                                                   "source_north"="N. Source"))

all_CG_source_growth$population <- plyr::revalue(all_CG_source_growth$population, 
                                                 c("Northern"="N. Garden",
                                                   "Southern"="S. Garden",
                                                   "source_south"="S. Source",
                                                   "source_north"="N. Source")) 
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
arctica_all_traits$LDMC_log <- log(arctica_all_traits$LDMC_g_g)
hist(arctica_all_traits$LDMC_log) # very mild right skew
hist(pulchra_all_traits$LDMC_g_g) # right skew
pulchra_all_traits$LDMC_log <- log(pulchra_all_traits$LDMC_g_g)
hist(pulchra_all_traits$LDMC_log) # better 
hist(richardsonii_all_traits$LDMC_g_g)# decent
richardsonii_all_traits$LDMC_log <- log(richardsonii_all_traits$LDMC_g_g)
hist(richardsonii_all_traits$LDMC_log)# ideal 

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
# SLA ----
# S. richardsonii ----
rich_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                      iter = 3000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_SLA) 
plot(rich_SLA)
pp_check(rich_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
rich_SLA_results <- model_summ(rich_SLA)
rich_SLA_results$Species <- "Salix richardsonii"

# S. pulchra ----
pulchra_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_SLA) # There were 1 divergent transitions after warmup
plot(pulchra_SLA)
pp_check(pulchra_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
pulchra_SLA_results <- model_summ(pulchra_SLA)
pulchra_SLA_results$Species <- "Salix pulchra"

# S. arctica ----
arctica_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_SLA) 
plot(arctica_SLA)
pp_check(arctica_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
arctica_SLA_results <- model_summ(arctica_SLA)
arctica_SLA_results$Species <- "Salix arctica"

# merging all extracted outputs
garden_sla_out <- rbind(rich_SLA_results, pulchra_SLA_results, arctica_SLA_results)

# back transforming from log
garden_SLA_out_back <- garden_sla_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                 "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = exp(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = exp(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = exp(Estimate), 
         Est.Error_trans = exp(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_SLA_out_back, "output/traits/garden_SLA_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_SLA_out_back) <- c("Intercept", "Northern Source", "SouthernSource",  "Southern Garden", 
                                       "Year", "Sigma", 
                                       " Intercept", " Northern Source", " SouthernSource", " Southern Garden", " Year", 
                                       " Sigma", 
                                       "Intercept ", "Northern Source ", "SouthernSource ", "Southern Garden ", "Year ", 
                                       "Sigma ")

# making sure Rhat keeps the .00 
garden_SLA_out_back$Rhat <- as.character(formatC(garden_SLA_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_SLA <- garden_SLA_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: Specific leaf area of northern garden, northern source, southern garden, southern source willows. 
      Model structure per species: (log(SLA) ~ population + (1|year). 
      Model output back-transformed in the table below.", 
      col.names = c("Estimate",
                    "Est. Error",
                    "Lower 95% CI (log)",
                    "Upper 95% CI (log)", 
                    "Rhat", 
                    "Bulk Effective Sample Size",
                    "Tail Effective Sample Size", 
                    "Effect",
                    "Sample Size",
                    "Species",  
                    "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                    "Estimate transformed", 
                    "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_SLA, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_SLA, file = "output/traits/SLA_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# LDMC ----
# S. richardsonii ----
rich_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                           iter = 3000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99)) # 2 divergent transitions after warmup
summary(rich_LDMC_log)
tab_model(rich_LDMC_log)
plot(rich_LDMC_log)
pp_check(rich_LDMC_log, type = "dens_overlay", ndraws = 100) 
rich_LDMC_results <- model_summ(rich_LDMC_log)
rich_LDMC_results$Species <- "Salix richardsonii"

# S. pulchra ----
pulchra_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(pulchra_LDMC_log) 
tab_model(pulchra_LDMC_log)
plot(pulchra_LDMC_log)
pp_check(pulchra_LDMC_log, type = "dens_overlay", ndraws = 100) 
pulchra_LDMC_results <- model_summ(pulchra_LDMC_log)
pulchra_LDMC_results$Species <- "Salix pulchra"

# S. arctica ---- 
arctica_LDMC_log <- brms::brm(log(LDMC_g_g) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99)) #There were 5 divergent transitions after warmup.
summary(arctica_LDMC_log)
tab_model(arctica_LDMC_log)
plot(arctica_LDMC_log)
pp_check(arctica_LDMC_log, type = "dens_overlay", ndraws = 100) 
arctica_LDMC_results <- model_summ(arctica_LDMC_log)
arctica_LDMC_results$Species <- "Salix arctica"

# merging all extracted outputs
garden_ldmc_out <- rbind(rich_LDMC_results, pulchra_LDMC_results, arctica_LDMC_results)

# back transforming from log
garden_LDMC_out_back <- garden_ldmc_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = exp(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = exp(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = exp(Estimate), 
         Est.Error_trans = exp(Est.Error)) %>% 
  select(-CI_range)
# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_LDMC_out_back) <- c("Intercept", "Northern Source", "SouthernSource",  "Southern Garden", 
                                   "Year", "Sigma", 
                                   " Intercept", " Northern Source", " SouthernSource", " Southern Garden", " Year", 
                                   " Sigma", 
                                   "Intercept ", "Northern Source ", "SouthernSource ", "Southern Garden ", "Year ", 
                                   "Sigma ")

# making sure Rhat keeps the .00 
garden_LDMC_out_back$Rhat <- as.character(formatC(garden_LDMC_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(garden_LDMC_out_back, "output/traits/garden_LDMC_out_back.csv")

# creating table
kable_LDMC <- garden_LDMC_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: Leaf dry matter content of northern garden, northern source, southern garden, southern source willows. 
      Model structure per species: (log(SLA) ~ population + (1|year). 
      Model output back-transformed in the table below.", 
      col.names = c("Estimate",
                    "Est. Error",
                    "Lower 95% CI (log)",
                    "Upper 95% CI (log)", 
                    "Rhat", 
                    "Bulk Effective Sample Size",
                    "Tail Effective Sample Size", 
                    "Effect",
                    "Sample Size",
                    "Species",  
                    "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                    "Estimate transformed", 
                    "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_LDMC, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_LDMC, file = "output/traits/LDMC_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# LA ----
# S. richardsonii ----
rich_LA <- brms::brm(log(LA) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000, 
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LA) 
plot(rich_LA)
pp_check(rich_LA, type = "dens_overlay", ndraws = 100)
rich_LA_results <- model_summ(rich_LA)
rich_LA_results$Species <- "Salix richardsonii"
# S. pulchra ----
pulchra_LA <- brms::brm(log(LA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(pulchra_LA) 
plot(pulchra_LA)
pp_check(pulchra_LA, type = "dens_overlay", ndraws = 100) 
pulchra_LA_results <- model_summ(pulchra_LA)
pulchra_LA_results$Species <- "Salix pulchra"
# S. arctica ----
arctica_LA <- brms::brm(log(LA)  ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LA) 
plot(arctica_LA)
pp_check(arctica_LA, type = "dens_overlay", ndraws = 100) 
arctica_LA_results <- model_summ(arctica_LA)
arctica_LA_results$Species <- "Salix arctica"

# merging all extracted outputs
garden_LA_out <- rbind(rich_LA_results, pulchra_LA_results, arctica_LA_results)

# back transforming from log
garden_LA_out_back <- garden_LA_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = exp(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = exp(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = exp(Estimate), 
         Est.Error_trans = exp(Est.Error)) %>% 
  select(-CI_range)
# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_LA_out_back) <- c("Intercept", "Northern Source", "SouthernSource",  "Southern Garden", 
                                    "Year", "Sigma", 
                                    " Intercept", " Northern Source", " SouthernSource", " Southern Garden", " Year", 
                                    " Sigma", 
                                    "Intercept ", "Northern Source ", "SouthernSource ", "Southern Garden ", "Year ", 
                                    "Sigma ")

# making sure Rhat keeps the .00 
garden_LA_out_back$Rhat <- as.character(formatC(garden_LA_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(garden_LA_out_back, "output/garden_LA_out_back.csv")
# creating table
kable_LA <- garden_LA_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: Leaf area  of northern garden, northern source, southern garden, southern source willows. 
      Model structure per species: (log(SLA) ~ population + (1|year). 
      Model output back-transformed in the table below.", 
      col.names = c( "Estimate",
                    "Est. Error",
                    "Lower 95% CI (log)",
                    "Upper 95% CI (log)", 
                    "Rhat", 
                    "Bulk Effective Sample Size",
                    "Tail Effective Sample Size", 
                    "Effect",
                    "Sample Size",
                    "Species",  
                    "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                    "Estimate transformed", 
                    "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
column_spec(kable_LA, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_LA, file = "output/LA_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# LMA ----
# not including because inverse of SLA
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
# S. richardsonii ----
rich_LL <- brms::brm(mean_leaf_length ~ population + (1|year), data = richardsonii_all_growth, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000, 
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LL)
plot(rich_LL)
pp_check(rich_LL, type = "dens_overlay", ndraws = 100) 
rich_LL_results <- model_summ(rich_LL)
rich_LL_results$Species <- "Salix richardsonii"
# S. pulchra ----
pulchra_LL <- brms::brm(mean_leaf_length ~ population + (1|year), data = pulchra_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99)) # There were 1 divergent transitions after warmup
summary(pulchra_LL)
plot(pulchra_LL)
pp_check(pulchra_LL, type = "dens_overlay", ndraws = 100) 
pulchra_LL_results <- model_summ(pulchra_LL)
pulchra_LL_results$Species <- "Salix pulchra"

# S. arctica ----
# no leaf length for S. arctic from source pop
# make common garden only model 
arctica_cg_growth <- arctica_all_growth %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))

arctica_LL_CG <- brms::brm((mean_leaf_length) ~ population + (1|year), data = arctica_cg_growth, family = gaussian(), chains = 3,
                           iter = 5000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LL_CG)
plot(arctica_LL_CG)
pp_check(arctica_LL_CG, type = "dens_overlay", ndraws = 100)
arctica_LL_results <- model_summ(arctica_LL_CG)
arctica_LL_results$Species <- "Salix arctica"
# merging all extracted outputs
garden_LL_out <- rbind(rich_LL_results, pulchra_LL_results, arctica_LL_results)

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_LL_out) <- c("Intercept", "Northern Source", "SouthernSource",  "Southern Garden", 
                                    "Year", "Sigma", 
                                    " Intercept", " Northern Source", " SouthernSource", " Southern Garden", " Year", 
                                    " Sigma", 
                                    "Intercept ", "Southern Garden ", "Year ", 
                                    "Sigma ")

# making sure Rhat keeps the .00 
garden_LL_out$Rhat <- as.character(formatC(garden_LL_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(garden_LL_out, "output/traits/garden_LL_out_back.csv")
# creating table
kable_LL <- garden_LL_out %>% 
  kbl(caption="Table.xxx BRMS model outputs: Leaf length  of northern garden, northern source, southern garden, southern source willows. 
      Model structure per species: (log(SLA) ~ population + (1|year). Note S. arctica only comparing garden populations. 
      Model output back-transformed in the table below.", 
      col.names = c( "Estimate",
                     "Est. Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Effect",
                     "Sample Size",
                     "Species"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
column_spec(kable_LL, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_LL, file = "output/traits/LL_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# PLOTS ---- 
# note: always put richardsonii, pulchra then arctica 
# reordering levels to go northern source, northern garden, southern source, southern garden
pulchra_all_traits$population <- ordered(pulchra_all_traits$population, 
                                           levels = c("N. Source", 
                                                      "N. Garden", 
                                                      "S. Source",  
                                                      "S. Garden"))

richardsonii_all_traits$population <- ordered(richardsonii_all_traits$population, 
                                           levels = c("N. Source", 
                                                      "N. Garden",
                                                      "S. Source", 
                                                      "S. Garden"))

arctica_all_traits$population <- ordered(arctica_all_traits$population, 
                                           levels = c("N. Source", 
                                                      "N. Garden",
                                                      "S. Source", 
                                                      "S. Garden"))

richardsonii_all_growth$population <- ordered(richardsonii_all_growth$population, 
                                           levels = c("N. Source", 
                                                      "N. Garden",
                                                      "S. Source", 
                                                      "S. Garden"))

pulchra_all_growth$population <- ordered(pulchra_all_growth$population, 
                                              levels = c("N. Source", 
                                                         "N. Garden",
                                                         "S. Source", 
                                                         "S. Garden"))

arctica_cg_growth$population <- ordered(arctica_cg_growth$population, 
                                              levels = c("N. Garden",
                                                         "S. Garden"))

pal  <- c("#2A788EFF", "#440154FF", "#FDE725FF","#7AD151FF")

theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=16),
                                 axis.text.x  = element_text(vjust=0.5, size=16, colour = "black", angle = 60), 
                                 axis.title.y = element_text(face="bold", size=16),
                                 axis.text.y  = element_text(vjust=0.5, size=16, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 16, face = "bold.italic", hjust = 0.5),
                                 legend.title=element_text(size=16),
                                 legend.text=element_text(size = 15))}
# SLA ---- 
# richardsonii ----
richard_sla <- (conditional_effects(rich_SLA)) # extracting conditional effects from bayesian model
richard_sla_data <- richard_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
richard_sla_data_trans <- richard_sla_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(rich_sla_plot <-ggplot(richard_sla_data_trans) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = SLA, colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("SLA (UNIT)\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix richardsonii") +
    theme_shrub())
# pulchra ----
pul_sla <- (conditional_effects(pulchra_SLA)) # extracting conditional effects from bayesian model
pul_sla_data <- pul_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
pul_sla_data_trans <- pul_sla_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(pul_sla_plot <-ggplot(pul_sla_data_trans) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = SLA, colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix pulchra") +
    theme_shrub())
# arctica ----
arc_sla <- (conditional_effects(arctica_SLA)) # extracting conditional effects from bayesian model
arc_sla_data <- arc_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
arc_sla_data_trans <- arc_sla_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(arc_sla_plot <-ggplot(arc_sla_data_trans) +
    geom_point(data = arctica_all_traits, aes(x = population, y = (SLA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(sla_panel <- ggarrange(rich_sla_plot, pul_sla_plot, arc_sla_plot, 
                       common.legend = TRUE, legend = "none",
                           ncol = 3, nrow = 1))
# raw data for reference 
(sla_plot <- ggplot(all_CG_source_traits) +
    geom_boxplot(aes(x= population, y = SLA, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("Specific leaf area ()") +
    xlab("") +
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
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 60, vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))


# LMDC ---- 
# richardsonii ----
richard_ldmc <- (conditional_effects(rich_LDMC_log)) # extracting conditional effects from bayesian model
richard_ldmc_data <- richard_ldmc[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
richard_ldmc_data_trans <- richard_ldmc_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(rich_ldmc_plot <-ggplot(richard_ldmc_data_trans) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = LDMC_g_g, colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("LDMC (UNIT)\n") +
    xlab("" ) +
    coord_cartesian(ylim=c(0, 0.9)) +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub())
# pulchra ----
pul_ldmc <- (conditional_effects(pulchra_LDMC_log)) # extracting conditional effects from bayesian model
pul_ldmc_data <- pul_ldmc[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
pul_ldmc_data_trans <- pul_ldmc_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(pul_ldmc_plot <-ggplot(pul_ldmc_data_trans) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = (LDMC_g_g), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" )     +
    coord_cartesian(ylim=c(0, 0.9)) +
  scale_color_manual(values=pal) +
    labs(title = "Salix pulchra") +
    theme_shrub())
# arctica ----
arc_ldmc <- (conditional_effects(arctica_LDMC_log)) # extracting conditional effects from bayesian model
arc_ldmc_data <- arc_ldmc[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
arc_ldmc_data_trans <- arc_ldmc_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(arc_ldmc_plot <-ggplot(arc_ldmc_data_trans) +
    geom_point(data = arctica_all_traits, aes(x = population, y = (LDMC_g_g), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(0, 0.9)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(ldmc_panel <- ggarrange(rich_ldmc_plot, pul_ldmc_plot, arc_ldmc_plot, 
                        common.legend = TRUE, legend = "bottom",
                        ncol = 3, nrow = 1))
# LA ----
# richardsonii ----
richard_la <- (conditional_effects(rich_LA)) # extracting conditional effects from bayesian model
richard_la_data <- richard_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
richard_la_data_trans <- richard_la_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)


(rich_la_plot <-ggplot(richard_la_data_trans) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = log(LA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("Leaf Area (UNIT)\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub())
# pulchra ----
pul_la <- (conditional_effects(pulchra_LA)) # extracting conditional effects from bayesian model
pul_la_data <- pul_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
pul_la_data_trans <- pul_la_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(pul_la_plot <-ggplot(pul_la_data_trans) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = log(LA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans,colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    labs(title = "Salix pulchra") +
    theme_shrub())
# arctica ----
arc_la <- (conditional_effects(arctica_LA)) # extracting conditional effects from bayesian model
arc_la_data <- arc_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
arc_la_data_trans <- arc_la_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(estimate__ - CI_range)) %>% 
  mutate(CI_high_trans = exp(estimate__ + CI_range)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

(arc_la_plot <-ggplot(arc_la_data_trans) +
    geom_point(data = arctica_all_traits, aes(x = population, y = log(LA), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    labs(title = "Salix arctica") +
    theme_shrub())

(la_panel <- ggarrange(rich_la_plot, pul_la_plot, arc_la_plot, 
                         common.legend = TRUE, legend = "bottom",
                         ncol = 3, nrow = 1))

# LEAF LENGTH -----

# richardsonii ----
richard_ll <- (conditional_effects(rich_LL)) # extracting conditional effects from bayesian model
richard_ll_data <- richard_ll[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population

(rich_ll_plot <-ggplot(richard_ll_data) +
    geom_point(data = richardsonii_all_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__, colour = population),
                  size = 1, alpha = 1) +
    ylab("Leaf Length (mm)\n") +
    xlab("") +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub())
# pulchra ----
pul_ll <- (conditional_effects(pulchra_LL)) # extracting conditional effects from bayesian model
pul_ll_data <- pul_ll[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population

(pul_ll_plot <-ggplot(pul_ll_data) +
    geom_point(data = pulchra_all_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    labs(title = "Salix pulchra") +
    theme_shrub())

garden_LL_out_pop <- tibble::rownames_to_column(garden_LL_out, "population") %>% 
  filter(population %in% c("Intercept", "Northern Source", "SouthernSource", "Southern Garden"))


# arctica ----
arc_ll <- (conditional_effects(arctica_LL_CG)) # extracting conditional effects from bayesian model
arc_ll_data <- arc_ll[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population

pal_garden <-c("#440154FF", "#7AD151FF")

(arc_ll_plot <-ggplot(arc_ll_data) +
    geom_point(data = arctica_cg_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(title = "Salix arctica") +
    theme_shrub())


(ll_panel <- ggarrange(rich_ll_plot, pul_ll_plot, arc_ll_plot, 
                       common.legend = TRUE, legend = "bottom",
                       ncol = 3, nrow = 1))


(trait_panel <- ggarrange(rich_sla_plot, pul_sla_plot, arc_sla_plot,
  rich_ldmc_plot, pul_ldmc_plot, arc_ldmc_plot,
  rich_la_plot, pul_la_plot, arc_la_plot,
  rich_ll_plot, pul_ll_plot, arc_ll_plot, 
                          common.legend = TRUE, legend = "bottom", 
  ncol = 6, nrow = 2))
# SLA LDMC panel 
(sla_ldmc_panel <- ggarrange(sla_panel, ldmc_panel, 
                          common.legend = TRUE, legend = "bottom", 
                          ncol = 1, nrow = 2))
# save 
ggsave("figures/sla_ldmc_panel.png", height = 10, width = 12, dpi = 300)
