# BAYESIAN phenology script ----
# BY Erica and Madi 
# Last update: 20/03/2023

# Libraries----
library(plyr)
library(tidyverse)
library(brms)
library(tidybayes)
library(ggplot2)
library(knitr) # For kable tables
library(kableExtra) # For kable tables

# Load data ----
all_phenocam_data_salix <- read_csv("data/phenology/all_phenocam_update.csv")
all_growing_season <- read_csv("data/phenology/all_growing_season_salix.csv")

# Wrangle data ------
# ordering levels so source and garden populations side by side
all_phenocam_data_salix$population <- plyr::revalue(all_phenocam_data_salix$population, 
                                                    c("QHI"="Northern Garden",
                                                      "Kluane"="Southern Garden",
                                                      "Southern_source"="Southern Source",
                                                      "Northern_source"="Northern Source"))

#all_phenocam_data_salix$population <- ordered(all_phenocam_data_salix$population, 
#                                              levels = c("Northern Source", 
#                                                         "Northern Garden", 
#                                                         "Southern Source",
#                                                         "Southern Garden"))

all_phenocam_data_salix$Year <- as.factor(all_phenocam_data_salix$Year)

all_growing_season$population <- plyr::revalue(all_growing_season$population, 
                                               c("Northern"="Northern Garden",
                                                 "Southern"="Southern Garden",
                                                 "KP"="Southern Source",
                                                 "QHI"="Northern Source"))

#all_growing_season$population <- ordered(all_growing_season$population, 
#                                         levels = c("Northern Source", 
#                                                    "Northern Garden", 
#                                                  "Southern Source",
#                                                    "Southern Garden"))
# SPECIES SPECIFIC datasets: CG + Sources -----
all_phenocam_rich <- all_phenocam_data_salix %>%
  filter(Species == "Salix richardsonii")

all_phenocam_pulchra <- all_phenocam_data_salix %>%
  filter(Species == "Salix pulchra")

all_phenocam_arctica <- all_phenocam_data_salix %>%
  filter(Species == "Salix arctica")

# SOURCE POP ONLY species specific datasets -----
all_phenocam_rich_source <- all_phenocam_rich %>%
  filter(population %in% c("Northern Source", "Southern Source"))
all_phenocam_rich_source$population <- as.character(all_phenocam_rich_source$population)
all_phenocam_rich_source$population <- as.factor(all_phenocam_rich_source$population)
unique(all_phenocam_rich_source$population)

all_phenocam_pul_source <- all_phenocam_pulchra %>%
  filter(population %in% c("Northern Source", "Southern Source"))
all_phenocam_pul_source$population <- as.character(all_phenocam_pul_source$population)
all_phenocam_pul_source$population <- as.factor(all_phenocam_pul_source$population)
unique(all_phenocam_pul_source$population)

all_phenocam_arc_source <- all_phenocam_arctica %>%
  filter(population %in% c("Northern Source", "Southern Source"))
all_phenocam_arc_source$population <- as.character(all_phenocam_arc_source$population)
all_phenocam_arc_source$population <- as.factor(all_phenocam_arc_source$population)
unique(all_phenocam_arc_source$population)

# CG only species specific data -----
all_phenocam_rich_garden <- all_phenocam_rich %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))
all_phenocam_rich_garden$population <- as.character(all_phenocam_rich_garden$population)
all_phenocam_rich_garden$population <- as.factor(all_phenocam_rich_garden$population)
unique(all_phenocam_rich_garden$population)

all_phenocam_pul_garden <- all_phenocam_pulchra %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))
all_phenocam_pul_garden$population <- as.character(all_phenocam_pul_garden$population)
all_phenocam_pul_garden$population <- as.factor(all_phenocam_pul_garden$population)
unique(all_phenocam_pul_garden$population)

all_phenocam_arc_garden <- all_phenocam_arctica %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))
all_phenocam_arc_garden$population <- as.character(all_phenocam_arc_garden$population)
all_phenocam_arc_garden$population <- as.factor(all_phenocam_arc_garden$population)
unique(all_phenocam_arc_garden$population)

# Sp specific growing season data
all_growing_season_rich <- all_growing_season %>%
  filter(Species == "Salix richardsonii")

all_growing_season_pul <- all_growing_season %>%
  filter(Species == "Salix pulchra")

all_growing_season_arc <- all_growing_season %>%
  filter(Species == "Salix arctica")

# exploring variables ------
hist(all_phenocam_pul_source$First_bud_burst_DOY, breaks=30) # defo not normal
hist(all_phenocam_rich_source$First_bud_burst_DOY, breaks=30) # defo not normal
hist(all_phenocam_arc_source$First_bud_burst_DOY, breaks=30) # defo not normal

hist(all_phenocam_pul_source$First_leaf_yellow_DOY, breaks=30) # defo not normal
hist(all_phenocam_rich_source$First_leaf_yellow_DOY, breaks=30) # defo not normal
hist(all_phenocam_arc_source$First_leaf_yellow_DOY, breaks=30) # defo not normal

# scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# 2. extract model result function =====
# with random effects:
model_summ_pheno <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Year
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "Year"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge it all together
}
#without random effects 
model_summ_pheno_no_rf <- function(x) {
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
# MODELLING ------
# 1. LEAF EMERGENCE (only source pops) -------

# Salix richardsonii -------
source_rich_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_emerg) # not significant diff. 
plot(source_rich_emerg)
pp_check(source_rich_emerg , type = "dens_overlay", nsamples = 100) # fine??

# extract output with function
source_rich_emerg_extract <- model_summ_pheno(source_rich_emerg)

# extraction for model output table
rownames(source_rich_emerg_extract) <- c("Intercept", "Southern Source", "Year", "Sigma")

source_rich_emerg_extract_df <- source_rich_emerg_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")

# Salix pulchra -------
source_pul_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_pul_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_emerg) # not significant
plot(source_pul_emerg)
pp_check(source_pul_emerg, type = "dens_overlay", nsamples = 100) # not too happy with that....

# extract output with function
source_pul_emerg_extract <- model_summ_pheno(source_pul_emerg)

# extraction for model output table
rownames(source_pul_emerg_extract) <- c("Intercept", "Southern Source", "Year", "Sigma")

source_pul_emerg_extract_df <- source_pul_emerg_extract %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
source_emerg <- rbind(source_rich_emerg_extract_df, source_pul_emerg_extract_df) 
                        
# save df of results 
write.csv(source_emerg, "output/source_emerg.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(source_emerg) <- c("Intercept", "Southern Source", "Year", 
                                       "Sigma", " Intercept", " Southern Source", " Year", "Sigma ")

# making sure Rhat keeps the .00 
source_emerg$Rhat <- as.character(formatC(source_emerg$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_source_emerg <- source_emerg %>% 
  kbl(caption="Table.xxx BRMS model outputs: first leaf emergence day of year of shrubs in northern (QHI) vs southern (Kluane) source populations. 
      Model structure per species: First_bud_burst_DOY ~ population + (1|Year). Missing Salix arctica", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_source_emerg, 2, width = NULL, bold = FALSE, italic = TRUE)

# Salix arctica -------
# MISSING KLUANE PLATEAU DATA so cannot run 

# 1.1. LEAF EMERGENCE (CG vs SOURCES)  ------
# Salix richardsonii -----
all_phenocam_rich$First_bud_burst_DOY_center <- center_scale(all_phenocam_rich$First_bud_burst_DOY) 

garden_rich_emerg_compare <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                               data = all_phenocam_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_emerg_compare)
plot(garden_rich_emerg_compare)
pp_check(garden_rich_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good

# Salix pulchra -----
all_phenocam_pulchra$First_bud_burst_DOY_center <- center_scale(all_phenocam_pulchra$First_bud_burst_DOY) 

garden_pul_emerg_compare <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                       data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000, 
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_emerg_compare)
plot(garden_pul_emerg_compare)
pp_check(garden_pul_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good

# Salix arctica -----
all_phenocam_arctica$First_bud_burst_DOY_center <- center_scale(all_phenocam_arctica$First_bud_burst_DOY) 

garden_arc_emerg_compare <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                      data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                      iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_emerg_compare)
plot(garden_arc_emerg_compare)
pp_check(garden_arc_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good


# 1.2. LEAF EMERGENCE (CG ONLY MODELS) ------
# Salix richardsonii -----
all_phenocam_rich_garden$First_bud_burst_DOY_center <- center_scale(all_phenocam_rich_garden$First_bud_burst_DOY) 

garden_rich_emerg <- brms::brm(First_bud_burst_DOY_center ~ population,
                                       data = all_phenocam_rich_garden, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_emerg) # no significant difference
plot(garden_rich_emerg)
pp_check(garden_rich_emerg, type = "dens_overlay", ndraws = 100) # looks not great... but limited data
garden_rich_emerg_results <- model_summ_pheno_no_rf(garden_rich_emerg)
garden_rich_emerg_results$species <- "Salix richardsonii"

# Salix pulchra -----
all_phenocam_pul_garden$First_bud_burst_DOY_center <- center_scale(all_phenocam_pul_garden$First_bud_burst_DOY) 

garden_pul_emerg <- brms::brm(First_bud_burst_DOY ~ population,
                               data = all_phenocam_pul_garden, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_emerg) # yes significant. (southern emergence later?)
plot(garden_pul_emerg)
pp_check(garden_pul_emerg,type = "dens_overlay", ndraws = 100) # looks ok
garden_pul_emerg_results <- model_summ_pheno_no_rf(garden_pul_emerg)
garden_pul_emerg_results$species <- "Salix pulchra"

# Salix arctica -----
all_phenocam_arc_garden$First_bud_burst_DOY_center <- center_scale(all_phenocam_arc_garden$First_bud_burst_DOY) 

garden_arc_emerg <- brms::brm(First_bud_burst_DOY_center ~ population,
                              data = all_phenocam_arc_garden, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_emerg) # YEs significant 
plot(garden_arc_emerg)
pp_check(garden_arc_emerg,type = "dens_overlay", ndraws = 100) # looks ok
garden_arc_emerg_results <- model_summ_pheno_no_rf(garden_arc_emerg)
garden_arc_emerg_results$species <- "Salix arctica"

# merging all extracted outputs
garden_emerg_out <- rbind(garden_rich_emerg_results, garden_pul_emerg_results, garden_arc_emerg_results)

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_emerg_out) <- c("Intercept", "Southern Garden", "Sigma", 
                                    " Intercept", " Southern Garden"," Sigma", 
                                    "Intercept ","Southern Garden ","Sigma ")

# making sure Rhat keeps the .00 
garden_emerg_out$Rhat <- as.character(formatC(garden_emerg_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(garden_emerg_out, "output/phenology/garden_leaf_emergence_out.csv")
# creating table
kable_emerg_garden <- garden_emerg_out %>% 
  kbl(caption="Table.xxx BRMS model outputs: Day of year (DOY) of northern garden vs southern garden willows. 
      Model structure per species: DOY leaf emergence ~ population. Data scaled to center on 0.", 
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

column_spec(kable_emerg_garden, 2, width = NULL, bold = FALSE, italic = TRUE)
save_kable(kable_emerg_garden, file = "output/phenology/emerg_garden_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# 2. LEAF YELLOWING (only source pops) -----
# Salix richardsonii -------
all_phenocam_rich_source$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_rich_source$First_leaf_yellow_DOY) 

source_rich_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_yellow) # no significant diff
plot(source_rich_yellow)
pp_check(source_rich_yellow, type = "dens_overlay", ndraws = 100) # looks ok

source_rich_yellow_extract <- model_summ_pheno(source_rich_yellow)

# Salix pulchra -------
all_phenocam_pul_source$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pul_source$First_leaf_yellow_DOY) 

source_pul_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                data = all_phenocam_pul_source, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_yellow) # no significant diff
plot(source_pul_yellow)
pp_check(source_pul_yellow, type = "dens_overlay", ndraws = 100) # looks ok

source_pul_yellow_extract <- model_summ_pheno(source_pul_yellow)

# Salix arctica -------
# Missing KP data so cannot run

# 2.1. LEAF YELLOWING (source vs garden) -------
# Salix richardsonii ------
all_phenocam_rich$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_rich$First_leaf_yellow_DOY) 

garden_rich_yellow_compare <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                        data = all_phenocam_rich, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_rich_yellow_compare)
plot(garden_rich_yellow_compare)
pp_check(garden_rich_yellow_compare, type = "dens_overlay", ndraws = 100) # looks good

garden_rich_yellow_compare_extract <- model_summ_pheno(garden_rich_yellow_compare)

# Salix pulchra ------
all_phenocam_pulchra$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pulchra$First_leaf_yellow_DOY) 

garden_pul_yellow_compare <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                        data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_yellow_compare)
plot(garden_pul_yellow_compare)
pp_check(garden_pul_yellow_compare, type = "dens_overlay", ndraws = 100) # looks good

garden_pul_yellow_compare_extract <- model_summ_pheno(garden_pul_yellow_compare)

# Salix arctica ------
all_phenocam_arctica$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_arctica$First_leaf_yellow_DOY) 

garden_arc_yellow_compare <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                       data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_yellow_compare)
plot(garden_arc_yellow_compare)
pp_check(garden_arc_yellow_compare, type = "dens_overlay", ndraws = 100) # looks good

garden_arc_yellow_compare_extract <- model_summ_pheno(garden_arc_yellow_compare)

# 2.2.  LEAF YELLOWING (only CG) -----
# Salix richardsonii -------
all_phenocam_rich_garden$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_rich_garden$First_leaf_yellow_DOY) 

garden_rich_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population,
                                        data = all_phenocam_rich_garden, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_yellow) # significant (later for southern shrubs?)
plot(garden_rich_yellow) 
pp_check(garden_rich_yellow, type = "dens_overlay", nsamples = 100) # looks good

garden_rich_yellow_extract <- model_summ_pheno_no_rf(garden_rich_yellow)
garden_rich_yellow_extract$Species <- "Salix richardsonii"

# Salix pulchra -------
all_phenocam_pul_garden$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pul_garden$First_leaf_yellow_DOY) 

garden_pul_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population,
                                data = all_phenocam_pul_garden, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_yellow) # significant (later for southern shrubs?)
plot(garden_pul_yellow)
pp_check(garden_pul_yellow, type = "dens_overlay", ndraws = 100) # looks decent

garden_pul_yellow_extract <- model_summ_pheno_no_rf(garden_pul_yellow)
garden_pul_yellow_extract$Species <- "Salix pulchra"

# Salix arctica -------
all_phenocam_arc_garden$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_arc_garden$First_leaf_yellow_DOY) 

garden_arc_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population,
                               data = all_phenocam_arc_garden, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000,
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_yellow) # significant 
plot(garden_arc_yellow)
pp_check(garden_arc_yellow, type = "dens_overlay", ndraws = 100) # looks good

garden_arc_yellow_extract <- model_summ_pheno_no_rf(garden_arc_yellow)
garden_arc_yellow_extract$Species <- "Salix arctica"

# merging all extracted outputs
garden_yellow_out <- rbind(garden_rich_yellow_extract, garden_pul_yellow_extract, garden_arc_yellow_extract)

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_yellow_out) <- c("Intercept", "Southern Garden", "Sigma", 
                                " Intercept", " Southern Garden"," Sigma", 
                                "Intercept ","Southern Garden ","Sigma ")

# making sure Rhat keeps the .00 
garden_yellow_out$Rhat <- as.character(formatC(garden_yellow_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(garden_yellow_out, "output/phenology/garden_leaf_yellow_out.csv")

# creating table
kable_yellow_garden <- garden_yellow_out %>% 
  kbl(caption="Table.xxx BRMS model outputs: Day of year (DOY) of first leaf yellowing northern garden vs southern garden willows. 
      Model structure per species: DOY leaf emergence ~ population. Data scaled to center on 0.", 
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
column_spec(kable_yellow_garden, 2, width = NULL, bold = FALSE, italic = TRUE)
save_kable(kable_yellow_garden, file = "output/phenology/yellow_garden_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# 3. GROWING SEASON LENGTH -----
# Salix richardsonii ------
growing_season_rich <- brms::brm(growing_season.y ~ population + (1|Year), 
                                data = all_growing_season_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_rich) 
plot(growing_season_rich)
pp_check(growing_season_rich, type = "dens_overlay", ndraws = 100) # looks good
season_rich_results <- model_summ_pheno(growing_season_rich)
season_rich_results$Species <- "Salix richardsonii"

# Salix pulchra ------
growing_season_pul <- brms::brm(growing_season.y ~ population + (1|Year), 
                                 data = all_growing_season_pul, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000,
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_pul) # 
plot(growing_season_pul)
pp_check(growing_season_pul, type = "dens_overlay", ndraws = 100) # looks good
season_pul_results <- model_summ_pheno(growing_season_pul)
season_pul_results$Species <- "Salix pulchra"

# Salix arctica ------
growing_season_arc <- brms::brm(growing_season.y ~ population + (1|Year),
                                data = all_growing_season_arc, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_arc)
plot(growing_season_arc)
pp_check(growing_season_arc, type = "dens_overlay", ndraws = 100) # looks good
season_arc_results <- model_summ_pheno(growing_season_arc)
season_arc_results$Species <- "Salix arctica"

season_results <- rbind(season_rich_results, season_pul_results, season_arc_results)

# adding spaces before/after each name so they let me repeat them in the table
rownames(season_results) <- c("Intercept", "Northern Source", "Southern Garden",  "Southern Source", 
                             "Year", "Sigma", 
                             " Intercept", " Northern Source", " Southern Garden", " Southern Source", " Year", 
                             " Sigma", 
                             "Intercept ", "Northern Source ", "Southern Garden ", "Year ", 
                             "Sigma ")

# making sure Rhat keeps the .00 
season_results$Rhat <- as.character(formatC(season_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(season_results, "output/phenology/season_outputs.csv")



# PLOTS ====
# bud burst ----
# S. richardsonii
# S. pulchra 
# S. arctica 
# first yellow leaf ----
# S. richardsonii
# S. pulchra 
# S. arctica 


