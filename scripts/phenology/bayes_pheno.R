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
library(ggpubr)

# Load data ----
all_phenocam_data_salix <- read_csv("data/phenology/all_phenocam_update.csv")
all_growing_season <- read_csv("data/phenology/all_growing_season_salix.csv")

# Wrangle data ------
# ordering levels so source and garden populations side by side
all_phenocam_data_salix$population <- plyr::revalue(all_phenocam_data_salix$population, 
                                                    c("Northern Garden"="N. Garden",
                                                      "Southern Garden"="S. Garden",
                                                      "Southern Source"="S. Source",
                                                      "Northern Source"="N. Source"))
# species code from QHI pheno plots is different, change to Salix arctica 
all_phenocam_data_salix$Species <- plyr::revalue(all_phenocam_data_salix$Species, 
                                            c("SALARC"="Salix arctica"))

#all_phenocam_data_salix$population <- ordered(all_phenocam_data_salix$population, 
#                                              levels = c("Northern Source", 
#                                                         "Northern Garden", 
#                                                         "Southern Source",
#                                                         "Southern Garden"))

all_phenocam_data_salix$Year <- as.factor(all_phenocam_data_salix$Year)

all_growing_season$population <- plyr::revalue(all_growing_season$population, 
                                               c("Northern"="N. Garden",
                                                 "Southern"="S. Garden",
                                                 "KP"="S. Source",
                                                 "QHI"="S. Source"))

#all_growing_season$population <- ordered(all_growing_season$population, 
#                                         levels = c("Northern Source", 
#                                                    "Northern Garden", 
#                                                  "Southern Source",
#                                                    "Southern Garden"))

# calculate growing season length in all_phenocam_data_salix data sheet 

all_phenocam_data_salix <- all_phenocam_data_salix %>% 
  mutate(growing_season_length = (First_leaf_yellow_DOY-First_bud_burst_DOY))

# SPECIES SPECIFIC datasets: CG + Sources -----
all_phenocam_rich <- all_phenocam_data_salix %>%
  filter(Species == "Salix richardsonii")

all_phenocam_pulchra <- all_phenocam_data_salix %>%
  filter(Species == "Salix pulchra")

all_phenocam_arctica <- all_phenocam_data_salix %>%
  filter(Species == "Salix arctica")

# SOURCE POP ONLY species specific datasets -----
all_phenocam_rich_source <- all_phenocam_rich %>%
  filter(population %in% c("N. Source", "S. Source"))
all_phenocam_rich_source$population <- as.character(all_phenocam_rich_source$population)
all_phenocam_rich_source$population <- as.factor(all_phenocam_rich_source$population)
unique(all_phenocam_rich_source$population)

all_phenocam_pul_source <- all_phenocam_pulchra %>%
  filter(population %in% c("N. Source", "S. Source"))
all_phenocam_pul_source$population <- as.character(all_phenocam_pul_source$population)
all_phenocam_pul_source$population <- as.factor(all_phenocam_pul_source$population)
unique(all_phenocam_pul_source$population)

all_phenocam_arc_source <- all_phenocam_arctica %>%
  filter(population %in% c("N. Source", "S. Source"))
all_phenocam_arc_source$population <- as.character(all_phenocam_arc_source$population)
all_phenocam_arc_source$population <- as.factor(all_phenocam_arc_source$population)
unique(all_phenocam_arc_source$population)

# CG only species specific data -----
all_phenocam_rich_garden <- all_phenocam_rich %>% 
  filter(population %in% c("N. Garden", "S. Garden"))
all_phenocam_rich_garden$population <- as.character(all_phenocam_rich_garden$population)
all_phenocam_rich_garden$population <- as.factor(all_phenocam_rich_garden$population)
unique(all_phenocam_rich_garden$population)

all_phenocam_pul_garden <- all_phenocam_pulchra %>% 
  filter(population %in% c("N. Garden", "S. Garden"))
all_phenocam_pul_garden$population <- as.character(all_phenocam_pul_garden$population)
all_phenocam_pul_garden$population <- as.factor(all_phenocam_pul_garden$population)
unique(all_phenocam_pul_garden$population)

all_phenocam_arc_garden <- all_phenocam_arctica %>% 
  filter(population %in% c("N. Garden", "S. Garden"))
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
all_phenocam_rich_source$First_bud_burst_DOY_scaled <- center_scale(all_phenocam_rich_source$First_bud_burst_DOY)
source_rich_emerg_scaled <- brms::brm(First_bud_burst_DOY_scaled ~ population + (1|Year),
                               data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(source_rich_emerg_scaled)

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
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)", 
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
saveRDS(garden_rich_emerg_compare, file = "output/phenology/garden_rich_emerg_compare.rds")
garden_rich_emerg_compare<- readRDS(file = "output/phenology/garden_rich_emerg_compare.rds")

# Salix pulchra -----
all_phenocam_pulchra$First_bud_burst_DOY_center <- center_scale(all_phenocam_pulchra$First_bud_burst_DOY) 

garden_pul_emerg_compare <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                       data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000, 
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_emerg_compare)
plot(garden_pul_emerg_compare)
pp_check(garden_pul_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good
saveRDS(garden_pul_emerg_compare, file = "output/phenology/garden_pul_emerg_compare.rds")
garden_pul_emerg_compare<- readRDS(file = "output/phenology/garden_pul_emerg_compare.rds")

# Salix arctica -----
all_phenocam_arctica$First_bud_burst_DOY_center <- center_scale(all_phenocam_arctica$First_bud_burst_DOY) 

garden_arc_emerg_compare <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                      data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                      iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_emerg_compare)
tab_model(garden_arc_emerg_compare)
plot(garden_arc_emerg_compare)
pp_check(garden_arc_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good
saveRDS(garden_arc_emerg_compare, file = "output/phenology/garden_arc_emerg_compare.rds")
garden_arc_emerg_compare<- readRDS(file = "output/phenology/garden_arc_emerg_compare.rds")


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
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)", 
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
saveRDS(garden_rich_yellow_compare, file = "output/phenology/garden_rich_yellow_compare.rds")
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
saveRDS(garden_pul_yellow_compare, file = "output/models/garden_pul_yellow_compare.rds")

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
saveRDS(garden_arc_yellow_compare, file = "output/models/garden_arc_yellow_compare.rds")
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
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)", 
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
growing_season_rich <- brms::brm(growing_season_length ~ population + (1|Year), 
                                data = all_phenocam_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_rich) 
plot(growing_season_rich)
pp_check(growing_season_rich, type = "dens_overlay", ndraws = 100) # looks decent
season_rich_results <- model_summ_pheno(growing_season_rich)
season_rich_results$Species <- "Salix richardsonii"

# center on 0
all_phenocam_rich$growing_season_length_scale <- center_scale(all_phenocam_rich$growing_season_length)
growing_season_rich_scale <- brms::brm(growing_season_length_scale ~ population + (1|Year), 
                                 data = all_phenocam_rich, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000,
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(growing_season_rich_scale) 
plot(growing_season_rich_scale)
pp_check(growing_season_rich_scale, type = "dens_overlay", ndraws = 100) # looks decent

# Salix pulchra ------
growing_season_pul <- brms::brm(growing_season.y ~ population + (1|Year), 
                                 data = all_growing_season_pul, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000,
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_pul) # 
plot(growing_season_pul)
pp_check(growing_season_pul, type = "dens_overlay", ndraws = 100) # looks decent
season_pul_results <- model_summ_pheno(growing_season_pul)
season_pul_results$Species <- "Salix pulchra"

# center on 0
all_phenocam_pulchra$growing_season_length_scale <- center_scale(all_phenocam_pulchra$growing_season_length)
growing_season_pul_scaled <- brms::brm(growing_season_length_scale ~ population + (1|Year), 
                                data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(growing_season_pul_scaled) # 
plot(growing_season_pul_scaled)
pp_check(growing_season_pul_scaled, type = "dens_overlay", ndraws = 100) # looks decent

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

# center on 0
all_phenocam_arctica$growing_season_length_scale <- center_scale(all_phenocam_arctica$growing_season_length)
growing_season_arc_scaled <- brms::brm(growing_season_length_scale ~ population + (1|Year), 
                                       data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(growing_season_arc_scaled) # 
plot(growing_season_arc_scaled)
pp_check(growing_season_arc_scaled, type = "dens_overlay", ndraws = 100) # looks decent
# compile non-scaled results, should change to scaled though I think (Madi)
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
season_results <- read.csv("output/phenology/season_outputs.csv", row.names=1)

kable_season_garden <- season_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: Growing season length northern vs southern willows in common garden and source populations. 
      Model structure per species: Growing season length ~ population. Data scaled to center on 0.", 
      col.names = c( "Estimate",
                     "Est. Error",
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Effect",
                     "Sample Size",
                     "Species"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")
column_spec(kable_season_garden, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_season_garden, file = "output/phenology/season__length_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)


# PLOTS ====
pal  <- c("#2A788EFF", "#440154FF", "#FDE725FF","#7AD151FF") # for reall levels 
pal_garden <- c("#440154FF", "#7AD151FF") # for only garden 
pal_arc  <- c("#2A788EFF", "#440154FF", "#7AD151FF") # for when southern source is missing  

theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=20, colour = "black", angle = 45), 
                                 axis.title.y = element_text(face="bold", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=20, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 20, face = "bold.italic", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# reorder levels to be consistent
all_growing_season_rich$population <- ordered(all_growing_season_rich$population, 
                                         levels = c("N. Source", 
                                                    "N. Garden", 
                                                    "S. Source",  
                                                    "S. Garden"))
all_growing_season_pul$population <- ordered(all_growing_season_pul$population, 
                                              levels = c("N. Source", 
                                                         "N. Garden", 
                                                         "S. Source",  
                                                         "S. Garden"))
all_growing_season_arc$population <- ordered(all_growing_season_arc$population, 
                                              levels = c("N. Source", 
                                                         "N. Garden", 
                                                         "S. Source",  
                                                         "S. Garden"))

all_phenocam_rich$population <- ordered(all_phenocam_rich$population, 
                                        levels = c("N. Source", 
                                                   "N. Garden", 
                                                   "S. Source",  
                                                   "S. Garden"))
all_phenocam_pulchra$population <- ordered(all_phenocam_pulchra$population, 
                                        levels = c("N. Source", 
                                                   "N. Garden", 
                                                   "S. Source",  
                                                   "S. Garden"))
all_phenocam_arctica$population <- ordered(all_phenocam_arctica$population, 
                                        levels = c("N. Source", 
                                                   "N. Garden", 
                                                   "S. Source",  
                                                   "S. Garden"))

# LEAF EMERGENCE CG vs SOURCES ----
# S. richardsonii ------
ric_emerg <- (conditional_effects(garden_rich_emerg_compare)) # extracting conditional effects from bayesian model
ric_emerg_data <- ric_emerg[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population
(ric_emerg_plot <-ggplot(ric_emerg_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf emergence DOY (centered) \n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))
# back transform scaled data for figure 
m_rich_emerg <- mean(all_phenocam_rich$First_bud_burst_DOY, na.rm = T)

richard_emerg_trans <- ric_emerg_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_rich_emerg)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_rich_emerg)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_rich_emerg), 
                Est.Error_trans = (se__ + m_rich_emerg)) %>% 
  dplyr::select(-CI_range) 

(ric_emerg_plot_scaled <-ggplot(richard_emerg_trans) +
    geom_point(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf emergence DOY \n") +
    xlab("\n" ) +
    coord_cartesian(ylim=c(100, 185)) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pulchra ------
pul_emerg <- (conditional_effects(garden_pul_emerg_compare)) # extracting conditional effects from bayesian model
pul_emerg_data <- pul_emerg[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_emerg_plot <-ggplot(pul_emerg_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_phenocam_pulchra, aes(x = population, y = First_bud_burst_DOY_center, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf emergence DOY (centered) \n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    labs(title = "Salix pulchra"))
# back transform scaled data for figure 
m_pul_emerg <- mean(all_phenocam_pulchra$First_bud_burst_DOY, na.rm = T)
pulchra_emerg_trans <- pul_emerg_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_pul_emerg)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_pul_emerg)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_pul_emerg), 
                Est.Error_trans = (se__ + m_pul_emerg)) %>% 
  dplyr::select(-CI_range) 

(pul_emerg_plot_scaled <-ggplot(pulchra_emerg_trans) +
    geom_point(data = all_phenocam_pulchra, aes(x = population, y = First_bud_burst_DOY, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf emergence DOY \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    coord_cartesian(ylim=c(100, 185)) +
    labs(title = "Salix pulchra"))

# S. arctica -------
arc_emerg <- (conditional_effects(garden_arc_emerg_compare)) # extracting conditional effects from bayesian model
arc_emerg_data <- arc_emerg[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_emerg_plot <-ggplot(arc_emerg_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_phenocam_arctica, aes(x = population, y = First_bud_burst_DOY_center, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf emergence DOY (centered) \n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_arc) +
    theme_shrub() +
    labs(title = "Salix arctica"))
# back transform scaled data for figure 
m_arc_emerg <- mean(all_phenocam_arctica$First_bud_burst_DOY, na.rm = T)
arc_emerg_trans <- arc_emerg_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_arc_emerg)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_arc_emerg)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_arc_emerg), 
                Est.Error_trans = (se__ + m_arc_emerg)) %>% 
  dplyr::select(-CI_range) 

(arc_emerg_plot_scaled <-ggplot(arc_emerg_trans) +
    geom_point(data = all_phenocam_arctica, aes(x = population, y = First_bud_burst_DOY, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf emergence DOY \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal_arc) +
    theme_shrub() +
    coord_cartesian(ylim=c(100, 185)) +
    labs(title = "Salix arctica"))

# arrange 
(leaf_emerg_panel <- ggarrange(ric_emerg_plot, pul_emerg_plot, arc_emerg_plot, 
                             common.legend = TRUE, legend = "bottom",
                             ncol = 3, nrow = 1))
ggsave("figures/phenology/green_up_panel.png", height = 10, width = 12, dpi = 300)

# arrange unscaled data 
(leaf_emerg_panel_unscale <- ggarrange(ric_emerg_plot_scaled, pul_emerg_plot_scaled, arc_emerg_plot_scaled, 
                               common.legend = TRUE, legend = "bottom",
                               ncol = 3, nrow = 1))
ggsave("figures/phenology/green_up_unscale_panel.png", height = 5, width = 12, dpi = 300)

# LEAF YELLOW ----
# S. richardsonii-----
ric_yellow <- (conditional_effects(garden_rich_yellow_compare)) # extracting conditional effects from bayesian model
ric_yellow_data <- ric_yellow[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_yellow_plot <-ggplot(ric_yellow_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_phenocam_rich, aes(x = population, y = First_leaf_yellow_DOY_center, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf yellowing DOY (centered) \n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))
# back transform scaled data for figure 
m_rich_yellow <- mean(all_phenocam_rich$First_leaf_yellow_DOY, na.rm = T)
richard_yellow_trans <- ric_yellow_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_rich_yellow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_rich_yellow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_rich_yellow), 
                Est.Error_trans = (se__ + m_rich_yellow)) %>% 
  dplyr::select(-CI_range) 

(ric_yellow_plot_scaled <-ggplot(richard_yellow_trans) +
    geom_point(data = all_phenocam_rich, aes(x = population, y = First_leaf_yellow_DOY, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf yellowing DOY \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(170, 250)) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pulchra -----
pul_yellow <- (conditional_effects(garden_pul_yellow_compare)) # extracting conditional effects from bayesian model
pul_yellow_data <- pul_yellow[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_yellow_plot <-ggplot(pul_yellow_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_phenocam_pulchra, aes(x = population, y = First_leaf_yellow_DOY_center, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf yellowing DOY (centered) \n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))
# back transform scaled data for figure 
m_pul_yellow <- mean(all_phenocam_pulchra$First_leaf_yellow_DOY, na.rm = T)
pulchra_yellow_trans <- pul_yellow_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_pul_yellow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_pul_yellow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_pul_yellow), 
                Est.Error_trans = (se__ + m_pul_yellow)) %>% 
  dplyr::select(-CI_range) 

(pul_yellow_plot_scaled <-ggplot(pulchra_yellow_trans) +
    geom_point(data = all_phenocam_pulchra, aes(x = population, y = First_leaf_yellow_DOY, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf yellowing DOY \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    coord_cartesian(ylim=c(170, 250)) +
    labs(title = "Salix pulchra"))
# S. arctica ------
arc_yellow <- (conditional_effects(garden_arc_yellow_compare)) # extracting conditional effects from bayesian model
arc_yellow_data <- arc_yellow[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_yellow_plot <-ggplot(arc_yellow_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_phenocam_arctica, aes(x = population, y = First_leaf_yellow_DOY_center, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf yellowing DOY (centered) \n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix arctica"))
# back transform scaled data for figure 
m_arc_yellow <- mean(all_phenocam_arctica$First_leaf_yellow_DOY, na.rm = T)
arctica_yellow_trans <- arc_yellow_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_arc_yellow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_arc_yellow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_arc_yellow), 
                Est.Error_trans = (se__ + m_arc_yellow)) %>% 
  dplyr::select(-CI_range) 

(arc_yellow_plot_scaled <-ggplot(arctica_yellow_trans) +
    geom_point(data = all_phenocam_arctica, aes(x = population, y = First_leaf_yellow_DOY, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("First leaf yellowing DOY \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal_arc) +
    theme_shrub() +
    coord_cartesian(ylim=c(170, 250)) +
    labs(title = "Salix arctica"))
# arrange 
(leaf_yellow_panel <- ggarrange(ric_yellow_plot, pul_yellow_plot, arc_yellow_plot, 
                               common.legend = TRUE, legend = "bottom",
                               ncol = 3, nrow = 1))
# arrange unscaled data figures 
(leaf_yellow_panel_unscale <- ggarrange(ric_yellow_plot_scaled, pul_yellow_plot_scaled, arc_yellow_plot_scaled, 
                                common.legend = TRUE, legend = "bottom",
                                ncol = 3, nrow = 1))
ggsave("figures/phenology/yellowing_panel.png", height = 5, width = 12, dpi = 300)

# GROWING SEASON -------
# S. richardsonii -------
ric_grow <- (conditional_effects(growing_season_rich)) # extracting conditional effects from bayesian model
ric_grow_data <- ric_grow[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_growing_plot <-ggplot(ric_grow_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_growing_season_rich, aes(x = population, y = growing_season.y, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Growing season length (days) \n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))


# S. pulchra ----------
pul_grow <- (conditional_effects(growing_season_pul)) # extracting conditional effects from bayesian model
pul_grow_data <- pul_grow[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_growing_plot <-ggplot(pul_grow_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_growing_season_pul, aes(x = population, y = growing_season.y, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Growing season length (days) \n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica --------
arc_grow <- (conditional_effects(growing_season_arc)) # extracting conditional effects from bayesian model
arc_grow_data <- arc_grow[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_growing_plot <-ggplot(arc_grow_data) +
    #geom_violin(data = all_phenocam_rich, aes(x = population, y = First_bud_burst_DOY_center, fill = population, colour = population),
    #          alpha = 0.1)+ # raw data
    geom_jitter(data = all_growing_season_arc, aes(x = population, y = growing_season.y, colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Growing season length (days) \n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix arctica"))

# arrange 
(growing_season_panel <- ggarrange(ric_growing_plot, pul_growing_plot, arc_growing_plot, 
                                common.legend = TRUE, legend = "bottom",
                                ncol = 3, nrow = 1))
# growing season scaled (and then not in figures) ----

# S. richardsonii -------
ric_grow <- (conditional_effects(growing_season_rich_scale)) # extracting conditional effects from bayesian model
ric_grow_data <- ric_grow[[1]] # making the extracted model outputs into a dataset (for plotting)

m_rich_grow <- mean(all_phenocam_rich$growing_season_length, na.rm = T)
rich_grow_trans <- ric_grow_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_rich_grow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_rich_grow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_rich_grow), 
                Est.Error_trans = (se__ + m_rich_grow)) %>% 
  dplyr::select(-CI_range) 


(rich_grow_plot_scaled <-ggplot(rich_grow_trans) +
    geom_point(data = all_phenocam_rich, aes(x = population, y = growing_season_length, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Growing season length \n (# days) \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pulchra ----------
pul_grow_scale <- (conditional_effects(growing_season_pul_scaled)) # extracting conditional effects from bayesian model
pul_grow_data <- pul_grow_scale[[1]] # making the extracted model outputs into a dataset (for plotting)
m_pul_grow <- mean(all_phenocam_pulchra$growing_season_length, na.rm = T)
pulchra_grow_trans <- pul_grow_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_pul_grow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_pul_grow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_pul_grow), 
                Est.Error_trans = (se__ + m_pul_grow)) %>% 
  dplyr::select(-CI_range) 

(pul_grow_plot_scaled <-ggplot(pulchra_grow_trans) +
    geom_point(data = all_phenocam_pulchra, aes(x = population, y = growing_season_length, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Growing season length \n (# days) \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal) +
    theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica --------
arc_grow_scale <- (conditional_effects(growing_season_arc_scaled)) # extracting conditional effects from bayesian model
arc_grow_data <- arc_grow_scale[[1]] # making the extracted model outputs into a dataset (for plotting)

m_arc_grow <- mean(all_phenocam_arctica$growing_season_length, na.rm = T)
arctica_grow_trans <- arc_grow_data %>% 
  dplyr::mutate(CI_range = (estimate__ - lower__)) %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_arc_grow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_arc_grow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_arc_grow), 
                Est.Error_trans = (se__ + m_arc_grow)) %>% 
  dplyr::select(-CI_range) 

(arc_grow_plot_scaled <-ggplot(arctica_grow_trans) +
    geom_point(data = all_phenocam_arctica, aes(x = population, y = growing_season_length, colour = population),
               alpha = 0.5)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Growing season length \n (# days) \n") +
    xlab("\n" ) +
    scale_color_manual(values=pal_arc) +
    theme_shrub() +
    labs(title = "Salix arctica"))

# arrange 
(growing_season_panel_unscaled <- ggarrange(rich_grow_plot_scaled, pul_grow_plot_scaled, arc_grow_plot_scaled, 
                                   common.legend = TRUE, legend = "bottom",
                                   ncol = 3, nrow = 1))
ggsave("figures/phenology/grow_season_panel.png", height = 5, width = 12, dpi = 300)

# arrange all 
(pheno_panel_unscaled <- ggarrange(leaf_emerg_panel_unscale, leaf_yellow_panel_unscale, growing_season_panel_unscaled, 
                                            common.legend = TRUE, legend = "bottom",
                                            ncol = 1, nrow = 3))

# NEW overall figure-----
# S.rich ----
richard_emerg_trans_2 <- richard_emerg_trans %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY_center")

richard_yellow_trans_2 <- richard_yellow_trans %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY_center")

richard_emerg_yellow <- rbind(richard_emerg_trans_2,richard_yellow_trans_2)

all_phenocam_rich_1 <- all_phenocam_rich %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY")%>%
  dplyr::select(-First_leaf_yellow_DOY) 

all_phenocam_rich_2 <- all_phenocam_rich %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY") %>%
  dplyr::select(-First_bud_burst_DOY)

all_phenocam_rich_all <- rbind(all_phenocam_rich_1, all_phenocam_rich_2)

(richard_emerg_yellow_plot_scaled <-ggplot(richard_emerg_yellow) +
    geom_point(data = all_phenocam_rich_all, aes(x = population, y = DOY, colour = population),
               alpha = 0.2)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  linewidth = 0.4, alpha = 0.5, width=0.2)+
    geom_line(aes(x = effect1__, y = Estimate_trans, group = population, colour = population), 
              linewidth = 1, alpha = 1)+
    ylab("\nDOY") +
    xlab("Population\n" ) +
    coord_cartesian(ylim=c(120, 250))+
    scale_color_manual(values=pal)+
    coord_flip() + 
    theme_shrub() +
    ggtitle(expression(italic("Salix richardsonii"))) +
    theme(text=element_text(family="Helvetica Light")))


# S. pul----
pul_emerg_trans_1 <-pulchra_emerg_trans %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY_center")

pul_yellow_trans_2 <- pulchra_yellow_trans %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY_center")

pul_emerg_yellow <- rbind(pul_yellow_trans_2,pul_emerg_trans_1)

all_phenocam_pul_1 <- all_phenocam_pulchra %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY")%>%
  dplyr::select(-First_leaf_yellow_DOY) 

all_phenocam_pul_2 <- all_phenocam_pulchra %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY") %>%
  dplyr::select(-First_bud_burst_DOY)

all_phenocam_pul_all <- rbind(all_phenocam_pul_1, all_phenocam_pul_2)

(pul_emerg_yellow_plot_scaled <-ggplot(pul_emerg_yellow) +
    geom_point(data = all_phenocam_pul_all, aes(x = population, y = DOY, colour = population),
               alpha = 0.2)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  linewidth = 0.4, alpha = 0.5, width=0.2)+
    geom_line(aes(x = effect1__, y = Estimate_trans, group = population, colour = population), 
              linewidth = 1, alpha = 1)+
    ylab("\nDOY") +
    xlab("Population\n" ) +
    coord_cartesian(ylim=c(120, 250))+
    scale_color_manual(values=pal)+
    coord_flip() + 
    theme_shrub() +
    ggtitle(expression(italic("Salix pulchra"))) +
    theme(text=element_text(family="Helvetica Light")))

# S. arc----
arc_emerg_trans_1 <-arc_emerg_trans %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY_center")

arc_yellow_trans_2 <- arctica_yellow_trans %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY_center")

arc_emerg_yellow <- rbind(arc_emerg_trans_1,arc_yellow_trans_2)

all_phenocam_arc_1 <- all_phenocam_arctica %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY")%>%
  dplyr::select(-First_leaf_yellow_DOY) 

all_phenocam_arc_2 <- all_phenocam_arctica %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY") %>%
  dplyr::select(-First_bud_burst_DOY)

all_phenocam_arc_all <- rbind(all_phenocam_arc_1, all_phenocam_arc_2)

(arc_emerg_yellow_plot_scaled <-ggplot(arc_emerg_yellow) +
    geom_point(data = all_phenocam_arc_all, aes(x = population, y = DOY, colour = population),
               alpha = 0.2)+
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  linewidth = 0.4, alpha = 0.5, width=0.2)+
    geom_line(aes(x = effect1__, y = Estimate_trans, group = population, colour = population), 
              linewidth = 1, alpha = 1)+
    ylab("\nDOY") +
    xlab("Population\n" ) +
    coord_cartesian(ylim=c(120, 250))+
    scale_color_manual(values=pal)+
    coord_flip() + 
    theme_shrub() +
    ggtitle(expression(italic("Salix arctica"))) +
    theme(text=element_text(family="Helvetica Light")))
# arrange 
(pheno_panel_new <- ggarrange(richard_emerg_yellow_plot_scaled, pul_emerg_yellow_plot_scaled, arc_emerg_yellow_plot_scaled, 
                              common.legend = TRUE, legend = "bottom",
                              ncol = 3, nrow = 1))
ggsave(pheno_panel_new, filename ="outputs/figures/pheno_panel_new.png", width = 20, height = 6.53, units = "in")


