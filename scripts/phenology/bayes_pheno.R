# BAYESIAN phenology script ----
# BY Erica and Madi 
# Last update: 19/10/2023 by Madi 

# Libraries----
library(plyr)
library(tidyverse)
library(brms)
library(tidybayes)
library(ggplot2)
library(knitr) # For kable tables
library(kableExtra) # For kable tables
library(ggpubr)
library(ggeffects)

# Load data ----
# 2023: 
all_pheno_2023 <- read.csv("data/phenology/all_phenology_2023.csv")

# from 2022: 
#all_phenocam_data_salix <- read.csv("data/phenology/all_phenocam_update.csv")
#all_growing_season <- read.csv("data/phenology/all_growing_season_salix.csv")

# Wrangle data ------
# ordering levels so source and garden populations side by side
all_pheno_2023$population <- plyr::revalue(all_pheno_2023$population, 
                                                    c("Northern Garden"="N. Garden",
                                                      "Southern Garden"="S. Garden",
                                                      "Southern Source"="S. Source",
                                                      "Northern Source"="N. Source"))

#all_phenocam_data_salix$population <- ordered(all_phenocam_data_salix$population, 
#                                              levels = c("Northern Source", 
#                                                         "Northern Garden", 
#                                                         "Southern Source",
#                                                         "Southern Garden"))

all_pheno_2023$Year <- as.factor(all_pheno_2023$Year)

#all_growing_season$population <- plyr::revalue(all_growing_season$population, 
#                                               c("Northern"="N. Garden",
#                                                 "Southern"="S. Garden",
#                                                 "KP"="S. Source",
#                                                 "QHI"="S. Source"))

#all_growing_season$population <- ordered(all_growing_season$population, 
#                                         levels = c("Northern Source", 
#                                                    "Northern Garden", 
#                                                  "Southern Source",
#                                                    "Southern Garden"))

# calculate growing season length in all_phenocam_data_salix data sheet 

# SPECIES SPECIFIC datasets: CG + Sources -----
all_phenocam_rich <- all_pheno_2023 %>%
  filter(Species == "Salix richardsonii")

all_phenocam_pulchra <- all_pheno_2023 %>%
  filter(Species == "Salix pulchra")

all_phenocam_arctica <- all_pheno_2023 %>%
  filter(Species == "Salix arctica")

summary_pheno <- all_pheno_2023 %>% 
  group_by(Species, population) %>% 
  summarise(mean_growing = mean(growing_season, na.rm = T))

# # SOURCE POP ONLY species specific datasets -----
# all_phenocam_rich_source <- all_phenocam_rich %>%
#   filter(population %in% c("N. Source", "S. Source"))
# all_phenocam_rich_source$population <- as.character(all_phenocam_rich_source$population)
# all_phenocam_rich_source$population <- as.factor(all_phenocam_rich_source$population)
# unique(all_phenocam_rich_source$population)
# 
# all_phenocam_pul_source <- all_phenocam_pulchra %>%
#   filter(population %in% c("N. Source", "S. Source"))
# all_phenocam_pul_source$population <- as.character(all_phenocam_pul_source$population)
# all_phenocam_pul_source$population <- as.factor(all_phenocam_pul_source$population)
# unique(all_phenocam_pul_source$population)
# 
# all_phenocam_arc_source <- all_phenocam_arctica %>%
#   filter(population %in% c("N. Source", "S. Source"))
# all_phenocam_arc_source$population <- as.character(all_phenocam_arc_source$population)
# all_phenocam_arc_source$population <- as.factor(all_phenocam_arc_source$population)
# unique(all_phenocam_arc_source$population)

# # CG only species specific data -----
# all_phenocam_rich_garden <- all_phenocam_rich %>% 
#   filter(population %in% c("N. Garden", "S. Garden"))
# all_phenocam_rich_garden$population <- as.character(all_phenocam_rich_garden$population)
# all_phenocam_rich_garden$population <- as.factor(all_phenocam_rich_garden$population)
# unique(all_phenocam_rich_garden$population)
# 
# all_phenocam_pul_garden <- all_phenocam_pulchra %>% 
#   filter(population %in% c("N. Garden", "S. Garden"))
# all_phenocam_pul_garden$population <- as.character(all_phenocam_pul_garden$population)
# all_phenocam_pul_garden$population <- as.factor(all_phenocam_pul_garden$population)
# unique(all_phenocam_pul_garden$population)
# 
# all_phenocam_arc_garden <- all_phenocam_arctica %>% 
#   filter(population %in% c("N. Garden", "S. Garden"))
# all_phenocam_arc_garden$population <- as.character(all_phenocam_arc_garden$population)
# all_phenocam_arc_garden$population <- as.factor(all_phenocam_arc_garden$population)
# unique(all_phenocam_arc_garden$population)

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

# LEAF EMERGENCE (CG vs SOURCES)  ------
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
garden_rich_emerg_compare <- readRDS(file = "output/phenology/garden_rich_emerg_compare.rds")

# extract output with function
rich_emerg_results <- model_summ_pheno(garden_rich_emerg_compare)

rich_emerg_results <- rich_emerg_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

rich_emerg_results_2 <- rich_emerg_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
rich_emerg_results_2[2,1] <- rich_emerg_results_2[2,1] + rich_emerg_results_2[1,1]
rich_emerg_results_2[3,1] <- rich_emerg_results_2[3,1] + rich_emerg_results_2[1,1]
rich_emerg_results_2[4,1] <- rich_emerg_results_2[4,1] + rich_emerg_results_2[1,1]
# change lower CI by adding 
rich_emerg_results_2[2,3] <- rich_emerg_results_2[2,3] + rich_emerg_results_2[1,3]
rich_emerg_results_2[3,3] <- rich_emerg_results_2[3,3] + rich_emerg_results_2[1,3]
rich_emerg_results_2[4,3] <- rich_emerg_results_2[4,3] + rich_emerg_results_2[1,3]
# change upper CI
rich_emerg_results_2[2,4] <- rich_emerg_results_2[2,4] + rich_emerg_results_2[1,4]
rich_emerg_results_2[3,4] <- rich_emerg_results_2[3,4] + rich_emerg_results_2[1,4]
rich_emerg_results_2[4,4] <- rich_emerg_results_2[4,4] + rich_emerg_results_2[1,4]

# extraction for model output table
rownames(rich_emerg_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Year  ", "Sigma  ")
rownames(rich_emerg_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Year ", "Sigma ")

ric_emerg_extract_df_1 <- rich_emerg_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_rich_emerg <- mean(all_phenocam_rich$First_bud_burst_DOY, na.rm = T)

ric_emerg_extract_df <- rich_emerg_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_rich_emerg)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_rich_emerg)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_rich_emerg)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

ric_emerg_extract_all <- full_join(ric_emerg_extract_df_1, ric_emerg_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(ric_emerg_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Year", "Sigma")

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
garden_pul_emerg_compare <- readRDS(file = "output/phenology/garden_pul_emerg_compare.rds")

# extract output with function
pul_emerg_results <- model_summ_pheno(garden_pul_emerg_compare)

pul_emerg_results <- pul_emerg_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

pul_emerg_results_2 <- pul_emerg_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
pul_emerg_results_2[2,1] <- pul_emerg_results_2[2,1] + pul_emerg_results_2[1,1]
pul_emerg_results_2[3,1] <- pul_emerg_results_2[3,1] + pul_emerg_results_2[1,1]
pul_emerg_results_2[4,1] <- pul_emerg_results_2[4,1] + pul_emerg_results_2[1,1]
# change lower CI by adding 
pul_emerg_results_2[2,3] <- pul_emerg_results_2[2,3] + pul_emerg_results_2[1,3]
pul_emerg_results_2[3,3] <- pul_emerg_results_2[3,3] + pul_emerg_results_2[1,3]
pul_emerg_results_2[4,3] <- pul_emerg_results_2[4,3] + pul_emerg_results_2[1,3]
# change upper CI
pul_emerg_results_2[2,4] <- pul_emerg_results_2[2,4] + pul_emerg_results_2[1,4]
pul_emerg_results_2[3,4] <- pul_emerg_results_2[3,4] + pul_emerg_results_2[1,4]
pul_emerg_results_2[4,4] <- pul_emerg_results_2[4,4] + pul_emerg_results_2[1,4]

# extraction for model output table
rownames(pul_emerg_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Year ", "Sigma ")

m_pul_emerg <- mean(all_phenocam_pulchra$First_bud_burst_DOY, na.rm = T)

pul_emerg_extract_df_1 <- pul_emerg_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_emerg_extract_df <- pul_emerg_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_pul_emerg)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_pul_emerg)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_pul_emerg)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

pul_emerg_extract_all <- full_join(pul_emerg_extract_df_1, pul_emerg_extract_df, 
                                   by = c("effect" = "effect", "nobs"="nobs",
                                          "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                          "Species"="Species", "Rhat"="Rhat"))

rownames(pul_emerg_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Year", "Sigma")

# Salix arctica -----
all_phenocam_arctica$First_bud_burst_DOY_center <- center_scale(all_phenocam_arctica$First_bud_burst_DOY) 

garden_arc_emerg_compare <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                      data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                      iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_emerg_compare)
plot(garden_arc_emerg_compare)
pp_check(garden_arc_emerg_compare, type = "dens_overlay", nsamples = 100) # looks good
saveRDS(garden_arc_emerg_compare, file = "output/phenology/garden_arc_emerg_compare.rds")
garden_arc_emerg_compare <- readRDS(file = "output/phenology/garden_arc_emerg_compare.rds")

sum_arc_pheno <- all_phenocam_arctica %>% 
  dplyr::group_by(population) %>% 
  summarize(mean_emerg = mean(First_bud_burst_DOY, na.rm = T), 
            mean_yello = mean(First_leaf_yellow_DOY, na.rm = T))

# extract output with function
arc_emerg_results <- model_summ_pheno(garden_arc_emerg_compare)

arc_emerg_results <- arc_emerg_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

arc_emerg_results_2 <- arc_emerg_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
arc_emerg_results_2[2,1] <- arc_emerg_results_2[2,1] + arc_emerg_results_2[1,1]
arc_emerg_results_2[3,1] <- arc_emerg_results_2[3,1] + arc_emerg_results_2[1,1]
# change lower CI by adding 
arc_emerg_results_2[2,3] <- arc_emerg_results_2[2,3] + arc_emerg_results_2[1,3]
arc_emerg_results_2[3,3] <- arc_emerg_results_2[3,3] + arc_emerg_results_2[1,3]
# change upper CI
arc_emerg_results_2[2,4] <- arc_emerg_results_2[2,4] + arc_emerg_results_2[1,4]
arc_emerg_results_2[3,4] <- arc_emerg_results_2[3,4] + arc_emerg_results_2[1,4]

# extraction for model output table
rownames(arc_emerg_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Year ", "Sigma ")

m_arc_emerg <- mean(all_phenocam_arctica$First_bud_burst_DOY, na.rm = T)

arc_emerg_extract_df_1 <- arc_emerg_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_emerg_extract_df <- arc_emerg_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_arc_emerg)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_arc_emerg)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_arc_emerg)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect") 

arc_emerg_extract_all <- full_join(arc_emerg_extract_df_1, arc_emerg_extract_df, 
                                   by = c("effect" = "effect", "nobs"="nobs",
                                          "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                          "Species"="Species", "Rhat"="Rhat"))

rownames(arc_emerg_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Year", "Sigma")

# merging all extracted outputs
emerg_pheno_out <- rbind(ric_emerg_extract_all, pul_emerg_extract_all, arc_emerg_extract_all)

# adding spaces before/after each name so they let me repeat them in the table
rownames(emerg_pheno_out) <- c("Intercept", "Northern Source", "Southern Garden",  "Southern Source", 
                                    "Year", "Sigma", 
                                    " Intercept", " Northern Source", " Southern Garden", " Southern Source", " Year", 
                                    " Sigma", 
                                    "Intercept ", "Northern Source ", "Southern Garden ", "Year ", 
                                    "Sigma ")

# save df of results 
write.csv(emerg_pheno_out, "output/phenology/emerg_pheno_out_back.csv")
emerg_pheno_out <- read.csv("output/phenology/emerg_pheno_out_back.csv", row.names = 1)
# making sure Rhat keeps the .00 
emerg_pheno_out$Rhat <- as.character(formatC(emerg_pheno_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_emerg_pheno <- emerg_pheno_out %>% 
  kbl(caption="Table.xxx BRMS model outputs: Leaf emergence day of year for northern garden, northern source, sourthern garden and southern source populations. 
      Scaled to center on zero and transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate (scaled)",
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)",
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect", 
                     "Estimate (backscaled)",  
                     "Error",
                     "Lower 95% CI (backscaled)", 
                     "Upper 95% CI (backscaled)"
                     ), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
row_spec(kable_emerg_pheno, 1:12, align = "c") 
column_spec(kable_emerg_pheno, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_emerg_pheno, file = "output/phenology/kable_emerg.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)
# (source) LEAF EMERGENCE

# # Salix richardsonii
# all_phenocam_rich_source$First_bud_burst_DOY_scaled <- center_scale(all_phenocam_rich_source$First_bud_burst_DOY)
# source_rich_emerg_scaled <- brms::brm(First_bud_burst_DOY_scaled ~ population + (1|Year),
#                                       data = all_phenocam_rich_source, family = gaussian(), chains = 3,
#                                       iter = 3000, warmup = 1000, 
#                                       control = list(max_treedepth = 15, adapt_delta = 0.99))
# summary(source_rich_emerg_scaled)
# 
# source_rich_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
#                                data = all_phenocam_rich_source, family = gaussian(), chains = 3,
#                                iter = 3000, warmup = 1000, 
#                                control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(source_rich_emerg) # not significant diff. 
# plot(source_rich_emerg)
# pp_check(source_rich_emerg , type = "dens_overlay", nsamples = 100) # fine??
# 
# # extract output with function
# source_rich_emerg_extract <- model_summ_pheno(source_rich_emerg)
# 
# # extraction for model output table
# rownames(source_rich_emerg_extract) <- c("Intercept", "Southern Source", "Year", "Sigma")
# 
# source_rich_emerg_extract_df <- source_rich_emerg_extract %>% 
#   mutate(Species = rep("Salix richardsonii")) %>%
#   #"Sample Size" = rep(69)) %>%
#   relocate("Species", .before = "Estimate") %>%
#   relocate("nobs", .before = "effect")
# 
# # Salix pulchra
# source_pul_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
#                               data = all_phenocam_pul_source, family = gaussian(), chains = 3,
#                               iter = 3000, warmup = 1000, 
#                               control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(source_pul_emerg) # not significant
# plot(source_pul_emerg)
# pp_check(source_pul_emerg, type = "dens_overlay", nsamples = 100) # not too happy with that....
# 
# # extract output with function
# source_pul_emerg_extract <- model_summ_pheno(source_pul_emerg)
# 
# # extraction for model output table
# rownames(source_pul_emerg_extract) <- c("Intercept", "Southern Source", "Year", "Sigma")
# 
# source_pul_emerg_extract_df <- source_pul_emerg_extract %>% 
#   mutate(Species = rep("Salix pulchra")) %>%
#   #"Sample Size" = rep(69)) %>%
#   relocate("Species", .before = "Estimate") %>%
#   relocate("nobs", .before = "effect")
# 
# # merging all extracted outputs
# source_emerg <- rbind(source_rich_emerg_extract_df, source_pul_emerg_extract_df) 
# 
# # save df of results 
# write.csv(source_emerg, "output/source_emerg.csv")
# 
# # adding spaces before/after each name so they let me repeat them in the table
# rownames(source_emerg) <- c("Intercept", "Southern Source", "Year", 
#                             "Sigma", " Intercept", " Southern Source", " Year", "Sigma ")
# 
# # making sure Rhat keeps the .00 
# source_emerg$Rhat <- as.character(formatC(source_emerg$Rhat, digits = 2, format = 'f')) #new character variable with format specification
# 
# # creating table
# kable_source_emerg <- source_emerg %>% 
#   kbl(caption="Table.xxx BRMS model outputs: first leaf emergence day of year of shrubs in northern (QHI) vs southern (Kluane) source populations. 
#       Model structure per species: First_bud_burst_DOY ~ population + (1|Year). Missing Salix arctica", 
#       col.names = c( "Species","Estimate",
#                      "Est. Error",
#                      "Lower 95% CI (scaled)",
#                      "Upper 95% CI (scaled)", 
#                      "Rhat", 
#                      "Bulk Effective Sample Size",
#                      "Tail Effective Sample Size", 
#                      "Sample Size",
#                      "Effect"), digits=2, align = "c") %>% 
#   kable_classic(full_width=FALSE, html_font="Cambria")
# 
# # making species column in cursive
# column_spec(kable_source_emerg, 2, width = NULL, bold = FALSE, italic = TRUE)
# 
# # Salix arctica
# # MISSING KLUANE PLATEAU DATA so cannot run 
# 
# # (CG) LEAF EMERGENCE
# # Salix richardsonii
# all_phenocam_rich_garden$First_bud_burst_DOY_center <- center_scale(all_phenocam_rich_garden$First_bud_burst_DOY) 
# garden_rich_emerg <- brms::brm(First_bud_burst_DOY_center ~ population,
#                                        data = all_phenocam_rich_garden, family = gaussian(), chains = 3,
#                                        iter = 3000, warmup = 1000, 
#                                       control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(garden_rich_emerg) # no significant difference
# plot(garden_rich_emerg)
# pp_check(garden_rich_emerg, type = "dens_overlay", ndraws = 100) # looks not great... but limited data
# garden_rich_emerg_results <- model_summ_pheno_no_rf(garden_rich_emerg)
# garden_rich_emerg_results$species <- "Salix richardsonii"
# 
# # Salix pulchra
# all_phenocam_pul_garden$First_bud_burst_DOY_center <- center_scale(all_phenocam_pul_garden$First_bud_burst_DOY) 
# 
# garden_pul_emerg <- brms::brm(First_bud_burst_DOY ~ population,
#                                data = all_phenocam_pul_garden, family = gaussian(), chains = 3,
#                                iter = 3000, warmup = 1000, 
#                                control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(garden_pul_emerg) # yes significant. (southern emergence later?)
# plot(garden_pul_emerg)
# pp_check(garden_pul_emerg,type = "dens_overlay", ndraws = 100) # looks ok
# garden_pul_emerg_results <- model_summ_pheno_no_rf(garden_pul_emerg)
# garden_pul_emerg_results$species <- "Salix pulchra"
# 
# # Salix arctica
# all_phenocam_arc_garden$First_bud_burst_DOY_center <- center_scale(all_phenocam_arc_garden$First_bud_burst_DOY) 
# 
# garden_arc_emerg <- brms::brm(First_bud_burst_DOY_center ~ population,
#                               data = all_phenocam_arc_garden, family = gaussian(), chains = 3,
#                               iter = 3000, warmup = 1000, 
#                               control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(garden_arc_emerg) # YEs significant 
# plot(garden_arc_emerg)
# pp_check(garden_arc_emerg,type = "dens_overlay", ndraws = 100) # looks ok
# garden_arc_emerg_results <- model_summ_pheno_no_rf(garden_arc_emerg)
# garden_arc_emerg_results$species <- "Salix arctica"
# 
# # merging all extracted outputs
# garden_emerg_out <- rbind(garden_rich_emerg_results, garden_pul_emerg_results, garden_arc_emerg_results)
# 
# # adding spaces before/after each name so they let me repeat them in the table
# rownames(garden_emerg_out) <- c("Intercept", "Southern Garden", "Sigma", 
#                                     " Intercept", " Southern Garden"," Sigma", 
#                                     "Intercept ","Southern Garden ","Sigma ")
# 
# # making sure Rhat keeps the .00 
# garden_emerg_out$Rhat <- as.character(formatC(garden_emerg_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification
# 
# # save df of results 
# write.csv(garden_emerg_out, "output/phenology/garden_leaf_emergence_out.csv")
# # creating table
# kable_emerg_garden <- garden_emerg_out %>% 
#   kbl(caption="Table.xxx BRMS model outputs: Day of year (DOY) of northern garden vs southern garden willows. 
#       Model structure per species: DOY leaf emergence ~ population. Data scaled to center on 0.", 
#       col.names = c( "Estimate",
#                      "Est. Error",
#                      "Lower 95% CI (scaled)",
#                      "Upper 95% CI (scaled)", 
#                      "Rhat", 
#                      "Bulk Effective Sample Size",
#                      "Tail Effective Sample Size", 
#                      "Effect",
#                      "Sample Size",
#                      "Species"), digits=2, align = "c") %>% 
#   kable_classic(full_width=FALSE, html_font="Cambria")
# 
# column_spec(kable_emerg_garden, 2, width = NULL, bold = FALSE, italic = TRUE)
# save_kable(kable_emerg_garden, file = "output/phenology/emerg_garden_results.pdf",
#            bs_theme = "simplex",
#            self_contained = TRUE,
#            extra_dependencies = NULL,
#            latex_header_includes = NULL,
#            keep_tex =TRUE,
#            density = 300)

# LEAF YELLOWING (source vs garden) -------
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
garden_rich_yellow_compare <- readRDS(file = "output/phenology/garden_rich_yellow_compare.rds")

# extract output with function
rich_yellow_results <- model_summ_pheno(garden_rich_yellow_compare)

rich_yellow_results <- rich_yellow_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

rich_yellow_results_2 <- rich_yellow_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
rich_yellow_results_2[2,1] <- rich_yellow_results_2[2,1] + rich_yellow_results_2[1,1]
rich_yellow_results_2[3,1] <- rich_yellow_results_2[3,1] + rich_yellow_results_2[1,1]
rich_yellow_results_2[4,1] <- rich_yellow_results_2[4,1] + rich_yellow_results_2[1,1]
# change lower CI by adding 
rich_yellow_results_2[2,3] <- rich_yellow_results_2[2,3] + rich_yellow_results_2[1,3]
rich_yellow_results_2[3,3] <- rich_yellow_results_2[3,3] + rich_yellow_results_2[1,3]
rich_yellow_results_2[4,3] <- rich_yellow_results_2[4,3] + rich_yellow_results_2[1,3]
# change upper CI
rich_yellow_results_2[2,4] <- rich_yellow_results_2[2,4] + rich_yellow_results_2[1,4]
rich_yellow_results_2[3,4] <- rich_yellow_results_2[3,4] + rich_yellow_results_2[1,4]
rich_yellow_results_2[4,4] <- rich_yellow_results_2[4,4] + rich_yellow_results_2[1,4]

# extraction for model output table
rownames(rich_yellow_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Year  ", "Sigma  ")
rownames(rich_yellow_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Year ", "Sigma ")

ric_yellow_extract_df_1 <- rich_yellow_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_rich_yellow <- mean(all_phenocam_rich$First_leaf_yellow_DOY, na.rm = T)

ric_yellow_extract_df <- rich_yellow_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_rich_yellow)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_rich_yellow)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_rich_yellow)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

ric_yellow_extract_all <- full_join(ric_yellow_extract_df_1, ric_yellow_extract_df, 
                                   by = c("effect" = "effect", "nobs"="nobs",
                                          "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                          "Species"="Species", "Rhat"="Rhat"))

rownames(ric_yellow_extract_all) <- c("Instercept", "Northern Source", "Southern Garden", "Southern Source", "Year", "Sigma")

# Salix pulchra ------
all_phenocam_pulchra$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pulchra$First_leaf_yellow_DOY) 

garden_pul_yellow_compare <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                        data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_yellow_compare)
plot(garden_pul_yellow_compare)
pp_check(garden_pul_yellow_compare, type = "dens_overlay", ndraws = 100) # looks good
saveRDS(garden_pul_yellow_compare, file = "output/phenology/garden_pul_yellow_compare.rds")
garden_pul_yellow_compare <- readRDS(file = "output/phenology/garden_pul_yellow_compare.rds")

# extract output with function
pul_yellow_results <- model_summ_pheno(garden_pul_yellow_compare)

pul_yellow_results <- pul_yellow_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

pul_yellow_results_2 <- pul_yellow_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
pul_yellow_results_2[2,1] <- pul_yellow_results_2[2,1] + pul_yellow_results_2[1,1]
pul_yellow_results_2[3,1] <- pul_yellow_results_2[3,1] + pul_yellow_results_2[1,1]
pul_yellow_results_2[4,1] <- pul_yellow_results_2[4,1] + pul_yellow_results_2[1,1]
# change lower CI by adding 
pul_yellow_results_2[2,3] <- pul_yellow_results_2[2,3] + pul_yellow_results_2[1,3]
pul_yellow_results_2[3,3] <- pul_yellow_results_2[3,3] + pul_yellow_results_2[1,3]
pul_yellow_results_2[4,3] <- pul_yellow_results_2[4,3] + pul_yellow_results_2[1,3]
# change upper CI
pul_yellow_results_2[2,4] <- pul_yellow_results_2[2,4] + pul_yellow_results_2[1,4]
pul_yellow_results_2[3,4] <- pul_yellow_results_2[3,4] + pul_yellow_results_2[1,4]
pul_yellow_results_2[4,4] <- pul_yellow_results_2[4,4] + pul_yellow_results_2[1,4]

# extraction for model output table
rownames(pul_yellow_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Year  ", "Sigma  ")
rownames(pul_yellow_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Year ", "Sigma ")

pul_yellow_extract_df_1 <- pul_yellow_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_pul_yellow <- mean(all_phenocam_pulchra$First_leaf_yellow_DOY, na.rm = T)

pul_yellow_extract_df <- pul_yellow_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_pul_yellow)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_pul_yellow)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_pul_yellow)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

pul_yellow_extract_all <- full_join(pul_yellow_extract_df_1, pul_yellow_extract_df, 
                                    by = c("effect" = "effect", "nobs"="nobs",
                                           "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                           "Species"="Species", "Rhat"="Rhat"))

rownames(pul_yellow_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Year", "Sigma")

# Salix arctica ------
all_phenocam_arctica$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_arctica$First_leaf_yellow_DOY) 

garden_arc_yellow_compare <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                       data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_yellow_compare)
plot(garden_arc_yellow_compare)
pp_check(garden_arc_yellow_compare, type = "dens_overlay", ndraws = 100) # looks good
saveRDS(garden_arc_yellow_compare, file = "output/phenology/garden_arc_yellow_compare.rds")
garden_arc_yellow_compare <- readRDS(file = "output/phenology/garden_arc_yellow_compare.rds")

# extract output with function
arc_yellow_results <- model_summ_pheno(garden_arc_yellow_compare)

arc_yellow_results <- arc_yellow_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

arc_yellow_results_2 <- arc_yellow_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding intercept to other rows for estimate + CIs 
arc_yellow_results_2[2,1] <- arc_yellow_results_2[2,1] + arc_yellow_results_2[1,1]
arc_yellow_results_2[3,1] <- arc_yellow_results_2[3,1] + arc_yellow_results_2[1,1]
# change lower CI by adding 
arc_yellow_results_2[2,3] <- arc_yellow_results_2[2,3] + arc_yellow_results_2[1,3]
arc_yellow_results_2[3,3] <- arc_yellow_results_2[3,3] + arc_yellow_results_2[1,3]
# change upper CI
arc_yellow_results_2[2,4] <- arc_yellow_results_2[2,4] + arc_yellow_results_2[1,4]
arc_yellow_results_2[3,4] <- arc_yellow_results_2[3,4] + arc_yellow_results_2[1,4]

# extraction for model output table
rownames(arc_yellow_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ",  "Year  ", "Sigma  ")
rownames(arc_yellow_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ",  "Year ", "Sigma ")

arc_yellow_extract_df_1 <- arc_yellow_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_arc_yellow <- mean(all_phenocam_arctica$First_leaf_yellow_DOY, na.rm = T)

arc_yellow_extract_df <- arc_yellow_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_arc_yellow)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_arc_yellow)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_arc_yellow)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

arc_yellow_extract_all <- full_join(arc_yellow_extract_df_1, arc_yellow_extract_df, 
                                    by = c("effect" = "effect", "nobs"="nobs",
                                           "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                           "Species"="Species", "Rhat"="Rhat"))

rownames(arc_yellow_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Year", "Sigma")

# merging all extracted outputs
yellow_pheno_out <- rbind(ric_yellow_extract_all, pul_yellow_extract_all, arc_yellow_extract_all)

# adding spaces before/after each name so they let me repeat them in the table
rownames(yellow_pheno_out) <- c("Intercept", "Northern Source", "Southern Garden",  "Southern Source", 
                                "Year", "Sigma", 
                                " Intercept", " Northern Source", " Southern Garden", " Southern Source", " Year", 
                                " Sigma", 
                                "Intercept ", "Northern Source ", "Southern Garden ", "Year ", 
                                "Sigma ")

# save df of results 
write.csv(yellow_pheno_out, "output/phenology/yellow_pheno_out_back.csv")
yellow_pheno_out <- read.csv("output/phenology/yellow_pheno_out_back.csv", row.names = 1)
# making sure Rhat keeps the .00 
yellow_pheno_out$Rhat <- as.character(formatC(yellow_pheno_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_yellow_pheno <- yellow_pheno_out %>% 
  kbl(caption="Table.xxx BRMS model outputs: Leaf yellowing day of year for northern garden, northern source, sourthern garden and southern source populations. 
      Scaled to center on zero and transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate (scaled)",
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)",
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect", 
                     "Error",
                     "Estimate (backscaled)",  
                     "Lower 95% CI (backscaled)", 
                     "Upper 95% CI (backscaled)"
      ), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
row_spec(kable_yellow_pheno, 1:12, align = "c") 
column_spec(kable_yellow_pheno, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_yellow_pheno, file = "output/phenology/kable_yellow.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)
# (source) LEAF YELLOWING
# # Salix richardsonii
# all_phenocam_rich_source$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_rich_source$First_leaf_yellow_DOY) 
# 
# source_rich_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
#                                 data = all_phenocam_rich_source, family = gaussian(), chains = 3,
#                                 iter = 3000, warmup = 1000, 
#                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(source_rich_yellow) # no significant diff
# plot(source_rich_yellow)
# pp_check(source_rich_yellow, type = "dens_overlay", ndraws = 100) # looks ok
# 
# source_rich_yellow_extract <- model_summ_pheno(source_rich_yellow)
# 
# # Salix pulchra
# all_phenocam_pul_source$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pul_source$First_leaf_yellow_DOY) 
# 
# source_pul_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
#                                data = all_phenocam_pul_source, family = gaussian(), chains = 3,
#                                iter = 3000, warmup = 1000, 
#                                control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(source_pul_yellow) # no significant diff
# plot(source_pul_yellow)
# pp_check(source_pul_yellow, type = "dens_overlay", ndraws = 100) # looks ok
# 
# source_pul_yellow_extract <- model_summ_pheno(source_pul_yellow)
# 
# # Salix arctica
# # Missing KP data so cannot run

# (CG) LEAF YELLOWING
# Salix richardsonii
# all_phenocam_rich_garden$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_rich_garden$First_leaf_yellow_DOY) 
# 
# garden_rich_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population,
#                                         data = all_phenocam_rich_garden, family = gaussian(), chains = 3,
#                                         iter = 3000, warmup = 1000,
#                                         control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(garden_rich_yellow) # significant (later for southern shrubs?)
# plot(garden_rich_yellow) 
# pp_check(garden_rich_yellow, type = "dens_overlay", nsamples = 100) # looks good
# 
# garden_rich_yellow_extract <- model_summ_pheno_no_rf(garden_rich_yellow)
# garden_rich_yellow_extract$Species <- "Salix richardsonii"
# 
# # Salix pulchra
# all_phenocam_pul_garden$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pul_garden$First_leaf_yellow_DOY) 
# 
# garden_pul_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population,
#                                 data = all_phenocam_pul_garden, family = gaussian(), chains = 3,
#                                 iter = 3000, warmup = 1000,
#                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(garden_pul_yellow) # significant (later for southern shrubs?)
# plot(garden_pul_yellow)
# pp_check(garden_pul_yellow, type = "dens_overlay", ndraws = 100) # looks decent
# 
# garden_pul_yellow_extract <- model_summ_pheno_no_rf(garden_pul_yellow)
# garden_pul_yellow_extract$Species <- "Salix pulchra"
# 
# # Salix arctica
# all_phenocam_arc_garden$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_arc_garden$First_leaf_yellow_DOY) 
# 
# garden_arc_yellow <- brms::brm(First_leaf_yellow_DOY_center ~ population,
#                                data = all_phenocam_arc_garden, family = gaussian(), chains = 3,
#                                iter = 3000, warmup = 1000,
#                                control = list(max_treedepth = 15, adapt_delta = 0.99))
# 
# summary(garden_arc_yellow) # significant 
# plot(garden_arc_yellow)
# pp_check(garden_arc_yellow, type = "dens_overlay", ndraws = 100) # looks good
# 
# garden_arc_yellow_extract <- model_summ_pheno_no_rf(garden_arc_yellow)
# garden_arc_yellow_extract$Species <- "Salix arctica"
# 
# # merging all extracted outputs
# garden_yellow_out <- rbind(garden_rich_yellow_extract, garden_pul_yellow_extract, garden_arc_yellow_extract)
# 
# # adding spaces before/after each name so they let me repeat them in the table
# rownames(garden_yellow_out) <- c("Intercept", "Southern Garden", "Sigma", 
#                                 " Intercept", " Southern Garden"," Sigma", 
#                                 "Intercept ","Southern Garden ","Sigma ")

# making sure Rhat keeps the .00 
# garden_yellow_out$Rhat <- as.character(formatC(garden_yellow_out$Rhat, digits = 2, format = 'f')) #new character variable with format specification
# 
# # save df of results 
# write.csv(garden_yellow_out, "output/phenology/garden_leaf_yellow_out.csv")
# garden_yellow_out <- read.csv("output/phenology/garden_leaf_yellow_out.csv")
# # creating table
# kable_yellow_garden <- garden_yellow_out %>% 
#   kbl(caption="Table.xxx BRMS model outputs: Day of year (DOY) of first leaf yellowing northern garden vs southern garden willows. 
#       Model structure per species: DOY leaf emergence ~ population. Data scaled to center on 0.", 
#       col.names = c( "Estimate",
#                      "Est. Error",
#                      "Lower 95% CI (scaled)",
#                      "Upper 95% CI (scaled)", 
#                      "Rhat", 
#                      "Bulk Effective Sample Size",
#                      "Tail Effective Sample Size", 
#                      "Effect",
#                      "Sample Size",
#                      "Species"), digits=2, align = "c") %>% 
#   kable_classic(full_width=FALSE, html_font="Cambria")
# column_spec(kable_yellow_garden, 2, width = NULL, bold = FALSE, italic = TRUE)
# save_kable(kable_yellow_garden, file = "output/phenology/yellow_garden_results.pdf",
#            bs_theme = "simplex",
#            self_contained = TRUE,
#            extra_dependencies = NULL,
#            latex_header_includes = NULL,
#            keep_tex =TRUE,
#            density = 300)

# 3. GROWING SEASON LENGTH -----
# Salix richardsonii ------
# center on 0
all_phenocam_rich$growing_season_length_scale <- center_scale(all_phenocam_rich$growing_season)
growing_season_rich_scale <- brms::brm(growing_season_length_scale ~ population, 
                                 data = all_phenocam_rich, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000,
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

# removed year as random effect because only one year for northern source 
summary(growing_season_rich_scale) 
plot(growing_season_rich_scale)
pp_check(growing_season_rich_scale, type = "dens_overlay", ndraws = 100) # looks decent
saveRDS(growing_season_rich_scale, file = "output/phenology/garden_ric_growing_compare.rds")
growing_season_rich_scale <- readRDS(file = "output/phenology/garden_ric_growing_compare.rds")

# extract output with function
rich_season_results <- model_summ_pheno_no_rf(growing_season_rich_scale)

rich_season_results <- rich_season_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

rich_season_results_2 <- rich_season_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
rich_season_results_2[2,1] <- rich_season_results_2[2,1] + rich_season_results_2[1,1]
rich_season_results_2[3,1] <- rich_season_results_2[3,1] + rich_season_results_2[1,1]
rich_season_results_2[4,1] <- rich_season_results_2[4,1] + rich_season_results_2[1,1]
# change lower CI by adding 
rich_season_results_2[2,3] <- rich_season_results_2[2,3] + rich_season_results_2[1,3]
rich_season_results_2[3,3] <- rich_season_results_2[3,3] + rich_season_results_2[1,3]
rich_season_results_2[4,3] <- rich_season_results_2[4,3] + rich_season_results_2[1,3]
# change upper CI
rich_season_results_2[2,4] <- rich_season_results_2[2,4] + rich_season_results_2[1,4]
rich_season_results_2[3,4] <- rich_season_results_2[3,4] + rich_season_results_2[1,4]
rich_season_results_2[4,4] <- rich_season_results_2[4,4] + rich_season_results_2[1,4]

# extraction for model output table
rownames(rich_season_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sigma  ")
rownames(rich_season_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sigma ")

ric_season_extract_df_1 <- rich_season_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_rich_grow <- mean(all_phenocam_rich$growing_season, na.rm = T)

ric_season_extract_df <- rich_season_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_rich_grow)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_rich_grow)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_rich_grow)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

ric_season_extract_all <- full_join(ric_season_extract_df_1, ric_season_extract_df, 
                                   by = c("effect" = "effect", "nobs"="nobs",
                                          "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                          "Species"="Species", "Rhat"="Rhat"))

rownames(ric_season_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sigma")

# Salix pulchra ------
# center on 0
all_phenocam_pulchra$growing_season_length_scale <- center_scale(all_phenocam_pulchra$growing_season)
growing_season_pul_scaled <- brms::brm(growing_season_length_scale ~ population + (1|Year), 
                                data = all_phenocam_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000,
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(growing_season_pul_scaled) # 
plot(growing_season_pul_scaled)
pp_check(growing_season_pul_scaled, type = "dens_overlay", ndraws = 100) # looks decent
saveRDS(growing_season_pul_scaled, file = "output/phenology/garden_pul_growing_compare.rds")
growing_season_pul_scaled <- readRDS(file = "output/phenology/garden_pul_growing_compare.rds")

# extract output with function
pul_season_results <- model_summ_pheno(growing_season_pul_scaled)

pul_season_results <- pul_season_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

pul_season_results_2 <- pul_season_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
pul_season_results_2[2,1] <- pul_season_results_2[2,1] + pul_season_results_2[1,1]
pul_season_results_2[3,1] <- pul_season_results_2[3,1] + pul_season_results_2[1,1]
pul_season_results_2[4,1] <- pul_season_results_2[4,1] + pul_season_results_2[1,1]
# change lower CI by adding 
pul_season_results_2[2,3] <- pul_season_results_2[2,3] + pul_season_results_2[1,3]
pul_season_results_2[3,3] <- pul_season_results_2[3,3] + pul_season_results_2[1,3]
pul_season_results_2[4,3] <- pul_season_results_2[4,3] + pul_season_results_2[1,3]
# change upper CI
pul_season_results_2[2,4] <- pul_season_results_2[2,4] + pul_season_results_2[1,4]
pul_season_results_2[3,4] <- pul_season_results_2[3,4] + pul_season_results_2[1,4]
pul_season_results_2[4,4] <- pul_season_results_2[4,4] + pul_season_results_2[1,4]

# extraction for model output table
rownames(pul_season_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Year  ", "Sigma  ")
rownames(pul_season_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Year ", "Sigma ")

pul_season_extract_df_1 <- pul_season_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_pul_grow <- mean(all_phenocam_pulchra$growing_season, na.rm = T)

pul_season_extract_df <- pul_season_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_pul_grow)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_pul_grow)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_pul_grow)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

pul_season_extract_all <- full_join(pul_season_extract_df_1, pul_season_extract_df, 
                                    by = c("effect" = "effect", "nobs"="nobs",
                                           "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                           "Species"="Species", "Rhat"="Rhat"))

rownames(pul_season_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Year", "Sigma")

# Salix arctica ------
# center on 0
all_phenocam_arctica$growing_season_length_scale <- center_scale(all_phenocam_arctica$growing_season)
growing_season_arc_scaled <- brms::brm(growing_season_length_scale ~ population + (1|Year), 
                                       data = all_phenocam_arctica, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(growing_season_arc_scaled) # 
plot(growing_season_arc_scaled)
pp_check(growing_season_arc_scaled, type = "dens_overlay", ndraws = 100) # looks decent
saveRDS(growing_season_arc_scaled, file = "output/phenology/garden_arc_growing_compare.rds")
growing_season_arc_scaled <- readRDS(file = "output/phenology/garden_arc_growing_compare.rds")

# extract output with function
arc_season_results <- model_summ_pheno(growing_season_arc_scaled)

arc_season_results <- arc_season_results %>% 
  dplyr::rename("l_95_CI_scale_og" = "l-95% CI", 
                "u_95_CI_scale_og" = "u-95% CI", 
                "Estimate (scale og)"= "Estimate")

arc_season_results_2 <- arc_season_results %>% 
  dplyr::rename("l_95_CI_scale_sum" = "l_95_CI_scale_og", 
                "u_95_CI_scale_sum" = "u_95_CI_scale_og",
                "Estimate_scale_sum"= "Estimate (scale og)")

# change estimates by adding estimate to other rows 
arc_season_results_2[2,1] <- arc_season_results_2[2,1] + arc_season_results_2[1,1]
arc_season_results_2[3,1] <- arc_season_results_2[3,1] + arc_season_results_2[1,1]
# change lower CI by adding 
growing_season_arc_scaled_results[2,3] <- arc_season_results_2[2,3] + arc_season_results_2[1,3]
growing_season_arc_scaled_results[3,3] <- arc_season_results_2[3,3] + arc_season_results_2[1,3]
# change upper CI
arc_season_results_2[2,4] <- arc_season_results_2[2,4] + arc_season_results_2[1,4]
arc_season_results_2[3,4] <- arc_season_results_2[3,4] + arc_season_results_2[1,4]

# extraction for model output table
rownames(arc_season_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Year  ", "Sigma  ")
rownames(arc_season_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ",  "Year ", "Sigma ")

arc_season_extract_df_1 <- arc_season_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (scale og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

m_arc_grow <- mean(all_phenocam_arctica$growing_season, na.rm = T)

arc_season_extract_df <- arc_season_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  dplyr::mutate(l_95_CI_scale_sum = ((l_95_CI_scale_sum) + m_arc_grow)) %>% 
  dplyr::mutate(u_95_CI_scale_sum = ((u_95_CI_scale_sum) + m_arc_grow)) %>% 
  dplyr::mutate(Estimate_scale_sum = (Estimate_scale_sum + m_arc_grow)) %>% 
  relocate("Species", .before = "Estimate_scale_sum") %>%
  relocate("nobs", .before = "effect")

arc_season_extract_all <- full_join(arc_season_extract_df_1, arc_season_extract_df, 
                                    by = c("effect" = "effect", "nobs"="nobs",
                                           "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                           "Species"="Species", "Rhat"="Rhat"))

rownames(arc_season_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Year", "Sigma")

# compile results 
season_results <- rbind(ric_season_extract_all, pul_season_extract_all, arc_season_extract_all)

# adding spaces before/after each name so they let me repeat them in the table
rownames(season_results) <- c("Intercept", "Northern Source", "Southern Garden",  "Southern Source", 
                              "Sigma", 
                              " Intercept", " Northern Source", " Southern Garden", " Southern Source", " Year", 
                              " Sigma", 
                              "Intercept ", "Northern Source ", "Southern Garden ", "Year ", 
                              "Sigma ")

# save df of results 
write.csv(season_results, "output/phenology/season_outputs.csv")
season_results <- read.csv("output/phenology/season_outputs.csv", row.names=1)
# making sure Rhat keeps the .00 
season_results$Rhat <- as.character(formatC(season_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification

kable_season_garden <- season_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: Growing season length northern vs southern willows in common garden and source populations. 
      Model structure per species: Growing season length ~ population + (1|year). Data scaled to center on 0.", 
      col.names = c( "Species",
        "Estimate (scaled)",
                     "Lower 95% CI (scaled)",
                     "Upper 95% CI (scaled)", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Effect",
        "Error",
                     "Sample Size",
                      "Estimate (backscaled)", 
                     "Lower 95% CI (backscaled)",
                     "Upper 95% CI (backscaled)"
                     ), digits=2, align = "c") %>% 
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
theme_shrub <- function(){ theme(legend.position = "bottom",
                                 axis.title.x = element_text(face="bold", family = "Helvetica Light", size=16),
                                 axis.text.x  = element_text(vjust=0.5, size=16, family = "Helvetica Light", colour = "black"), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=16),
                                 axis.text.y  = element_text(vjust=0.5, size=16, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 16, family = "Helvetica Light", face = "italic", hjust = 0.5),
                                 legend.title = element_text(size=14, family = "Helvetica Light"),
                                 legend.key=element_blank(),
                                 strip.text.x = element_text(
                                   size = 15, color = "black", face = "italic", family = "Helvetica Light"),
                                 strip.background = element_blank(),
                                 legend.text=element_text(size = 12, family = "Helvetica Light"))}

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
# pal  <- c("#2A788EFF", "#440154FF", "#FDE725FF","#7AD151FF") # for reall levels 
# pal_garden <- c("#440154FF", "#7AD151FF") # for only garden 
# pal_arc  <- c("#2A788EFF", "#440154FF", "#7AD151FF") # for when southern source is missing  


# LEAF EMERGENCE prep ----
# S. richardsonii ------
ric_emerg <- (conditional_effects(garden_rich_emerg_compare)) # extracting conditional effects from bayesian model
ric_emerg_data <- ric_emerg[[1]] # making the extracted model outputs into a dataset (for plotting)
# back transform scaled data for figure 
m_rich_emerg <- mean(all_phenocam_rich$First_bud_burst_DOY, na.rm = T)

richard_emerg_trans <- ric_emerg_data %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_rich_emerg)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_rich_emerg)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_rich_emerg), 
                Est.Error_trans = (se__ + m_rich_emerg)) 

# S. pulchra ------
pul_emerg <- (conditional_effects(garden_pul_emerg_compare)) # extracting conditional effects from bayesian model
pul_emerg_data <- pul_emerg[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

# back transform scaled data for figure 
m_pul_emerg <- mean(all_phenocam_pulchra$First_bud_burst_DOY, na.rm = T)
pulchra_emerg_trans <- pul_emerg_data %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_pul_emerg)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_pul_emerg)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_pul_emerg), 
                Est.Error_trans = (se__ + m_pul_emerg)) 
# S. arctica -------
arc_emerg <- (conditional_effects(garden_arc_emerg_compare)) # extracting conditional effects from bayesian model
arc_emerg_data <- arc_emerg[[1]] # making the extracted model outputs into a dataset (for plotting)
# back transform scaled data for figure 
m_arc_emerg <- mean(all_phenocam_arctica$First_bud_burst_DOY, na.rm = T)
arc_emerg_trans <- arc_emerg_data %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_arc_emerg)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_arc_emerg)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_arc_emerg), 
                Est.Error_trans = (se__ + m_arc_emerg)) 
# LEAF YELLOW ----
# S. richardsonii-----
ric_yellow <- (conditional_effects(garden_rich_yellow_compare)) # extracting conditional effects from bayesian model
ric_yellow_data <- ric_yellow[[1]] # making the extracted model outputs into a dataset (for plotting)

# back transform scaled data for figure 
m_rich_yellow <- mean(all_phenocam_rich$First_leaf_yellow_DOY, na.rm = T)
richard_yellow_trans <- ric_yellow_data %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_rich_yellow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_rich_yellow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_rich_yellow), 
                Est.Error_trans = (se__ + m_rich_yellow)) 
# S. pulchra -----
pul_yellow <- (conditional_effects(garden_pul_yellow_compare)) # extracting conditional effects from bayesian model
pul_yellow_data <- pul_yellow[[1]] # making the extracted model outputs into a dataset (for plotting)
# back transform scaled data for figure 
m_pul_yellow <- mean(all_phenocam_pulchra$First_leaf_yellow_DOY, na.rm = T)
pulchra_yellow_trans <- pul_yellow_data %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_pul_yellow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_pul_yellow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_pul_yellow), 
                Est.Error_trans = (se__ + m_pul_yellow)) 
# S. arctica ------
arc_yellow <- (conditional_effects(garden_arc_yellow_compare)) # extracting conditional effects from bayesian model
arc_yellow_data <- arc_yellow[[1]] # making the extracted model outputs into a dataset (for plotting)
# back transform scaled data for figure 
m_arc_yellow <- mean(all_phenocam_arctica$First_leaf_yellow_DOY, na.rm = T)
arctica_yellow_trans <- arc_yellow_data %>% 
  dplyr::mutate(CI_low_trans = ((lower__) + m_arc_yellow)) %>% 
  dplyr::mutate(CI_high_trans = ((upper__) + m_arc_yellow)) %>% 
  dplyr::mutate(Estimate_trans = (estimate__ + m_arc_yellow), 
                Est.Error_trans = (se__ + m_arc_yellow)) 

# GROWING SEASON season scaled prep
# # S. richardsonii -------
# ric_grow <- (conditional_effects(growing_season_rich_scale)) # extracting conditional effects from bayesian model
# ric_grow_data <- ric_grow[[1]] # making the extracted model outputs into a dataset (for plotting)
# 
# m_rich_grow <- mean(all_phenocam_rich$growing_season, na.rm = T)
# rich_grow_trans <- ric_grow_data %>% 
#   dplyr::mutate(CI_low_trans = ((lower__) + m_rich_grow)) %>% 
#   dplyr::mutate(CI_high_trans = ((upper__) + m_rich_grow)) %>% 
#   dplyr::mutate(Estimate_trans = (estimate__ + m_rich_grow), 
#                 Est.Error_trans = (se__ + m_rich_grow))  
# 
# # S. pulchra ----------
# pul_grow_scale <- (conditional_effects(growing_season_pul_scaled)) # extracting conditional effects from bayesian model
# pul_grow_data <- pul_grow_scale[[1]] # making the extracted model outputs into a dataset (for plotting)
# m_pul_grow <- mean(all_phenocam_pulchra$growing_season, na.rm = T)
# pulchra_grow_trans <- pul_grow_data %>% 
#   dplyr::mutate(CI_low_trans = ((lower__) + m_pul_grow)) %>% 
#   dplyr::mutate(CI_high_trans = ((upper__) + m_pul_grow)) %>% 
#   dplyr::mutate(Estimate_trans = (estimate__ + m_pul_grow), 
#                 Est.Error_trans = (se__ + m_pul_grow)) 
# 
# # S. arctica --------
# arc_grow_scale <- (conditional_effects(growing_season_arc_scaled)) # extracting conditional effects from bayesian model
# arc_grow_data <- arc_grow_scale[[1]] # making the extracted model outputs into a dataset (for plotting)
# 
# m_arc_grow <- mean(all_phenocam_arctica$growing_season, na.rm = T)
# arctica_grow_trans <- arc_grow_data %>% 
#   dplyr::mutate(CI_low_trans = ((lower__) + m_arc_grow)) %>% 
#   dplyr::mutate(CI_high_trans = ((upper__) + m_arc_grow)) %>% 
#   dplyr::mutate(Estimate_trans = (estimate__ + m_arc_grow), 
#                 Est.Error_trans = (se__ + m_arc_grow)) 
# NEW overall figure-----
# S.rich ----
richard_emerg_trans_2 <- richard_emerg_trans %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY_center") %>% 
  dplyr::mutate(stage = "bud_burst")

richard_yellow_trans_2 <- richard_yellow_trans %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY_center") %>% 
  dplyr::mutate(stage = "yellowing")

richard_emerg_yellow <- rbind(richard_emerg_trans_2,richard_yellow_trans_2)
all_phenocam_rich_1 <- all_phenocam_rich %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY")%>%
  dplyr::select(-First_leaf_yellow_DOY) %>% 
  dplyr::mutate(stage = "bud_burst")

all_phenocam_rich_2 <- all_phenocam_rich %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY") %>%
  dplyr::select(-First_bud_burst_DOY) %>% 
  dplyr::mutate(stage = "yellowing")

all_phenocam_rich_all <- rbind(all_phenocam_rich_1, all_phenocam_rich_2)
all_phenocam_rich_all$Species <- "Salix richardsonii"
# pulchra 

pul_emerg_trans_1 <-pulchra_emerg_trans %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY_center") %>% 
  dplyr::mutate(stage = "bud_burst")

pul_yellow_trans_2 <- pulchra_yellow_trans %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY_center") %>% 
  dplyr::mutate(stage = "yellowing")

pul_emerg_yellow <- rbind(pul_yellow_trans_2,pul_emerg_trans_1)

all_phenocam_pul_1 <- all_phenocam_pulchra %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY")%>%
  dplyr::select(-First_leaf_yellow_DOY) %>% 
  dplyr::mutate(stage = "bud_burst")

all_phenocam_pul_2 <- all_phenocam_pulchra %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY") %>%
  dplyr::select(-First_bud_burst_DOY) %>% 
  dplyr::mutate(stage = "yellowing")

all_phenocam_pul_all <- rbind(all_phenocam_pul_1, all_phenocam_pul_2)
all_phenocam_pul_all$Species <- "Salix pulchra"

# arctica
arc_emerg_trans_1 <-arc_emerg_trans %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY_center") %>% 
  dplyr::mutate(stage = "bud_burst")

arc_yellow_trans_2 <- arctica_yellow_trans %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY_center") %>% 
  dplyr::mutate(stage = "yellowing")

arc_emerg_yellow <- rbind(arc_emerg_trans_1,arc_yellow_trans_2)

all_phenocam_arc_1 <- all_phenocam_arctica %>%
  dplyr::rename("DOY" = "First_bud_burst_DOY")%>%
  dplyr::select(-First_leaf_yellow_DOY) %>% 
  dplyr::mutate(stage = "bud_burst")

all_phenocam_arc_2 <- all_phenocam_arctica %>%
  dplyr::rename("DOY" = "First_leaf_yellow_DOY") %>%
  dplyr::select(-First_bud_burst_DOY) %>% 
  dplyr::mutate(stage = "yellowing")


all_phenocam_arc_all <- rbind(all_phenocam_arc_1, all_phenocam_arc_2)

all_phenocam_figure_predictions <- rbind(all_phenocam_rich_all, all_phenocam_pul_all, all_phenocam_arc_all)

all_pheno_fig_raw <- all_phenocam_figure_predictions %>% 
  dplyr::select(Species, DOY, population, stage) %>% 
  mutate(group_color = (case_when(str_detect(population, '^S') ~ 'south',
                                TRUE ~ 'north'))) %>% 
  mutate(group_shape = (case_when(grepl("Garden", population) ~ "garden",
                                  grepl("Source", population, ignore.case = TRUE) ~"source")))

richard_emerg_yellow$Species <- "Salix richardsonii"
pul_emerg_yellow$Species <- "Salix pulchra"
arc_emerg_yellow$Species <- "Salix arctica"

all_pheno_fig_pred_merge <- rbind(richard_emerg_yellow, pul_emerg_yellow, arc_emerg_yellow)

all_pheno_fig_pred <- all_pheno_fig_pred_merge %>%
  mutate(group_color = (case_when(str_detect(population, '^S') ~ 'south',
                                  TRUE ~ 'north'))) %>% 
  mutate(group_shape = (case_when(grepl("Garden", population) ~ "garden",
                                  grepl("Source", population, ignore.case = TRUE) ~"source")))


# (facet_pheno_plot <-ggplot(all_pheno_fig_pred) + # model predictions
#     geom_jitter(data = all_pheno_fig_raw, aes(y = group_color, x = DOY, colour = group_color, shape = group_shape),
#                 alpha = 0.5, position = position_dodge(width = 0.75)) + # raw data
#     geom_point(aes(x = Estimate_trans, y = group_color, shape = group_shape, color = group_color), position = position_dodge(width = 0.75), size = 6)+
#     geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = group_color, colour = group_color, shape = group_shape),
#                   size = 1, alpha = 1, width=0.75, position = position_dodge(width = 0.75)) +
#     #ylab(expression(atop("Specific leaf", paste("area (",mm^{2}," ",mg^{-1},")"))))+
#     #xlab("" ) +
#     geom_line(aes(x = Estimate_trans , y = group_color, colour = group_color, linetype = group_shape), 
#               linewidth = 1, alpha = 1, position = position_dodge(width = 0.75))+
#     scale_color_manual(values=pal_garden, guide = "none") +
#     scale_fill_manual(values=pal_garden) +
#     #coord_cartesian(ylim=c(5, 25)) +
#     scale_shape_manual(values = shapes_garden)+
#     facet_wrap(~Species) +
#     theme_shrub()+ 
#     theme(legend.background=element_blank(), legend.key=element_blank())+
#     guides(shape=guide_legend(title = "Location")) +
#     theme(axis.text.x=element_blank())+
#     theme(axis.title.y = element_text(margin = margin (r = 10))))

pal_garden <- c("#332288", "#7ad151")
shapes_garden <- c(16, 17)
shape_stage <- c(1, 16, 2, 17)
lines_garden <- c(1,2)

all_pheno_fig_pred$Species <- ordered(all_pheno_fig_pred$Species, 
                                      levels = c("Salix richardsonii", 
                                                 "Salix pulchra",
                                                 "Salix arctica"))

all_pheno_fig_raw$Species <- ordered(all_pheno_fig_raw$Species, 
                                      levels = c("Salix richardsonii", 
                                                 "Salix pulchra",
                                                 "Salix arctica"))

# FACET ----
all_pheno_fig_pred[nrow(all_pheno_fig_pred)+1,] <- NA
all_pheno_fig_raw[nrow(all_pheno_fig_raw)+1,] <- NA

all_pheno_fig_pred <- all_pheno_fig_pred %>% 
  drop_na(Species)
all_pheno_fig_raw <- all_pheno_fig_raw %>% 
  drop_na(Species)

all_pheno_fig_pred$population <- ordered(all_pheno_fig_pred$population, 
                                         levels = c("N. Source", 
                                                    "N. Garden", 
                                                    "",
                                                    "S. Source",
                                                    "S. Garden"))

all_pheno_fig_raw$population <- ordered(all_pheno_fig_raw$population, 
                                        levels = c("N. Source", 
                                                   "N. Garden", 
                                                   "",
                                                   "S. Source",
                                                   "S. Garden"))
all_pheno_fig_pred$shape_stage <- paste(all_pheno_fig_pred$group_shape,all_pheno_fig_pred$stage)

all_pheno_fig_raw$shape_stage <- paste(all_pheno_fig_raw$group_shape,all_pheno_fig_raw$stage)


write.csv(all_pheno_fig_pred, "data/phenology/all_pheno_fig_pred.csv")
write.csv(all_pheno_fig_raw, "data/phenology/all_pheno_fig_raw.csv")

all_pheno_fig_pred <- read.csv("data/phenology/all_pheno_fig_pred.csv")
all_pheno_fig_raw <- read.csv("data/phenology/all_pheno_fig_raw.csv")

(facet_pheno_plot <-ggplot(all_pheno_fig_pred) + # model predictions
    geom_point(data = all_pheno_fig_raw, aes(y = population, x = DOY, colour = group_color, shape = group_shape),
                alpha = 0.5, position = position_jitter(h = 0.2)) + # raw data
    geom_point(aes(x = Estimate_trans, y = population, shape = group_shape, color = group_color), size = 4)+
    geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = population, 
                      colour = group_color, shape = group_shape),
                  size = 1, alpha = 1, width=0.4) +
    xlab("\n Day of year") +
    ylab("") +
   geom_line(aes(x = Estimate_trans , y = population, colour = group_color, linetype = group_shape), 
              linewidth = 1, alpha = 1)+
    scale_color_manual(values=pal_garden, guide = "none") +
    scale_fill_manual(values=pal_garden, guide = "none") +
    scale_x_continuous(limits = c(110, 240), breaks = seq(110, 240, by = 30)) +
    scale_shape_manual(values = shapes_garden)+
    scale_linetype_manual(values = lines_garden, guide = "none")+
    scale_y_discrete(drop=FALSE,
                     labels=c('', '\n \n \n North', "", "", '\n \n South'), expand=c(0.2, 0.2)) +
    facet_grid(~Species, scales = "free_x", drop=T) +
    theme_shrub()+ 
    theme(legend.background=element_blank(), legend.key=element_blank(), 
          legend.position = "bottom", 
          axis.ticks.y = element_blank())+
    guides(shape=guide_legend(title = "Location")))

  #geom_vline(data=filter(all_pheno_fig_pred, Species=="Salix richardsonii"), aes(xintercept=137), linetype ="dashed") + 
  #geom_vline(data=filter(all_pheno_fig_pred, Species=="Salix pulchra"), aes(xintercept=140), linetype ="dashed") + 
  #geom_vline(data=filter(all_pheno_fig_pred, Species=="Salix arctica"), aes(xintercept=125), linetype ="dashed") +
  

(facet_pheno_plot_vert <-ggplot(all_pheno_fig_pred) + # model predictions
    geom_point(data = all_pheno_fig_raw, aes(y = DOY, x = population, colour = group_color, 
                                             shape = shape_stage), alpha = 0.5) + # raw data
    geom_point(aes(x = population, y = Estimate_trans, shape = shape_stage, 
                   color = group_color), size = 3, stroke = 1)+
    geom_errorbar(aes(ymin = CI_low_trans, ymax = CI_high_trans, x = population, 
                      colour = group_color),
                  size = 1, alpha = 1, width=0.4) +
    ylab("Day of year ") +
    xlab("") +
    geom_line(aes(x = population , y = Estimate_trans, colour = group_color), 
              linewidth = 0.8, alpha = 1)+
    scale_color_manual(values=pal_garden, guide = "none") +
    scale_fill_manual(values=pal_garden, guide = "none") +
    scale_y_continuous(limits = c(110, 240), breaks = seq(110, 240, by = 30)) +
    scale_shape_manual(labels = c("Garden emergence", "Garden yellowing", 
                                  "Source emergence", "Source yellowing"), values = shape_stage)+
    scale_x_discrete(drop=FALSE,
                     labels=c('      North', '', "", "      South", ''), expand=c(0.2, 0.2)) +
    facet_grid(~Species, scales = "free_y", drop=T) +
    theme_shrub()+ 
    theme(legend.background=element_blank(), legend.key=element_blank(), 
          legend.position = "bottom", 
          axis.ticks.x = element_blank(), 
          axis.text.x  = element_text(angle = 0), 
          legend.box="vertical", legend.margin=margin())+
    guides(shape=guide_legend(title = "Location - \n Stage", nrow=2,byrow=TRUE)))

library(pBrackets)
(facet_pheno_plot_vert <-ggplot(all_pheno_fig_pred) + # model predictions
    geom_point(data = all_pheno_fig_raw, aes(y = DOY, x = population, colour = group_color, 
                                             shape = shape_stage), alpha = 0.5) + # raw data
    geom_point(aes(x = population, y = Estimate_trans, shape = shape_stage, 
                   color = group_color), size = 3, stroke = 1)+
    geom_errorbar(aes(ymin = CI_low_trans, ymax = CI_high_trans, x = population, 
                      colour = group_color),
                  size = 1, alpha = 1, width=0.4) +
    ylab("Day of year ") +
    xlab("") +
    geom_line(aes(x = population , y = Estimate_trans, colour = group_color), 
              linewidth = 0.8, alpha = 1, linetype = "dashed", position = position_nudge(x = 0.25))+
    scale_color_manual(values=pal_garden, guide = "none") +
    scale_fill_manual(values=pal_garden, guide = "none") +
    scale_y_continuous(limits = c(110, 240), breaks = seq(110, 240, by = 30)) +
    scale_shape_manual(labels = c("Garden emergence", "Garden yellowing", 
                                  "Source emergence", "Source yellowing"), values = shape_stage)+
    scale_x_discrete(drop=FALSE,
                     labels=c('      North', '', "", "      South", ''), expand=c(0.2, 0.2)) +
    facet_grid(~Species, scales = "free_y", drop=T) +
    theme_shrub()+ 
    theme(legend.background=element_blank(), legend.key=element_blank(), 
          legend.position = "bottom", 
          axis.ticks.x = element_blank(), 
          axis.text.x  = element_text(angle = 0), 
          legend.box="vertical", legend.margin=margin())+
    guides(shape=guide_legend(title = "Location - \n Stage", nrow=2,byrow=TRUE)))

grid.locator(unit="native") 

grid.brackets(54, 119, 54, 219, lwd=2, col="red", xpd = FALSE)
segments(x0 = c(54, 54), y0 = -119, y1 = -219, xpd = TRUE) # vertical lines


ggsave("output/figures/pheno_panel.png",  height = 12, width = 18, unit = "cm", dpi = 500, device = png)


(facet_presen_plot1 <-ggplot(all_pheno_fig_pred) + # model predictions
    geom_point(aes(x = Estimate_trans, y = population, shape = group_shape, color = group_color), size = 4)+
    geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = population, 
                      colour = group_color, shape = group_shape),
                  size = 1, alpha = 1, width=0.4) +
    xlab("\n Day of year") +
    ylab("") +
    scale_color_manual(values=pal_garden, guide = "none") +
    scale_fill_manual(values=pal_garden, guide = "none") +
    scale_x_continuous(limits = c(110, 240), breaks = seq(110, 240, by = 30)) +
    scale_shape_manual(values = shapes_garden)+
    scale_linetype_manual(values = lines_garden, guide = "none")+
    scale_y_discrete(drop=FALSE,
                     labels=c('', '\n \n \n North', "", "", '\n \n South'), expand=c(0.2, 0.2)) +
    facet_grid(~Species, scales = "free_x", drop=T) +
    theme_shrub()+ 
    theme(legend.background=element_blank(), legend.key=element_blank(), 
          legend.position = "bottom", 
          axis.ticks.y = element_blank())+
    guides(shape=guide_legend(title = "Location")))

ggsave("output/figures/pheno_panel_pres1.png",  height = 5, width = 12, unit = "in", dpi = 500, device = png)

(facet_presen_plot2 <-ggplot(all_pheno_fig_pred) + # model predictions
    geom_point(aes(x = Estimate_trans, y = population, shape = group_shape, color = group_color), size = 4)+
    geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = population, 
                      colour = group_color, shape = group_shape),
                  size = 1, alpha = 1, width=0.4) +
    xlab("\n Day of year") +
    ylab("") +
    geom_line(aes(x = Estimate_trans , y = population, colour = group_color, linetype = group_shape), 
              linewidth = 1, alpha = 1)+
    scale_color_manual(values=pal_garden, guide = "none") +
    scale_fill_manual(values=pal_garden, guide = "none") +
    scale_x_continuous(limits = c(110, 240), breaks = seq(110, 240, by = 30)) +
    scale_shape_manual(values = shapes_garden)+
    scale_linetype_manual(values = lines_garden, guide = "none")+
    scale_y_discrete(drop=FALSE,
                     labels=c('', '\n \n \n North', "", "", '\n \n South'), expand=c(0.2, 0.2)) +
    facet_grid(~Species, scales = "free_x", drop=T) +
    theme_shrub()+ 
    theme(legend.background=element_blank(), legend.key=element_blank(), 
          legend.position = "bottom", 
          axis.ticks.y = element_blank())+
    guides(shape=guide_legend(title = "Location")))

ggsave("output/figures/pheno_panel_pres2.png",  height = 5, width = 12, unit = "in", dpi = 500, device = png)



# old species specific plots: 
(rich_emerg_yellow_plot_scaled <-ggplot(richard_emerg_yellow) +
    geom_point(data = all_phenocam_rich_all, aes(x =DOY , y =population , colour = population),
               alpha = 0.2)+
    geom_point(aes(x = Estimate_trans, y = effect1__ , colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = effect1__  ,colour = population),
                  linewidth = 0.4, alpha = 0.5, width=0.2)+
    geom_line(aes(x = Estimate_trans , y = effect1__, group = population, colour = population), 
              linewidth = 1, alpha = 1)+
    ylab("") +
    scale_x_continuous(limits = c(120, 240), breaks = seq(120, 240, by = 20)) +
    xlab("" ) +
    scale_color_manual(values=pal)+
    theme_shrub() +
    ggtitle(expression(italic("Salix richardsonii"))))

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
    geom_point(data = all_phenocam_pul_all, aes(x =DOY , y =population , colour = population),
               alpha = 0.2)+
    geom_point(aes(x = Estimate_trans, y = effect1__ , colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = effect1__  ,colour = population),
                  linewidth = 0.4, alpha = 0.5, width=0.2)+
    geom_line(aes(x = Estimate_trans , y = effect1__, group = population, colour = population), 
              linewidth = 1, alpha = 1)+
    ylab("") +
    scale_x_continuous(limits = c(120, 240), breaks = seq(120, 240, by = 20)) +
    xlab("\n DOY" ) +
    scale_color_manual(values=pal)+
    theme_shrub() +
    ggtitle(expression(italic("Salix pulchra"))))

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

# changing x and y instead of using coor flip so we can set axis limits easily 
(arc_emerg_yellow_plot_scaled <-ggplot(arc_emerg_yellow) +
    geom_point(data = all_phenocam_arc_all, aes(x =DOY , y =population , colour = population),
               alpha = 0.2)+
    geom_point(aes(x = Estimate_trans, y = effect1__ , colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(xmin = CI_low_trans, xmax = CI_high_trans, y = effect1__  ,colour = population),
                  linewidth = 0.4, alpha = 0.5, width=0.2)+
    geom_line(aes(x = Estimate_trans , y = effect1__, group = population, colour = population), 
              linewidth = 1, alpha = 1)+
    scale_x_continuous(limits = c(120, 240), breaks = seq(120, 240, by = 20)) +
    ylab("") +
    xlab("") +
    scale_color_manual(values=pal_arc)+
    theme_shrub() +
    ggtitle(expression(italic("Salix arctica"))))

# arrange 
(pheno_panel_new <- ggarrange(rich_emerg_yellow_plot_scaled, pul_emerg_yellow_plot_scaled, arc_emerg_yellow_plot_scaled, 
                              common.legend = TRUE, legend = "bottom",
                              labels = c("a)", "b)", "c)"),
                              ncol = 3, nrow = 1))

ggsave(pheno_panel_new, filename ="figures/phenology/pheno_panel_2023.png",
       width = 20, height = 6.53, units = "in", device = png)

# mean snow melt and return timings ----
timings_mean <- all_pheno_2023 %>% 
  mutate(snow_free_length = (Snow_return_EoS_DOY-Snow_melt_DOY)) %>% 
  group_by(population) %>% 
  summarise(mean_snow_melt = mean(Snow_melt_DOY, na.rm = TRUE), 
            low_range_melt = min(Snow_melt_DOY, na.rm = TRUE), 
            high_range_melt = max(Snow_melt_DOY, na.rm = TRUE),
            mean_snow_return = mean(Snow_return_EoS_DOY, na.rm = TRUE),
            low_range_return = min(Snow_return_EoS_DOY, na.rm = TRUE), 
            high_range_return = max(Snow_return_EoS_DOY, na.rm = TRUE),
            snow_free_days_mean = mean(snow_free_length, na.rm = TRUE), 
            sd_snow_free = sd(snow_free_length, na.rm = T))


# old stand alone figures ----
# rich emerg plot unscaled 
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
# rich emerg scaled 
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
# pulchra emerg unscaled 

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
# pulchra emerg scaled 
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
# arctica emerg unscaled 
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
# arctica emerg scaled
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

# rich yellow unscaled 
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

# rich yellow scaled 
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

# pulchra yellow unscaled 
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
# pulchra yellow scaled 
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
# arctica yellow scaled 
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
# arctica yellow unscaled 

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
# GROWING SEASON
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
# pulchra growing season
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
# GROWING SEASON unscaled in models 
# S. richardsonii
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
# S. pulchra
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
# S. arctica
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
# save! 
ggsave("figures/phenology/pheno_2023.png", height = 5, width = 12, dpi = 300)

