# BAYESIAN traits results models -----
# Script by Madi
# Last update: 06/09/2023

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
library(ggeffects)
library(cowplot)

# DATA ----
all_CG_source_traits <- read.csv("data/all_CG_source_traits_2023.csv") # most traits
#all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") # leaf length
all_CG_source_growth <- read.csv("data/common_garden_data_2023/all_data_2023.csv") # 2023 data

# omit one anamonolously higher LMDC value from QHI 2015 
all_CG_source_traits <- all_CG_source_traits %>% 
  filter(LDMC_g_g < 0.76 | is.na(LDMC_g_g)) %>% 
  filter(SLA < 26 | is.na(SLA)) %>% 
  mutate(LDMC_percent = (LDMC_g_g *100)) %>% # change LDMC into percent instead
  mutate(LA_cm2 = (LA/100))

str(all_CG_source_traits)
str(all_CG_source_growth)

# reclass variables 
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$year <- as.factor(all_CG_source_growth$Year)

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
# also filter extreme outliers 
arctica_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix arctica") %>% 
  filter(LDMC_g_g < 0.60 | is.na(LDMC_g_g)) %>% 
  filter(SLA < 24 | is.na(SLA)) 
pulchra_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix pulchra") %>% 
  filter(LDMC_g_g < 0.7000000)
richardsonii_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix richardsonii") %>% 
  filter(SLA < 24 | is.na(SLA)) 

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

model_summ_no_re <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  sigma$nobs <- obs
  
  modelTerms <- as.data.frame(bind_rows(fixed, sigma))  # merge together
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
saveRDS(rich_SLA, file = "output/traits/models/sla_richardsonii_compare.rds")
rich_SLA <- readRDS("output/traits/models/sla_richardsonii_compare.rds")
rich_SLA.pred <- ggpredict(rich_SLA, terms = c('population'))

# extract output with function
rich_SLA_results <- model_summ(rich_SLA)
rich_SLA_results$Species <- "Salix richardsonii"

rich_SLA_results <- rich_SLA_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

rich_SLA_results_2 <- rich_SLA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
rich_SLA_results_2[2,1] <- rich_SLA_results_2[2,1] + rich_SLA_results_2[1,1]
rich_SLA_results_2[3,1] <- rich_SLA_results_2[3,1] + rich_SLA_results_2[1,1]
rich_SLA_results_2[4,1] <- rich_SLA_results_2[4,1] + rich_SLA_results_2[1,1]
# change lower CI by adding 
rich_SLA_results_2[2,3] <- rich_SLA_results_2[2,3] + rich_SLA_results_2[1,3]
rich_SLA_results_2[3,3] <- rich_SLA_results_2[3,3] + rich_SLA_results_2[1,3]
rich_SLA_results_2[4,3] <- rich_SLA_results_2[4,3] + rich_SLA_results_2[1,3]
# change upper CI
rich_SLA_results_2[2,4] <- rich_SLA_results_2[2,4] + rich_SLA_results_2[1,4]
rich_SLA_results_2[3,4] <- rich_SLA_results_2[3,4] + rich_SLA_results_2[1,4]
rich_SLA_results_2[4,4] <- rich_SLA_results_2[4,4] + rich_SLA_results_2[1,4]

# extraction for model output table
rownames(rich_SLA_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(rich_SLA_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

ric_sla_extract_df_1 <- rich_SLA_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

ric_sla_extract_df <- rich_SLA_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

ric_sla_extract_all <- full_join(ric_sla_extract_df_1, ric_sla_extract_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(ric_sla_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation (none are sig diff from each other)
# N. Garden  |     15.28 | [13.42, 17.01]
# N. Source  |     11.63 | [10.37, 13.62]
# S. Source  |     15.62 | [13.90, 17.53]
# S. Garden  |     12.74 | [11.20, 14.08]

# S. pulchra ----
pulchra_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_SLA) 
plot(pulchra_SLA)
pp_check(pulchra_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
saveRDS(pulchra_SLA, file = "output/traits/models/sla_pulchra_compare.rds")
pulchra_SLA <- readRDS("output/traits/models/sla_pulchra_compare.rds")

pul_SLA.pred <- ggpredict(pulchra_SLA, terms = c('population'))

# extract output with function
pulchra_SLA_results <- model_summ(pulchra_SLA)

pulchra_SLA_results <- pulchra_SLA_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

pulchra_SLA_results_2 <- pulchra_SLA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
pulchra_SLA_results_2[2,1] <- pulchra_SLA_results_2[2,1] + pulchra_SLA_results_2[1,1]
pulchra_SLA_results_2[3,1] <- pulchra_SLA_results_2[3,1] + pulchra_SLA_results_2[1,1]
pulchra_SLA_results_2[4,1] <- pulchra_SLA_results_2[4,1] + pulchra_SLA_results_2[1,1]
# change lower CI by adding 
pulchra_SLA_results_2[2,3] <- pulchra_SLA_results_2[2,3] + pulchra_SLA_results_2[1,3]
pulchra_SLA_results_2[3,3] <- pulchra_SLA_results_2[3,3] + pulchra_SLA_results_2[1,3]
pulchra_SLA_results_2[4,3] <- pulchra_SLA_results_2[4,3] + pulchra_SLA_results_2[1,3]
# change upper CI
pulchra_SLA_results_2[2,4] <- pulchra_SLA_results_2[2,4] + pulchra_SLA_results_2[1,4]
pulchra_SLA_results_2[3,4] <- pulchra_SLA_results_2[3,4] + pulchra_SLA_results_2[1,4]
pulchra_SLA_results_2[4,4] <- pulchra_SLA_results_2[4,4] + pulchra_SLA_results_2[1,4]

# extraction for model output table
rownames(pulchra_SLA_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(pulchra_SLA_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

pul_sla_extract_df_1 <- pulchra_SLA_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_sla_extract_df <- pulchra_SLA_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_sla_extract_all <- full_join(pul_sla_extract_df_1, pul_sla_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(pul_sla_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation 
# N. Garden  |     15.69 | [14.08, 17.19] 
# N. Source  |     10.19 | [ 9.28, 11.32] **
# S. Source  |     15.60 | [14.18, 17.44]
# S. Garden  |     12.39 | [11.21, 13.46] *

# S. arctica ----
arctica_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_SLA) 
plot(arctica_SLA)
pp_check(arctica_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
saveRDS(arctica_SLA, file = "output/traits/models/sla_arctica_compare.rds")
arctica_SLA <- readRDS("output/traits/models/sla_arctica_compare.rds")
arc_SLA.pred <- ggpredict(arctica_SLA, terms = c('population'))

# extract output with function
arctica_SLA_results <- model_summ(arctica_SLA)

arctica_SLA_results <- arctica_SLA_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

arctica_SLA_results_2 <- arctica_SLA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
arctica_SLA_results_2[2,1] <- arctica_SLA_results_2[2,1] + arctica_SLA_results_2[1,1]
arctica_SLA_results_2[3,1] <- arctica_SLA_results_2[3,1] + arctica_SLA_results_2[1,1]
arctica_SLA_results_2[4,1] <- arctica_SLA_results_2[4,1] + arctica_SLA_results_2[1,1]
# change lower CI by adding 
arctica_SLA_results_2[2,3] <- arctica_SLA_results_2[2,3] + arctica_SLA_results_2[1,3]
arctica_SLA_results_2[3,3] <- arctica_SLA_results_2[3,3] + arctica_SLA_results_2[1,3]
arctica_SLA_results_2[4,3] <- arctica_SLA_results_2[4,3] + arctica_SLA_results_2[1,3]
# change upper CI
arctica_SLA_results_2[2,4] <- arctica_SLA_results_2[2,4] + arctica_SLA_results_2[1,4]
arctica_SLA_results_2[3,4] <- arctica_SLA_results_2[3,4] + arctica_SLA_results_2[1,4]
arctica_SLA_results_2[4,4] <- arctica_SLA_results_2[4,4] + arctica_SLA_results_2[1,4]

# extraction for model output table
rownames(arctica_SLA_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(arctica_SLA_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

arc_sla_extract_df_1 <- arctica_SLA_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_sla_extract_df <- arctica_SLA_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_sla_extract_all <- full_join(arc_sla_extract_df_1, arc_sla_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(arc_sla_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation (none sig diff)
# N. Garden  |     11.69 | [ 8.81, 14.52]
# N. Source  |     12.06 | [ 9.42, 15.58]
# S. Source  |     13.77 | [10.60, 17.69]
# S. Garden  |     11.08 | [ 8.36, 14.00]

# merging all extracted outputs

# merging all extracted outputs
garden_sla_out <- rbind(ric_sla_extract_all, pul_sla_extract_all, 
                        arc_sla_extract_all) 

garden_sla_out <- garden_sla_out %>%
  dplyr::rename("Estimate_log_sum" = "Estimate (log sum)")

# back transforming from log
garden_sla_out_back <- garden_sla_out %>%
  mutate(CI_low_trans = exp(l_95_CI_log_sum)) %>% 
  mutate(CI_high_trans = exp(u_95_CI_log_sum)) %>% 
  mutate(Estimate_trans = exp(Estimate_log_sum))%>%
  relocate(CI_low_trans, .before = Rhat) %>%
  relocate(CI_high_trans, .before = Rhat) %>%
  relocate(Estimate_trans, .before = CI_low_trans)%>%
  relocate(Estimate_log_sum, .before = Estimate_trans) %>%
  relocate(l_95_CI_log_sum, .before = Estimate_trans) %>%
  relocate(u_95_CI_log_sum, .before = Estimate_trans)

# save df of results 
write.csv(garden_sla_out_back, "output/traits/garden_SLA_out_back.csv")
garden_sla_out_back <- read.csv("output/traits/garden_SLA_out_back.csv", row.names = 1)

# making sure Rhat keeps the .00 
garden_sla_out_back$Rhat <- as.character(formatC(garden_sla_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_sla <- garden_sla_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: specific leaf area of northern garden, northern source, sourthern garden and southern source populations. 
      Log transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate (log)",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)",
                     "Estimate (log sum)",  
                     "Lower 95% CI 
                    (log sum)", 
                    "Upper 95% CI (log sum)",  
                     "Estimate (transformed)","Lower 95% CI 
                    (transformed)", 
                    "Upper 95% CI (transformed)",
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
row_spec(kable_sla, 1:12, align = "c") 
column_spec(kable_sla, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_sla,file = "output/traits/kable_sla.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# LDMC ----
# S. richardsonii ----
rich_LDMC_log <- brms::brm(log(LDMC_percent) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                           iter = 3000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99)) # 2 divergent transitions after warmup
summary(rich_LDMC_log)
tab_model(rich_LDMC_log)
plot(rich_LDMC_log)
pp_check(rich_LDMC_log, type = "dens_overlay", ndraws = 100) 
saveRDS(rich_LDMC_log, file = "output/traits/models/ldmc_richardsonii_compare.rds")
rich_LDMC_log <- readRDS("output/traits/models/ldmc_richardsonii_compare.rds")
rich_LDMC.pred <- ggpredict(rich_LDMC_log, terms = c('population'))

# extract output with function
rich_LDMC_results <- model_summ(rich_LDMC_log)

rich_LDMC_results <- rich_LDMC_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

rich_LDMC_results_2 <- rich_LDMC_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
rich_LDMC_results_2[2,1] <- rich_LDMC_results_2[2,1] + rich_LDMC_results_2[1,1]
rich_LDMC_results_2[3,1] <- rich_LDMC_results_2[3,1] + rich_LDMC_results_2[1,1]
rich_LDMC_results_2[4,1] <- rich_LDMC_results_2[4,1] + rich_LDMC_results_2[1,1]
# change lower CI by adding 
rich_LDMC_results_2[2,3] <- rich_LDMC_results_2[2,3] + rich_LDMC_results_2[1,3]
rich_LDMC_results_2[3,3] <- rich_LDMC_results_2[3,3] + rich_LDMC_results_2[1,3]
rich_LDMC_results_2[4,3] <- rich_LDMC_results_2[4,3] + rich_LDMC_results_2[1,3]
# change upper CI
rich_LDMC_results_2[2,4] <- rich_LDMC_results_2[2,4] + rich_LDMC_results_2[1,4]
rich_LDMC_results_2[3,4] <- rich_LDMC_results_2[3,4] + rich_LDMC_results_2[1,4]
rich_LDMC_results_2[4,4] <- rich_LDMC_results_2[4,4] + rich_LDMC_results_2[1,4]

# extraction for model output table
rownames(rich_LDMC_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(rich_LDMC_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

ric_ldmc_extract_df_1 <- rich_LDMC_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

ric_ldmc_extract_df <- rich_LDMC_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_ldmc_extract_all <- full_join(ric_ldmc_extract_df_1, ric_ldmc_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(rich_ldmc_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation (none sig diff)
# N. Garden  |     28.99 | [23.89, 36.58]
# N. Source  |     33.16 | [24.01, 40.97]
# S. Source  |     27.94 | [22.83, 35.55]
# S. Garden  |     32.07 | [26.58, 40.48] 

# S. pulchra ----
pulchra_LDMC_log <- brms::brm(log(LDMC_percent) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(pulchra_LDMC_log) 
plot(pulchra_LDMC_log)
pp_check(pulchra_LDMC_log, type = "dens_overlay", ndraws = 100) 
saveRDS(pulchra_LDMC_log, file = "output/traits/models/ldmc_pulchra_compare.rds")
pulchra_LDMC_log <- readRDS("output/traits/models/ldmc_pulchra_compare.rds")
pul_LDMC.pred <- ggpredict(pulchra_LDMC_log, terms = c('population'))

# extract output with function
pulchra_LDMC_results <- model_summ(pulchra_LDMC_log)

pulchra_LDMC_results <- pulchra_LDMC_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

pulchra_LDMC_results_2 <- pulchra_LDMC_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
pulchra_LDMC_results_2[2,1] <- pulchra_LDMC_results_2[2,1] + pulchra_LDMC_results_2[1,1]
pulchra_LDMC_results_2[3,1] <- pulchra_LDMC_results_2[3,1] + pulchra_LDMC_results_2[1,1]
pulchra_LDMC_results_2[4,1] <- pulchra_LDMC_results_2[4,1] + pulchra_LDMC_results_2[1,1]
# change lower CI by adding 
pulchra_LDMC_results_2[2,3] <- pulchra_LDMC_results_2[2,3] + pulchra_LDMC_results_2[1,3]
pulchra_LDMC_results_2[3,3] <- pulchra_LDMC_results_2[3,3] + pulchra_LDMC_results_2[1,3]
pulchra_LDMC_results_2[4,3] <- pulchra_LDMC_results_2[4,3] + pulchra_LDMC_results_2[1,3]
# change upper CI
pulchra_LDMC_results_2[2,4] <- pulchra_LDMC_results_2[2,4] + pulchra_LDMC_results_2[1,4]
pulchra_LDMC_results_2[3,4] <- pulchra_LDMC_results_2[3,4] + pulchra_LDMC_results_2[1,4]
pulchra_LDMC_results_2[4,4] <- pulchra_LDMC_results_2[4,4] + pulchra_LDMC_results_2[1,4]

# extraction for model output table
rownames(pulchra_LDMC_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(pulchra_LDMC_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

pul_ldmc_extract_df_1 <- pulchra_LDMC_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_ldmc_extract_df <- pulchra_LDMC_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_ldmc_extract_all <- full_join(pul_ldmc_extract_df_1, pul_ldmc_extract_df, 
                                   by = c("effect" = "effect", "nobs"="nobs",
                                          "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                          "Species"="Species", "Rhat"="Rhat"))

rownames(pul_ldmc_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation (none sig diff)
# N. Garden  |     35.84 | [27.35, 46.86]
# N. Source  |     44.61 | [31.12, 58.22]
# S. Source  |     32.20 | [24.41, 42.84]
# S. Garden  |     36.86 | [28.26, 48.09]

# S. arctica ---- 
# not log transforming bc that gives divergent transistions 
arctica_LDMC <- brms::brm(LDMC_percent ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(arctica_LDMC)
plot(arctica_LDMC)
pp_check(arctica_LDMC, type = "dens_overlay", ndraws = 100) 
saveRDS(arctica_LDMC, file = "output/traits/models/ldmc_arctica_compare.rds")
arctica_LDMC <- readRDS("output/traits/models/ldmc_arctica_compare.rds")
arc_LDMC.pred <- ggpredict(arctica_LDMC, terms = c('population'))

# extract output with function
arctica_LDMC_results <- model_summ(arctica_LDMC)

arctica_LDMC_results <- arctica_LDMC_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

arctica_LDMC_results_2 <- arctica_LDMC_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
arctica_LDMC_results_2[2,1] <- arctica_LDMC_results_2[2,1] + arctica_LDMC_results_2[1,1]
arctica_LDMC_results_2[3,1] <- arctica_LDMC_results_2[3,1] + arctica_LDMC_results_2[1,1]
arctica_LDMC_results_2[4,1] <- arctica_LDMC_results_2[4,1] + arctica_LDMC_results_2[1,1]
# change lower CI by adding 
arctica_LDMC_results_2[2,3] <- arctica_LDMC_results_2[2,3] + arctica_LDMC_results_2[1,3]
arctica_LDMC_results_2[3,3] <- arctica_LDMC_results_2[3,3] + arctica_LDMC_results_2[1,3]
arctica_LDMC_results_2[4,3] <- arctica_LDMC_results_2[4,3] + arctica_LDMC_results_2[1,3]
# change upper CI
arctica_LDMC_results_2[2,4] <- arctica_LDMC_results_2[2,4] + arctica_LDMC_results_2[1,4]
arctica_LDMC_results_2[3,4] <- arctica_LDMC_results_2[3,4] + arctica_LDMC_results_2[1,4]
arctica_LDMC_results_2[4,4] <- arctica_LDMC_results_2[4,4] + arctica_LDMC_results_2[1,4]

# extraction for model output table
rownames(arctica_LDMC_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(arctica_LDMC_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

arc_ldmc_extract_df_1 <- arctica_LDMC_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_ldmc_extract_df <- arctica_LDMC_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_ldmc_extract_all <- full_join(pul_ldmc_extract_df_1, pul_ldmc_extract_df, 
                                  by = c("effect" = "effect", "nobs"="nobs",
                                         "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                         "Species"="Species", "Rhat"="Rhat"))

rownames(arc_ldmc_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year ", "Sigma")

# interpretation (none sig diff)
# N. Garden  |     31.41 | [23.73, 42.69]
# N. Source  |     34.13 | [24.15, 44.57]
# S. Source  |     27.42 | [20.29, 37.62]
# S. Garden  |     32.31 | [24.22, 43.89]

# merging all extracted outputs
garden_ldmc_out <- rbind(rich_ldmc_extract_all, pul_ldmc_extract_all, 
                        arc_ldmc_extract_all) 

garden_ldmc_out <- garden_ldmc_out %>%
  dplyr::rename("Estimate_log_sum" = "Estimate (log sum)")

# back transforming from log
garden_ldmc_out_back <- garden_ldmc_out %>%
  mutate(CI_low_trans = exp(l_95_CI_log_sum)) %>% 
  mutate(CI_high_trans = exp(u_95_CI_log_sum)) %>% 
  mutate(Estimate_trans = exp(Estimate_log_sum))%>%
  relocate(CI_low_trans, .before = Rhat) %>%
  relocate(CI_high_trans, .before = Rhat) %>%
  relocate(Estimate_trans, .before = CI_low_trans)%>%
  relocate(Estimate_log_sum, .before = Estimate_trans) %>%
  relocate(l_95_CI_log_sum, .before = Estimate_trans) %>%
  relocate(u_95_CI_log_sum, .before = Estimate_trans)

# save df of results 
write.csv(garden_ldmc_out_back, "output/traits/garden_LDMC_out_back.csv")
garden_ldmc_out_back <- read.csv("output/traits/garden_LDMC_out_back.csv", row.names = 1)

# making sure Rhat keeps the .00 
garden_ldmc_out_back$Rhat <- as.character(formatC(garden_ldmc_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_ldmc <- garden_ldmc_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: leaf dry matter content by northern garden, northern source, sourthern garden and southern source populations. 
      Log transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate (log)",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)",
                     "Estimate (log sum)",  
                     "Lower 95% CI 
                    (log sum)", 
                     "Upper 95% CI (log sum)",  
                     "Estimate (transformed)","Lower 95% CI 
                    (transformed)", 
                     "Upper 95% CI (transformed)",
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
row_spec(kable_ldmc, 1:12, align = "c") 
column_spec(kable_ldmc, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_ldmc, file = "output/traits/kable_ldmc.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# LA ----
# S. richardsonii ----
# running models with only LA from Madi's 2021, 2022, 2023 measurements because 2015 and 2017 are sketchy 
richardsonii_mad_traits <- richardsonii_all_traits %>% 
  filter(year %in% c(2021, 2022, 2023)) %>% 
  filter(LA_cm2 <140)
hist(richardsonii_mad_traits$LA_cm2) 

rich_LA_mad <- brms::brm((LA_cm2) ~ population + (1|year), data = richardsonii_mad_traits, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000, 
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LA_mad)
plot(rich_LA_mad)
pp_check(rich_LA_mad, type = "dens_overlay", ndraws = 100)
saveRDS(rich_LA_mad, file = "output/traits/models/la_richardsonii_compare.rds")
rich_LA_mad <- readRDS("output/traits/models/la_richardsonii_compare.rds")
rich_LA_mad.pred <- ggpredict(rich_LA_mad, terms = c('population'))

#rich_LA <- brms::brm(log(LA_cm2) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
#                     iter = 3000, warmup = 1000, 
#                     control = list(max_treedepth = 15, adapt_delta = 0.99))
#summary(rich_LA) 
#plot(rich_LA)
#pp_check(rich_LA, type = "dens_overlay", ndraws = 100)
#saveRDS(rich_LA, file = "output/traits/models/la_richardsonii_compare.rds")
#rich_LA <- readRDS("output/traits/models/la_richardsonii_compare.rds")
#rich_LA.pred <- ggpredict(rich_LA, terms = c('population'))

# extract output with function
rich_LA_results <- model_summ(rich_LA_mad)

rich_LA_results <- rich_LA_results %>% 
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI")

rich_LA_results_2 <- rich_LA_results %>% 
  dplyr::rename("l_95_CI_sum" = "l_95_CI", 
                "u_95_CI_sum" = "u_95_CI",
                "Estimate (sum)"= "Estimate")

# change estimates by adding estimate to other rows 
rich_LA_results_2[2,1] <- rich_LA_results_2[2,1] + rich_LA_results_2[1,1]
rich_LA_results_2[3,1] <- rich_LA_results_2[3,1] + rich_LA_results_2[1,1]
rich_LA_results_2[4,1] <- rich_LA_results_2[4,1] + rich_LA_results_2[1,1]
# change lower CI by adding 
rich_LA_results_2[2,3] <- rich_LA_results_2[2,3] + rich_LA_results_2[1,3]
rich_LA_results_2[3,3] <- rich_LA_results_2[3,3] + rich_LA_results_2[1,3]
rich_LA_results_2[4,3] <- rich_LA_results_2[4,3] + rich_LA_results_2[1,3]
# change upper CI
rich_LA_results_2[2,4] <- rich_LA_results_2[2,4] + rich_LA_results_2[1,4]
rich_LA_results_2[3,4] <- rich_LA_results_2[3,4] + rich_LA_results_2[1,4]
rich_LA_results_2[4,4] <- rich_LA_results_2[4,4] + rich_LA_results_2[1,4]

# extraction for model output table
rownames(rich_LA_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(rich_LA_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

rich_la_extract_df_1 <- rich_LA_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_la_extract_df <- rich_LA_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_la_extract_all <- full_join(rich_la_extract_df_1, rich_la_extract_df, 
                                  by = c("effect" = "effect", "nobs"="nobs",
                                         "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                         "Species"="Species", "Rhat"="Rhat"))

rownames(rich_la_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# S. pulchra ----
# running models with only LA from Madi's 2021, 2022, 2023 measurements because 2015 and 2017 are sketchy 
pulchra_mad_traits <- pulchra_all_traits %>% 
  filter(year %in% c(2021, 2022, 2023))
hist(pulchra_mad_traits$LA_cm2) 
pulchra_mad_traits$LA_cm2_log <- log(pulchra_mad_traits$LA_cm2)
hist(pulchra_mad_traits$LA_cm2_log) # meh 

pulchra_LA_mad <- brms::brm((LA_cm2) ~ population + (1|year), data = pulchra_mad_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
pulchra_LA.mad.pred <- ggpredict(rich_LA_mad, terms = c('population'))
summary(pulchra_LA_mad)
plot(pulchra_LA_mad)
pp_check(pulchra_LA_mad, type = "dens_overlay", ndraws = 100) # okay
pulchra_LA.pred <- ggpredict(pulchra_LA_mad, terms = c('population'))


#pulchra_LA <- brms::brm(log(LA_cm2) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
#                        iter = 3000, warmup = 1000, 
#                        control = list(max_treedepth = 15, adapt_delta = 0.99))
#summary(pulchra_LA) 
#plot(pulchra_LA)
#pp_check(pulchra_LA, type = "dens_overlay", ndraws = 100) 
#saveRDS(pulchra_LA, file = "output/traits/models/la_pulchra_compare.rds")
#pulchra_LA <- readRDS("output/traits/models/la_pulchra_compare.rds")
#pulchra_LA.pred <- ggpredict(pulchra_LA, terms = c('population'))

# extract output with function
pulchra_LA_results <- model_summ(pulchra_LA_mad)

pulchra_LA_results <- pulchra_LA_results %>% 
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI")

pulchra_LA_results_2 <- pulchra_LA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI", 
                "u_95_CI_log_sum" = "u_95_CI",
                "Estimate (sum)"= "Estimate")

# change estimates by adding estimate to other rows 
pulchra_LA_results_2[2,1] <- pulchra_LA_results_2[2,1] + pulchra_LA_results_2[1,1]
pulchra_LA_results_2[3,1] <- pulchra_LA_results_2[3,1] + pulchra_LA_results_2[1,1]
pulchra_LA_results_2[4,1] <- pulchra_LA_results_2[4,1] + pulchra_LA_results_2[1,1]
# change lower CI by adding 
pulchra_LA_results_2[2,3] <- pulchra_LA_results_2[2,3] + pulchra_LA_results_2[1,3]
pulchra_LA_results_2[3,3] <- pulchra_LA_results_2[3,3] + pulchra_LA_results_2[1,3]
pulchra_LA_results_2[4,3] <- pulchra_LA_results_2[4,3] + pulchra_LA_results_2[1,3]
# change upper CI
pulchra_LA_results_2[2,4] <- pulchra_LA_results_2[2,4] + pulchra_LA_results_2[1,4]
pulchra_LA_results_2[3,4] <- pulchra_LA_results_2[3,4] + pulchra_LA_results_2[1,4]
pulchra_LA_results_2[4,4] <- pulchra_LA_results_2[4,4] + pulchra_LA_results_2[1,4]

# extraction for model output table
rownames(pulchra_LA_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(pulchra_LA_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year  ", "Sigma ")

pul_la_extract_df_1 <- pulchra_LA_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_la_extract_df <- pulchra_LA_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_la_extract_all <- full_join(pul_la_extract_df_1, pul_la_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(pul_la_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# S. arctica ----
arctica_mad_traits <- arctica_all_traits %>% 
  filter(year %in% c(2021, 2022, 2023))
hist(arctica_mad_traits$LA_cm2) 
arctica_mad_traits$LA_cm2_log <- log(arctica_mad_traits$LA_cm2)
hist(arctica_mad_traits$LA_cm2_log)
# no year as random effect because limited data 
arctica_mad_LA <- brms::brm((LA_cm2)  ~ population, data = arctica_mad_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_mad_LA)
plot(arctica_mad_LA)
pp_check(arctica_mad_LA, type = "dens_overlay", ndraws = 100) 
saveRDS(arctica_mad_LA, file = "output/traits/models/la_arctica_compare.rds")
arctica_mad_LA <- readRDS("output/traits/models/la_arctica_compare.rds")
arctica_LA.pred <- ggpredict(arctica_mad_LA, terms = c('population'))

#arctica_LA <- brms::brm(log(LA_cm2)  ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
#                          iter = 3000, warmup = 1000, 
#                        control = list(max_treedepth = 15, adapt_delta = 0.99))
#summary(arctica_LA) 
#plot(arctica_LA)
#pp_check(arctica_LA, type = "dens_overlay", ndraws = 100) 
#saveRDS(arctica_LA, file = "output/traits/models/la_arctica_compare.rds")
#arctica_LA <- readRDS("output/traits/models/la_arctica_compare.rds")
#arctica_LA.pred <- ggpredict(arctica_LA, terms = c('population'))

# extract output with function
arctica_LA_results <- model_summ_no_re(arctica_mad_LA)

arctica_LA_results <- arctica_LA_results %>% 
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI")

arctica_LA_results_2 <- arctica_LA_results %>% 
  dplyr::rename("l_95_CI_sum" = "l_95_CI", 
                "u_95_CI_sum" = "u_95_CI", 
                "Estimate (sum)" = "Estimate")

# change estimates by adding estimate to other rows 
arctica_LA_results_2[2,1] <- arctica_LA_results_2[2,1] + arctica_LA_results_2[1,1]
arctica_LA_results_2[3,1] <- arctica_LA_results_2[3,1] + arctica_LA_results_2[1,1]
arctica_LA_results_2[4,1] <- arctica_LA_results_2[4,1] + arctica_LA_results_2[1,1]
# change lower CI by adding 
arctica_LA_results_2[2,3] <- arctica_LA_results_2[2,3] + arctica_LA_results_2[1,3]
arctica_LA_results_2[3,3] <- arctica_LA_results_2[3,3] + arctica_LA_results_2[1,3]
arctica_LA_results_2[4,3] <- arctica_LA_results_2[4,3] + arctica_LA_results_2[1,3]
# change upper CI
arctica_LA_results_2[2,4] <- arctica_LA_results_2[2,4] + arctica_LA_results_2[1,4]
arctica_LA_results_2[3,4] <- arctica_LA_results_2[3,4] + arctica_LA_results_2[1,4]
arctica_LA_results_2[4,4] <- arctica_LA_results_2[4,4] + arctica_LA_results_2[1,4]

# extraction for model output table
rownames(arctica_LA_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ",  "Sigma  ")
rownames(arctica_LA_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Sigma ")

arc_la_extract_df_1 <- arctica_LA_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_la_extract_df <- arctica_LA_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_la_extract_all <- full_join(arc_la_extract_df_1, arc_la_extract_df, 
                                by = c("effect" = "effect", "nobs"="nobs",
                                       "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                       "Species"="Species", "Rhat"="Rhat"))

rownames(arc_la_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Sigma")

# merging all extracted outputs

# merging all extracted outputs
garden_LA_out <- rbind(rich_la_extract_all, pul_la_extract_all, 
                         arc_la_extract_all) 

garden_LA_out <- garden_LA_out %>%
  dplyr::rename("Estimate_sum" = "Estimate (sum)")

# back transforming from log
garden_la_out_back <- garden_LA_out %>%
  mutate(CI_low_trans = exp(l_95_CI_log_sum)) %>% 
  mutate(CI_high_trans = exp(u_95_CI_log_sum)) %>% 
  mutate(Estimate_trans = exp(Estimate_log_sum))%>%
  relocate(CI_low_trans, .before = Rhat) %>%
  relocate(CI_high_trans, .before = Rhat) %>%
  relocate(Estimate_trans, .before = CI_low_trans)%>%
  relocate(Estimate_log_sum, .before = Estimate_trans) %>%
  relocate(l_95_CI_log_sum, .before = Estimate_trans) %>%
  relocate(u_95_CI_log_sum, .before = Estimate_trans)

# save df of results 
write.csv(garden_la_out_back, "output/traits/garden_LA_out_back.csv")
garden_la_out_back <- read.csv("output/traits/garden_LA_out_back.csv", row.names = 1)

# making sure Rhat keeps the .00 
garden_la_out_back$Rhat <- as.character(formatC(garden_la_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_la <- garden_la_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: leaf area by northern garden, northern source, sourthern garden and southern source populations. 
      Log transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate (log)",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)",
                     "Estimate (log sum)",  
                     "Lower 95% CI 
                    (log sum)", 
                     "Upper 95% CI (log sum)",  
                     "Estimate (transformed)","Lower 95% CI 
                    (transformed)", 
                     "Upper 95% CI (transformed)",
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
row_spec(kable_la, 1:12, align = "c") 
column_spec(kable_la, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_la, file = "output/traits/kable_la.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# LEAF LENGTH ----
# S. richardsonii ----
rich_LL <- brms::brm(mean_leaf_length ~ population + (1|year), data = richardsonii_all_growth, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000, 
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LL)
plot(rich_LL)
pp_check(rich_LL, type = "dens_overlay", ndraws = 100) 
saveRDS(rich_LL, file = "output/traits/models/ll_richardsonii_compare.rds")
rich_LL <- readRDS("output/traits/models/ll_richardsonii_compare.rds")
rich_LL.pred <- ggpredict(rich_LL, terms = c('population'))
# extract output with function
rich_LL_results <- model_summ(rich_LL)

rich_LL_results <- rich_LL_results %>% 
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI", 
                "Estimate"= "Estimate")

rich_LL_results_2 <- rich_LL_results %>% 
  dplyr::rename("l_95_CI_sum" = "l_95_CI", 
                "u_95_CI_sum" = "u_95_CI",
                "Estimate (sum)"= "Estimate")

# change estimates by adding estimate to other rows 
rich_LL_results_2[2,1] <- rich_LL_results_2[2,1] + rich_LL_results_2[1,1]
rich_LL_results_2[3,1] <- rich_LL_results_2[3,1] + rich_LL_results_2[1,1]
rich_LL_results_2[4,1] <- rich_LL_results_2[4,1] + rich_LL_results_2[1,1]
# change lower CI by adding 
rich_LL_results_2[2,3] <- rich_LL_results_2[2,3] + rich_LL_results_2[1,3]
rich_LL_results_2[3,3] <- rich_LL_results_2[3,3] + rich_LL_results_2[1,3]
rich_LL_results_2[4,3] <- rich_LL_results_2[4,3] + rich_LL_results_2[1,3]
# change upper CI
rich_LL_results_2[2,4] <- rich_LL_results_2[2,4] + rich_LL_results_2[1,4]
rich_LL_results_2[3,4] <- rich_LL_results_2[3,4] + rich_LL_results_2[1,4]
rich_LL_results_2[4,4] <- rich_LL_results_2[4,4] + rich_LL_results_2[1,4]

# extraction for model output table
rownames(rich_LL_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(rich_LL_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

rich_ll_extract_df_1 <- rich_LL_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_ll_extract_df <- rich_LL_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_ll_extract_all <- full_join(rich_ll_extract_df_1, rich_ll_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(rich_ll_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation 
# N. Garden  |     22.86 | [16.60, 28.71] *
# N. Source  |     42.54 | [35.36, 49.41] **
# S. Source  |     51.45 | [44.59, 58.10] **
# S. Garden  |     40.68 | [34.35, 46.41] **

# S. pulchra ----
pulchra_LL <- brms::brm(mean_leaf_length ~ population + (1|year), data = pulchra_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(pulchra_LL)
plot(pulchra_LL)
pp_check(pulchra_LL, type = "dens_overlay", ndraws = 100) 
saveRDS(pulchra_LL, file = "output/traits/models/ll_pulchra_compare.rds")
pulchra_LL <- readRDS("output/traits/models/ll_pulchra_compare.rds")
pul_LL.pred <- ggpredict(pulchra_LL, terms = c('population'))

# extract output with function
pul_LL_results <- model_summ(pulchra_LL)

pul_LL_results <- pul_LL_results %>% 
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI", 
                "Estimate"= "Estimate")

pul_LL_results_2 <- pul_LL_results %>% 
  dplyr::rename("l_95_CI_sum" = "l_95_CI", 
                "u_95_CI_sum" = "u_95_CI",
                "Estimate (sum)"= "Estimate")

# change estimates by adding estimate to other rows 
pul_LL_results_2[2,1] <- pul_LL_results_2[2,1] + pul_LL_results_2[1,1]
pul_LL_results_2[3,1] <- pul_LL_results_2[3,1] + pul_LL_results_2[1,1]
pul_LL_results_2[4,1] <- pul_LL_results_2[4,1] + pul_LL_results_2[1,1]
# change lower CI by adding 
pul_LL_results_2[2,3] <- pul_LL_results_2[2,3] + pul_LL_results_2[1,3]
pul_LL_results_2[3,3] <- pul_LL_results_2[3,3] + pul_LL_results_2[1,3]
pul_LL_results_2[4,3] <- pul_LL_results_2[4,3] + pul_LL_results_2[1,3]
# change upper CI
pul_LL_results_2[2,4] <- pul_LL_results_2[2,4] + pul_LL_results_2[1,4]
pul_LL_results_2[3,4] <- pul_LL_results_2[3,4] + pul_LL_results_2[1,4]
pul_LL_results_2[4,4] <- pul_LL_results_2[4,4] + pul_LL_results_2[1,4]

# extraction for model output table
rownames(pul_LL_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(pul_LL_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

pul_ll_extract_df_1 <- pul_LL_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_ll_extract_df <- pul_LL_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_ll_extract_all <- full_join(pul_ll_extract_df_1, pul_ll_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(pul_ll_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")

# interpretation 
# N. Garden  |     20.66 | [14.65, 27.20]
# N. Source  |     35.57 | [28.43, 42.81]
# S. Source  |     50.80 | [43.92, 58.00]
# S. Garden  |     36.53 | [30.53, 43.07]

# S. arctica ----
# no leaf length for S. arctic from source pop
# make common garden only model 
#arctica_cg_growth <- arctica_all_growth %>% 
#  filter(population %in% c("N. Garden", "S. Garden"))

arctica_LL_CG <- brms::brm((mean_leaf_length) ~ population + (1|year) + (1|SampleID_standard), data = arctica_all_growth, family = gaussian(), chains = 3,
                           iter = 3000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LL_CG)
plot(arctica_LL_CG)
pp_check(arctica_LL_CG, type = "dens_overlay", ndraws = 100)
saveRDS(arctica_LL_CG, file = "output/traits/models/ll_arctica_compare.rds")
arctica_LL_CG <- readRDS("output/traits/models/ll_arctica_compare.rds")
arc_LL.pred <- ggpredict(arctica_LL_CG, terms = c('population'))

# extract output with function
arc_LL_results <- model_summ(arctica_LL_CG)

arc_LL_results <- arc_LL_results %>% 
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI", 
                "Estimate"= "Estimate")

arc_LL_results_2 <- arc_LL_results %>% 
  dplyr::rename("l_95_CI_sum" = "l_95_CI", 
                "u_95_CI_sum" = "u_95_CI",
                "Estimate (sum)"= "Estimate")

# change estimates by adding estimate to other rows 
arc_LL_results_2[2,1] <- arc_LL_results_2[2,1] + arc_LL_results_2[1,1]
arc_LL_results_2[3,1] <- arc_LL_results_2[3,1] + arc_LL_results_2[1,1]
arc_LL_results_2[4,1] <- arc_LL_results_2[4,1] + arc_LL_results_2[1,1]
# change lower CI by adding 
arc_LL_results_2[2,3] <- arc_LL_results_2[2,3] + arc_LL_results_2[1,3]
arc_LL_results_2[3,3] <- arc_LL_results_2[3,3] + arc_LL_results_2[1,3]
arc_LL_results_2[4,3] <- arc_LL_results_2[4,3] + arc_LL_results_2[1,3]
# change upper CI
arc_LL_results_2[2,4] <- arc_LL_results_2[2,4] + arc_LL_results_2[1,4]
arc_LL_results_2[3,4] <- arc_LL_results_2[3,4] + arc_LL_results_2[1,4]
arc_LL_results_2[4,4] <- arc_LL_results_2[4,4] + arc_LL_results_2[1,4]

# extraction for model output table
rownames(arc_LL_results) <- c("Intercept  ", "Northern Source ", "Southern Source  ", "Southern Garden ", "Year  ", "Sigma  ")
rownames(arc_LL_results_2) <- c("Intercept ", "Northern Source ", "Southern Source ", "Southern Garden ", "Year ", "Sigma ")

arc_ll_extract_df_1 <- arc_LL_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_ll_extract_df <- arc_LL_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_ll_extract_all <- full_join(arc_ll_extract_df_1, arc_ll_extract_df, 
                                by = c("effect" = "effect", "nobs"="nobs",
                                       "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                       "Species"="Species", "Rhat"="Rhat"))

rownames(arc_ll_extract_all) <- c("Intercept", "Northern Source", "Southern Source", "Southern Garden", "Year", "Sigma")
# interpretation 
# N. Garden  |     25.64 | [17.75, 33.88]
# N. Source  |     30.77 | [21.08, 40.40]
# sS. Source  |     40.49 | [30.81, 50.90]
# S. Garden  |     28.68 | [20.78, 36.85]

# merging all extracted outputs
garden_LL_out <- rbind(rich_ll_extract_all, pul_ll_extract_all, arc_ll_extract_all)

garden_LL_out <- garden_LL_out %>%
  dplyr::rename("Estimate_sum" = "Estimate (sum)", 
                "lower_CI" = "l_95_CI",
                "upper_CI" = "u_95_CI", 
                "lower_CI_trans" = "l_95_CI_sum", 
                "upper_CI_trans" = "u_95_CI_sum")

# back transforming from 
garden_ll_out_back <- garden_LL_out %>% 
  relocate(lower_CI, .before = Rhat) %>%
  relocate(upper_CI, .before = Rhat) %>%
  relocate(Estimate, .before = lower_CI)
# save df of results 
write.csv(garden_ll_out_back, "output/traits/garden_LL_out_back.csv")
garden_ll_out_back <- read.csv("output/traits/garden_LL_out_back.csv", row.names = 1)

# making sure Rhat keeps the .00 
garden_ll_out_back$Rhat <- as.character(formatC(garden_ll_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_ll <- garden_ll_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: leaf length in northern garden, northern source, sourthern garden and southern source populations. 
      Log transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate",
                     "Lower 95% CI",
                     "Upper 95% CI",
                     "Rhat",
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect", 
                     "Estimate (sum)",  
                     "Lower 95% CI (sum)", 
                     "Upper 95% CI (sum)"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in italics
row_spec(kable_ll, 1:12, align = "c") 
column_spec(kable_ll, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_ll, file = "output/traits/kable_ll.pdf",
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
richardsonii_mad_traits$population <-  ordered(pulchra_all_traits$population, 
                                           levels = c("N. Source", 
                                                      "N. Garden", 
                                                      "S. Source",  
                                                      "S. Garden"))

pulchra_mad_traits$population <-  ordered(pulchra_all_traits$population, 
                                               levels = c("N. Source", 
                                                          "N. Garden", 
                                                          "S. Source",  
                                                          "S. Garden"))
arctica_mad_traits$population <-  ordered(pulchra_all_traits$population, 
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
                                 axis.title.x = element_text(face="bold", family = "Helvetica Light", size=20),
                                 axis.text.x  = element_text(vjust=0.5, size=20, family = "Helvetica Light", colour = "black", angle = 270), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=20),
                                 axis.text.y  = element_text(vjust=0.5, size=20, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 20, family = "Helvetica Light", face = "italic", hjust = 0.5),
                                 legend.title=element_text(size=16, family = "Helvetica Light"),
                                 legend.text=element_text(size = 15, family = "Helvetica Light"))}
# SLA ---- 
# richardsonii ----
richard_sla <- (conditional_effects(rich_SLA)) # extracting conditional effects from bayesian model
richard_sla_data <- richard_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
richard_sla_data_trans <- richard_sla_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_high_trans = exp(upper__)) %>% 
  mutate(CI_low_trans = exp(lower__)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

colnames(rich_SLA.pred) = c('population','fit', 'lwr', 'upr')

(rich_sla_plot <-ggplot(rich_SLA.pred) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = SLA, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0)) + # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab(expression(atop("Specific Leaf Area", paste("(",mm^{2}," ",mg^{-1},")"))))+
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix richardsonii", size = 20, family = "Helvetica Light") +
    theme_shrub()+
    theme(axis.title.y = element_text(margin = margin (r = 10))))


# pulchra ----
pul_sla <- (conditional_effects(pulchra_SLA)) # extracting conditional effects from bayesian model
pul_sla_data <- pul_sla[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
pul_sla_data_trans <- pul_sla_data %>% 
  mutate(CI_range = (estimate__ - lower__)) %>% 
  mutate(CI_low_trans = exp(lower__)) %>% 
  mutate(CI_high_trans = exp(upper__)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) %>% 
  select(-CI_range)

colnames(pul_SLA.pred) = c('population','fit', 'lwr', 'upr')

(pul_sla_plot <-ggplot(pul_SLA.pred) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = SLA, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix pulchra") +
    theme_shrub())
# arctica ----
colnames(arc_SLA.pred) = c('population','fit', 'lwr', 'upr')

(arc_sla_plot <-ggplot(arc_SLA.pred) +
    geom_point(data = arctica_all_traits, aes(x = population, y = (SLA), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(sla_panel <- ggarrange(rich_sla_plot, pul_sla_plot, arc_sla_plot, 
                       common.legend = TRUE, legend = "none",
                       labels = c("A", "B", "C"),
                       label.x = c(0.1, 0.1, 0.1),
                       font.label = list(size = 18, color = "black", face = "bold", family = NULL),
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
colnames(rich_LDMC.pred) = c('population','fit', 'lwr', 'upr')

(rich_ldmc_plot <-ggplot(rich_LDMC.pred) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = LDMC_percent, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
#    ylab(expression(paste("Leaf dry matter content (%)"))) +
    ylab(expression(atop("Leaf dry matter content", paste("(%)"))))+
    xlab("" ) +
    coord_cartesian(ylim=c(15, 70)) +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub()+ 
    theme(axis.title.y = element_text(margin = margin (r = 10))))


# pulchra ----
colnames(pul_LDMC.pred) = c('population','fit', 'lwr', 'upr')

(pul_ldmc_plot <-ggplot(pul_LDMC.pred) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = (LDMC_percent), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n") +
    xlab("" )     +
    coord_cartesian(ylim=c(15, 70)) +
  scale_color_manual(values=pal) +
    labs(title = "Salix pulchra") +
    theme_shrub())
# arctica ----
colnames(arc_LDMC.pred) = c('population','fit', 'lwr', 'upr')

(arc_ldmc_plot <-ggplot(arc_LDMC.pred) +
    geom_point(data = arctica_all_traits, aes(x = population, y = (LDMC_percent), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(15, 70)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(ldmc_panel <- ggarrange(rich_ldmc_plot, pul_ldmc_plot, arc_ldmc_plot, 
                        common.legend = TRUE, legend = "bottom",
                        labels = c("D", "E", "F"),
                        label.x = c(0.1, 0.1, 0.1),
                        font.label = list(size = 18, color = "black", face = "bold", family = NULL),
                        ncol = 3, nrow = 1))

# raw data for reference 
(ldmc_plot <- ggplot(all_CG_source_traits) +
    geom_boxplot(aes(x= population, y = LDMC_percent, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("LDMC (%)") +
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

# LA ----
# richardsonii ----
colnames(rich_LA_mad.pred) = c('population','fit', 'lwr', 'upr')

(rich_la_plot <-ggplot(rich_LA_mad.pred) +
    geom_point(data = richardsonii_mad_traits, aes(x = population, y = (LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +    coord_cartesian(ylim=c(0, 120)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120))+
    ylab(expression("Leaf Area cm"^"2"))+
    xlab("" ) +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub())

#richard_la <- (conditional_effects(rich_LA_mad)) # extracting conditional effects from bayesian model
#richard_la_data <- richard_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
#richard_la_data_trans <- richard_la_data %>% 
#    mutate(CI_low_trans = exp(lower__)) %>% 
#    mutate(CI_high_trans = exp(upper__)) %>% 
#    mutate(Estimate_trans = exp(estimate__), 
#           Est.Error_trans = exp(se__)) 
  
#(rich_la_plot <-ggplot(richard_la_data_trans) +
#    geom_point(data = richardsonii_all_traits, aes(x = population, y = (LA_cm2), colour = population),
#               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
#    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
#    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
#                  size = 1, alpha = 1) +
#    ylab(expression(paste("\n Leaf area (",mm^{2},")"))) +
#    xlab("" ) +
#    scale_color_manual(values=pal) +
#    labs(title = "Salix richardsonii") +
#    theme_shrub())
# keep it logged 
#(rich_la_plot_log <-ggplot(richard_la_data) +
 #   geom_point(data = richardsonii_all_traits, aes(x = population, y = log(LA_cm2), colour = population),
#               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
#    geom_point(aes(x = effect1__, y = estimate__, colour = population), size = 6)+
#    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__, colour = population),
#                  size = 1, alpha = 1) +
#    ylab(expression(paste("\n log Leaf area (",cm^{2},")"))) +
#    xlab("" ) +
#    scale_color_manual(values=pal) +
#    coord_cartesian(ylim=c(3, 10)) +
#    labs(title = "Salix richardsonii") +
#    theme_shrub())

# pulchra ----
colnames(pulchra_LA.pred) = c('population','fit', 'lwr', 'upr')

(pul_la_plot <-ggplot(pulchra_LA.pred) +
    geom_point(data = pulchra_mad_traits, aes(x = population, y = (LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n") +
    coord_cartesian(ylim=c(0, 120)) +
    xlab("" ) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120))+
    scale_color_manual(values=pal) +
    labs(title = "Salix pulchra") +
    theme_shrub())

# arctica ----
colnames(arctica_LA.pred) = c('population','fit', 'lwr', 'upr')

(arc_la_plot <-ggplot(arctica_LA.pred) +
    geom_point(data = arctica_mad_traits, aes(x = population, y = (LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n") +
    xlab("" ) +
    coord_cartesian(ylim=c(0, 120)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120))+
    scale_color_manual(values=pal) +
    labs(title = "Salix pulchra") +
    theme_shrub())

# keep it log transformed 
#(arc_la_plot <-ggplot(arc_la_data) +
#    geom_point(data = arctica_all_traits, aes(x = population, y = log(LA_cm2), colour = population),
#               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
#    geom_point(aes(x = effect1__, y = estimate__, colour = population), size = 6)+
#    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__, colour = population),
#                  size = 1, alpha = 1) +
#    ylab("") +
#    xlab("" ) +
#    scale_color_manual(values=pal) +
#    coord_cartesian(ylim=c(3, 10)) +
#    labs(title = "Salix arctica") +
#    theme_shrub())

(la_panel <- ggarrange(rich_la_plot, pul_la_plot, arc_la_plot, 
                        common.legend = TRUE, legend = "none",
                        labels = c("A", "B", "C"),
                        label.x = c(0.1, 0.1, 0.1),
                        font.label = list(size = 18, color = "black", face = "bold", family = NULL),
                        ncol = 3, nrow = 1))

ggsave("figures/leaf_area_panel.png", height = 10, width = 12, dpi = 300)

# LEAF LENGTH -----

# richardsonii ----
colnames(rich_LL.pred) = c('population','fit', 'lwr', 'upr')

(rich_ll_plot <-ggplot(rich_LL.pred) +
    geom_point(data = richardsonii_all_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("\n Leaf Length (mm)\n") +
    xlab("") +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(0, 90)) +
    labs(title = "Salix richardsonii") +
    theme_shrub())
# pulchra ----
colnames(pul_LL.pred) = c('population','fit', 'lwr', 'upr')

(pul_ll_plot <-ggplot(pul_LL.pred) +
    geom_point(data = pulchra_all_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(0, 90)) +
    labs(title = "Salix pulchra") +
    theme_shrub())

# arctica ----
colnames(arc_LL.pred) = c('population','fit', 'lwr', 'upr')

(arc_ll_plot <-ggplot(arc_LL.pred) +
    geom_point(data = arctica_all_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1, width=0.75) +
    ylab("") +
    xlab("" ) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(title = "Salix arctica") +
    coord_cartesian(ylim=c(0, 90)) +
    theme_shrub())

(ll_panel <- ggarrange(rich_ll_plot, pul_ll_plot, arc_ll_plot, 
                       common.legend = TRUE, legend = "none",
                       labels = c("A", "B", "C"),
                       ncol = 3, nrow = 1))
ggsave("figures/leaf_length_panel.png", height = 10, width = 12, dpi = 300)

# Arrange plots ----

# leaf length + area panel 
(size_trait_panel <- ggarrange(la_panel, ll_panel, 
                               common.legend = TRUE, legend = "bottom", 
                               ncol = 1, nrow = 2))
ggsave("figures/leaf_size_panel.png", height = 10, width = 12, dpi = 300) 

# SLA LDMC panel 
(sla_ldmc_panel <- ggarrange(sla_panel, ldmc_panel, 
                          common.legend = TRUE, legend = "bottom",
                          ncol = 1, nrow = 2))

# save 
ggsave("figures/sla_ldmc_panel.png", height = 10, width = 12, dpi = 300)
