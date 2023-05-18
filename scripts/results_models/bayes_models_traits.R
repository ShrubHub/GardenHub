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
library(ggeffects)

# DATA ----
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") # most traits
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") # leaf length

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
rownames(rich_SLA_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(rich_SLA_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

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

rownames(ric_sla_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation (none are sig diff from each other)
# N Garden = estimate 2.70 , CI = 2.54 to 2.85
# N Source = estimate 2.70+(-0.24) = 2.46, CI = 2.13 to 2.81
# S Source = estimate = 2.75, CI = 2.46 to 3.04
# S Garden = estimate = 2.57, CI = 2.29 to 2.85

# S. pulchra ----
pulchra_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_SLA) # There were 1 divergent transitions after warmup
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
rownames(pulchra_SLA_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(pulchra_SLA_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

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

rownames(pul_sla_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation 
# N Garden = estimate = 2.74 , CI = 2.62 to 2.85
# N Source = estimate = 2.33, CI = 2.07 to 2.58 **
# S Source = estimate = 2.75, CI = 2.50 to 2.99 
# S Garden = estimate = 2.56, CI = 2.32 to 2.78 *

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
rownames(arctica_SLA_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(arctica_SLA_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

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

rownames(arc_sla_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation (none sig diff)
# N Garden = estimate = 2.41 , CI = 2.18 to 2.64
# N Source = estimate = 2.45, CI = 2.07 to 2.84 
# S Source = estimate = 2.59, CI = 2.22 to 2.96 
# S Garden = estimate = 2.34, CI = 1.97 to 2.70 

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
  kbl(caption="Table.xxx BRMS model outputs: specific leaf area by northern garden, northern source, sourthern garden and southern source populations. 
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
rownames(rich_LDMC_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(rich_LDMC_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

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

rownames(rich_ldmc_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation (none sig diff)
# N Garden = estimate = 3.42 , CI = 3.07 to 3.76
# N Source = estimate = 3.48, CI = 2.89 to 4.04
# S Source = estimate = 3.36, CI = 2.90 to 3.81
# S Garden = estimate = 3.48, CI = 3.04 to 3.90 

# S. pulchra ----
pulchra_LDMC_log <- brms::brm(log(LDMC_percent) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(pulchra_LDMC_log) 
tab_model(pulchra_LDMC_log)
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
rownames(pulchra_LDMC_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(pulchra_LDMC_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

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

rownames(pul_ldmc_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation (none sig diff)
# N Garden = estimate = 3.63 , CI = 3.18 to 4.07
# N Source = estimate = 3.78, CI = 3.07 to 4.48
# S Source = estimate = 3.50, CI = 2.90 to 4.09
# S Garden = estimate = 3.61, CI = 3.05 to 4.16 

# S. arctica ---- 
arctica_LDMC_log <- brms::brm(log(LDMC_percent) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99)) #There were 5 divergent transitions after warmup.
summary(arctica_LDMC_log)
tab_model(arctica_LDMC_log)
plot(arctica_LDMC_log)
pp_check(arctica_LDMC_log, type = "dens_overlay", ndraws = 100) 
saveRDS(arctica_LDMC_log, file = "output/traits/models/ldmc_arctica_compare.rds")
arctica_LDMC_log <- readRDS("output/traits/models/ldmc_arctica_compare.rds")
arc_LDMC.pred <- ggpredict(arctica_LDMC_log, terms = c('population'))

# extract output with function
arctica_LDMC_results <- model_summ(arctica_LDMC_log)

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
rownames(arctica_LDMC_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(arctica_LDMC_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

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

rownames(arc_ldmc_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation (none sig diff)
# N Garden = estimate = 3.46 , CI = 2.87 to 4.00
# N Source = estimate = 3.54, CI = 2.79 to 4.23
# S Source = estimate = 3.35, CI = 2.64 to 4.01
# S Garden = estimate = 3.55, CI = 2.86 to 4.18 

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
rich_LA <- brms::brm(log(LA_cm2) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000, 
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LA) 
plot(rich_LA)
pp_check(rich_LA, type = "dens_overlay", ndraws = 100)
saveRDS(rich_LA, file = "output/traits/models/la_richardsonii_compare.rds")
rich_LA <- readRDS("output/traits/models/la_richardsonii_compare.rds")

# extract output with function
rich_LA_results <- model_summ(rich_LA)

rich_LA_results <- rich_LA_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

rich_LA_results_2 <- rich_LA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

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
rownames(rich_LA_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(rich_LA_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

rich_la_extract_df_1 <- rich_LA_results %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_la_extract_df <- rich_LA_results_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_la_extract_all <- full_join(rich_la_extract_df_1, rich_la_extract_df, 
                                  by = c("effect" = "effect", "nobs"="nobs",
                                         "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                         "Species"="Species", "Rhat"="Rhat"))

rownames(rich_la_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")
# interpretation 
# N Garden = estimate = 1.64 , CI = 0.05 to 3.17
# N Source = estimate = 3.14, CI = 1.25 to 4.96
# S Source = estimate = 2.69, CI = 0.84 to 4.46
# S Garden = estimate = 2.64, CI = 0.86 to 4.37

# S. pulchra ----
pulchra_LA <- brms::brm(log(LA_cm2) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(pulchra_LA) 
plot(pulchra_LA)
pp_check(pulchra_LA, type = "dens_overlay", ndraws = 100) 
saveRDS(pulchra_LA, file = "output/traits/models/la_pulchra_compare.rds")
pulchra_LA <- readRDS("output/traits/models/la_pulchra_compare.rds")

# extract output with function
pulchra_LA_results <- model_summ(pulchra_LA)

pulchra_LA_results <- pulchra_LA_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

pulchra_LA_results_2 <- pulchra_LA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

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
rownames(pulchra_LA_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(pulchra_LA_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

pul_la_extract_df_1 <- pulchra_LA_results %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_la_extract_df <- pulchra_LA_results_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_la_extract_all <- full_join(pul_la_extract_df_1, pul_la_extract_df, 
                                 by = c("effect" = "effect", "nobs"="nobs",
                                        "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                        "Species"="Species", "Rhat"="Rhat"))

rownames(pul_la_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation 
# N Garden = estimate = 1.21 , CI = -0.52 to 2.82
# N Source = estimate = 3.14, CI = 0.50 to 4.53
# S Source = estimate = 2.69, CI = 0.14 to 4.06
# S Garden = estimate = 2.64, CI = -0.40 to 3.69

# S. arctica ----
arctica_LA <- brms::brm(log(LA_cm2)  ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LA) 
plot(arctica_LA)
pp_check(arctica_LA, type = "dens_overlay", ndraws = 100) 
saveRDS(arctica_LA, file = "output/traits/models/la_arctica_compare.rds")
arctica_LA <- readRDS("output/traits/models/la_arctica_compare.rds")

# extract output with function
arctica_LA_results <- model_summ(arctica_LA)

arctica_LA_results <- arctica_LA_results %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

arctica_LA_results_2 <- arctica_LA_results %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

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
rownames(arctica_LA_results) <- c("Intercept  ", "Northern Source ", "Southern Garden  ", "Southern Source ", "Sample age  ", "Sigma  ")
rownames(arctica_LA_results_2) <- c("Intercept ", "Northern Source ", "Southern Garden ", "Southern Source ", "Sample age ", "Sigma ")

arc_la_extract_df_1 <- arctica_LA_results %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_la_extract_df <- arctica_LA_results_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_la_extract_all <- full_join(arc_la_extract_df_1, arc_la_extract_df, 
                                by = c("effect" = "effect", "nobs"="nobs",
                                       "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                       "Species"="Species", "Rhat"="Rhat"))

rownames(arc_la_extract_all) <- c("Intercept", "Northern Source", "Southern Garden", "Southern Source", "Sample age", "Sigma")

# interpretation 
# N Garden = estimate = 1.93 , CI = -0.11 to 3.98
# N Source = estimate = 3.14, CI = 0.86 to 6.08
# S Source = estimate = 2.69, CI = -0.76 to 4.42
# S Garden = estimate = 2.64, CI = -0.32 to 4.75

# merging all extracted outputs

# merging all extracted outputs
garden_LA_out <- rbind(rich_la_extract_all, pul_la_extract_all, 
                         arc_la_extract_all) 

garden_LA_out <- garden_LA_out %>%
  dplyr::rename("Estimate_log_sum" = "Estimate (log sum)")

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

rich_LL_results <- model_summ(rich_LL)
rich_LL_results$Species <- "Salix richardsonii"
rich_LL.pred <- ggpredict(rich_LL, terms = c('population'))

# interpretation 
# N Garden = estimate = 22.01 , CI = 15.16 to 28.65 *
# N Source = estimate = 41.93, CI = 31.36 to 52.34 **
# S Source = estimate = 50.87, CI = 40.28 to 61.20 **
# S Garden = estimate = 40.13, CI = 31.39 to 48.70 **

# S. pulchra ----
pulchra_LL <- brms::brm(mean_leaf_length ~ population + (1|year), data = pulchra_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99)) # There were 1 divergent transitions after warmup
summary(pulchra_LL)
plot(pulchra_LL)
pp_check(pulchra_LL, type = "dens_overlay", ndraws = 100) 
saveRDS(pulchra_LL, file = "output/traits/models/ll_pulchra_compare.rds")
pulchra_LL <- readRDS("output/traits/models/ll_pulchra_compare.rds")
pul_LL.pred <- ggpredict(pulchra_LL, terms = c('population'))

pulchra_LL_results <- model_summ(pulchra_LL)
pulchra_LL_results$Species <- "Salix pulchra"

pul_LL.pred <- ggpredict(pulchra_LL, terms = c('population'))

# interpretation 
# N Garden = estimate = 20.01 , CI = 12.13 to 27.35 *
# N Source = estimate = 35.04, CI = 23.05 to 46.55**
# S Source = estimate = 50.31, CI = 38.37 to 61.78**
# S Garden = estimate = 36.26, CI = 26.60 to 45.28**

# S. arctica ----
# no leaf length for S. arctic from source pop
# make common garden only model 
arctica_cg_growth <- arctica_all_growth %>% 
  filter(population %in% c("N. Garden", "S. Garden"))

arctica_LL_CG <- brms::brm((mean_leaf_length) ~ population + (1|year) + (1|SampleID_standard), data = arctica_cg_growth, family = gaussian(), chains = 3,
                           iter = 5000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LL_CG)
plot(arctica_LL_CG)
pp_check(arctica_LL_CG, type = "dens_overlay", ndraws = 100)
saveRDS(arctica_LL_CG, file = "output/traits/models/ll_arctica_compare.rds")
arctica_LL_CG <- readRDS("output/traits/models/ll_arctica_compare.rds")
arc_LL.pred <- ggpredict(arctica_LL_CG, terms = c('population'))

arctica_LL_results <- model_summ(arctica_LL_CG)
arctica_LL_results$Species <- "Salix arctica"

arc_LL.pred <- ggpredict(arctica_LL_CG, terms = c('population'))

# interpretation 
# N Garden = estimate = 23.16 , CI = 16.13 to 29.90 
# S Garden = estimate = 26.19, CI = 14.93 to 37.17

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
garden_LL_out <- garden_LL_out %>% 
  select(-Est.Error)
# save df of results 
write.csv(garden_LL_out, "output/traits/garden_LL_out_back.csv")
# creating table
kable_LL <- garden_LL_out %>% 
  kbl(caption="Table.xxx BRMS model outputs: Leaf length  of northern garden, northern source, southern garden, southern source willows. 
      Model structure per species: leaf length ~ population + (1|year) + (1|sample_id). Note S. arctica only comparing garden populations. 
      Model output back-transformed in the table below.", 
      col.names = c( "Estimate",
                     "Lower 95% CI",
                     "Upper 95% CI", 
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
                  size = 1, alpha = 1) +
    ylab(expression(paste("Specific Leaf Area (",mm^{2}," ",mg^{-1},")\n"))) +
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
                  size = 1, alpha = 1) +
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
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(5, 25)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(sla_panel <- ggarrange(rich_sla_plot, pul_sla_plot, arc_sla_plot, 
                       common.legend = TRUE, legend = "none",
                       labels = c("A", "B", "C"),
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
                  size = 1, alpha = 1) +
    ylab(expression(paste("Leaf dry matter content (%)"))) +
    xlab("" ) +
    coord_cartesian(ylim=c(15, 80)) +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub())

# pulchra ----
colnames(pul_LDMC.pred) = c('population','fit', 'lwr', 'upr')

(pul_ldmc_plot <-ggplot(pul_LDMC.pred) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = (LDMC_percent), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" )     +
    coord_cartesian(ylim=c(15, 80)) +
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
                  size = 1, alpha = 1) +
    ylab("\n") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(15, 80)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(ldmc_panel <- ggarrange(rich_ldmc_plot, pul_ldmc_plot, arc_ldmc_plot, 
                        common.legend = TRUE, legend = "bottom",
                        labels = c("D", "E", "F"),
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
richard_la <- (conditional_effects(rich_LA)) # extracting conditional effects from bayesian model
richard_la_data <- richard_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
richard_la_data_trans <- richard_la_data %>% 
    mutate(CI_low_trans = exp(lower__)) %>% 
    mutate(CI_high_trans = exp(upper__)) %>% 
    mutate(Estimate_trans = exp(estimate__), 
           Est.Error_trans = exp(se__)) 
  
(rich_la_plot <-ggplot(richard_la_data_trans) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = (LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab(expression(paste("\n Leaf area (",mm^{2},")"))) +
    xlab("" ) +
    scale_color_manual(values=pal) +
    labs(title = "Salix richardsonii") +
    theme_shrub())
# keep it logged 
(rich_la_plot_log <-ggplot(richard_la_data) +
    geom_point(data = richardsonii_all_traits, aes(x = population, y = log(LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = effect1__, y = estimate__, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__, colour = population),
                  size = 1, alpha = 1) +
    ylab(expression(paste("\n log Leaf area (",cm^{2},")"))) +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(3, 10)) +
    labs(title = "Salix richardsonii") +
    theme_shrub())

# pulchra ----
pul_la <- (conditional_effects(pulchra_LA)) # extracting conditional effects from bayesian model
pul_la_data <- pul_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
pul_la_data_trans <- pul_la_data %>% 
  mutate(CI_low_trans = exp(lower__)) %>% 
  mutate(CI_high_trans = exp(upper__)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) 

(pul_la_plot <-ggplot(pul_la_data_trans) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = (LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans,colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans, colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    labs(title = "Salix pulchra") +
    theme_shrub())
# keep it log transformed 
(pul_la_plot_log <-ggplot(pul_la_data) +
    geom_point(data = pulchra_all_traits, aes(x = population, y = log(LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = effect1__, y = estimate__, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__, colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    coord_cartesian(ylim=c(3, 10)) +
    labs(title = "Salix pulchra") +
    theme_shrub())

# arctica ----
arc_la <- (conditional_effects(arctica_LA)) # extracting conditional effects from bayesian model
arc_la_data <- arc_la[[1]] # making the extracted model outputs into a dataset (for plotting)
#[[1]] is to extract the first term in the model which in our case is population
arc_la_data_trans <- arc_la_data %>% 
  mutate(CI_low_trans = exp(lower__)) %>% 
  mutate(CI_high_trans = exp(upper__)) %>% 
  mutate(Estimate_trans = exp(estimate__), 
         Est.Error_trans = exp(se__)) 

(arc_la_plot <-ggplot(arc_la_data_trans) +
    geom_point(data = arctica_all_traits, aes(x = population, y = (LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = effect1__, y = Estimate_trans, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = CI_low_trans, ymax = CI_high_trans,colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    labs(title = "Salix arctica") +
    theme_shrub())

# keep it log transformed 
(arc_la_plot_log <-ggplot(arc_la_data) +
    geom_point(data = arctica_all_traits, aes(x = population, y = log(LA_cm2), colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = effect1__, y = estimate__, colour = population), size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__, colour = population),
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(3, 10)) +
    labs(title = "Salix arctica") +
    theme_shrub())

(la_panel <- ggarrange(rich_la_plot, pul_la_plot, arc_la_plot, 
                         common.legend = TRUE, legend = "bottom",
                         ncol = 3, nrow = 1))
(la_panel_log <- ggarrange(rich_la_plot_log, pul_la_plot_log, arc_la_plot_log, 
                       common.legend = TRUE, legend = "none",
                       ncol = 3, nrow = 1))
# LEAF LENGTH -----

# richardsonii ----
colnames(rich_LL.pred) = c('population','fit', 'lwr', 'upr')

(rich_ll_plot <-ggplot(rich_LL.pred) +
    geom_point(data = richardsonii_all_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1) +
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
                  size = 1, alpha = 1) +
    ylab("") +
    xlab("" ) +
    scale_color_manual(values=pal) +
    coord_cartesian(ylim=c(0, 90)) +
    labs(title = "Salix pulchra") +
    theme_shrub())

# arctica ----
colnames(arc_LL.pred) = c('population','fit', 'lwr', 'upr')

pal_garden <-c("#440154FF", "#7AD151FF")

(arc_ll_plot <-ggplot(arc_LL.pred) +
    geom_point(data = arctica_cg_growth, aes(x = population, y = mean_leaf_length, colour = population),
               alpha = 0.5, position = position_jitter(w = 0.09, h = 0))+ # raw data
    geom_point(aes(x = population, y = fit, colour = population), size = 6)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  size = 1, alpha = 1) +
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

# arrange plots ----

(trait_panel <- ggarrange(rich_sla_plot, pul_sla_plot, arc_sla_plot,
  rich_ldmc_plot, pul_ldmc_plot, arc_ldmc_plot,
  rich_la_plot, pul_la_plot, arc_la_plot,
  rich_ll_plot, pul_ll_plot, arc_ll_plot, 
                          common.legend = TRUE, legend = "bottom", 
  ncol = 6, nrow = 2))

(traits_plot <-ggarrange(sla_panel, ldmc_panel,la_panel_log,  ll_panel, 
                               common.legend = TRUE, legend = "none", 
                               ncol = 2, nrow = 2))

# leaf length + area panel 
(size_trait_panel <- ggarrange(la_panel_log, ll_panel, 
                               common.legend = TRUE, legend = "bottom", 
                               ncol = 1, nrow = 2))
ggsave("figures/size_trait_panel.png", height = 10, width = 12, dpi = 300) 

# SLA LDMC panel 
(sla_ldmc_panel <- ggarrange(sla_panel, ldmc_panel, 
                          common.legend = TRUE, legend = "bottom",
                          ncol = 1, nrow = 2))
# save 
ggsave("figures/sla_ldmc_panel.png", height = 10, width = 12, dpi = 300)
