# BAYESIAN growth methods models -----
# Script by Erica and Madi
# Last update: 27/10/2023 by Madi

# 1. Loading libraries ----

library(brms)
library(tidyverse)
library(tidybayes)
library(ggeffects)
library(dplyr)
library(knitr) # For kable tables
library(kableExtra) # For kable tables
library(gridExtra)
library(ggpubr)

# 2. Loading data ---- 
unique_source_mother <- read.csv("data/source_pops/unique_source_mother.csv")

# Functions -------
# 1. scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# 2. extract model result function =====

model_summ_methods <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$SampleYear
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "SampleYear"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge it all together
}

# no random effect model summary function 
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

# 3. Wrangling ----
# variables in right format
str(unique_source_mother)
unique_source_mother$SampleYear <- as.factor(unique_source_mother$SampleYear)
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique_source_mother$year <-  format(as.Date(unique_source_mother$SampleDate, format="%d/%m/%Y"),"%Y")
unique(unique_source_mother$SampleYear)

# Species specific datasets
unique_source_mother_rich <- unique_source_mother %>%
  filter(Species == "Salix richardsonii")

unique_source_mother_pulchra <- unique_source_mother %>%
  filter(Species == "Salix pulchra")

unique_source_mother_arctica <- unique_source_mother %>%
  filter(Species == "Salix arctica")

# exploring variables distribution
hist(unique_source_mother_arctica$Canopy_Height_cm) # right skew
hist(unique_source_mother_arctica$mean_width) # right skew
hist(unique_source_mother_arctica$mean_stem_elong) # right skew
hist(unique_source_mother_arctica$Stem_diameter, breaks = 30) #  right skew - so weird
hist(unique_source_mother_arctica$biovolume,  breaks = 30)#  right skew

# 4. Modelling -----

# a. CANOPY HEIGHT -----
# Salix richardsonii -------
source_rich_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_height) # sig. lower heights for QHi
plot(source_rich_height)
pp_check(source_rich_height, type = "dens_overlay", ndraws = 100)  # good) 
saveRDS(source_rich_height, file = "output/models/source_rich_height.rds")
source_rich_height <- readRDS(file = "output/models/source_rich_height.rds")

rich_source_height.pred <- ggpredict(source_rich_height, terms = c('Site'))

# extract output with function
source_rich_height_dat <- model_summ_methods(source_rich_height)
source_rich_height_dat <- source_rich_height_dat %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

source_rich_height_dat_2 <- source_rich_height_dat %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
source_rich_height_dat_2[2,1] <- source_rich_height_dat_2[2,1] + source_rich_height_dat_2[1,1]
# change lower CI by adding 
source_rich_height_dat_2[2,3] <- source_rich_height_dat_2[2,3] + source_rich_height_dat_2[1,3]
# change upper CI
source_rich_height_dat_2[2,4] <- source_rich_height_dat_2[2,4] + source_rich_height_dat_2[1,4]

# extraction for model output table
rownames(source_rich_height_dat) <- c("Intercept", "Northern source", "Sample year", "Sigma")
rownames(source_rich_height_dat_2) <- c("Intercept", "Northern source", "Sample year", "Sigma")

source_rich_height_df_1 <- source_rich_height_dat %>% 
  mutate(Species = rep("Salix richardsonii"))  %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

source_rich_height_df <- source_rich_height_dat_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")

rich_source_extract_all <- full_join(source_rich_height_df_1, source_rich_height_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(rich_source_extract_all) <- c("Intercept ", "Northern Source ", "Sample year", "Sigma  ")



# Salix pulchra ------
source_pul_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_height) # sig lower heights for QHi
plot(source_pul_height)
pp_check(source_pul_height, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(source_pul_height, file = "output/models/source_pul_height.rds")
source_pul_height <- readRDS(file = "output/models/source_pul_height.rds")

pul_source_height.pred <- ggpredict(source_pul_height, terms = c('Site'))

# extract output with function
source_pul_height_dat<- model_summ_methods(source_pul_height)
source_pul_height_dat <- source_pul_height_dat %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

source_pul_height_dat_2 <- source_pul_height_dat %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
source_pul_height_dat_2[2,1] <- source_pul_height_dat_2[2,1] + source_pul_height_dat_2[1,1]
# change lower CI by adding 
source_pul_height_dat_2[2,3] <- source_pul_height_dat_2[2,3] + source_pul_height_dat_2[1,3]
# change upper CI
source_pul_height_dat_2[2,4] <- source_pul_height_dat_2[2,4] + source_pul_height_dat_2[1,4]

# extraction for model output table
rownames(source_pul_height_dat) <- c("Intercept", "Northern source", "Sample year", "Sigma")
rownames(source_pul_height_dat_2) <- c("Intercept", "Northern source", "Sample year", "Sigma")

source_pul_height_df_1 <- source_pul_height_dat %>% 
  mutate(Species = rep("Salix pulchra"))  %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

source_pul_height_df <- source_pul_height_dat_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")

pul_source_extract_all <- full_join(source_pul_height_df_1, source_pul_height_df, 
                                     by = c("effect" = "effect", "nobs"="nobs",
                                            "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                            "Species"="Species", "Rhat"="Rhat"))

rownames(pul_source_extract_all) <- c("Intercept ", "Northern Source ", "Sample year", "Sigma  ")

# Salix arctica -----
source_arc_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_height) # no significant difference
plot(source_arc_height)
pp_check(source_arc_height, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(source_arc_height, file = "output/models/source_arc_height.rds")
source_arc_height <- readRDS(file = "output/models/source_arc_height.rds")

arc_source_height.pred <- ggpredict(source_arc_height, terms = c('Site'))

# extract output with function
source_arc_height_dat <- model_summ_methods(source_arc_height)
source_arc_height_dat <- source_arc_height_dat %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

source_arc_height_dat_2 <- source_arc_height_dat %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
source_arc_height_dat_2[2,1] <- source_arc_height_dat_2[2,1] + source_arc_height_dat_2[1,1]
# change lower CI by adding 
source_arc_height_dat_2[2,3] <- source_arc_height_dat_2[2,3] + source_arc_height_dat_2[1,3]
# change upper CI
source_arc_height_dat_2[2,4] <- source_arc_height_dat_2[2,4] + source_arc_height_dat_2[1,4]

# extraction for model output table
rownames(source_arc_height_dat) <- c("Intercept", "Northern source", "Sample year", "Sigma")
rownames(source_arc_height_dat_2) <- c("Intercept", "Northern source", "Sample year", "Sigma")

source_arc_height_df_1 <- source_arc_height_dat %>% 
  mutate(Species = rep("Salix arctica"))  %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

source_arc_height_df <- source_arc_height_dat_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")

arc_source_extract_all <- full_join(source_arc_height_df_1, source_arc_height_df, 
                                    by = c("effect" = "effect", "nobs"="nobs",
                                           "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                           "Species"="Species", "Rhat"="Rhat"))

rownames(arc_source_extract_all) <- c("Intercept ", "Northern Source ", "Sample year", "Sigma  ")


# merging all extracted outputs
source_heights_out <- rbind(rich_source_extract_all, pul_source_extract_all, 
                            arc_source_extract_all) 
source_heights_out <- source_heights_out %>%
  dplyr::rename("Estimate_log_sum" = "Estimate (log sum)")

# back transforming from log
source_heights_out_back <- source_heights_out %>%
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
write.csv(source_heights_out_back, "output/source_heights_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(source_heights_out_back) <- c("Intercept", "Northern source", "Sample year", 
                                       "Sigma", " Intercept", " Northern source", " Sample year", 
                                       " Sigma", "Intercept ", "Northern source ", "Sample year ", 
                                       "Sigma ")
# making sure Rhat keeps the .00 
source_heights_out_back$Rhat <- as.character(formatC(source_heights_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_heights_source <- source_heights_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: canopy heights of northern vs southern shrubs in source populations. 
      Model structure per species: log(Canopy_Height_cm) ~ Site + (1|SampleYear). 
      Including model output back-transformed in the table below.", 
      col.names = c( "Species",
                     "Estimate (log)",
                     "Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)",
                     "Estimate (log sum)",  
                     "Lower 95% CI 
                    (log sum)", "Upper 95% CI
                    (log sum)",  
                     "Estimate (transformed)","Lower 95% CI 
                    (transformed)", "Upper 95% CI
                    (transformed)",
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_heights_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 1:12, align = "c") 

save_kable(kable_heights_source, file = "output/source/source_height_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# b. STEM ELONGATION ---- 
# Salix richardsonii -------
source_rich_elong <- brms::brm(log(mean_stem_elong) ~ Site,
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_elong)# shorter elongations for QHI
plot(source_rich_elong)
pp_check(source_rich_elong,  type = "dens_overlay", nsamples = 100)
saveRDS(source_rich_elong, file = "output/models/source_rich_elong.rds")
source_rich_elong <- readRDS("output/models/source_rich_elong.rds")
rich_soruce_elong.pred <- ggpredict(source_rich_elong, terms = c('Site'))
View(rich_soruce_elong.pred)

# extract output with function
source_rich_elong_dat <- model_summ_no_re(source_rich_elong)
source_rich_elong_dat <- source_rich_elong_dat %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

source_rich_elong_dat_2 <- source_rich_elong_dat %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate_log_sum"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
source_rich_elong_dat_2[2,1] <- source_rich_elong_dat_2[2,1] + source_rich_elong_dat_2[1,1]
# change lower CI by adding 
source_rich_elong_dat_2[2,3] <- source_rich_elong_dat_2[2,3] + source_rich_elong_dat_2[1,3]
# change upper CI
source_rich_elong_dat_2[2,4] <- source_rich_elong_dat_2[2,4] + source_rich_elong_dat_2[1,4]

# extraction for model output table
rownames(source_rich_elong_dat) <- c("Intercept", "Northern source", "Sigma")
rownames(source_rich_elong_dat_2) <- c("Intercept", "Northern source", "Sigma")

source_rich_elong_df_1 <- source_rich_elong_dat %>% 
  mutate(Species = rep("Salix richardsonii"))  %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

source_rich_elong_df <- source_rich_elong_dat_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate_log_sum") %>%
  relocate("nobs", .before = "effect")

rich_source_elong_all <- full_join(source_rich_elong_df_1, source_rich_elong_df, 
                                     by = c("effect" = "effect", "nobs"="nobs",
                                            "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                            "Species"="Species", "Rhat"="Rhat"))

rownames(rich_source_elong_all) <- c("Intercept ", "Northern Source ", "Sigma  ")

# Salix pulchra -------
source_pul_elong <- brms::brm(log(mean_stem_elong) ~ Site,
                               data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_elong)# shorter elongations for QHi
plot(source_pul_elong)
pp_check(source_pul_elong,  type = "dens_overlay", nsamples = 100)
saveRDS(source_pul_elong, file = "output/models/source_pul_elong.rds")
source_pul_elong <- readRDS("output/models/source_pul_elong.rds")
source_pul_elong.pred <- ggpredict(source_pul_elong, terms = c('Site'))
View(source_pul_elong.pred)

# extract output with function
source_pul_elong_dat <- model_summ_no_re(source_pul_elong)
source_pul_elong_dat <- source_pul_elong_dat %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

source_pul_elong_dat_2 <- source_pul_elong_dat %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate_log_sum"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
source_pul_elong_dat_2[2,1] <- source_pul_elong_dat_2[2,1] + source_pul_elong_dat_2[1,1]
# change lower CI by adding 
source_pul_elong_dat_2[2,3] <- source_pul_elong_dat_2[2,3] + source_pul_elong_dat_2[1,3]
# change upper CI
source_pul_elong_dat_2[2,4] <- source_pul_elong_dat_2[2,4] + source_pul_elong_dat_2[1,4]

# extraction for model output table
rownames(source_pul_elong_dat) <- c("Intercept", "Northern source", "Sigma")
rownames(source_pul_elong_dat_2) <- c("Intercept", "Northern source", "Sigma")

source_pul_elong_df_1 <- source_pul_elong_dat %>% 
  mutate(Species = rep("Salix pulchra"))  %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

source_pul_elong_df <- source_pul_elong_dat_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate_log_sum") %>%
  relocate("nobs", .before = "effect")

pul_source_elong_all <- full_join(source_pul_elong_df_1, source_pul_elong_df, 
                                   by = c("effect" = "effect", "nobs"="nobs",
                                          "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                          "Species"="Species", "Rhat"="Rhat"))

rownames(pul_source_elong_all) <- c("Intercept ", "Northern Source ", "Sigma  ")
# merging all extracted outputs
source_elong_out <- rbind(rich_source_elong_all, pul_source_elong_all) 

# back transforming from log
source_elong_out_back <- source_elong_out %>%
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
write.csv(source_elong_out_back, "output/source_elong_out_back.csv")

# Salix arctica: cannot do it because we dont have arctica on KP
#source_arc_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|SampleYear),
  #                             data = unique_source_mother_arctica, family = gaussian(), chains = 3,
    #                           iter = 5000, warmup = 1000, 
      #                         control = list(max_treedepth = 20, adapt_delta = 0.99))

#summary(source_arc_elong)# shorter elongations for QHi
#plot(source_arc_elong)
#pp_check(source_arc_elong,  type = "dens_overlay", nsamples = 100)

# c. WIDTH ----
# Salix richardsonii -------
source_rich_width_mod<- brms::brm((mean_width) ~ Site + (1|SampleYear),
                               data = unique_source_mother_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_width_mod) # significantly LARGER width for QHI shrubs
plot(source_rich_width_mod)
pp_check(source_rich_width_mod, type = "dens_overlay", nsamples = 100)  # fine
saveRDS(source_rich_width_mod, file = "output/models/source_rich_width.rds")
source_rich_width_mod <- readRDS(file = "output/models/source_rich_width.rds")
rich_source_width.pred <- ggpredict(source_rich_width_mod, terms = c('Site'))

# extract output with function
source_rich_width <- model_summ_methods(source_rich_width_mod)

# extraction for model output table
rownames(source_rich_width) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_rich_width_df <- source_rich_width %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra -----
source_pul_width<- brms::brm((mean_width) ~ Site + (1|SampleYear),
                              data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_width) # no sig diff 
pul_source_width.pred <- ggpredict(source_pul_width, terms = c('Site'))

plot(source_pul_width)
pp_check(source_pul_width, type = "dens_overlay", nsamples = 100)  # fine
saveRDS(source_pul_width, file = "output/models/source_pul_width.rds")
source_pul_width <- readRDS(file = "output/models/source_pul_width.rds")
pul_source_width.pred <- ggpredict(source_pul_width, terms = c('Site'))

# extract output with function
source_pul_width <- model_summ_methods(source_pul_width)

# extraction for model output table
rownames(source_pul_width) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_pul_width_df <- source_pul_width %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix arctica -------
source_arc_width<- brms::brm((mean_width) ~ Site + (1|SampleYear),
                             data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_width) # no significant diff
plot(source_arc_width)
pp_check(source_arc_width, type = "dens_overlay", nsamples = 100)  # fine
saveRDS(source_arc_width, file = "output/models/source_arc_width.rds")
source_arc_width <- readRDS(file = "output/models/source_arc_width.rds")
arc_source_width.pred <- ggpredict(source_arc_width, terms = c('Site'))

# extract output with function
source_arc_width <- model_summ_methods(source_arc_width)

# extraction for model output table
rownames(source_arc_width) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_arc_width_df <- source_arc_width %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
source_width_out <- rbind(source_rich_width_df, source_pul_width_df, 
                            source_arc_width_df) 

# back transforming from log
source_width_out_back <- source_width_out %>%
  dplyr::rename("l_95_CI" = "l-95% CI", 
                "u_95_CI" = "u-95% CI") 

# save df of results 
write.csv(source_width_out_back, "output/source_width_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(source_width_out_back) <- c("Intercept", "Northern source", "Sample year", 
                                       "Sigma", " Intercept", " Northern source", " Sample year", 
                                       " Sigma", "Intercept ", "Northern source ", "Sample year ", 
                                       "Sigma ")

# making sure Rhat keeps the .00 
source_width_out_back$Rhat <- as.character(formatC(source_width_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_width_source <- source_width_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: canopy widths of northern vs southern shrubs in source populations. 
      Model structure per species: (mean_width) ~ Site + (1|SampleYear). 
      Including model output back-transformed in the table below.", 
      col.names = c( "Species",
                     "Estimate",
                     "Error",
                     "Lower 95% CI",
                     "Upper 95% CI", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"
                     ), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_width_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 1:12, align = "c") 

save_kable(kable_width_source, file = "output/source/source_width_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# d. STEM DIAMETER -----
# Salix richardsonii -------
source_rich_diam <- brms::brm(log(Stem_diameter) ~ Site + (1|SampleYear),
                              data = unique_source_mother_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_diam) # lower stem diameter for QHi shrubs
plot(source_rich_diam)
pp_check(source_rich_diam, type = "dens_overlay", nsamples = 100) 

# extract output with function
source_rich_diam <- model_summ_methods(source_rich_diam)

# extraction for model output table
rownames(source_rich_diam) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_rich_diam_df <- source_rich_diam %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra ------
source_pul_diam <- brms::brm(log(Stem_diameter) ~ Site + (1|SampleYear),
                              data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_diam) # lower stem diameter for QHi shrubs
plot(source_pul_diam)
pp_check(source_pul_diam, type = "dens_overlay", nsamples = 100) 

# extract output with function
source_pul_diam <- model_summ_methods(source_pul_diam)

# extraction for model output table
rownames(source_pul_diam) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_pul_diam_df <- source_pul_diam %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
source_diam_out <- rbind(source_rich_diam_df, source_pul_diam_df) 

# back transforming from log
source_diam_out_back <- source_diam_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(source_diam_out_back, "output/source_diam_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(source_diam_out_back) <- c("Intercept", "Northern source", "Sample year", 
                                     "Sigma", " Intercept", " Northern source", " Sample year", 
                                     " Sigma")

# making sure Rhat keeps the .00 
source_diam_out_back$Rhat <- as.character(formatC(source_diam_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_diam_source <- source_diam_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: stem diameter of northern vs southern shrubs in source populations. 
      Model structure per species: log(Stem diameter) ~ Site + (1|SampleYear). 
      Including model output back-transformed in the table below. No Salix arctica due to lack of data from the southern source. ", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect",
                     "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                     "Estimate transformed", 
                     "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_diam_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 1:12, align = "c") 

# Salix arctica : cant do it because we dont have arctica on KP
#source_arc_diam <- brms::brm(log(Stem_diameter) ~ Site + (1|SampleYear),
  ##                            data = unique_source_mother_arctica, family = gaussian(), chains = 3,
   #                           iter = 3000, warmup = 1000, 
    #                          control = list(max_treedepth = 15, adapt_delta = 0.99))

#summary(source_arc_diam) # lower stem diameter for QHi shrubs
#plot(source_arc_diam)
#pp_check(source_arc_diam, type = "dens_overlay", nsamples = 100) 

# d.BIOVOLUME----
# Salix richardsonii -------
source_rich_biovol <- brms::brm(log(biovolume) ~ Site + (1|SampleYear),
                             data = unique_source_mother_rich, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(source_rich_biovol) # no significant difference
plot(source_rich_biovol)
pp_check(source_rich_biovol, type = "dens_overlay", nsamples = 100) 

# extract output with function
source_rich_biovol <- model_summ_methods(source_rich_biovol)

# extraction for model output table
rownames(source_rich_biovol) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_rich_biovol_df <- source_rich_biovol %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra -------
source_pul_biovol <- brms::brm(log(biovolume) ~ Site + (1|SampleYear),
                                data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(source_pul_biovol) # smaller biovolume for QHI shrubs
plot(source_pul_biovol)
pp_check(source_pul_biovol, type = "dens_overlay", nsamples = 100) 

# extract output with function
source_pul_biovol <- model_summ_methods(source_pul_biovol)

# extraction for model output table
rownames(source_pul_biovol) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_pul_biovol_df <- source_pul_biovol %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix arctica -------
source_arc_biovol <- brms::brm(log(biovolume) ~ Site + (1|SampleYear),
                               data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(source_arc_biovol) # no significant difference
plot(source_arc_biovol)
pp_check(source_arc_biovol, type = "dens_overlay", nsamples = 100) 

# extract output with function
source_arc_biovol <- model_summ_methods(source_arc_biovol)

# extraction for model output table
rownames(source_arc_biovol) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_arc_biovol_df <- source_arc_biovol %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
source_biovol_out <- rbind(source_rich_biovol_df, source_pul_biovol_df, 
                          source_arc_biovol_df) 


# back transforming from log
source_biovol_out_back <- source_biovol_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(source_biovol_out_back, "output/source_biovol_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(source_biovol_out_back) <- c("Intercept", "Northern source", "Sample year", 
                                     "Sigma", " Intercept", " Northern source", " Sample year", 
                                     " Sigma", "Intercept ", "Northern source ", "Sample year ", 
                                     "Sigma ")

# making sure Rhat keeps the .00 
source_biovol_out_back$Rhat <- as.character(formatC(source_biovol_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
(kable_biovol_source <- source_biovol_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: biovolume (height x width x width) northern vs southern shrubs in source populations. 
      Model structure per species: log(biovolume) ~ Site + (1|SampleYear). 
      Including model output back-transformed in the table below.", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect",
                     "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                     "Estimate transformed", 
                     "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")%>% 
  column_spec(2, width = NULL, bold = FALSE, italic = TRUE)%>% 
  row_spec(1:12, align = "r") %>%
 column_spec(1:2,latex_valign	= "m") )
  
# making species column in cursive
column_spec(kable_biovol_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 2:12, align = "c") 


# merge all outputs 
all_source_outputs <- rbind(source_heights_out_back, source_width_out_back,
                            source_elong_out_back, source_diam_out_back,
                            source_biovol_out_back)


# DATA VISUALISATION --------
pal_source <- c("#332288", "#7ad151")


theme_shrub <- function(){ theme(legend.position = "bottom",
                                 axis.title.x = element_text(face="bold", family = "Helvetica Light", size=12),
                                 axis.text.x  = element_text(vjust=0.5, size=12, family = "Helvetica Light", colour = "black"), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=12),
                                 axis.text.y  = element_text(vjust=0.5, size=12, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 12, family = "Helvetica Light", face = "italic", hjust = 0.5),
                                 legend.title = element_text(size=12, family = "Helvetica Light"),
                                 legend.key=element_blank(),
                                 strip.text.x = element_text(
                                   size = 14, color = "black", face = "italic", family = "Helvetica Light"),
                                 strip.background = element_blank(),
                                 legend.text=element_text(size = 12, family = "Helvetica Light"))}

# CANOPY HEIGHT -------
# S.rich ----
colnames(rich_source_height.pred) = c('Site','fit','error', 'lwr', 'upr')

rich_source_height.pred$Site <- ordered(rich_source_height.pred$Site,
                                           levels = c("Qikiqtaruk",
                                                      "Kluane"))
unique_source_mother_rich$Site <- ordered(unique_source_mother_rich$Site,
                                        levels = c("Qikiqtaruk",
                                                   "Kluane"))

(rich_source_height_plot <-ggplot(rich_source_height.pred) +
    geom_jitter(data = unique_source_mother_rich, aes(x = Site, y = Canopy_Height_cm, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit,colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_source, labels=c('North', 'South')) +
    scale_y_continuous(breaks = seq(0, 200, by = 25)) +
    theme_shrub()+
    theme(axis.title.x=element_blank(), 
          legend.position = "none")+
    scale_x_discrete(labels = c('North', 'South'))+
    ggtitle(expression(italic("Salix richardsonii"))))

# S. pul----
colnames(pul_source_height.pred) = c('Site','fit','error', 'lwr', 'upr')

pul_source_height.pred$Site <- ordered(pul_source_height.pred$Site,
                                        levels = c("Qikiqtaruk",
                                                   "Kluane"))
unique_source_mother_pulchra$Site <- ordered(unique_source_mother_pulchra$Site,
                                          levels = c("Qikiqtaruk",
                                                     "Kluane"))

(pul_source_height_plot <-ggplot(pul_source_height.pred) +
    geom_jitter(data = unique_source_mother_pulchra, aes(x = Site, y = Canopy_Height_cm, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit,colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_source, labels=c('North', 'South')) +
    scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, by = 30)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North', 'South'))+
    ggtitle(expression(italic("Salix pulchra"))))

# S.arctica -----
colnames(arc_source_height.pred) = c('Site','fit','error', 'lwr', 'upr')

# arc_source_height.pred$Site <- plyr::revalue(arc_source_height.pred$Site ,
#                                                    c("Qikiqtaruk"="N. Source",
#                                                      "Kluane"="S. Source"))

arc_source_height.pred$Site <- ordered(arc_source_height.pred$Site,
                                       levels = c("Qikiqtaruk",
                                                  "Kluane"))
unique_source_mother_arctica$Site <- ordered(unique_source_mother_arctica$Site,
                                             levels = c("Qikiqtaruk",
                                                        "Kluane"))

(arc_source_height_plot <-ggplot(arc_source_height.pred) +
    geom_jitter(data = unique_source_mother_arctica, aes(x = Site, y = Canopy_Height_cm, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit,colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_source) +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North','South'))+
    ggtitle(expression(italic("Salix arctica"))))

# arrange 
(source_growth_heights_plots <- ggarrange(rich_source_height_plot, pul_source_height_plot, arc_source_height_plot, 
                                common.legend = TRUE, legend = "none",
                                ncol = 3, nrow = 1))
ggsave(source_growth_heights_plots, filename ="output/figures/source_growth_heights_plots.png", 
       width = 12, height = 6., units = "in", device = png)


# WIDTH -----
# S.rich ----
colnames(rich_source_width.pred) = c('Site','fit','error', 'lwr', 'upr')

rich_source_width.pred$Site <- ordered(rich_source_width.pred$Site,
                                       levels = c("Qikiqtaruk",
                                                  "Kluane"))

(rich_source_width_plot <-ggplot(rich_source_width.pred) +
    geom_jitter(data = unique_source_mother_rich, aes(x = Site, y = mean_width, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit,colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Canopy width (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_source, labels=c('North', 'South')) +
    scale_y_continuous(breaks = seq(0, 500, by = 100)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North','South')))

# S. pul----
colnames(pul_source_width.pred) = c('Site','fit', 'error', 'lwr', 'upr')

pul_source_width.pred$Site <- ordered(pul_source_width.pred$Site,
                                       levels = c("Qikiqtaruk",
                                                  "Kluane"))

(pul_source_width_plot <-ggplot(pul_source_width.pred) +
    geom_jitter(data = unique_source_mother_pulchra, aes(x = Site, y = mean_width, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit,colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Canopy width (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_source, labels=c('North', 'South')) +
    scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North','South')))

# S.arctica -----
colnames(arc_source_width.pred) = c('Site','fit', 'error', 'lwr', 'upr')

arc_source_width.pred$Site <- plyr::revalue(arc_source_width.pred$Site ,
                                                   c("N. Source" = "Qikiqtaruk",
                                                     "S. Source"="Kluane"))

arc_source_width.pred$Site <- ordered(arc_source_width.pred$Site,
                                      levels = c("Qikiqtaruk",
                                                 "Kluane"))

(arc_source_width_plot <-ggplot(arc_source_width.pred) +
    geom_jitter(data = unique_source_mother_arctica, aes(x = Site, y = mean_width, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit,colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Canopy width (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_source) +
    scale_y_continuous(limits = c(-1, 65), breaks = seq(0, 65, by = 15)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North','South')))

# arrange 
(source_growth_width_plots <- ggarrange(rich_source_width_plot, pul_source_width_plot, arc_source_width_plot, 
                                          common.legend = TRUE, legend = "none",
                                          ncol = 3, nrow = 1))
ggsave(source_growth_width_plots, filename ="output/figures/source_growth_width_plot.png", 
       width = 12, height = 6, units = "in", device = png)

(source_size_panel <- ggarrange(source_growth_heights_plots, source_growth_width_plots, 
                             common.legend = TRUE, legend = "none",
                             ncol = 1, nrow = 2))

ggsave(source_size_panel, filename ="outputs/figures/source_size_plots.png", 
       height = 8, width = 12, dpi = 300, units = "in", device = png)


# STEM ELONG -----
# S.rich ----
colnames(rich_soruce_elong.pred) = c('Site','fit', 'error', 'lwr', 'upr')

rich_soruce_elong.pred$Site <- ordered(rich_soruce_elong.pred$Site,
                                        levels = c("Qikiqtaruk",
                                                   "Kluane"))
unique_source_mother_rich$Site <- ordered(unique_source_mother_rich$Site,
                                          levels = c("Qikiqtaruk",
                                                     "Kluane"))
(rich_source_elong_plot <-ggplot(rich_soruce_elong.pred) +
    geom_jitter(data = unique_source_mother_rich, aes(x = Site, y = mean_stem_elong, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit, colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Stem elongation (mm)\n") +
    xlab("\n Population") +
    scale_color_manual(values=pal_source) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0,100)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North', 'South'))+
    ggtitle(expression(italic("Salix richardsonii"))))
# S. pul----
colnames(source_pul_elong.pred) = c('Site','fit', 'error', 'lwr', 'upr')

source_pul_elong.pred$Site <- ordered(source_pul_elong.pred$Site,
                                       levels = c("Qikiqtaruk",
                                                  "Kluane"))
unique_source_mother_pulchra$Site <- ordered(unique_source_mother_pulchra$Site,
                                          levels = c("Qikiqtaruk",
                                                     "Kluane"))
(pul_source_elong_plot <-ggplot(source_pul_elong.pred) +
    geom_jitter(data = unique_source_mother_pulchra, aes(x = Site, y = mean_stem_elong, colour = Site),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    geom_point(aes(x = Site, y = fit, colour = Site), width=0.5, size = 4, shape = 17)+
    geom_errorbar(aes(x = Site, ymin = lwr, ymax = upr, colour = Site),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Stem elongation (mm)\n") +
    xlab("\n Population") +
    scale_color_manual(values=pal_source) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0,100)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North', 'South'))+
    ggtitle(expression(italic("Salix pulchra"))))

# S. arctica just for reference 
(arc_source_elong_plot <-ggplot(unique_source_mother_arctica) +
    geom_jitter(data = unique_source_mother_arctica, aes(x = Site, y = mean_stem_elong, colour = Site),
                alpha = 1, position = position_jitter(w = 0.09, h = 0), shape = 17)+
    ylab("Stem elongation (mm)\n") +
    xlab("\n Population") +
    scale_color_manual(values=pal_source) +
    scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0,50)) +
    theme_shrub()+
    theme(legend.position = "none", 
          axis.title.x=element_blank())+
    scale_x_discrete(labels = c('North', 'South'))+
    ggtitle(expression(italic("Salix arctica"))))
# panel together 
(source_elong_plots <- ggarrange(rich_source_elong_plot, pul_source_elong_plot, arc_source_elong_plot, 
                                          common.legend = TRUE, legend = "none",
                                          ncol = 3, nrow = 1))
ggsave(source_elong_plots, filename ="output/figures/source_elong_plots.png", 
       width = 12, height = 6., units = "in", device = png)

(source_size_panel <- ggarrange(source_growth_heights_plots, source_growth_width_plots,
                                source_elong_plots,
                                common.legend = TRUE, legend = "none",
                                ncol = 1, nrow = 3))

ggsave(source_size_panel, filename ="outputs/figures/source_size_plots.png", 
       height = 12, width = 12, dpi = 500, units = "in", device = png)

# STEM DIAM -----
# S.rich ----
rich_source_diam <- (conditional_effects(source_rich_diam)) # extracting conditional effects from bayesian model
rich_source_diam_data <- rich_source_diam[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_source_diam_plot <-ggplot(rich_source_diam_data) +
    geom_violin(data = unique_source_mother_rich, aes(x = Site, y = log(Stem_diameter), fill = Site, colour = Site),
                alpha = 0.1)+ # raw data
    geom_jitter(data = unique_source_mother_rich, aes(x = Site, y = log(Stem_diameter), colour = Site),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = Site), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = Site),
                  alpha = 1,  width=0.5) +
    ylab("Stem diameter (log, mm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pul----
pul_source_diam <- (conditional_effects(source_pul_diam)) # extracting conditional effects from bayesian model
pul_source_diam_data <- pul_source_diam[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_source_diam_plot <-ggplot(pul_source_diam_data) +
    geom_violin(data = unique_source_mother_pulchra, aes(x = Site, y = log(Stem_diameter), fill = Site, colour = Site),
                alpha = 0.1)+ # raw data
    geom_jitter(data = unique_source_mother_pulchra, aes(x = Site, y = log(Stem_diameter), colour = Site),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = Site), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = Site),
                  alpha = 1,  width=0.5) +
    ylab("Stem diameter (log, mm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))


# BIOVOLUME ------
# S.rich ----
rich_source_biovol <- (conditional_effects(source_rich_biovol)) # extracting conditional effects from bayesian model
rich_source_biovol_data <- rich_source_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_source_biovol_plot <-ggplot(rich_source_biovol_data) +
    geom_violin(data = unique_source_mother_rich, aes(x = Site, y = log(biovolume), fill = Site, colour = Site),
                alpha = 0.1)+ # raw data
    geom_jitter(data = unique_source_mother_rich, aes(x = Site, y = log(biovolume), colour = Site),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = Site), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = Site),
                  alpha = 1,  width=0.5) +
    ylab("Biovolume (log, cm3)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pul----
pul_source_biovol <- (conditional_effects(source_pul_biovol)) # extracting conditional effects from bayesian model
pul_source_biovol_data <- pul_source_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_source_biovol_plot <-ggplot(pul_source_biovol_data) +
    geom_violin(data = unique_source_mother_pulchra, aes(x = Site, y = log(biovolume), fill = Site, colour = Site),
                alpha = 0.1)+ # raw data
    geom_jitter(data = unique_source_mother_pulchra, aes(x = Site, y = log(biovolume), colour = Site),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = Site), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = Site),
                  alpha = 1,  width=0.5) +
    ylab("Biovolume (log, cm3)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))

# S.arctica -----
arc_source_biovol <- (conditional_effects(source_arc_biovol)) # extracting conditional effects from bayesian model
arc_source_biovol_data <- arc_source_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_source_biovol_plot <-ggplot(arc_source_biovol_data) +
    geom_violin(data = unique_source_mother_arctica, aes(x = Site, y = log(biovolume), fill = Site, colour = Site),
                alpha = 0.1)+ # raw data
    geom_jitter(data = unique_source_mother_arctica, aes(x = Site, y = log(biovolume), colour = Site),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = Site), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = Site),
                  alpha = 1,  width=0.5) +
    ylab("Biovolume (log, cm3)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix arctica"))

# PANEL ----
(growth_source_panel <- ggarrange(rich_source_height_plot, pul_source_height_plot,arc_source_height_plot, 
                            rich_source_elong_plot, pul_source_elong_plot,
                            rich_source_width_plot, pul_source_width_plot,arc_source_width_plot,
                          rich_source_diam_plot, pul_source_diam_plot,
                          rich_source_biovol_plot, pul_source_biovol_plot, arc_source_biovol_plot, 
                          common.legend = TRUE, legend = "bottom", 
                          ncol = 6, nrow = 3))

(source_height_panel <- ggarrange(rich_source_height_plot, pul_source_height_plot, arc_source_height_plot, 
                       common.legend = TRUE, legend = "bottom",
                       ncol = 3, nrow = 1))

(source_elong_panel <- ggarrange( rich_source_elong_plot, pul_source_elong_plot,
                                  common.legend = TRUE, legend = "bottom",
                                  ncol = 2, nrow = 1))

(source_width_panel <- ggarrange( rich_source_width_plot, pul_source_width_plot,arc_source_width_plot,
                                  common.legend = TRUE, legend = "bottom",
                                  ncol = 3, nrow = 1))

(source_diam_panel <- ggarrange( rich_source_diam_plot, pul_source_diam_plot,
                                  common.legend = TRUE, legend = "bottom",
                                  ncol = 2, nrow = 1))


(source_biovol_panel <- ggarrange( rich_source_biovol_plot, pul_source_biovol_plot, arc_source_biovol_plot, 
                                 common.legend = TRUE, legend = "bottom",
                                 ncol = 3, nrow = 1))

