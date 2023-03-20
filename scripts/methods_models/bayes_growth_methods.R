# BAYESIAN growth methods models -----
# Script by Erica
# Last update: 21/02/2023

# 1. Loading libraries ----

library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(knitr) # For kable tables
library(kableExtra) # For kable tables
library(gridExtra)
library(ggpubr)

# 2. Loading data ---- 
unique_source_mother <- read_csv("data/source_pops/unique_source_mother.csv")

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

summary(source_rich_height) # lower heights for QHi
plot(source_rich_height)
pp_check(source_rich_height, type = "dens_overlay", nsamples = 100)  # good) 

# extract output with function
source_rich_height <- model_summ_methods(source_rich_height)

# extraction for model output table
rownames(source_rich_height) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_rich_height_df <- source_rich_height %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra ------
source_pul_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_height) # lower heights for QHi
plot(source_pul_height)
pp_check(source_pul_height, type = "dens_overlay", nsamples = 100)  # good) 

# extract output with function
source_pul_height <- model_summ_methods(source_pul_height)

# extraction for model output table
rownames(source_pul_height) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_pul_height_df <- source_pul_height %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix arctica -----
source_arc_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_height) # no significant difference
plot(source_arc_height)
pp_check(source_arc_height, type = "dens_overlay", nsamples = 100)  # good) 

# extract output with function
source_arc_height <- model_summ_methods(source_arc_height)

# extraction for model output table
rownames(source_arc_height) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_arc_height_df <- source_arc_height %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
source_heights_out <- rbind(source_rich_height_df, source_pul_height_df, 
                            source_arc_height_df) 


# back transforming from log
source_heights_out_back <- source_heights_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

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
                     "Error transformed"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_heights_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 1:12, align = "c") 

# b. STEM ELONGATION ---- 
# Salix richardsonii -------
source_rich_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|SampleYear),
                                data = unique_source_mother_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_elong)# shorter elongations for QHi
plot(source_rich_elong)
pp_check(source_rich_elong,  type = "dens_overlay", nsamples = 100)

# extract output with function
source_rich_elong <- model_summ_methods(source_rich_elong)

# extraction for model output table
rownames(source_rich_elong) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_rich_elong_df <- source_rich_elong %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra -------
source_pul_elong <- brms::brm(log(mean_stem_elong) ~ Site + (1|SampleYear),
                               data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_elong)# shorter elongations for QHi
plot(source_pul_elong)
pp_check(source_pul_elong,  type = "dens_overlay", nsamples = 100)

# extract output with function
source_pul_elong <- model_summ_methods(source_pul_elong)

# extraction for model output table
rownames(source_pul_elong) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_pul_elong_df <- source_pul_elong %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
source_elong_out <- rbind(source_rich_elong_df, source_pul_elong_df) 
                        
# back transforming from log
source_elong_out_back <- source_elong_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(source_elong_out_back, "output/source_elong_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(source_elong_out_back) <- c("Intercept", "Northern source", "Sample year", 
                                       "Sigma", " Intercept", " Northern source", " Sample year", 
                                       " Sigma", "Intercept ", "Northern source ", "Sample year ", 
                                       "Sigma ")

# making sure Rhat keeps the .00 
source_elong_out_back$Rhat <- as.character(formatC(source_elong_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_elong_source <- source_elong_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: mean stem elongation of northern vs southern shrubs in source populations. 
      Model structure per species: log(mean_stem_elongation) ~ Site + (1|SampleYear). 
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
column_spec(kable_elong_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 1:12, align = "c") 

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
source_rich_width<- brms::brm(log(mean_width) ~ Site + (1|SampleYear),
                               data = unique_source_mother_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_width) # significantly LARGER width for QHI shrubs
plot(source_rich_width)
pp_check(source_rich_width, type = "dens_overlay", nsamples = 100)  # fine

# extract output with function
source_rich_width <- model_summ_methods(source_rich_width)

# extraction for model output table
rownames(source_rich_width) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_rich_width_df <- source_rich_width %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra -----
source_pul_width<- brms::brm(log(mean_width) ~ Site + (1|SampleYear),
                              data = unique_source_mother_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_width) # significantly lower width for QHI shrubs
plot(source_pul_width)
pp_check(source_pul_width, type = "dens_overlay", nsamples = 100)  # fine

# extract output with function
source_pul_width <- model_summ_methods(source_pul_width)

# extraction for model output table
rownames(source_pul_width) <- c("Intercept", "Northern source", "Sample year", "Sigma")
source_pul_width_df <- source_pul_width %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")


# Salix arctica -------
source_arc_width<- brms::brm(log(mean_width) ~ Site + (1|SampleYear),
                             data = unique_source_mother_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_width) # no significant diff
plot(source_arc_width)
pp_check(source_arc_width, type = "dens_overlay", nsamples = 100)  # fine

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
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

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
      Model structure per species: log(mean_width) ~ Site + (1|SampleYear). 
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
                     "Error transformed"), digits=2, align = "l") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_width_source, 2, width = NULL, bold = FALSE, italic = TRUE)
row_spec(kable_heights_source, 1:12, align = "c") 


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


# merge and plot all ----
all_source_outputs <- rbind(source_heights_out_back, source_width_out_back,
                            source_elong_out_back, source_diam_out_back,
                            source_biovol_out_back)
