# BAYESIAN maternal effects / propagation effects ------
# script by Erica and Madi
# last update: 28/09/2023 by madi 

# 1. Loading libraries ----
library(brms)
library(tidyverse)
library(ggplot2)
library(tidybayes)
library(gridExtra)
library(ggpubr)
library(knitr) # For kable tables
library(kableExtra) # For kable tables

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
model_summ_simple <- function(x) {
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

# data ----
max_cg_heights <- read.csv("data/common_garden_data_2023/max_heights_cg.csv")
max_cg_widths <- read.csv("data/common_garden_data_2023/max_widths_cg.csv")
max_cg_biovol <-  read.csv("data/common_garden_data_2023/max_biovol_cg.csv")
mother_data <- read.csv("data/source_pops/mother_data.csv")

# format maternal data 
# rename variables to make clear mother metrics before merge
mother_data_merge <-  mother_data %>% 
  dplyr::rename("Mother_Canopy_Height_cm" = "Canopy_Height_cm", 
                "Mother_Width_cm" = "Width_cm",
                "Mother_Width_2_cm" = "Width_2_cm",
                "Mother_Mother_LS" = "Mother_LS",
                "Mother_Length_1_mm" = "Length_1_mm",
                "Mother_Length_2_mm" = "Length_2_mm",
                "Mother_Length_3_mm" = "Length_3_mm",
                "Mother_Stem_Elongation_1_mm" = "Stem_Elongation_1_mm",
                "Mother_Stem_Elongation_2_mm" = "Stem_Elongation_2_mm",
                "Mother_Stem_Elongation_3_mm" = "Stem_Elongation_3_mm",
                "Mother_mean_stem_elong" = "mean_stem_elong",
                "Mother_mean_leaf_length" = "mean_leaf_length",
                "Mother_mean_width" = "mean_width", 
                "Mother_biovolume" = "biovolume") %>% 
  # dplyr::select(-Match, -...1, -...2) %>%   # these columns are useless and will only cause anger and pain
  dplyr::select(-SampleID, - Site) %>%
  dplyr::mutate(Site = case_when(SampleSite %in% c("Kluane", "Kluane Plateau", "Pika Camp", "Printers Pass") ~ 'Kluane', 
                                 SampleSite %in% c("Qikiqtaruk","QHI") ~ 'Qikiqtaruk'))%>%
  dplyr::select(-SampleSite) # also drop this because it's weird inconsistent and will cause merging issues 

mother_data_merge <- mother_data_merge %>% 
  mutate(Species = ifelse(grepl("SA", mother_data_merge $SampleID_standard), "Salix arctica",
                          ifelse(grepl("SR", mother_data_merge $SampleID_standard), "Salix richardsonii", 
                                 ifelse(grepl("SP", mother_data_merge $SampleID_standard), "Salix pulchra", NA))))

mother_data_merge$Species <- as.factor(mother_data_merge$Species)

# need to merge mother data with max values from 2023
max_cg_heights_merge <- max_cg_heights %>% 
  dplyr::select(c(Species, max_canopy_height_cm, population, SampleID_standard)) 

max_cg_biovol_merge <- max_cg_biovol %>% 
  dplyr::select((c(Species, max_biovol, population, SampleID_standard)))

max_cg_widths_merge <- max_cg_widths %>% 
  dplyr::select(c(Species, max_mean_width_cm, population, SampleID_standard))

mother_data_merge_1 <- mother_data_merge %>% 
  dplyr::select(-c(SampleDate, Date_propagated, Match))

mother_cg_height <- full_join(mother_data_merge_1, max_cg_heights_merge, 
                              by = c("SampleID_standard" = "SampleID_standard", 
                                     "Species" = "Species"))

mother_cg_widths <- full_join(mother_cg_height, max_cg_widths_merge, 
                              by = c("SampleID_standard" = "SampleID_standard", 
                                     "Species" = "Species", "population" = "population")) %>% 
  mutate(mother_width_all = coalesce(Mother_mean_width, Mother_Width_cm)) # take mother width one if no mother widths 1 and 2 to make mean 

mother_cg <- full_join(mother_cg_widths, max_cg_biovol_merge, 
                       by = c("SampleID_standard" = "SampleID_standard", 
                              "Species" = "Species"))
write.csv(mother_cg, "data/source_pops/mother_cg_2023.csv")

# 2. Loading data ---- 
mother_cg <- read.csv("data/source_pops/mother_cg_2023.csv")

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
mother_cg_rich_height <- mother_cg_rich %>% 
  drop_na(max_canopy_height_cm) %>% 
  drop_na(Mother_Canopy_Height_cm) # only 62 observations

maternal_rich_height_site <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm) * Site,
                                  data = mother_cg_rich_height, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(maternal_rich_height_site) # not significant 
pp_check(maternal_rich_height_site, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(maternal_rich_height_site, file = "output/maternal_propagation/maternal_ric_height.rds")
mat_rich_height_results <- model_summ_simple(maternal_rich_height_site)
maternal_rich_height.pred <- ggpredict(maternal_rich_height_site)

mat_rich_height_results$Species <- "Salix richardsonii"


# Salix pulchra -------
mother_cg_pulchra_height <- mother_cg_pulchra %>% 
  drop_na(max_canopy_height_cm) %>% 
  drop_na(Mother_Canopy_Height_cm) # 101 observations

maternal_pul_height <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm) * Site,
                                  data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_height) # not significant 
plot(maternal_pul_height)
pp_check(maternal_pul_height, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(maternal_pul_height, file = "output/maternal_propagation/maternal_pul_height.rds")
mat_pul_height_results <- model_summ_simple(maternal_pul_height)
mat_pul_height_results$Species <- "Salix pulchra"

# Salix arctica --------
mother_cg_arctica_height <- mother_cg_arctica %>% 
  drop_na(max_canopy_height_cm) %>% 
  drop_na(Mother_Canopy_Height_cm) #  32 observations

maternal_arc_height <- brms::brm(log(max_canopy_height_cm) ~ log(Mother_Canopy_Height_cm) * Site ,
                                 data = mother_cg_arctica_height, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(maternal_arc_height) #significant effect of maternal heights on child heights 
plot(maternal_arc_height)
pp_check(maternal_arc_height, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(maternal_arc_height, file = "output/maternal_propagation/maternal_arc_height.rds")
mat_arc_height_results <- model_summ_simple(maternal_arc_height)
mat_arc_height_results$Species <- "Salix arctica" 

mat_height_results <- rbind(mat_rich_height_results, mat_pul_height_results, mat_arc_height_results)

# adding spaces before/after each name so they let me repeat them in the table
rownames(mat_height_results) <- c("Intercept", "log(Mother Canopy Height)", "Site:Qikiqtaruk", "log(Mother Canopy Height)*Site", "Sigma",
                             " Intercept", " log(Mother Canopy Height)", " Site:Qikiqtaruk", " log(Mother Canopy Height)*Site", " Sigma", 
                            "Intercept ", "log(Mother Canopy Height) ", "Site:Qikiqtaruk ", "log(Mother Canopy Height)*Site ", "Sigma ")

# making sure Rhat keeps the .00 
mat_height_results$Rhat <- as.character(formatC(mat_height_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(mat_height_results, "output/maternal_propagation/maternal_height_output.csv")

kable_mat_height <- mat_height_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: maternal height effects analysis across species. 
      Model structure per species: (log(max height) ~ log(maternal height) * Site.", 
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
column_spec(kable_mat_height, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_mat_height, file = "output/maternal_propagation/mat_height_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# WIDTH ------
# Salix richardsonii ------
mother_cg_rich_width <- mother_cg_rich %>% 
  drop_na(max_mean_width_cm) %>% 
  drop_na(mother_width_all) # only 45 observations

maternal_rich_width <- brms::brm(log(max_mean_width_cm) ~ log(mother_width_all) * Site,
                                  data = mother_cg_rich_width, family = gaussian(), chains = 3,
                                  iter = 5000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_width) # not significant  
plot(maternal_rich_width)
pp_check(maternal_rich_width, type = "dens_overlay", ndraws = 100)  # good) 
saveRDS(maternal_rich_width, file = "output/maternal_propagation/maternal_ric_width.rds")

mat_rich_width_results <- model_summ_simple(maternal_rich_width)
mat_rich_width_results$Species <- "Salix richardsonii"

# Salix pulchra -------
mother_cg_pulchra_width <- mother_cg_pulchra %>% 
  drop_na(max_mean_width_cm) %>% 
  drop_na(mother_width_all) # 58 observations

maternal_pul_width <- brms::brm(log(max_mean_width_cm) ~ log(mother_width_all) * Site,
                                 data = mother_cg_pulchra_width, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_width) # not significant but negative estimate 
plot(maternal_pul_width)
pp_check(maternal_pul_width, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(maternal_pul_width, file = "output/maternal_propagation/maternal_pul_width.rds")

mat_pul_width_results <- model_summ_simple(maternal_pul_width)
mat_pul_width_results$Species <- "Salix pulchra"

# Salix arctica --------
mother_cg_arctica_width <- mother_cg_arctica %>% 
  drop_na(max_mean_width_cm) %>% 
  drop_na(mother_width_all) # 20 observations

maternal_arc_width <- brms::brm(log(max_mean_width_cm) ~ log(mother_width_all) * Site,
                                data = mother_cg_arctica_width, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_arc_width) # not significant
plot(maternal_arc_width)
pp_check(maternal_arc_width, type = "dens_overlay", nsamples = 100)  # okay 
saveRDS(maternal_arc_width, file = "output/maternal_propagation/maternal_arc_width.rds")
mat_arc_width_results <- model_summ_simple(maternal_arc_width)
mat_arc_width_results$Species <- "Salix arctica"

mat_width_results <- rbind(mat_rich_width_results,mat_pul_width_results,mat_arc_width_results)

# adding spaces before/after each name so they let me repeat them in the table
rownames(mat_width_results) <- c("Intercept", "log(Mother Canopy Width)", "Site:Qikiqtaruk", "log(Mother Canopy Width)*Site", "Sigma",
                                  " Intercept", " log(Mother Canopy Width)", " Site:Qikiqtaruk", " log(Mother Canopy Width)*Site", " Sigma", 
                                  "Intercept ", "log(Mother Canopy Width) ", "Site:Qikiqtaruk ", "log(Mother Canopy Width)*Site ", "Sigma ")


# making sure Rhat keeps the .00 
mat_width_results$Rhat <- as.character(formatC(mat_width_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(mat_width_results, "output/maternal_propagation/maternal_width_output.csv")

kable_mat_width <- mat_width_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: maternal width effects analysis across species. 
      Model structure per species: (log(max height) ~ log(maternal height) * Site.", 
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
column_spec(kable_mat_width, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_mat_width, file = "output/maternal_propagation/mat_width_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# BIOVOLUME -------
# Salix richardsonii ------
maternal_rich_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) * Site,
                                 data = mother_cg_rich, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_rich_biovol) # not significant
plot(maternal_rich_biovol)
pp_check(maternal_rich_biovol, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(maternal_rich_biovol, file = "output/maternal_propagation/maternal_ric_biovol.rds")

mat_rich_biovol_results <- model_summ_simple(maternal_rich_biovol)
mat_rich_biovol_results$Species <- "Salix richardsonii"

# Salix pulchra -------
maternal_pul_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) * Site,
                                  data = mother_cg_pulchra, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_pul_biovol) # not significant
plot(maternal_pul_biovol)
pp_check(maternal_pul_biovol, type = "dens_overlay", nsamples = 100)  # good) 
saveRDS(maternal_pul_biovol, file = "output/maternal_propagation/maternal_pul_biovol.rds")

mat_pul_biovol_results <- model_summ_simple(maternal_pul_biovol)
mat_pul_biovol_results$Species <- "Salix pulchra"

# Salix arctica --------
maternal_arc_biovol <- brms::brm(log(max_biovol) ~ log(Mother_biovolume) * Site,
                                 data = mother_cg_arctica, family = gaussian(), chains = 3,
                                 iter = 3000, warmup = 1000, 
                                 control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(maternal_arc_biovol) # not significant
plot(maternal_arc_biovol)
pp_check(maternal_arc_biovol, type = "dens_overlay", nsamples = 100)  # decent? 
saveRDS(maternal_arc_biovol, file = "output/maternal_propagation/maternal_arc_biovol.rds")

mat_arc_biovol_results <- model_summ_simple(maternal_arc_biovol)
mat_arc_biovol_results$Species <- "Salix arctica"

mat_biovol_results <- rbind(mat_rich_biovol_results,mat_pul_biovol_results,mat_arc_biovol_results)
# adding spaces before/after each name so they let me repeat them in the table
rownames(mat_biovol_results) <- c("Intercept", "log(Mother Canopy Biovolume)", "Site:Qikiqtaruk", "log(Mother Canopy Biovolume)*site", "Sigma",
                                 " Intercept", " log(Mother Canopy Biovolume)", " Site:Qikiqtaruk", " log(Mother Canopy Biovolume)*site", " Sigma", 
                                 "Intercept ", "log(Mother Canopy Biovolume) ", "Site:Qikiqtaruk ", "log(Mother Canopy Biovolume)*site ", "Sigma ")

# making sure Rhat keeps the .00 
mat_biovol_results$Rhat <- as.character(formatC(mat_biovol_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# save df of results 
write.csv(mat_biovol_results, "output/maternal_propagation/maternal_biovol_output.csv")
kable_mat_biovol <- mat_biovol_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: maternal biovolume effects analysis across species. 
      Model structure per species: (log(max biovolume) ~ log(maternal biovolume) * Site.", 
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
column_spec(kable_mat_biovol, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_mat_biovol, file = "output/maternal_propagation/mat_biovol_results.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# 4.2. PROPAGATION EFFECTS ------

# Height vs Cutting length ------
# Salix richardsonii ------
prop_height_rich <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) * Site,
                                  data = mother_cg_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_height_rich) # not significant
plot(prop_height_rich)
pp_check(prop_height_rich, type = "dens_overlay", ndraws = 100)  # good 
saveRDS(prop_height_rich, file = "output/maternal_propagation/propagation_ric_height.rds")

prop_rich_results <- model_summ_simple(prop_height_rich)
prop_rich_results$Species <- "Salix richardsonii"

# Salix pulchra -------
prop_height_pul <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) * Site,
                       data = mother_cg_pulchra, family = gaussian(), chains = 3,
                       iter = 3000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_height_pul) # not significant
plot(prop_height_pul)
pp_check(prop_height_pul, type = "dens_overlay", ndraws = 100)  # good 
saveRDS(prop_height_pul, file = "output/maternal_propagation/propagation_pul_height.rds")

prop_pul_results <- model_summ_simple(prop_height_pul)
prop_pul_results$Species <- "Salix pulchra"

# Salix arctica --------
prop_height_arc <- brms::brm(log(max_canopy_height_cm) ~ log(Cutting_length) * Site,
                      data = mother_cg_arctica, family = gaussian(), chains = 3,
                      iter = 3000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_height_arc) # not significant
plot(prop_height_arc)
pp_check(prop_height_arc, type = "dens_overlay", ndraws = 100)  # okay? kind of wonky 
saveRDS(prop_height_arc, file = "output/maternal_propagation/propagation_arc_height.rds")

prop_arc_results <- model_summ_simple(prop_height_arc)
prop_arc_results$Species <- "Salix arctica"

prop_results <- rbind(prop_rich_results,prop_pul_results,prop_arc_results)
# adding spaces before/after each name so they let me repeat them in the table
rownames(prop_results) <- c("Intercept", "log(Cutting length)", "Site:Qikiqtaruk", "log(Cutting length) * Site", "Sigma",
                                  " Intercept", " log(Cutting length)", " Site:Qikiqtaruk", " log(Cutting length) * Site", " Sigma", 
                                  "Intercept ", "log(Cutting length) ", "Site:Qikiqtaruk ", "log(Cutting length) * Site ", "Sigma ")

# making sure Rhat keeps the .00 
prop_results$Rhat <- as.character(formatC(prop_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification
# save df of results 
write.csv(prop_results, "output/maternal_propagation/prop_height_output.csv")

kable_prop_height <- prop_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: propagation effects on maximum height analysis across species. 
      Model structure per species: (log(max height) ~ log(cutting length)*Site.", 
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
column_spec(kable_prop_height, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_prop_height, file = "output/maternal_propagation/kable_prop_height.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# Biovolume vs Cutting length ------
# Salix richardsonii ------
prop_biovol_rich <- brms::brm(log(max_biovol) ~ log(Cutting_length) * Site,
                       data = mother_cg_rich, family = gaussian(), chains = 3,
                       iter = 3000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_rich) # not significant
plot(prop_biovol_rich)
pp_check(prop_biovol_rich, type = "dens_overlay", ndraws = 100)  # kind of bimodal? 
saveRDS(prop_biovol_rich, file = "output/maternal_propagation/propagation_ric_biovol.rds")


# Salix pulchra -------
prop_biovol_pul <- brms::brm(log(max_biovol) ~ log(Cutting_length) * Site,
                              data = mother_cg_pulchra, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_pul) # not significant
plot(prop_biovol_pul)
pp_check(prop_biovol_pul, type = "dens_overlay", ndraws = 100)  # good
saveRDS(prop_biovol_pul, file = "output/maternal_propagation/propagation_pul_biovol.rds")

# Salix arctica --------
prop_biovol_arc <- brms::brm(log(max_biovol) ~ log(Cutting_length) * Site,
                             data = mother_cg_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_biovol_arc) # not significant
plot(prop_biovol_arc)
pp_check(prop_biovol_arc, type = "dens_overlay", ndraws = 100)  # good) 
saveRDS(prop_biovol_arc, file = "output/maternal_propagation/propagation_arc_biovol.rds")

# Width vs Cutting length ------
# Salix richardsonii ------
prop_width_rich <- brms::brm(log(max_mean_width_cm) ~ log(Cutting_length) * Site,
                              data = mother_cg_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_width_rich) # not significant
plot(prop_width_rich)
pp_check(prop_width_rich, type = "dens_overlay", ndraws = 100)  # kind of bimodal-y 
saveRDS(prop_width_rich, file = "output/maternal_propagation/propagation_ric_width.rds")
prop_rich_width_results <- model_summ_simple(prop_width_rich)
prop_rich_width_results$Species <- "Salix richardsonii"

# Salix pulchra  ------
prop_width_pul <- brms::brm(log(max_mean_width_cm) ~ log(Cutting_length) * Site,
                             data = mother_cg_pulchra, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_width_pul) # not significant
plot(prop_width_pul)
pp_check(prop_width_pul, type = "dens_overlay", ndraws = 100)  # good 
saveRDS(prop_width_pul, file = "output/maternal_propagation/propagation_pul_width.rds")

prop_pul_width_results <- model_summ_simple(prop_width_pul)
prop_pul_width_results$Species <- "Salix pulchra"

# Salix arctica  ------
prop_width_arc <- brms::brm(log(max_mean_width_cm) ~ log(Cutting_length) * Site,
                            data = mother_cg_arctica, family = gaussian(), chains = 3,
                            iter = 3000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_width_arc) # not significant
plot(prop_width_arc)
pp_check(prop_width_arc, type = "dens_overlay", ndraws = 100)  # meh 
saveRDS(prop_width_arc, file = "output/maternal_propagation/propagation_arc_width.rds")

prop_arc_width_results <- model_summ_simple(prop_width_arc)
prop_arc_width_results$Species <- "Salix arctica"

prop_width_results <- rbind(prop_rich_width_results, prop_pul_width_results, prop_arc_width_results)
# adding spaces before/after each name so they let me repeat them in the table
rownames(prop_width_results) <- c("Intercept", "log(Cutting length)", "Site:Qikiqtaruk", "log(Cutting length) * Site", "Sigma",
                            " Intercept", " log(Cutting length)", " Site:Qikiqtaruk", " log(Cutting length) * Site", " Sigma", 
                            "Intercept ", "log(Cutting length) ", "Site:Qikiqtaruk ", "log(Cutting length) * Site ", "Sigma ")

# making sure Rhat keeps the .00 
prop_width_results$Rhat <- as.character(formatC(prop_width_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification
# save df of results 
write.csv(prop_width_results, "output/maternal_propagation/prop_width_output.csv")

kable_prop_width <- prop_width_results %>% 
  kbl(caption="Table.xxx BRMS model outputs: propagation effects on maximum width analysis across species. 
      Model structure per species: (log(max width) ~ log(cutting length)*Site.", 
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
column_spec(kable_prop_width, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_prop_width, file = "output/maternal_propagation/kable_prop_width.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# Cutting length vs mother canopy height  ------

# Salix richardsonii ------
prop_cutting_rich <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) * Site,
                              data = mother_cg_rich, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_rich) # YES. Taller mothers, longer cuttings 

ggscatter(mother_cg_rich, x = "Cutting_length", y = "Mother_Canopy_Height_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cutting length (cm)", ylab = "mother height (cm)")

plot(prop_cutting_rich)
pp_check(prop_cutting_rich, type = "dens_overlay", nsamples = 100)  # good) 

# with width because that's a better metric for S. arctica 
ggscatter(mother_cg_rich, x = "Cutting_length", y = "Mother_mean_width", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cutting length (cm)", ylab = "mother width (cm)")

# Salix pulchra -------
prop_cutting_pul <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) * Site,
                               data = mother_cg_pulchra, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_pul) # no

ggscatter(mother_cg_pulchra, x = "Cutting_length", y = "Mother_Canopy_Height_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cutting length (cm)", ylab = "mother height (cm)")

plot(prop_cutting_pul)
pp_check(prop_cutting_pul, type = "dens_overlay", nsamples = 100)  # good) 

# with width too
ggscatter(mother_cg_pulchra, x = "Cutting_length", y = "Mother_mean_width", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cutting length (cm)", ylab = "mother width (cm)")

# Salix arctica --------
prop_cutting_arc <- brms::brm(log(Cutting_length) ~ log(Mother_Canopy_Height_cm) * Site,
                              data = mother_cg_arctica, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(prop_cutting_arc) # longer cuttings from taller mothers 

ggscatter(mother_cg_arctica, x = "Cutting_length", y = "Mother_Canopy_Height_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cutting length (cm)", ylab = "mother height (cm)")

plot(prop_cutting_arc)
pp_check(prop_cutting_arc, type = "dens_overlay", nsamples = 100)  # good)
# with width because that's a better metric for S. arctica 
ggscatter(mother_cg_arctica, x = "Cutting_length", y = "Mother_mean_width", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cutting length (cm)", ylab = "mother width (cm)")

# 5. DATA VISUALISATION --------

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

# Maternal heights ------
#Salix richardsonii child height vs mother height
#rich_height_maternal <- (conditional_effects(maternal_rich_height_site)) # extracting conditional effects from bayesian model
#rich_height_maternal_data <- rich_height_maternal[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

#(rich_height_mat_plot <-ggplot(rich_height_maternal_data) +
#    geom_point(data = mother_cg_rich, aes(x = log(Mother_Canopy_Height_cm), y = log(max_canopy_height_cm)),
#               alpha = 0.5)+ # raw data
#    geom_line(aes(x = effect1__, y = estimate__))+
#    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
#                  alpha = 0.2) +
#    ylab("Child canopy height (log, cm) \n") +
#    xlab("\nMother canopy height (log, cm) " ) +
#    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
#    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
#    theme_shrub())

(rich_height_mat_plot <-  mother_cg_rich %>%
  add_predicted_draws(maternal_rich_height_site, allow_new_levels = TRUE) %>%
  ggplot(aes(x = log(Mother_Canopy_Height_cm), y = log(max_canopy_height_cm), color = Site, fill = Site)) +
           stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
           geom_point(data = mother_cg_rich) +
           ylab("Child canopy height (log, cm) \n") +
           xlab("\nMother canopy height \n (log, cm) ")+ 
   theme_shrub() +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95)+ 
   labs(title = "Salix richardsonii", size = 20, family = "Helvetica Light"))

#Salix pulchra child height vs mother height
#pul_height_maternal <- (conditional_effects(maternal_pul_height)) # extracting conditional effects from bayesian model
#pul_height_maternal_data <- pul_height_maternal[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

#(pul_height_mat_plot <-ggplot(pul_height_maternal_data) +
#    geom_point(data = mother_cg_pulchra, aes(x = log(Mother_Canopy_Height_cm), y = log(max_canopy_height_cm)),
#               alpha = 0.5)+ # raw data
#    geom_line(aes(x = effect1__, y = estimate__))+
#    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__),
#                alpha = 0.2) +
#    ylab("Child canopy height (log, cm) \n") +
#    xlab("\nMother canopy height (log, cm) " ) +
#    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
#    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
#    theme_shrub())

(pul_height_mat_plot <-  mother_cg_pulchra %>%
    add_predicted_draws(maternal_pul_height, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_Canopy_Height_cm), y = log(max_canopy_height_cm), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_pulchra) +
    ylab("Child canopy height (log, cm) \n") +
    xlab("\nMother canopy height \n (log, cm) ")+ 
    theme_shrub() +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) + 
   labs(title = "Salix pulchra", size = 20, family = "Helvetica Light"))

# Salix arctica maximum height vs maternal height 
(arc_height_mat_plot <-  mother_cg_arctica %>%
    add_predicted_draws(maternal_arc_height, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_Canopy_Height_cm), y = log(max_canopy_height_cm), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_arctica) +
    ylab("Child canopy height (log, cm) \n") +
    xlab("\nMother canopy height \n (log, cm) ") + 
    theme_shrub() +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95)+ 
   labs(title = "Salix arctica", size = 20, family = "Helvetica Light"))

# arrange maternal height and max height figure 
(mat_height_plots <- ggarrange(rich_height_mat_plot, pul_height_mat_plot, arc_height_mat_plot, 
                              common.legend = TRUE, legend = "bottom",
                              ncol = 3, nrow = 1))

ggsave(mat_height_plots, filename ="outputs/figures/maternal_height_panel_2023.png",
       width = 14.67, height = 6.53, units = "in")


# Maternal biovol ------
(rich_biovol_mat_plot <-  mother_cg_rich %>%
    add_predicted_draws(maternal_rich_biovol, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_biovolume), y = log(max_biovol), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_rich) +
    theme_shrub() +
    ylab("Child biovolume (log, cm3) \n") +
    xlab("\nMother biovolume (log, cm3) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix richardsonii"))

(pul_biovol_mat_plot <-  mother_cg_pulchra %>%
    add_predicted_draws(maternal_pul_biovol, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_biovolume), y = log(max_biovol), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_pulchra) +
    theme_shrub() +
    ylab("Child biovolume (log, cm3) \n") +
    xlab("\nMother biovolume (log, cm3) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix pulchra"))

(arc_biovol_mat_plot <-  mother_cg_arctica %>%
    add_predicted_draws(maternal_arc_biovol, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_biovolume), y = log(max_biovol), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_arctica) +
    theme_shrub() +
    ylab("Child biovolume (log, cm3) \n") +
    xlab("\nMother biovolume (log, cm3) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix arctica"))

# arrange maternal width fig. 
(mat_biovol_plots <- ggarrange(rich_biovol_mat_plot, pul_biovol_mat_plot, arc_biovol_mat_plot, 
                               common.legend = TRUE, legend = "bottom",
                               ncol = 3, nrow = 1))

# Maternal width ------
(rich_width_mat_plot <-  mother_cg_rich %>%
   add_predicted_draws(maternal_rich_width, allow_new_levels = TRUE) %>%
   ggplot(aes(x = log(mother_width_all), y = log(max_mean_width_cm), color = Site, fill = Site)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = mother_cg_rich) +
   theme_shrub() +
   ylab("Child canopy width (log, cm) \n") +
   xlab("\nMother canopy width (log, cm) ")+ 
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
   labs(title = "Salix richardsonii"))

(pul_width_mat_plot <-  mother_cg_pulchra %>%
    add_predicted_draws(maternal_pul_width, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_mean_width), y = log(max_mean_width_cm), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_pulchra) +
    theme_shrub() +
    ylab("Child canopy width (log, cm) \n") +
    xlab("\nMother canopy width (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix pulchra"))

(arc_width_mat_plot <-  mother_cg_arctica %>%
    add_predicted_draws(maternal_arc_width, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_mean_width), y = log(max_mean_width_cm), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_arctica) +
    theme_shrub() +
    ylab("Child canopy width (log, cm) \n") +
    xlab("\nMother canopy width (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix arctica"))

# arrange maternal width fig. 
(mat_width_plots <- ggarrange(rich_width_mat_plot, pul_width_mat_plot, arc_width_mat_plot, 
                              common.legend = TRUE, legend = "bottom",
                              ncol = 3, nrow = 1))

# Propagation: Height vs cutting length------
(rich_prop_1 <-  mother_cg_rich %>%
   add_predicted_draws(prop_rich, allow_new_levels = TRUE) %>%
   ggplot(aes(x = log(Cutting_length), y = log(max_canopy_height_cm), color = Site, fill = Site)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = mother_cg_rich) +
   theme_shrub() +
   ylab("Child canopy height (log, cm) \n") +
   xlab("\nMother cutting length (log, cm) ")+ 
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
   labs(title = "Salix richardsonii"))

(pul_prop_1 <-  mother_cg_pulchra %>%
    add_predicted_draws(prop_pul, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Cutting_length), y = log(max_canopy_height_cm), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_pulchra) +
    theme_shrub() +
    ylab("Child canopy height (log, cm) \n") +
    xlab("\nMother cutting length (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix pulchra"))

(arc_prop_1 <-  mother_cg_arctica %>%
    add_predicted_draws(prop_arc, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Cutting_length), y = log(max_canopy_height_cm), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_arctica) +
    theme_shrub() +
    ylab("Child canopy height (log, cm) \n") +
    xlab("\nMother cutting length (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix arctica"))

# arrange maternal width fig. 
(prop_plots_1 <- ggarrange(rich_prop_1, pul_prop_1, arc_prop_1, 
                              common.legend = TRUE, legend = "bottom",
                              ncol = 3, nrow = 1))

# Propagation:biovol vs cutting length------
(rich_prop_2 <-  mother_cg_rich %>%
   add_predicted_draws(prop_biovol_rich, allow_new_levels = TRUE) %>%
   ggplot(aes(x = log(Cutting_length), y = log(max_biovol), color = Site, fill = Site)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = mother_cg_rich) +
   theme_shrub() +
   ylab("Child biovolume (log, cm3) \n") +
   xlab("\nMother cutting length (log, cm) ")+ 
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
   labs(title = "Salix richardsonii"))

(pul_prop_2 <-  mother_cg_pulchra %>%
    add_predicted_draws(prop_biovol_pul, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Cutting_length), y = log(max_biovol), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_pulchra) +
    theme_shrub() +
    ylab("Child biovolume (log, cm3) \n") +
    xlab("\nMother cutting length (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix pulchra"))

(arc_prop_2 <-  mother_cg_arctica %>%
    group_by(Site) %>%
    add_predicted_draws(prop_biovol_arc, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Cutting_length), y = log(max_biovol), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_arctica) +
    theme_shrub() +
    ylab("Child biovolume (log, cm3) \n") +
    xlab("\nMother cutting length (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix arctica"))


# arrange maternal width fig. 
(prop_plots_2 <- ggarrange(rich_prop_2, pul_prop_2, arc_prop_2, 
                           common.legend = TRUE, legend = "bottom",
                           ncol = 3, nrow = 1))

# Propagation:Cutting length vs mother canopy height -----
(rich_prop_3 <-  mother_cg_rich %>%
   add_predicted_draws(prop_cutting_rich, allow_new_levels = TRUE) %>%
   ggplot(aes(x = log(Mother_Canopy_Height_cm), y = log(Cutting_length), color = Site, fill = Site)) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = mother_cg_rich) +
   theme_shrub() +
   ylab("Mother cutting length (log, cm) \n") +
   xlab("\nMother canopy height (log, cm) ")+ 
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
   labs(title = "Salix richardsonii"))

(pul_prop_3 <-  mother_cg_pulchra %>%
    add_predicted_draws(prop_cutting_pul, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_Canopy_Height_cm), y = log(Cutting_length), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_pulchra) +
    theme_shrub() +
    ylab("Mother cutting length (log, cm) \n") +
    xlab("\nMother canopy height (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix pulchra"))

(arc_prop_3 <-  mother_cg_arctica %>%
    add_predicted_draws(prop_cutting_arc, allow_new_levels = TRUE) %>%
    ggplot(aes(x = log(Mother_Canopy_Height_cm), y = log(Cutting_length), color = Site, fill = Site)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
    geom_point(data = mother_cg_arctica) +
    theme_shrub() +
    ylab("Mother cutting length (log, cm) \n") +
    xlab("\nMother canopy height (log, cm) ")+ 
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85)+ 
    labs(title = "Salix arctica"))

# arrange maternal width fig. 
(prop_plots_3 <- ggarrange(rich_prop_3, pul_prop_3, arc_prop_3, 
                           common.legend = TRUE, legend = "bottom",
                           ncol = 3, nrow = 1))
