# BAYESIAN growth results models -----
# Script by Erica and Madi
# Last update: 31/10/2023 by Madi aka halloween
# Code adapted from coding club tutorial by Louise Litrico:
# https://ourcodingclub.github.io/tutorials/brms/ 

# 3 available options for growth model structure: 
# 1) max height across all years (WE ARE DOING THIS ONE.)
# OR  2) using the last three years of August height data as a repeated measures test (without the shrubID nesting in the random effects), 
# which assumes that the shrubs aren’t really getting taller any more (seems true from the data, but that isn’t a priori), or 
# OR 3) only using the August 2022 data (aka the last time point).

# Loading libraries ----
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(knitr) # For kable tables
library(kableExtra) # For kable tables
library(gridExtra)
library(ggpubr)
library(ggeffects)

# Loading data ---- 
all_CG_source_growth <- read.csv("data/common_garden_data_2023/all_data_2023.csv") # 2023 data
# Only using max growth variables values
max_widths_cg <- read.csv("data/common_garden_data_2023/max_widths_cg.csv")
max_heights_cg <- read.csv("data/common_garden_data_2023/max_heights_cg.csv")
max_biovol_cg <- read.csv("data/common_garden_data_2023/max_biovol_cg.csv")
max_elong_cg <- read.csv("data/common_garden_data_2023/max_elong_cg.csv")
max_diam_cg <- read.csv("data/common_garden_data_2023/max_diam_cg.csv")

CG_growth <- all_CG_source_growth %>% # how many unique samples
  dplyr::filter(population != "S. Source") %>% 
  dplyr::filter(population != "N. Source") %>% 
  group_by(SampleID_standard) %>% 
  slice(1) 

CG_growth_bed <- all_CG_source_growth %>% # how many per bed
  dplyr::filter(population != "S. Source") %>% 
  dplyr::filter(population != "N. Source") %>% 
  drop_na(Canopy_Height_cm) %>% 
  group_by(Year, Bed) %>% 
  summarise(bed_number = length(unique(SampleID_standard)))


# Functions -------
# 1. scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# 2. extract model result function =====

model_summ_growth <- function(x) {
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

model_summ_RE <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random_age = sum$random$Sample_age
  random_ID = sum$random$SampleID_standard
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random_age$effect <- "random"
  random_ID$effect <- "random"
  sigma$effect <- "residual"
  
  fixed$nobs <- obs  # add column with number of observations
  random_age$nobs <- obs
  random_ID$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random_ID)[row.names(random_ID) == "sd(Intercept)...3"] <- "Sample_ID"
  row.names(random_age)[row.names(random_age) == "sd(Intercept)...4"] <- "Sample_age"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random_age, random_ID, sigma))  # merge together
}

# 2. extract model result function =====

model_summ_time <- function(x) {
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

# Data wrangling ------
# max height  
max_heights_cg$Species <- as.factor(max_heights_cg$Species)
max_heights_cg$SampleID_standard <- as.factor(max_heights_cg$SampleID_standard)
max_heights_cg$population <- as.factor(max_heights_cg$population)
max_heights_cg$Site <- as.factor(max_heights_cg$Site)
max_heights_cg$Sample_Date <- as.POSIXct(max_heights_cg$Sample_Date, format = '%Y/%m/%d')
max_heights_cg$Year <- as.factor(max_heights_cg$Year)
max_heights_cg$Sample_age <- as.factor(max_heights_cg$Sample_age)

# ordering levels
max_heights_cg$population <- plyr::revalue(max_heights_cg$population , 
                                          c("Northern"="N. Garden",
                                            "Southern"="S. Garden"))

# Separating into 3 datasets: one per spp.
max_heights_cg_rich <- max_heights_cg %>%
  filter (Species == "Salix richardsonii")

max_heights_cg_pul <- max_heights_cg %>%
  filter (Species == "Salix pulchra") 

max_heights_cg_arc <- max_heights_cg %>%
  filter (Species == "Salix arctica") 

# look at mean max heights per species 
mean(max_heights_cg_rich$max_canopy_height_cm) # 31.97
range(max_heights_cg_rich$max_canopy_height_cm) # 0.7 - 127.0
mean(max_heights_cg_pul$max_canopy_height_cm) # 21.87
range(max_heights_cg_pul$max_canopy_height_cm) # 1.5 101.0
mean(max_heights_cg_arc$max_canopy_height_cm) # 5.31
range(max_heights_cg_arc$max_canopy_height_cm) # 0.3 15.2


# exploring variables distribution
hist(max_heights_cg_rich$max_canopy_height_cm) # right skew
hist(max_heights_cg_pul$max_canopy_height_cm) #  right skew
hist(max_heights_cg_arc$max_canopy_height_cm)#  right skew/normal?

# max width 
max_widths_cg$Species <- as.factor(max_widths_cg$Species)
max_widths_cg$SampleID_standard <- as.factor(max_widths_cg$SampleID_standard)
max_widths_cg$population <- as.factor(max_widths_cg$population)
max_widths_cg$Site <- as.factor(max_widths_cg$Site)
max_widths_cg$Sample_Date <- as.POSIXct(max_widths_cg$Sample_Date, format = '%Y/%m/%d')
max_widths_cg$Year <- as.factor(max_widths_cg$Year)
max_widths_cg$Sample_age <- as.factor(max_widths_cg$Sample_age)

# ordering levels
max_widths_cg$population <- plyr::revalue(max_widths_cg$population , 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_widths_cg_rich <- max_widths_cg %>%
  filter (Species == "Salix richardsonii")

max_widths_cg_pul <- max_widths_cg %>%
  filter (Species == "Salix pulchra") 

max_widths_cg_arc <- max_widths_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_widths_cg_rich$max_mean_width_cm) # right skew
hist(max_widths_cg_pul$max_mean_width_cm) #  right skew
hist(max_widths_cg_arc$max_mean_width_cm)#  right skew

# max stem elongation 
max_elong_cg$Species <- as.factor(max_elong_cg$Species)
max_elong_cg$SampleID_standard <- as.factor(max_elong_cg$SampleID_standard)
max_elong_cg$population <- as.factor(max_elong_cg$population)
max_elong_cg$Site <- as.factor(max_elong_cg$Site)
max_elong_cg$Sample_Date <- as.POSIXct(max_elong_cg$Sample_Date, format = '%Y/%m/%d')
max_elong_cg$Year <- as.factor(max_elong_cg$Year)
max_elong_cg$Sample_age <- as.factor(max_elong_cg$Sample_age)

# ordering levels
max_elong_cg$population <- plyr::revalue(max_elong_cg$population , 
                                          c("Northern"="Northern Garden",
                                            "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_elong_cg_rich <- max_elong_cg %>%
  filter (Species == "Salix richardsonii")

max_elong_cg_pul <- max_elong_cg %>%
  filter (Species == "Salix pulchra") 

max_elong_cg_arc <- max_elong_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_elong_cg_rich$max_stem_elong) # right skew
hist(max_elong_cg_pul$max_stem_elong) #  right skew
hist(max_elong_cg_arc$max_stem_elong)#  right skew

# max biovolume 
max_biovol_cg$Species <- as.factor(max_biovol_cg$Species)
max_biovol_cg$SampleID_standard <- as.factor(max_biovol_cg$SampleID_standard)
max_biovol_cg$population <- as.factor(max_biovol_cg$population)
max_biovol_cg$Site <- as.factor(max_biovol_cg$Site)
max_biovol_cg$Sample_Date <- as.POSIXct(max_biovol_cg$Sample_Date, format = '%Y/%m/%d')
max_biovol_cg$Year <- as.factor(max_biovol_cg$Year)
max_biovol_cg$Sample_age <- as.factor(max_biovol_cg$Sample_age)

# ordering levels
max_biovol_cg$population <- plyr::revalue(max_biovol_cg$population , 
                                         c("Northern"="Northern Garden",
                                           "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_biovol_cg_rich <- max_biovol_cg %>%
  filter (Species == "Salix richardsonii")

max_biovol_cg_pul <- max_biovol_cg %>%
  filter (Species == "Salix pulchra") 

max_biovol_cg_arc <- max_biovol_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_biovol_cg_rich$max_biovol) # right skew
hist(max_biovol_cg_pul$max_biovol, breaks = 30) #  right skew - so weird
hist(max_biovol_cg_arc$max_biovol)#  right skew

# max stem diameter
max_diam_cg$Species <- as.factor(max_diam_cg$Species)
max_diam_cg$SampleID_standard <- as.factor(max_diam_cg$SampleID_standard)
max_diam_cg$population <- as.factor(max_diam_cg$population)
max_diam_cg$Site <- as.factor(max_diam_cg$Site)
max_diam_cg$Sample_Date <- as.POSIXct(max_diam_cg$Sample_Date, format = '%Y/%m/%d')
max_diam_cg$Year <- as.factor(max_diam_cg$Year)
max_diam_cg$Sample_age <- as.factor(max_diam_cg$Sample_age)

# ordering levels
max_diam_cg$population <- plyr::revalue(max_diam_cg$population , 
                                          c("Northern"="Northern Garden",
                                            "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_diam_cg_rich <- max_diam_cg %>%
  filter (Species == "Salix richardsonii")

max_diam_cg_pul <- max_diam_cg %>%
  filter (Species == "Salix pulchra") 

max_diam_cg_arc <- max_diam_cg %>%
  filter (Species == "Salix arctica") 

# exploring variables distribution
hist(max_diam_cg_rich$max_stem_diam) # right skew
hist(max_diam_cg_pul$max_stem_diam, breaks = 30) # right skew - so weird
hist(max_diam_cg_arc$max_stem_diam,  breaks = 30)# right skew


# MODELLING -------
# NB one model per species

# 1. CANOPY HEIGHT -----

# S. richardsonii ----
# all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale <- scale(all_CG_source_growth_garden_rich_height$Canopy_Height_cm, center = T)  # scaling time
# model
garden_rich_height <- brms::brm(log(max_canopy_height_cm) ~ population +(1|Sample_age),
                                data = max_heights_cg_rich, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_height) # significantly higher canopy heights for southern pop.
plot(garden_rich_height) # fine
pp_check(garden_rich_height,  type = "dens_overlay", nsamples = 100)  # good
saveRDS(garden_rich_height, file = "output/models/garden_rich_height.rds")
garden_rich_height <- readRDS("output/models/garden_rich_height.rds")
rich_height.pred <- ggpredict(garden_rich_height, terms = c('population'))

# estimate for northern: 2.4197356 = exp(2.4197356) = 11.24289 # updated with 2023 data
# estimate for southern: 2.4197356 + 1.1531522 = 3.572888 -> exp(3.572888) = 35.61931
# times larger
35.64226/11.12991
# 3.202385

# extract output with function
rich_extract <- model_summ_growth(garden_rich_height)

ric_extract <- rich_extract %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

ric_extract_2 <- ric_extract %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
ric_extract_2[2,1] <- ric_extract_2[2,1] + ric_extract_2[1,1]
# change lower CI by adding 
ric_extract_2[2,3] <- ric_extract_2[2,3] + ric_extract_2[1,3]
# change upper CI
ric_extract_2[2,4] <- ric_extract_2[2,4] + ric_extract_2[1,4]

# extraction for model output table
rownames(ric_extract) <- c("Intercept  ", "Southern Garden  ", "Sample age  ", "Sigma  ")
rownames(ric_extract_2) <- c("Intercept ", "Southern Garden ", "Sample age ", "Sigma ")

ric_extract_df_1 <- ric_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")

ric_extract_df <- ric_extract_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

ric_extract_all <- full_join(ric_extract_df_1, ric_extract_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(ric_extract_all) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")

# S. pulchra -----
garden_pul_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data =max_heights_cg_pul, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_height) # significantly higher canopy heights for southern pop.
plot(garden_pul_height) # fine
pp_check(garden_pul_height, type = "dens_overlay", ndraws = 100)  # good) 
saveRDS(garden_pul_height, file = "output/models/garden_pul_height.rds")
garden_pul_height <- readRDS("output/models/garden_pul_height.rds")
pul_height.pred <- ggpredict(garden_pul_height, terms = c('population'))

# estimate for northern: 2.3038383 = exp(2.3038383) = 10.01254
# estimate for southern: 2.3038383 + 0.9394663 = 3.243305 -> exp(3.243305) = 25.61825
# %diff
(25.61825-10.01254)/10.01254
# 1.558616

# times larger
25.61825/10.01254
# 2.558616

# extract output with function
pul_extract <- model_summ_growth(garden_pul_height)

pul_extract <- pul_extract %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

pul_extract_2 <- pul_extract %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
pul_extract_2[2,1] <- pul_extract_2[2,1] + pul_extract_2[1,1]
# change lower CI by adding 
pul_extract_2[2,3] <- pul_extract_2[2,3] + pul_extract_2[1,3]
# change upper CI
pul_extract_2[2,4] <- pul_extract_2[2,4] + pul_extract_2[1,4]

# extraction for model output table
rownames(pul_extract) <- c("Intercept  ", "Southern Garden  ", "Sample age  ", "Sigma  ")
rownames(pul_extract_2) <- c("Intercept ", "Southern Garden ", "Sample age ", "Sigma ")

pul_extract_df_1 <- pul_extract %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")

pul_extract_df <- pul_extract_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_extract_all <- full_join(pul_extract_df_1, pul_extract_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(pul_extract_all) <- c("Intercept ", "Southern Garden ", "Sample age ", "Sigma  ")

# S. arctica -----
garden_arc_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data = max_heights_cg_arc, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_height) # NOT significant difference (again makes sense!)
plot(garden_arc_height) # fine
pp_check(garden_arc_height, type = "dens_overlay", ndraws = 100) # good
saveRDS(garden_arc_height, file = "output/models/garden_arc_height.rds")
garden_arc_height <- readRDS("output/models/garden_arc_height.rds")
arc_height.pred <- ggpredict(garden_arc_height, terms = c('population'))

# extract output with function
arc_extract <- model_summ_growth(garden_arc_height)

arc_extract <- arc_extract %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

arc_extract_2 <- arc_extract %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
arc_extract_2[2,1] <- arc_extract_2[2,1] + arc_extract_2[1,1]
# change lower CI by adding 
arc_extract_2[2,3] <- arc_extract_2[2,3] + arc_extract_2[1,3]
# change upper CI
arc_extract_2[2,4] <- arc_extract_2[2,4] + arc_extract_2[1,4]

# extraction for model output table
rownames(arc_extract) <- c("Intercept  ", "Southern Garden  ", "Sample age  ", "Sigma  ")
rownames(arc_extract_2) <- c("Intercept ", "Southern Garden ", "Sample age ", "Sigma ")

arc_extract_df_1 <- arc_extract %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")

arc_extract_df <- arc_extract_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_extract_all <- full_join(arc_extract_df_1, arc_extract_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(arc_extract_all) <- c("Intercept  ", "Southern Garden  ", "Sample age  ", "Sigma  ")

# merging all extracted outputs
garden_heights_out <- rbind(ric_extract_all, pul_extract_all, arc_extract_all) 

garden_heights_out <- garden_heights_out %>%
  dplyr::rename("Estimate_log_sum" = "Estimate (log sum)")

# back transforming from log
garden_heights_out_back <- garden_heights_out %>%
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
write.csv(garden_heights_out_back, "output/garden_heights_out_back.csv")
garden_heights_out_back <- read.csv("output/garden_heights_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_heights_out_back) <- c("Intercept", "Southern Garden", "Sample age", 
                                       "Sigma", " Intercept", " Southern Garden", " Sample age", 
                                       " Sigma", "Intercept ", "Southern Garden ", "Sample age ", 
                                       "Sigma ")

# making sure Rhat keeps the .00 
garden_heights_out_back$Rhat <- as.character(formatC(garden_heights_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_heights <- garden_heights_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: max. heights of northern vs southern shrubs in the common garden. 
      Log transformed output in the table below.", 
      col.names = c( "Population",
        "Species",
        "Estimate (log)",
        "Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)",
                     "Estimate (log sum)",  "Lower 95% CI 
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
row_spec(kable_heights, 1:12, align = "c") 
column_spec(kable_heights, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_heights,file = "outputs/tables/kable_heights.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# WIDTH ------
# S. richardsonii -----
# garden_rich_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
#                                 data = max_widths_cg_rich, family = gaussian(), chains = 3,
#                                 iter = 5000, warmup = 1000, 
#                                 control = list(max_treedepth = 15, adapt_delta = 0.99))

garden_rich_width <- brms::brm(log(max_mean_width_cm) ~ population,
                               data = max_widths_cg_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_width) # significantly larger widths for southern shrubs in garden
plot(garden_rich_width) # fine
pp_check(garden_rich_width,  type = "dens_overlay", ndraws = 100) # good
saveRDS(garden_rich_width, file = "output/models/garden_rich_width.rds")
garden_rich_width <- readRDS("output/models/garden_rich_width.rds")
rich_width.pred <- ggpredict(garden_rich_width, terms = c('population'))

# extract output with function
rich_extract_width <- model_summ_no_re(garden_rich_width)

rich_extract_width <- rich_extract_width %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

rich_extract_width_2 <- rich_extract_width %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
rich_extract_width_2[2,1] <- rich_extract_width_2[2,1] + rich_extract_width_2[1,1]
# change lower CI by adding 
rich_extract_width_2[2,3] <- rich_extract_width_2[2,3] + rich_extract_width_2[1,3]
# change upper CI
rich_extract_width_2[2,4] <- rich_extract_width_2[2,4] + rich_extract_width_2[1,4]

# extraction for model output table
rownames(rich_extract_width) <- c("Intercept  ", "Southern Garden  ", "Sigma  ")
rownames(rich_extract_width_2) <- c("Intercept ", "Southern Garden ", "Sigma ")

rich_extract_width_df_1 <- rich_extract_width %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")

rich_extract_width_df <- rich_extract_width_2 %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

rich_width_all <- full_join(rich_extract_width_df_1, rich_extract_width_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(rich_width_all) <- c("Intercept", "Southern Garden", "Sigma")

# S. pulchra -----
garden_pul_width <- brms::brm(log(max_mean_width_cm) ~ population,
                               data = max_widths_cg_pul, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_pul_width) # significantly larger widths for southern shrubs in garden
plot(garden_pul_width) # fine
pp_check(garden_pul_width,  type = "dens_overlay", nsamples = 100) # fine
saveRDS(garden_pul_width, file = "output/models/garden_pul_width.rds")
garden_pul_width <-  readRDS("output/models/garden_pul_width.rds")
pul_width.pred <- ggpredict(garden_pul_width, terms = c('population'))

# extract output with function
pul_extract_width <- model_summ_no_re(garden_pul_width)

pul_extract_width <- pul_extract_width %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

pul_extract_width_2 <- pul_extract_width %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
pul_extract_width_2[2,1] <- pul_extract_width_2[2,1] + pul_extract_width_2[1,1]
# change lower CI by adding 
pul_extract_width_2[2,3] <- pul_extract_width_2[2,3] + pul_extract_width_2[1,3]
# change upper CI
pul_extract_width_2[2,4] <- pul_extract_width_2[2,4] + pul_extract_width_2[1,4]

# extraction for model output table
rownames(pul_extract_width) <- c("Intercept  ", "Southern Garden  ", "Sigma  ")
rownames(pul_extract_width_2) <- c("Intercept ", "Southern Garden ", "Sigma ")

pul_extract_width_df_1 <- pul_extract_width %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")

pul_extract_width_df <- pul_extract_width_2 %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

pul_width_extract_all <- full_join(pul_extract_width_df_1, pul_extract_width_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(pul_width_extract_all) <- c("Intercept ", "Southern Garden ", "Sigma  ")

# S. arctica -----
garden_arc_width <- brms::brm(log(max_mean_width_cm) ~ population,
                               data = max_widths_cg_arc, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_arc_width) # NOT significantly larger widths for southern shrubs in garden (makes sense)
plot(garden_arc_width) # fine
pp_check(garden_arc_width,  type = "dens_overlay", ndraws = 100) # fine
saveRDS(garden_arc_width, file = "output/models/garden_arc_width.rds")
garden_arc_width <- readRDS("output/models/garden_arc_width.rds")
arc_width.pred <- ggpredict(garden_arc_width, terms = c('population'))

# extract output with function
arc_extract_width <- model_summ_no_re(garden_arc_width)

arc_extract_width <- arc_extract_width %>% 
  dplyr::rename("l_95_CI_log_og" = "l-95% CI", 
                "u_95_CI_log_og" = "u-95% CI", 
                "Estimate (log og)"= "Estimate")

arc_extract_width_2 <- arc_extract_width %>% 
  dplyr::rename("l_95_CI_log_sum" = "l_95_CI_log_og", 
                "u_95_CI_log_sum" = "u_95_CI_log_og",
                "Estimate (log sum)"= "Estimate (log og)")

# change estimates by adding estimate to other rows 
arc_extract_width_2[2,1] <- arc_extract_width_2[2,1] + arc_extract_width_2[1,1]
# change lower CI by adding 
arc_extract_width_2[2,3] <- arc_extract_width_2[2,3] + arc_extract_width_2[1,3]
# change upper CI
arc_extract_width_2[2,4] <- arc_extract_width_2[2,4] + arc_extract_width_2[1,4]

# extraction for model output table
rownames(arc_extract_width) <- c("Intercept  ", "Southern Garden  ", "Sigma  ")
rownames(arc_extract_width_2) <- c("Intercept ", "Southern Garden ", "Sigma ")

arc_extract_width_df_1 <- arc_extract_width %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log og)") %>%
  relocate("nobs", .before = "effect")

arc_extract_width_df <- arc_extract_width_2 %>% 
  mutate(Species = rep("Salix arctica")) %>%
  relocate("Species", .before = "Estimate (log sum)") %>%
  relocate("nobs", .before = "effect")%>%
  dplyr::select(-Est.Error)

arc_width_extract_all <- full_join(arc_extract_width_df_1, arc_extract_width_df, 
                             by = c("effect" = "effect", "nobs"="nobs",
                                    "Bulk_ESS"="Bulk_ESS", "Tail_ESS"="Tail_ESS",
                                    "Species"="Species", "Rhat"="Rhat"))

rownames(arc_width_extract_all) <- c("Intercept  ", "Southern Garden  ", "Sigma  ")

# merging all extracted outputs
all_width_output <- rbind(rich_width_all, pul_width_extract_all, arc_width_extract_all)

all_width_output <- all_width_output %>%
  dplyr::rename("Estimate_log_sum" = "Estimate (log sum)")

# back transforming from log
garden_width_out_back <- all_width_output %>%
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
write.csv(garden_width_out_back, "output/garden_width_out_back.csv")
garden_width_out_back <- read.csv("output/garden_width_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_width_out_back) <- c("Intercept", "Southern Garden",  
                                      "Sigma", " Intercept", " Southern Garden",  
                                      " Sigma", "Intercept ", "Southern Garden ",  
                                      "Sigma ")

# making sure Rhat keeps the .00 
garden_width_out_back$Rhat <- as.character(formatC(garden_width_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_width <- garden_width_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: max. width of northern vs southern shrubs in the common garden. 
      Log transformed output in the table below.", 
      col.names = c( "Species",
                     "Estimate (log)",
                     "Error",
                     "Lower 95% CI (log)",
                     "Upper 95% CI (log)",
                     "Estimate (log sum)",  "Lower 95% CI 
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
row_spec(kable_width, 1:12, align = "c") 
column_spec(kable_width, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_width,file = "outputs/tables/kable_width.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# 3. BIOVOLUME ------
# S. richardsonii -----
# model
garden_rich_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                                data = max_biovol_cg_rich, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_rich_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol) # fine
pp_check(garden_rich_biovol,  type = "dens_overlay", nsamples = 100) # fine
saveRDS(garden_rich_biovol, file = "output/models/garden_rich_biovol.rds")

# extract output with function
rich_biovol_extract <- model_summ_growth(garden_rich_biovol)

# extraction for model output table
rownames(rich_biovol_extract) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_biovol_extract_df <- rich_biovol_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
  # "Sample Size" = rep(81)) %>% 
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. pulchra -----
garden_pul_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                               data = max_biovol_cg_pul, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_pul_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_pul_biovol) # fine
pp_check(garden_pul_biovol,  type = "dens_overlay", nsamples = 100) # fine
saveRDS(garden_pul_biovol, file = "output/models/garden_pul_biovol.rds")

# extract output with function
pul_extract_biovol <- model_summ_growth(garden_pul_biovol)

# extraction for model output table
rownames(pul_extract_biovol) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
pul_extract_biovol_df <- pul_extract_biovol %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  # "Sample Size" = rep(114)) %>%
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. arctica -----
garden_arc_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                               data = max_biovol_cg_arc, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_biovol) # NOT significant diff. 
plot(garden_arc_biovol) # fine
pp_check(garden_arc_biovol,  type = "dens_overlay", nsamples = 100) # fine
saveRDS(garden_arc_biovol, file = "output/models/garden_arc_biovol.rds")

# extract output with function
arc_extract_biovol <- model_summ_growth(garden_arc_biovol)

# extraction for model output table
rownames(arc_extract_biovol) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
arc_extract_biovol_df <- arc_extract_biovol %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(53)) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
garden_biovol_out <- rbind(rich_biovol_extract_df, pul_extract_biovol_df, 
                           arc_extract_biovol_df) 

# back transforming from log
garden_biovol_out_back <- garden_biovol_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_biovol_out_back, "output/garden_biovol_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_biovol_out_back) <- c("Intercept", "Southern Garden", "Sample age", 
                                      "Sigma", " Intercept", " Southern Garden", " Sample age", 
                                      " Sigma", "Intercept ", "Southern Garden ", "Sample age ", 
                                      "Sigma ")

# making sure Rhat keeps the .00 
garden_biovol_out_back$Rhat <- as.character(formatC(garden_biovol_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_biovol <- garden_biovol_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: max. biovolume (height x width 1 x width 2) of northern vs southern shrubs in the common garden. 
      Model structure per species: (log(max_biovolume) ~ population + (1|Sample_age). 
      Model output back-transformed in the table below.", 
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
column_spec(kable_biovol, 2, width = NULL, bold = FALSE, italic = TRUE)

# STEM ELONGATION ------

stem_elong_summary <- all_CG_source_growth %>% 
  dplyr::group_by(Species, population) %>% 
  summarize(mean_elong = (mean(mean_stem_elong, na.rm = TRUE) ))

rich_CG_source_growth <- all_CG_source_growth %>% 
  dplyr::filter(Species == "Salix richardsonii")

all_rich_elong <- brms::brm((mean_stem_elong) ~ population,
                               data = rich_CG_source_growth, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(all_rich_elong)

# S. richardsonii -----
# model
garden_rich_elong <- brms::brm(log(max_stem_elong) ~ population,
                               data = max_elong_cg_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_elong) # southern pop significantly longer stem elong
plot(garden_rich_elong) # fine
pp_check(garden_rich_elong, type = "dens_overlay", nsamples = 100) # good
#saveRDS(garden_rich_elong, file = "output/models/garden_rich_elong.rds")
ggpred_elong <- ggpredict(garden_rich_elong, terms = "population")

# extract output with function
rich_extract_elong <- model_summ_growth(garden_rich_elong)

# extraction for model output table
rownames(rich_extract_elong) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_extract_elong_df <- rich_extract_elong %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(89)) %>% 
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. pulchra -----
garden_pul_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                              data = max_elong_cg_pul, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_elong)# southern pop significantly longer stem elong
plot(garden_pul_elong) # fine
pp_check(garden_pul_elong, type = "dens_overlay", nsamples = 100)  # fine

saveRDS(garden_pul_elong, file = "output/models/garden_pul_elong.rds")
# output with function
pul_extract_elong <- model_summ_growth(garden_pul_elong)

# extraction for model output table
rownames(pul_extract_elong) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
pul_extract_elong_df <- pul_extract_elong %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(127)) %>%
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. Arctica -----
garden_arc_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                              data = max_elong_cg_arc, family = gaussian(), chains = 3,
                              iter = 3000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_arc_elong) # southern shrubs SIGNIFICANTLY SHORTER elong! 
plot(garden_arc_elong) # fine
pp_check(garden_arc_elong,  type = "dens_overlay", nsamples = 100) # fine
saveRDS(gardgarden_arc_elongen_pul_elong, file = "output/models/garden_arc_elong.rds")

# extract output with function
arc_extract_elong <- model_summ_growth(garden_arc_elong)

# extraction for model output table
rownames(arc_extract_elong) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
arc_extract_elong_df <- arc_extract_elong %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(60)) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
garden_elong_out <- rbind(rich_extract_elong_df, pul_extract_elong_df, 
                          arc_extract_elong_df) 

# back transforming from log
garden_elong_out_back <- garden_elong_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_elong_out_back, "output/garden_elong_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_elong_out_back) <- c("Intercept", "Southern Garden", "Sample age", 
                                     "Sigma", " Intercept", " Southern Garden", " Sample age", 
                                     " Sigma", "Intercept ", "Southern Garden ", "Sample age ", 
                                     "Sigma ")

# making sure Rhat keeps the .00 
garden_elong_out_back$Rhat <- as.character(formatC(garden_elong_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_elong <- garden_elong_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: max. stem elongation of northern vs southern shrubs in the common garden. 
      Model structure per species: (log(max_stem_elongation) ~ population + (1|Sample_age). 
      Mode output back-transformed in the table below.", 
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
column_spec(kable_elong, 2, width = NULL, bold = FALSE, italic = TRUE)


# STEM DIAMETER ------
# S. Richardsonii -----
# model
garden_rich_diam <- brms::brm(log(max_stem_diam) ~ population + (1|Sample_age),
                               data = max_diam_cg_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_diam) # significantly larger stem diameters for southern shrubs in garden
plot(garden_rich_diam) # fine
pp_check(garden_rich_diam,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
rich_extract_diam <- model_summ_growth(garden_rich_diam)

# extraction for model output table
rownames(rich_extract_diam) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_extract_diam_df <- rich_extract_diam %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(81)) %>% 
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. Pulchra -----
garden_pul_diam <- brms::brm(log(max_stem_diam) ~ population + (1|Sample_age),
                              data = max_diam_cg_pul, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_diam) # significantly larger stem diameters for southern shrubs in garden
plot(garden_pul_diam) # fine
pp_check(garden_pul_diam,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
pul_extract_diam <- model_summ_growth(garden_pul_diam)

# extraction for model output table
rownames(pul_extract_diam) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
pul_extract_diam_df <- pul_extract_diam %>% 
  mutate(Species = rep("Salix pulchra")) %>%
  # "Sample Size" = rep(114)) %>%
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")


# S. Arctica -----
garden_arc_diam <- brms::brm(log(max_stem_diam) ~ population + (1|Sample_age),
                              data = max_diam_cg_arc, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_diam) # no significant diff.
plot(garden_arc_diam) # fine
pp_check(garden_arc_diam,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
arc_extract_diam <- model_summ_growth(garden_arc_diam)

# extraction for model output table
rownames(arc_extract_diam) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
arc_extract_diam_df <- arc_extract_diam %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(53)) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
garden_diam_out <- rbind(rich_extract_diam_df, pul_extract_diam_df, 
                          arc_extract_diam_df) 

# back transforming from log
garden_diam_out_back <- garden_diam_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_diam_out_back, "output/garden_diam_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_diam_out_back) <- c("Intercept", "Southern Garden", "Sample age", 
                                     "Sigma", " Intercept", " Southern Garden", " Sample age", 
                                     " Sigma", "Intercept ", "Southern Garden ", "Sample age ", 
                                     "Sigma ")

# making sure Rhat keeps the .00 
garden_diam_out_back$Rhat <- as.character(formatC(garden_diam_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_diam <- garden_diam_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: max. stem diameter of northern vs southern shrubs in the common garden. 
      Model structure per species: (log(max_stem_diameter) ~ population + (1|Sample_age). 
      Model output back-transformed in the table below.", 
      col.names = c("Species","Estimate",
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
column_spec(kable_diam, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_diam,file = "output/kable_diam.pdf",
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex =TRUE,
           density = 300)

# DATA VISUALISATION -----
pal_garden <- c("#332288", "#7ad151")


theme_shrub <- function(){ theme(legend.position = "bottom",
                                 axis.title.x = element_text(face="bold", family = "Helvetica Light", size=16),
                                 axis.text.x  = element_text(vjust=0.5, size=15, family = "Helvetica Light", colour = "black", angle = 270), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=16),
                                 axis.text.y  = element_text(vjust=0.5, size=16, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 16, family = "Helvetica Light", face = "italic", hjust = 0.5),
                                 legend.title = element_text(size=18, family = "Helvetica Light"),
                                 legend.key=element_blank(),
                                 strip.text.x = element_text(
                                   size = 15, color = "black", face = "italic", family = "Helvetica Light"),
                                 strip.background = element_blank(),
                                 legend.text=element_text(size = 18, family = "Helvetica Light"))}

max_heights_cg_rich$population <- ordered(max_heights_cg_rich$population, 
                                      levels = c( "N. Garden", 
                                                  "S. Garden"))
max_heights_cg_pul$population <- ordered(max_heights_cg_pul$population, 
                                          levels = c( "N. Garden", 
                                                      "S. Garden"))
max_heights_cg_arc$population <- ordered(max_heights_cg_arc$population, 
                                          levels = c( "N. Garden", 
                                                      "S. Garden"))


# CANOPY HEIGHT -----
# S. richardsonii ----
# ric_heights <- (conditional_effects(garden_rich_height)) # extracting conditional effects from bayesian model
# ric_height_data <- ric_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# # [[1]] is to extract the first term in the model which in our case is population

colnames(rich_height.pred) = c('population','fit', 'lwr', 'upr')

(ric_max_height_plot <-ggplot(rich_height.pred) +
    geom_jitter(data = max_heights_cg_rich, aes(x = population, y = max_canopy_height_cm, colour = population),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0))+
    geom_point(aes(x = population, y = fit,colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Max. canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_garden, labels=c('Northern', 'Southern')) +
    scale_y_continuous(breaks = seq(0, 140, by = 20)) +
    theme_shrub()+
    theme(axis.text.x  = element_blank(), 
           axis.title.x=element_blank())+
    scale_x_discrete(labels = c('Northern','Southern'))+
    ggtitle(expression(italic("Salix richardsonii"))))

   
# S. pulchra ----
# pul_heights <- (conditional_effects(garden_pul_height)) # extracting conditional effects from bayesian model
# pul_height_data <- pul_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# # [[1]] is to extract the first term in the model which in our case is population

colnames(pul_height.pred) = c('population','fit', 'lwr', 'upr')

(pul_max_height_plot <-ggplot(pul_height.pred) +
    geom_jitter(data = max_heights_cg_pul, aes(x = population, y = max_canopy_height_cm, colour = population),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0))+
    geom_point(aes(x = population, y = fit,colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Max. canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_garden, labels=c('Northern', 'Southern')) +
    scale_y_continuous(breaks = seq(0, 100, by = 20)) +
    theme_shrub()+
    theme( axis.text.x  = element_blank(), 
           axis.title.x=element_blank())+
    scale_x_discrete(labels = c('Northern','Southern'))+
    ggtitle(expression(italic("Salix pulchra"))))

# S. arctica ----
# arc_heights <- (conditional_effects(garden_arc_height)) # extracting conditional effects from bayesian model
# arc_height_data <- arc_heights[[1]] # making the extracted model outputs into a dataset (for plotting)

colnames(arc_height.pred) = c('population','fit', 'lwr', 'upr')

(arc_max_height_plot <-ggplot(arc_height.pred) +
    geom_jitter(data = max_heights_cg_arc, aes(x = population, y = max_canopy_height_cm, colour = population),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0))+
    geom_point(aes(x = population, y = fit,colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  linewidth = 1, alpha = 1, width = 0.75) +
    ylab("Max. canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values=pal_garden, labels=c('Northern', 'Southern')) +
    scale_y_continuous(breaks = seq(0, 15, by = 5)) +
    theme_shrub()+
    theme( axis.text.x  = element_blank(), 
           axis.title.x=element_blank())+
    scale_x_discrete(labels = c('Northern','Southern'))+
    ggtitle(expression(italic("Salix arctica"))))

# arrange 
(growth_maxheights <- ggarrange(ric_max_height_plot, pul_max_height_plot, arc_max_height_plot, 
                           common.legend = TRUE, legend = "none",
                           ncol = 3, nrow = 1))

ggsave(growth_maxheights, filename ="output/figures/growth_maxheights.png", 
       width = 14.67, height = 6.53, units = "in", device = png)

# WIDTH ----
# S. richardsonii ----
# rich_width <- (conditional_effects(garden_rich_width)) # extracting conditional effects from bayesian model
# rich_width_data <- rich_width[[1]] # making the extracted model outputs into a dataset (for plotting)
# rich_width_data$population <- plyr::revalue(rich_width_data$population , 
#                                             c("Northern Garden"="N. Garden",
#                                               "Southern Garden"="S. Garden"))
# 
# rich_width_data$effect1__ <- plyr::revalue(rich_width_data$effect1__ , 
#                                            c("Northern Garden"="N. Garden",
#                                              "Southern Garden"="S. Garden"))

colnames(rich_width.pred) = c('population', 'fit', 'error', 'lwr', 'upr')

(rich_max_width_plot <-ggplot(rich_width.pred) +
    geom_jitter(data = max_widths_cg_rich, aes(x = population, y = max_mean_width_cm, colour = population),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0))+
    geom_point(aes(x = population, y = fit,colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  linewidth = 1, alpha = 1, width = 0.5) +
    ylab("Max. width (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values= pal_garden, labels=c('North', 'South')) +
    scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
    theme_shrub()+
    theme( axis.text.x  = element_text(angle = 0), 
           axis.title.x=element_blank(), 
           legend.position = "none")+
    scale_x_discrete(labels = c('North', 'South')))

# S. pulchra ----
# pul_width <- (conditional_effects(garden_pul_width)) # extracting conditional effects from bayesian model
# pul_width_data <- pul_width[[1]] # making the extracted model outputs into a dataset (for plotting)

colnames(pul_width.pred) = c('population', 'fit', 'error', 'lwr', 'upr')

(pul_max_width_plot <-ggplot(pul_width.pred) +
    geom_jitter(data = max_widths_cg_pul, aes(x = population, y = max_mean_width_cm, colour = population),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0))+
    geom_point(aes(x = population, y = fit,colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  linewidth = 1, alpha = 1, width = 0.5) +
    ylab("Max. width (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values= pal_garden, labels=c('North', 'South')) +
    scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 25)) +
    theme_shrub()+
    theme( axis.text.x  = element_text(angle = 0), 
           axis.title.x=element_blank(), 
           legend.position = "none")+
    scale_x_discrete(labels = c('North', 'South')))

# S. arctica ----
# arc_width <- (conditional_effects(garden_arc_width)) # extracting conditional effects from bayesian model
# arc_width_data <- arc_width[[1]] # making the extracted model outputs into a dataset (for plotting)

colnames(arc_width.pred) = c('population', 'fit', 'error', 'lwr', 'upr')

(arc_max_width_plot <-ggplot(arc_width.pred) +
    geom_jitter(data = max_widths_cg_pul, aes(x = population, y = max_mean_width_cm, colour = population),
                alpha = 0.3, position = position_jitter(w = 0.09, h = 0))+
    geom_point(aes(x = population, y = fit,colour = population), width=0.5, size = 4)+
    geom_errorbar(aes(x = population, ymin = lwr, ymax = upr, colour = population),
                  linewidth = 1, alpha = 1, width = 0.5) +
    ylab("Max. width (cm)\n") +
    xlab("\n Population" ) +
    scale_color_manual(values= pal_garden, labels=c('North', 'South')) +
    scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, by = 15)) +
    theme_shrub()+
    theme( axis.text.x  = element_text(angle = 0), 
           axis.title.x=element_blank(), 
           legend.position = "none")+
    scale_x_discrete(labels = c('North', 'South')))

# arrange 
(growth_maxwidth <- ggarrange(rich_max_width_plot, pul_max_width_plot, arc_max_width_plot, 
                                common.legend = TRUE, legend = "none",
                              labels = c("G)", "H)", "I)"),
                                ncol = 3, nrow = 1, 
  font.label=list(color="black",size = 12)))

ggsave(growth_maxwidth, filename ="output/figures/growth_max_widths.png", 
       width = 14.67, height = 6.53, units = "in", device = png)

(max_size_panel <- ggarrange(growth_maxheights, growth_maxwidth, 
                                common.legend = TRUE, legend = "bottom",
                                ncol = 1, nrow = 2))

ggsave(max_size_panel, filename ="outputs/figures/max_size_plots.png", 
       height = 8, width = 12, dpi = 300, units = "in", device = png)

# STEM ELONG  --------
# S. richardsonii ----
rich_elong <- (conditional_effects(garden_rich_elong)) # extracting conditional effects from bayesian model
rich_elong_data <- rich_elong[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_elong_plot <-ggplot(rich_elong_data) +
    geom_violin(data = max_elong_cg_rich, aes(x = population, y = log(max_stem_elong), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_elong_cg_rich, aes(x = population, y = log(max_stem_elong), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. stem elongation (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pulchra ------
pul_elong <- (conditional_effects(garden_pul_elong)) # extracting conditional effects from bayesian model
pul_elong_data <- pul_elong[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_elong_plot <-ggplot(pul_elong_data) +
    geom_violin(data = max_elong_cg_pul, aes(x = population, y = log(max_stem_elong), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_elong_cg_pul, aes(x = population, y = log(max_stem_elong), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. stem elongation (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica ------
arc_elong <- (conditional_effects(garden_arc_elong)) # extracting conditional effects from bayesian model
arc_elong_data <- arc_elong[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_elong_plot <-ggplot(arc_elong_data) +
    geom_violin(data = max_elong_cg_arc, aes(x = population, y = log(max_stem_elong), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_elong_cg_arc, aes(x = population, y = log(max_stem_elong), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. stem elongation (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix arctica"))

# arrange 
(growth_maxelong <- ggarrange(ric_elong_plot, pul_elong_plot, arc_elong_plot, 
                                common.legend = TRUE, legend = "bottom",
                                ncol = 3, nrow = 1))

# BIOVOLUME----
# S. richardsonii ----
rich_biovol <- (conditional_effects(garden_rich_biovol)) # extracting conditional effects from bayesian model
rich_biovol_data <- rich_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_biovol_plot <-ggplot(rich_biovol_data) +
    geom_violin(data = max_biovol_cg_rich, aes(x = population, y = log(max_biovol), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_biovol_cg_rich, aes(x = population, y = log(max_biovol), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. biovolume (log, cm3)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pulchra ----
pul_biovol <- (conditional_effects(garden_pul_biovol)) # extracting conditional effects from bayesian model
pul_biovol_data <- pul_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_biovol_plot <-ggplot(pul_biovol_data) +
    geom_violin(data = max_biovol_cg_pul, aes(x = population, y = log(max_biovol), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_biovol_cg_pul, aes(x = population, y = log(max_biovol), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. biovolume (log, cm3)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica ----
arc_biovol <- (conditional_effects(garden_arc_biovol)) # extracting conditional effects from bayesian model
arc_biovol_data <- arc_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_biovol_plot <-ggplot(arc_biovol_data) +
    geom_violin(data = max_biovol_cg_arc, aes(x = population, y = log(max_biovol), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_biovol_cg_arc, aes(x = population, y = log(max_biovol), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. biovolume (log, cm3)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix arctica"))

# arrange 
(growth_maxbiovol <- ggarrange(ric_biovol_plot, pul_biovol_plot, arc_biovol_plot, 
                              common.legend = TRUE, legend = "bottom",
                              ncol = 3, nrow = 1))





# STEM DIAM ------
# S. richardsonii ----
rich_diam <- (conditional_effects(garden_rich_diam)) # extracting conditional effects from bayesian model
rich_diam_data <- rich_diam[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_diam_plot <-ggplot(rich_diam_data) +
    geom_violin(data = max_diam_cg_rich, aes(x = population, y = log(max_stem_diam), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_diam_cg_rich, aes(x = population, y = log(max_stem_diam), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. stem diameter (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix richardsonii"))

# S. pulchra ----
pul_diam <- (conditional_effects(garden_pul_diam)) # extracting conditional effects from bayesian model
pul_diam_data <- pul_diam[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_diam_plot <-ggplot(pul_diam_data) +
    geom_violin(data = max_diam_cg_pul, aes(x = population, y = log(max_stem_diam), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_diam_cg_pul, aes(x = population, y = log(max_stem_diam), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. stem diameter (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix pulchra"))

# S. arctica  ----
arc_diam <- (conditional_effects(garden_arc_diam)) # extracting conditional effects from bayesian model
arc_diam_data <- arc_diam[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_diam_plot <-ggplot(arc_diam_data) +
    geom_violin(data = max_diam_cg_arc, aes(x = population, y = log(max_stem_diam), fill = population, colour = population),
                alpha = 0.1)+ # raw data
    geom_jitter(data = max_diam_cg_arc, aes(x = population, y = log(max_stem_diam), colour = population),
                alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Max. stem diameter (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() +
    labs(title = "Salix arctica"))

# arrange 
(growth_maxdiam <- ggarrange(ric_diam_plot, pul_diam_plot, arc_diam_plot, 
                              common.legend = TRUE, legend = "bottom",
                              ncol = 3, nrow = 1))

# try with ggpredict () ------
# Model predictions - get number of species per degree from here
ggpred_height <- ggpredict(garden_arc_height, terms = "population")
colnames(ggpred_height) = c('population', 'fit', 'lwr', 'upr', 'dunno')

(arc_height_plot <-ggplot(ggpred_height) +
    geom_point(data = max_heights_cg_arc, aes(x = population, y = log(max_canopy_height_cm), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = population , y = log(fit),colour = population), size = 4)+
    geom_errorbar(aes(x = population, ymin = log(lwr), ymax = log(upr),colour = population),
                  alpha = 1) +
    ylab("Max. canopy height (cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) # if i log everything it's exactly the same plot as with conditional effects! 

