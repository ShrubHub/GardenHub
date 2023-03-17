# BAYESIAN growth results models -----
# Script by Erica
# Last update: 02/03/2023 by Madi 
# Code adapted from coding club tutorial by Louise Litrico:
# https://ourcodingclub.github.io/tutorials/brms/ 

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

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")
# Only using max growth variables values
max_widths_cg <- read_csv("data/common_garden_data_2022/max_widths_cg.csv")
max_heights_cg <- read_csv("data/common_garden_data_2022/max_heights_cg.csv")
max_biovol_cg <- read_csv("data/common_garden_data_2022/max_biovol_cg.csv")
max_elong_cg <- read_csv("data/common_garden_data_2022/max_elong_cg.csv")
max_diam_cg <- read_csv("data/common_garden_data_2022/max_diam_cg.csv")

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
                                          c("Northern"="Northern Garden",
                                            "Southern"="Southern Garden"))

# Separating into 3 datasets: one per spp.
max_heights_cg_rich <- max_heights_cg %>%
  filter (Species == "Salix richardsonii")

max_heights_cg_pul <- max_heights_cg %>%
  filter (Species == "Salix pulchra") 

max_heights_cg_arc <- max_heights_cg %>%
  filter (Species == "Salix arctica") 

# look at mean max heights per species 
mean(max_heights_cg_rich$max_canopy_height_cm) # 31.25362
range(max_heights_cg_rich$max_canopy_height_cm) # 0.7 - 127.0
mean(max_heights_cg_pul$max_canopy_height_cm) # 21.90695 
range(max_heights_cg_pul$max_canopy_height_cm) # 1.5 101.0
mean(max_heights_cg_arc$max_canopy_height_cm) # 4.589855
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
hist(max_diam_cg_pul$max_stem_diam, breaks = 30) #  right skew - so weird
hist(max_diam_cg_arc$max_stem_diam,  breaks = 30)#  right skew

# Filtering data for height over time model 
all_CG_growth <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))

# making a growth rate column
all_CG_height_growth_rates <- all_CG_growth %>%
  group_by(SampleID_standard) %>% 
  arrange(Sample_age) %>%
  mutate(height_growth_diff = Canopy_Height_cm-lag(Canopy_Height_cm)) %>%
  mutate(height_growth_diffpercent = (height_growth_diff/Canopy_Height_cm)*100) %>%
  mutate(biovol_growth_diff = biovolume-lag(biovolume)) %>%
  mutate(biovol_growth_diffpercent = (biovol_growth_diff/biovolume)*100)

# Species specific 
all_CG_growth_ric <- all_CG_height_growth_rates %>%
  filter(Species == "Salix richardsonii")

all_CG_growth_pul<-  all_CG_height_growth_rates %>%
  filter(Species == "Salix pulchra")

all_CG_growth_arc <-all_CG_height_growth_rates %>%
  filter(Species == "Salix arctica")

# MODELLING -------
# NB one model per species

# 1. CANOPY HEIGHT -----

# S. Richardsonii ----
# all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale <- scale(all_CG_source_growth_garden_rich_height$Canopy_Height_cm, center = T)  # scaling time

# model
garden_rich_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data = max_heights_cg_rich, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_height) # significantly higher canopy heights for southern pop.
plot(garden_rich_height) # fine
pp_check(garden_rich_height,  type = "dens_overlay", nsamples = 100)  # good

# extract output with function
rich_extract <- model_summ_growth(garden_rich_height)

# extraction for model output table
rownames(rich_extract) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_extract_df <- rich_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
       # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")
              
# S. Pulchra -----
garden_pul_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data =max_heights_cg_pul, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_height) # significantly higher canopy heights for southern pop.
plot(garden_pul_height) # fine
pp_check(garden_pul_height, type = "dens_overlay", nsamples = 100)  # good) 

# extract output with function
pul_extract <- model_summ_growth(garden_pul_height)

# extraction for model output table
rownames(pul_extract) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
pul_extract_df <- pul_extract %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
         # "Sample Size" = rep(141)) %>%
  relocate("Species", .before = "Estimate")%>% 
    relocate("nobs", .before = "effect")

# S. Arctica -----
garden_arc_height <- brms::brm(log(max_canopy_height_cm) ~ population + (1|Sample_age),
                                data = max_heights_cg_arc, family = gaussian(), chains = 3, 
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_arc_height) # NOT significant difference (again makes sense!)
plot(garden_arc_height) # fine
pp_check(garden_arc_height, type = "dens_overlay", nsamples = 100)# good

# extract output with function
arc_extract <- model_summ_growth(garden_arc_height)

# extraction for model output table
rownames(arc_extract) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
arc_extract_df <- arc_extract %>% 
  mutate(Species = rep("Salix arctica")) %>%
         #"Sample Size" = rep(69)) %>%
  relocate("Species", .before = "Estimate") %>%
    relocate("nobs", .before = "effect")

# merging all extracted outputs
garden_heights_out <- rbind(rich_extract_df, pul_extract_df, 
                            arc_extract_df) 


# back transforming from log
garden_heights_out_back <- garden_heights_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_heights_out_back, "output/garden_heights_out_back.csv")

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
      Model structure per species: (log(max_canopy_height_cm) ~ population + (1|Sample_age). 
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
column_spec(kable_heights, 2, width = NULL, bold = FALSE, italic = TRUE)

save_kable(kable_heights,file = "output/kable_heights.pdf",
 bs_theme = "simplex",
  self_contained = TRUE,
  extra_dependencies = NULL,
  latex_header_includes = NULL,
  keep_tex =TRUE,
  density = 300)

# 1.1. HEIGHT OVER TIME MODEL -------
# Salix rich ------
height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population + (1|Year),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich) # significant height growth over time
plot(height_rich)
pp_check(height_rich, type = "dens_overlay", nsamples = 100) 

# height growth rate 
hist(all_CG_growth_ric$height_growth_diff) # normal 
height_rich_time <- brms::brm(height_growth_diff ~ population + (1|SampleID_standard) + (1|Sample_age),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich_time) # faster height growth rate over time
pp_check(height_rich_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
rich_extract_time <- model_summ_time(height_rich_time)

# extraction for model output table
rownames(rich_extract_time) <- c("Intercept", "Southern Garden", "Year", "Sigma")
rich_extract_time_df <- rich_extract_time %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra -------
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+ (1|Year),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul) # significant height growth over time
plot(height_pul)
pp_check(height_pul, type = "dens_overlay", nsamples = 100) 

# height growth rate 
hist(all_CG_growth_pul$height_growth_diff) # normal 
height_pul_time <- brms::brm(height_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                              data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(height_pul_time) # no diff
pp_check(height_pul_time, type = "dens_overlay", nsamples = 100) 

height_pul_time_2 <- brms::brm(log(Canopy_Height_cm) ~ Year*population,
                             data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul_time_2) # no diff
pp_check(height_pul_time_2, type = "dens_overlay", nsamples = 100) 

# extract output with function
pul_extract_time <- model_summ_time(height_pul_time)

# extraction for model output table
rownames(pul_extract_time) <- c("Intercept", "Southern Garden", "Year", "Sigma")
pul_extract_time_df <- pul_extract_time %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix arctica -------
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+ (1|Year),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_arc) # significant growth over time
plot(height_arc)
pp_check(height_arc, type = "dens_overlay", nsamples = 100) 

# height growth rate 
hist(all_CG_growth_arc$height_growth_diff) # normal 
height_arc_time <- brms::brm(height_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                             data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(height_arc_time) # faster for southern 
pp_check(height_arc_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
arc_extract_time <- model_summ_time(height_arc_time)

# extraction for model output table
rownames(arc_extract_time) <- c("Intercept", "Southern Garden", "Year", "Sigma")
arc_extract_time_df <- arc_extract_time %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

garden_heights_time <- rbind(rich_extract_time_df,pul_extract_time_df,arc_extract_time_df)

# save df of results 
write.csv(garden_heights_time, "output/garden_heights_time.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_heights_time) <- c("Intercept", "Southern Garden", "Year", 
                                       "Sigma", " Intercept", " Southern Garden", " Year", 
                                       " Sigma", "Intercept ", "Southern Garden ", "Year ", 
                                       "Sigma ")

# making sure Rhat keeps the .00 
garden_heights_time$Rhat <- as.character(formatC(garden_heights_time$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_heights_time <- garden_heights_time %>% 
  kbl(caption="Table.xxx BRMS model outputs: growth rate (height change per year) northern vs southern shrubs in the common garden. 
      Model structure per species: (height_difference ~ population + (1|Year).", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI ",
                     "Upper 95% CI ", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_heights_time, 2, width = NULL, bold = FALSE, italic = TRUE)



# 2. STEM ELONGATION ------

# S. Richardsonii -----
# model
garden_rich_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                               data = max_elong_cg_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_elong) # southern pop significantly longer stem elong
plot(garden_rich_elong) # fine
pp_check(garden_rich_elong, type = "dens_overlay", nsamples = 100) # goood

# extract output with function
rich_extract_elong <- model_summ_growth(garden_rich_elong)

# extraction for model output table
rownames(rich_extract_elong) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_extract_elong_df <- rich_extract_elong %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
         # "Sample Size" = rep(89)) %>% 
  relocate("Species", .before = "Estimate")%>% 
    relocate("nobs", .before = "effect")

# S. Pulchra -----
garden_pul_elong <- brms::brm(log(max_stem_elong) ~ population + (1|Sample_age),
                               data = max_elong_cg_pul, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_pul_elong)# southern pop significantly longer stem elong
plot(garden_pul_elong) # fine
pp_check(garden_pul_elong, type = "dens_overlay", nsamples = 100)  # fine

# extract output with function
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

# 3. BIOVOLUME ------
# S. Richardsonii -----
# model
garden_rich_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                               data = max_biovol_cg_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol) # fine
pp_check(garden_rich_biovol,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
rich_biovol_extract <- model_summ_growth(garden_rich_biovol)

# extraction for model output table
rownames(rich_biovol_extract) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_biovol_extract_df <- rich_biovol_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>%
         # "Sample Size" = rep(81)) %>% 
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. Pulchra -----
garden_pul_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                                data = max_biovol_cg_pul, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_biovol) # significantly larger biovolume for southern shrubs in garden
plot(garden_pul_biovol) # fine
pp_check(garden_pul_biovol,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
pul_extract_biovol <- model_summ_growth(garden_pul_biovol)

# extraction for model output table
rownames(pul_extract_biovol) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
pul_extract_biovol_df <- pul_extract_biovol %>% 
  mutate(Species = rep("Salix pulchra")) %>%
        # "Sample Size" = rep(114)) %>%
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. Arctica -----
garden_arc_biovol <- brms::brm(log(max_biovol) ~ population + (1|Sample_age),
                                data = max_biovol_cg_arc, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_biovol) # NOT significant diff. 
plot(garden_arc_biovol) # fine
pp_check(garden_arc_biovol,  type = "dens_overlay", nsamples = 100) # fine

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

# 3.1 BIOVOLUME over time ------
# S. Richardsonii -----
# model
garden_rich_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (1|Year) ,
                                data = all_CG_growth_ric, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_biovol_time) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol_time) # fine
pp_check(garden_rich_biovol_time,  type = "dens_overlay", ndraws = 100) # fine

# biovol growth rate 
hist(all_CG_growth_ric$biovol_growth_diff) # normal 
biovol_rich_time <- brms::brm(biovol_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                              data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_rich_time) # no diff
pp_check(biovol_rich_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
rich_extract_timeb <- model_summ_time(biovol_rich_time)

# extraction for model output table
rownames(rich_extract_timeb) <- c("Intercept", "Southern Garden", "Year", "Sigma")
rich_extract_timeb_df <- rich_extract_timeb %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. Pulchra -----
garden_pul_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (1|Year),
                               data = all_CG_growth_pul, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_biovol_time) # significantly larger biovolume for southern shrubs in garden
plot(garden_pul_biovol_time) # fine
pp_check(garden_pul_biovol_time,  type = "dens_overlay", nsamples = 100) # fine

# biovol growth rate 
hist(all_CG_growth_pul$biovol_growth_diff) # normal 
biovol_pul_time <- brms::brm(biovol_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                              data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_pul_time) # no diff
pp_check(biovol_pul_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
pul_extract_timeb <- model_summ_time(biovol_pul_time)

# extraction for model output table
rownames(pul_extract_timeb) <- c("Intercept", "Southern Garden", "Year", "Sigma")
pul_extract_timeb_df <- pul_extract_timeb %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. Arctica -----
garden_arc_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (1|Year),
                                    data = all_CG_growth_arc, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_biovol_time) # NOT significant diff. 
plot(garden_arc_biovol_time) # fine
pp_check(garden_arc_biovol_time,  type = "dens_overlay", nsamples = 100) # fine

# biovol growth rate 
hist(all_CG_growth_arc$biovol_growth_diff) # normal 
biovol_arc_time <- brms::brm(biovol_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                             data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_arc_time) # no diff
pp_check(biovol_arc_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
arc_extract_timeb <- model_summ_time(biovol_arc_time)

# extraction for model output table
rownames(arc_extract_timeb) <- c("Intercept", "Southern Garden", "Year", "Sigma")
arc_extract_timeb_df <- arc_extract_timeb %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

garden_heights_timeb <- rbind(rich_extract_timeb_df,pul_extract_timeb_df,arc_extract_timeb_df)

# save df of results 
write.csv(garden_heights_timeb, "output/garden_heights_timeb.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_heights_timeb) <- c("Intercept", "Southern Garden", "Year", 
                                   "Sigma", " Intercept", " Southern Garden", " Year", 
                                   " Sigma", "Intercept ", "Southern Garden ", "Year ", 
                                   "Sigma ")

# making sure Rhat keeps the .00 
garden_heights_timeb$Rhat <- as.character(formatC(garden_heights_timeb$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_biovol_time <- garden_heights_timeb %>% 
  kbl(caption="Table.xxx BRMS model outputs: growth rate (biovolume change per year) northern vs southern shrubs in the common garden. 
      Model structure per species: (biovolume_difference ~ population + (1|Year).", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI ",
                     "Upper 95% CI ", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_biovol_time, 2, width = NULL, bold = FALSE, italic = TRUE)


# 4. WIDTH ------
# S. Richardsonii -----
garden_rich_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
                                data = max_widths_cg_rich, family = gaussian(), chains = 3,
                                iter = 5000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_width) # significantly larger widths for southern shrubs in garden
plot(garden_rich_width) # fine
pp_check(garden_rich_width,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
rich_extract_width <- model_summ_growth(garden_rich_width)

# extraction for model output table
rownames(rich_extract_width) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
rich_extract_width_df <- rich_extract_width %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
         # "Sample Size" = rep(81)) %>% 
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. Pulchra -----
garden_pul_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
                               data = max_widths_cg_pul, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_width) # significantly larger widths for southern shrubs in garden
plot(garden_pul_width) # fine
pp_check(garden_pul_width,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
pul_extract_width <- model_summ_growth(garden_pul_width)

# extraction for model output table
rownames(pul_extract_width) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
pul_extract_width_df <- pul_extract_width %>% 
  mutate(Species = rep("Salix pulchra")) %>%
        # "Sample Size" = rep(114)) %>%
  relocate("Species", .before = "Estimate")%>% 
  relocate("nobs", .before = "effect")

# S. Arctica -----
garden_arc_width <- brms::brm(log(max_mean_width_cm) ~ population + (1|Sample_age),
                               data = max_widths_cg_arc, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_width) # NOT significantly larger widths for southern shrubs in garden (makes sense)
plot(garden_arc_width) # fine
pp_check(garden_arc_width,  type = "dens_overlay", nsamples = 100) # fine

# extract output with function
arc_extract_width <- model_summ_growth(garden_arc_width)

# extraction for model output table
rownames(arc_extract_width) <- c("Intercept", "Southern Garden", "Sample age", "Sigma")
arc_extract_width_df <- arc_extract_width %>% 
  mutate(Species = rep("Salix arctica")) %>% 
        # "Sample Size" = rep(53)) %>%
  relocate("Species", .before = "Estimate") %>%
  relocate("nobs", .before = "effect")

# merging all extracted outputs
garden_width_out <- rbind(rich_extract_width_df, rich_extract_width_df, 
                           arc_extract_width_df) 

# back transforming from log
garden_width_out_back <- garden_width_out %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_width_out_back, "output/garden_width_out_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_width_out_back) <- c("Intercept", "Southern Garden", "Sample age", 
                                      "Sigma", " Intercept", " Southern Garden", " Sample age", 
                                      " Sigma", "Intercept ", "Southern Garden ", "Sample age ", 
                                      "Sigma ")

# making sure Rhat keeps the .00 
garden_width_out_back$Rhat <- as.character(formatC(garden_width_out_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_width <- garden_width_out_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: max. width of northern vs southern shrubs in the common garden. 
      Model structure per species: (log(max_width) ~ population + (1|Sample_age). 
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
column_spec(kable_width, 2, width = NULL, bold = FALSE, italic = TRUE)

# 4. STEM DIAMETER ------
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
theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=12),
                                 axis.text.x  = element_text(vjust=0.5, size=12, colour = "black", angle = 45), 
                                 axis.title.y = element_text(face="bold", size=12),
                                 axis.text.y  = element_text(vjust=0.5, size=12, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# plot model output

# CANOPY HEIGHT richardsonii ----
ric_heights <- (conditional_effects(garden_rich_height)) # extracting conditional effects from bayesian model
ric_height_data <- ric_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(ric_height_plot <-ggplot(ric_height_data) +
    geom_violin(data = max_heights_cg_rich, aes(x = population, y = log(max_canopy_height_cm), fill = population, colour = population),
               alpha = 0.1)+ # raw data
    geom_jitter(data = max_heights_cg_rich, aes(x = population, y = log(max_canopy_height_cm), colour = population),
                 alpha = 0.8)+
    geom_point(aes(x = effect1__, y = estimate__,colour = population), width=0.5, size = 6)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1,  width=.5) +
    ylab("Salix richardsonii max. canopy height (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# CANOPY HEIGHT pulchra ----
pul_heights <- (conditional_effects(garden_pul_height)) # extracting conditional effects from bayesian model
pul_height_data <- pul_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(pul_height_plot <-ggplot(pul_height_data) +
    geom_point(data = max_heights_cg_pul, aes(x = population, y = log(max_canopy_height_cm), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
                  alpha = 1) +
    ylab("Salix pulchra max. canopy height (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# CANOPY HEIGHT arctica ----
arc_heights <- (conditional_effects(garden_arc_height)) # extracting conditional effects from bayesian model
arc_height_data <- arc_heights[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(arc_height_plot <-ggplot(arc_height_data) +
    geom_point(data = max_heights_cg_arc, aes(x = population, y = log(max_canopy_height_cm), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__,colour = population), size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__,colour = population),
               alpha = 1) +
    ylab("Salic arctica max. canopy height (log, cm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

library(gridExtra)
panel_heights_bayes <- grid.arrange(ric_height_plot, pul_height_plot, arc_height_plot, nrow = 1)

# HEIGHT OVER TIME PLOTS-----
(rich_heights_plot_new <- all_CG_growth_ric %>%
   group_by(population) %>%
   add_predicted_draws(height_rich) %>%
   ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Richardsonii canopy height (log cm)\n") +
   xlab("\nSample age"))

# Salix pulchra ------

(pul_heights_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(height_pul) %>%
   ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_pul) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Pulchra canopy height (log cm)\n") +
   xlab("\nSample age"))


# Salix arctica------
(arc_heights_plot_new <- all_CG_growth_arc %>%
   group_by(population) %>%
   add_predicted_draws(height_arc) %>%
   ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub() +
   ylab("Arctica canopy height (log cm)\n") +
   xlab("\nSample age"))

# STEM ELONG richardsonii ----
rich_elong <- (conditional_effects(garden_rich_elong)) # extracting conditional effects from bayesian model
rich_elong_data <- rich_elong[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_height_plot <-ggplot(rich_elong_data) +
    geom_point(data = max_elong_cg_rich, aes(x = population, y = log(max_stem_elong), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__), colour = "red", size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__),
                  alpha = 1) +
    ylab("Max. mean stem elongation (log, mm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())


# stem elong -----
(model_elong <- all_CG_source_growth_garden_rich %>%
   add_predicted_draws(garden_rich_elong) %>%  # adding the posterior distribution
   ggplot(aes(x = population, y = mean_stem_elong)) +  
   stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                   alpha = 0.5, colour = "black") +
   geom_point(data = all_CG_source_growth_garden_rich, colour = "darkseagreen4", size = 3) +   # raw data
   scale_fill_brewer(palette = "Greys") +
   ylab("Mean stem elongation (mm)\n") +  # latin name for red knot
   xlab("\nPopulation") +
   theme_bw() +
   theme(legend.title = element_blank(),
         legend.position = c(0.15, 0.85)))

# Biovolume richardsonii ----
rich_biovol <- (conditional_effects(garden_rich_biovol)) # extracting conditional effects from bayesian model
rich_biovol_data <- rich_biovol[[1]] # making the extracted model outputs into a dataset (for plotting)
# [[1]] is to extract the first term in the model which in our case is population

(rich_biovol_plot <-ggplot(rich_biovol_data) +
    geom_point(data = max_biovol_cg_rich, aes(x = population, y = log(max_biovol), colour = population),
               alpha = 0.5)+ # raw data
    geom_point(aes(x = effect1__, y = estimate__), colour = "red", size = 4)+
    geom_errorbar(aes(x = effect1__, ymin = lower__, ymax = upper__),
                  alpha = 1) +
    ylab("Max. biovol (log, mm)\n") +
    xlab("\n Population" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub())

# can do for all figures....

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


# OVER TIME MODELS ----
(ric_rate_plot <-ggplot() +
   geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = height_growth_diff, colour = population),
              alpha = 0.5)+ # raw data
   geom_smooth(data = all_CG_growth_ric, aes(x = Sample_age, y = height_growth_diff, colour = population,  fill = population),
              alpha = 0.5)+ # raw data
   ylab("Salix richardsonii growth rate (cm/year) \n") +
   xlab("\n Sample age" ) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub()) 

(pul_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = height_growth_diff, colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_pul, aes(x = Sample_age, y = height_growth_diff, colour = population,  fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix pulchra growth rate (cm/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

(arc_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = height_growth_diff, colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_arc, aes(x = Sample_age, y = height_growth_diff, colour = population, fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix arctica growth rate (cm/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

grid.arrange(ric_rate_plot, pul_rate_plot, arc_rate_plot, 
             nrow=1)

(ric_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_ric, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population,  fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix richardsonii biovolume growth rate (log, cm3/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

(pul_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_pul, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population,  fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix pulchra biovolume growth rate (log, cm3/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

(arc_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_arc, aes(x = Sample_age, y =log(biovol_growth_diff), colour = population, fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix arctica biovolume growth rate (log, cm3/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

grid.arrange(ric_rate_plot, pul_rate_plot, arc_rate_plot, 
             nrow=1)
