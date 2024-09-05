# models for Common garden manuscript 
# citation: TBD! 
# email for questions: madelaine.anderson@usherbrooke.ca

# Outline ----
# NB: within each section, one model per species is run aka three models. 
# NB: model outputs were saved after running, rerunning models may yield slightly different results (exact numbers due to random variation in iterations)

# 1. Growth models 
# 1.1 source population size: height, width, stem elongation 
# 1.2 common garden size over time: height, stem elongation
# 1.3 maximum size in common garden: width 

# 2. Phenology models
# 2.1 leaf bud burst 
# 2.2 leaf senescence onset 

# 3. Trait models 
# 3.1 specific leaf area
# 3.2 leaf dry matter content
# 3.3 leaf area
# 3.4 leaf length 

# libraries -----
library(plyr) # load before dplyr aka tidyverse 
library(tidyverse) # most data manip
library(brms) # models
library(ggeffects) # ggpredict function to see model outputs 

# Functions -------
#  scale function 
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# extract model result functions =====
# summary function for one random effect (e.g. source growth models)
model_summ_oneRE <- function(x) {
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
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "SampleYear" # call this whatever random effect is
  
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

# 1. GROWTH ----

# data ----
source_size_data <- read.csv("final_versions/data/source_size_data.csv")

source_size_data$SampleYear <- as.factor(source_size_data$SampleYear)
source_size_data$Site <- as.factor(source_size_data$Site)

# Species specific datasets:
source_size_data_rich <- source_size_data %>%
  filter(Species == "Salix richardsonii")

source_size_data_pulchra <- source_size_data %>%
  filter(Species == "Salix pulchra")

source_size_data_arctica <- source_size_data %>%
  filter(Species == "Salix arctica")

## 1.1. source population size ----
#### 1.1.1 height in source populations ----

#### S. richardsonii ----
source_rich_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                                data = source_size_data_rich, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000, 
                                control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_height) # lower heights for QHI site
plot(source_rich_height)
pp_check(source_rich_height, type = "dens_overlay", ndraws = 100)  # looks good
# saveRDS(source_rich_height, file = "final_versions/model_outputs/source_rich_height.rds")
# source_rich_height <- readRDS(file = "final_versions/model_outputs/source_rich_height.rds")

# view model estimates draws with ggpredict: 
rich_source_height.pred <- ggpredict(source_rich_height, terms = c('Site'))

# extract output with function
source_rich_height_dat <- model_summ_oneRE(source_rich_height)

#### S. pulchra ----
source_pul_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                               data = source_size_data_pulchra, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_height) # lower heights for QHI site 
plot(source_pul_height)
pp_check(source_pul_height, type = "dens_overlay", nsamples = 100)  # looks good 
# saveRDS(source_pul_height, file = "final_versions/model_outputs/source_pul_height.rds")
# source_pul_height <- readRDS(file = "final_versions/model_outputs/source_pul_height.rds")

# view model estimates draws with ggpredict: 
pul_source_height.pred <- ggpredict(source_pul_height, terms = c('Site'))

# extract output with function
source_pul_height_dat<- model_summ_oneRE(source_pul_height)

#### S. arctica ----
source_arc_height <- brms::brm(log(Canopy_Height_cm) ~ Site + (1|SampleYear),
                               data = source_size_data_arctica, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_height) # no significant difference between sites
plot(source_arc_height)
pp_check(source_arc_height, type = "dens_overlay", nsamples = 100)  # looks good 
# saveRDS(source_arc_height, file = "final_versions/model_outputs/source_arc_height.rds")
# source_arc_height <- readRDS(file = "final_versions/model_outputs/source_arc_height.rds")

# view model estimates draws with ggpredict: 
arc_source_height.pred <- ggpredict(source_arc_height, terms = c('Site'))

# extract output with function
source_arc_height_dat <- model_summ_oneRE(source_arc_height)


#### 1.1.2 width in source populations ----
#### S. richardsonii ----
source_rich_width_mod <- brms::brm((mean_width) ~ Site + (1|SampleYear),
                                  data = source_size_data_rich, family = gaussian(), chains = 3,
                                  iter = 3000, warmup = 1000, 
                                  control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_width_mod) # wider QHI shrubs but CI's overlap slightly 
plot(source_rich_width_mod) 
pp_check(source_rich_width_mod, type = "dens_overlay", nsamples = 100)  # fine
# saveRDS(source_rich_width_mod, file = "final_versions/model_outputs/source_rich_width.rds")
# source_rich_width_mod <- readRDS(file = "final_versions/model_outputs/source_rich_width.rds")
rich_source_width.pred <- ggpredict(source_rich_width_mod, terms = c('Site'))

# extract output with function
source_rich_width <- model_summ_oneRE(source_rich_width_mod)

#### S. pulchra ----
source_pul_width<- brms::brm((mean_width) ~ Site + (1|SampleYear),
                             data = source_size_data_pulchra, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_width) # no width differences between sites
pul_source_width.pred <- ggpredict(source_pul_width, terms = c('Site'))

plot(source_pul_width)
pp_check(source_pul_width, type = "dens_overlay", nsamples = 100)  # fine
# saveRDS(source_pul_width, file = "final_versions/model_outputs/source_pul_width.rds")
# source_pul_width <- readRDS(file = "final_versions/model_outputs/source_pul_width.rds")
pul_source_width.pred <- ggpredict(source_pul_width, terms = c('Site'))

# extract output with function
source_pul_width <- model_summ_oneRE(source_pul_width)

#### S. arctica ----
source_arc_width<- brms::brm((mean_width) ~ Site + (1|SampleYear),
                             data = source_size_data_arctica, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_arc_width) # no significant diff
plot(source_arc_width)
pp_check(source_arc_width, type = "dens_overlay", nsamples = 100)  # fine
# saveRDS(source_arc_width, file = "final_versions/model_outputs/source_arc_width.rds")
# source_arc_width <- readRDS(file = "final_versions/model_outputs/source_arc_width.rds")
arc_source_width.pred <- ggpredict(source_arc_width, terms = c('Site'))

# extract output with function
source_arc_width <- model_summ_oneRE(source_arc_width)

### 1.1.3 stem elongation in source populations -----
#### S. richardsonii ----
source_rich_elong <- brms::brm(log(mean_stem_elong) ~ Site,
                               data = source_size_data_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_rich_elong) # shorter stem elongation for QHI
plot(source_rich_elong)
pp_check(source_rich_elong,  type = "dens_overlay", nsamples = 100)
# saveRDS(source_rich_elong, file = "final_versions/model_outputs/source_rich_elong.rds")
# source_rich_elong <- readRDS("final_versions/model_outputs/source_rich_elong.rds")

# view model estimates 
rich_soruce_elong.pred <- ggpredict(source_rich_elong, terms = c('Site'))

# extract model output with function
source_rich_elong_dat <- model_summ_no_re(source_rich_elong)

#### S. pulchra ----
source_pul_elong <- brms::brm(log(mean_stem_elong) ~ Site,
                              data = source_size_data_pulchra, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(source_pul_elong) # shorter elongations for QHI 
plot(source_pul_elong)
pp_check(source_pul_elong,  type = "dens_overlay", nsamples = 100)
# saveRDS(source_pul_elong, file = "final_versions/model_outputs/source_pul_elong.rds")
# source_pul_elong <- readRDS("final_versions/model_outputs/source_pul_elong.rds")

source_pul_elong.pred <- ggpredict(source_pul_elong, terms = c('Site'))
# extract output with function
source_pul_elong_dat <- model_summ_no_re(source_pul_elong)

#### S. arctica source stem elongation not available for both populations 

## 1.2 common garden size over time ----
### 1.2.1 height over time ----
# data: 
all_CG_source_growth <- read.csv("final_versions/data/all_cg_source_data_2023.csv") # 2023 data
# model summary function: 
model_summ_growth <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Sample_age # change name of random effect here
  random = sum$random$SampleID_standard # change name of random effect here
  obs = sum$nobs
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  # fixed$effect <- "population"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  # random$effect <- "SampleID_standard"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "random" # could change rowname here of random effect if you'd like
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

# make common garden only and species specific datasets
all_CG_growth <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))

all_CG_growth_ric <- all_CG_growth %>%
  filter(Species == "Salix richardsonii")

all_CG_growth_pul<-  all_CG_growth %>%
  filter(Species == "Salix pulchra")

all_CG_growth_arc <-all_CG_growth %>%
  filter(Species == "Salix arctica")

### S. richardsonii ----
height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

# saveRDS(height_rich, file = "final_versions/model_outputs/height_rich_2023.rds")
# height_rich <- readRDS("final_versions/model_outputs/height_rich_2023.rds")
summary(height_rich)

ggpred_height_ric <- ggpredict(height_rich, terms = c("Sample_age", "population"))

### S. pulchra ----
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

# saveRDS(height_pul, file = "final_versions/model_outputs/height_pul_2023.rds")
# height_pul <- readRDS("final_versions/model_outputs/height_pul_2023.rds")
summary(height_pul)
ggpred_height_pul <- ggpredict(height_pul, terms = c("Sample_age", "population"))

### S. arctica ----
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# saveRDS(height_arc, file = "final_versions/model_outputs/height_arc_2023.rds")
# height_arc <- readRDS("final_versions/model_outputs/height_arc_2023.rds")
summary(height_arc) # growth over time
ggpred_height_arc <- ggpredict(height_arc, terms = c("Sample_age", "population"))


### 1.2.2 stem elongation over time ----
### S. richardsonii ----
stem_ric <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(Sample_age|SampleID_standard),
                      data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(stem_ric)
# saveRDS(stem_ric, file = "final_versions/model_outputs/stem_elong_ric_2023.rds")
# stem_ric <- readRDS("final_versions/model_outputs/stem_elong_ric_2023.rds")
ggpred_stem_ric <- ggpredict(stem_ric, terms = c("Sample_age", "population"))

### S. pulchra ----
all_CG_growth_pul_elong <- all_CG_growth_pul %>% 
  filter(mean_stem_elong < 550) # filter some extreme values that don't make biological sense 

stem_pul <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(Sample_age|SampleID_standard),
                      data = all_CG_growth_pul_elong,  family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(stem_pul)
# saveRDS(stem_pul, file = "final_versions/model_outputs/stem_elong_pul_2023.rds")
# stem_pul <- readRDS("final_versions/model_outputs/stem_elong_pul_2023.rds")

ggpred_stem_pul <- ggpredict(stem_pul, terms = c("Sample_age", "population"))

### S. arctica ----
stem_arc <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(Sample_age|SampleID_standard),
                      data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(stem_arc)
saveRDS(stem_arc, file = "final_versions/model_outputs/stem_elong_arc_2023.rds")
stem_arc <- readRDS("final_versions/model_outputs/stem_elong_arc_2023.rds")

ggpred_stem_arc <- ggpredict(stem_arc, terms = c("Sample_age", "population"))

##1.3 common garden maximum width ----
# data: 
max_widths_cg <- read.csv("final_versions/data/max_widths_cg.csv")

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

### S. richardsonii ----
garden_rich_width <- brms::brm(log(max_mean_width_cm) ~ population,
                               data = max_widths_cg_rich, family = gaussian(), chains = 3,
                               iter = 5000, warmup = 1000, 
                               control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(garden_rich_width) # larger widths for southern shrubs in garden
plot(garden_rich_width) # fine
pp_check(garden_rich_width,  type = "dens_overlay", ndraws = 100) # good
# saveRDS(garden_rich_width, file = "final_versions/model_outputs/garden_rich_width.rds")
# garden_rich_width <- readRDS("final_versions/model_outputs/garden_rich_width.rds")
rich_width.pred <- ggpredict(garden_rich_width, terms = c('population'))

### S. pulchra ----
garden_pul_width <- brms::brm(log(max_mean_width_cm) ~ population,
                              data = max_widths_cg_pul, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_pul_width) # significantly larger widths for southern shrubs in garden
plot(garden_pul_width) # fine
pp_check(garden_pul_width,  type = "dens_overlay", nsamples = 100) # fine
# saveRDS(garden_pul_width, file = "final_versions/model_outputs/garden_pul_width.rds")
# garden_pul_width <-  readRDS("final_versions/model_outputs/garden_pul_width.rds")
pul_width.pred <- ggpredict(garden_pul_width, terms = c('population'))

### S. arctica ----
garden_arc_width <- brms::brm(log(max_mean_width_cm) ~ population,
                              data = max_widths_cg_arc, family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(garden_arc_width) # widths are same for southern and northern shrubs in garden 
plot(garden_arc_width) # fine
pp_check(garden_arc_width,  type = "dens_overlay", ndraws = 100) # fine
# saveRDS(garden_arc_width, file = "final_versions/model_outputs/garden_arc_width.rds")
# garden_arc_width <- readRDS("final_versions/model_outputs/garden_arc_width.rds")
arc_width.pred <- ggpredict(garden_arc_width, terms = c('population'))

# 2. PHENOLOGY ----
# data: 
all_pheno_2023 <- read.csv("final_versions/data/all_phenology_2023.csv")

# rename levels so shorter:
all_pheno_2023$population <- plyr::revalue(all_pheno_2023$population, 
                                           c("Northern Garden"="N. Garden",
                                             "Southern Garden"="S. Garden",
                                             "Southern Source"="S. Source",
                                             "Northern Source"="N. Source"))
all_pheno_2023$Year <- as.factor(all_pheno_2023$Year)

# species specific datasets: 
all_pheno_rich <- all_pheno_2023 %>%
  filter(Species == "Salix richardsonii")

all_pheno_pulchra <- all_pheno_2023 %>%
  filter(Species == "Salix pulchra")

all_pheno_arctica <- all_pheno_2023 %>%
  filter(Species == "Salix arctica")


## 2.1 leaf bud burst ----
### S. richardsonii ----
all_phenocam_rich$First_bud_burst_DOY_center <- center_scale(all_phenocam_rich$First_bud_burst_DOY) 

rich_budburst <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                       data = all_pheno_rich, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000, 
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(rich_budburst)
plot(rich_budburst)
pp_check(rich_budburst, type = "dens_overlay", nsamples = 100) # looks good
# saveRDS(rich_budburst, file = "final_versions/model_outputs/rich_budburst.rds")
# rich_budburst <- readRDS(file = "final_versions/model_outputs/rich_budburst.rds")

### S. pulchra ----
all_phenocam_pulchra$First_bud_burst_DOY_center <- center_scale(all_phenocam_pulchra$First_bud_burst_DOY) 

pulchra_budburst <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                      data = all_pheno_pulchra, family = gaussian(), chains = 3,
                                      iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_budburst)
plot(pulchra_budburst)
pp_check(pulchra_budburst, type = "dens_overlay", nsamples = 100) # looks good
# saveRDS(pulchra_budburst, file = "final_versions/model_outputs/pulchra_budburst.rds")
# pulchra_budburst <- readRDS(file = "final_versions/model_outputs/pulchra_budburst.rds")

### S. arctica ----
all_phenocam_arctica$First_bud_burst_DOY_center <- center_scale(all_phenocam_arctica$First_bud_burst_DOY) 

arc_budburst <- brms::brm(First_bud_burst_DOY_center ~ population + (1|Year),
                                      data = all_pheno_arctica, family = gaussian(), chains = 3,
                                      iter = 3000, warmup = 1000, 
                                      control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(arc_budburst)
plot(arc_budburst)
pp_check(arc_budburst, type = "dens_overlay", nsamples = 100) # looks good
# saveRDS(arc_budburst, file = "final_versions/model_outputs/arctica_budburst.rds")
# arc_budburst <- readRDS(file = "final_versions/model_outputs/arctica_budburst.rds")

## 2.2 leaf senescence ----
# same data frames as budburst 

### S. richardsonii ----
all_phenocam_rich$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_rich$First_leaf_yellow_DOY) 

rich_yellowing <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                        data = all_pheno_rich, family = gaussian(), chains = 3,
                                        iter = 3000, warmup = 1000,
                                        control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_yellowing)
plot(rich_yellowing)
pp_check(rich_yellowing, type = "dens_overlay", ndraws = 100) # looks good
# saveRDS(rich_yellowing, file = "final_versions/model_outputs/rich_yellowing.rds")
# rich_yellowing <- readRDS(file = "final_versions/model_outputs/rich_yellowing.rds")

### S. pulchra ----
all_phenocam_pulchra$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_pulchra$First_leaf_yellow_DOY) 

pulchra_yellowing <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                       data = all_pheno_pulchra, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_yellowing)
plot(pulchra_yellowing)
pp_check(pulchra_yellowing, type = "dens_overlay", ndraws = 100) # looks good
# saveRDS(pulchra_yellowing, file = "final_versions/model_outputs/pulchra_yellowing.rds")
# pulchra_yellowing <- readRDS(file = "final_versions/model_outputs/pulchra_yellowing.rds")

### S. arctica ----
all_phenocam_arctica$First_leaf_yellow_DOY_center <- center_scale(all_phenocam_arctica$First_leaf_yellow_DOY) 

arctica_yellowing <- brms::brm(First_leaf_yellow_DOY_center ~ population + (1|Year),
                                       data = all_pheno_arctica, family = gaussian(), chains = 3,
                                       iter = 3000, warmup = 1000,
                                       control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(arctica_yellowing)
plot(arctica_yellowing)
pp_check(arctica_yellowing, type = "dens_overlay", ndraws = 100) # looks good
# saveRDS(arctica_yellowing, file = "final_versions/model_outputs/arctica_yellowing.rds")
# arctica_yellowing <- readRDS(file = "final_versions/model_outputs/arctica_yellowing.rds")

# 3. TRAITS -----
# data: 
all_CG_source_traits <- read.csv("final_versions/data/all_CG_source_traits_2023.csv") # most traits

all_CG_source_growth <- read.csv("data/common_garden_data_2023/all_data_2023.csv") # leaf length data

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

# species specific trait datasets: 
arctica_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix arctica") 
pulchra_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix pulchra") 
richardsonii_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix richardsonii") 

# species specific datasets for leaf length 
# leaf length data were collected with other growth measurements: 
arctica_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix arctica")
pulchra_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix pulchra")
richardsonii_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix richardsonii")

## 3.1 specific leaf area (SLA) ----
### S. richardsonii ----
rich_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_SLA) 
plot(rich_SLA)
pp_check(rich_SLA, type = "dens_overlay", ndraws = 100) # good 
# saveRDS(rich_SLA, file = "final_versions/model_outputs/sla_richardsonii.rds")
# rich_SLA <- readRDS("final_versions/model_outputs/sla_richardsonii.rds")
rich_SLA.pred <- ggpredict(rich_SLA, terms = c('population'))

### S. pulchra ----
pulchra_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(pulchra_SLA) 
plot(pulchra_SLA)
pp_check(pulchra_SLA, type = "dens_overlay", ndraws = 100) # good 
# saveRDS(pulchra_SLA, file = "final_versions/model_outputs/sla_pulchra.rds")
# pulchra_SLA <- readRDS("final_versions/model_outputs/sla_pulchra.rds")
pul_SLA.pred <- ggpredict(pulchra_SLA, terms = c('population'))

### S. arctica ----
arctica_SLA <- brms::brm(log(SLA) ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_SLA) 
plot(arctica_SLA)
pp_check(arctica_SLA, type = "dens_overlay", ndraws = 100) # pretty good 
# saveRDS(arctica_SLA, file = "final_versions/model_outputs/sla_arctica.rds")
# arctica_SLA <- readRDS("final_versions/model_outputs/sla_arctica.rds")
arc_SLA.pred <- ggpredict(arctica_SLA, terms = c('population'))

## 3.2 leaf dry matter content (LMDC) ----
### S. richardsonii ----
rich_LDMC_check <- brms::brm((LDMC_percent) ~ population + (1|year), data = richardsonii_all_traits, family = gaussian(), chains = 3,
                             iter = 3000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(rich_LDMC)
plot(rich_LDMC)
pp_check(rich_LDMC, type = "dens_overlay", ndraws = 100) 
# saveRDS(rich_LDMC, file = "final_versions/model_outputs/ldmc_richardsonii.rds")
# rich_LDMC <- readRDS("final_versions/model_outputs/ldmc_richardsonii.rds")
rich_LDMC.pred <- ggpredict(rich_LDMC, terms = c('population'))

### S. pulchra ----
pulchra_LDMC <- brms::brm((LDMC_percent) ~ population + (1|year), data = pulchra_all_traits, family = gaussian(), chains = 3,
                          iter = 3000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(pulchra_LDMC) 
plot(pulchra_LDMC)
pp_check(pulchra_LDMC, type = "dens_overlay", ndraws = 100) 
# saveRDS(pulchra_LDMC, file = "final_versions/model_outputs/ldmc_pulchra.rds")
# pulchra_LDMC <- readRDS("final_versions/model_outputs/ldmc_pulchra.rds")
pul_LDMC.pred <- ggpredict(pulchra_LDMC, terms = c('population'))

### S. arctica ----
arctica_LDMC <- brms::brm(LDMC_percent ~ population + (1|year), data = arctica_all_traits, family = gaussian(), chains = 3,
                          iter = 5000, warmup = 1000, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(arctica_LDMC)
plot(arctica_LDMC)
pp_check(arctica_LDMC, type = "dens_overlay", ndraws = 100) 
# saveRDS(arctica_LDMC, file = "final_versions/model_outputs/ldmc_arctica.rds")
# arctica_LDMC <- readRDS("final_versions/model_outputs/ldmc_arctica.rds")
arc_LDMC.pred <- ggpredict(arctica_LDMC, terms = c('population'))

## 3.3 leaf area ----
### S. richardsonii ----
rich_LA <- brms::brm((LA_cm2) ~ population + (1|year), data = richardsonii_all_traits, 
                     family = gaussian(), chains = 3,
                         iter = 3000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LA)
plot(rich_LA)
pp_check(rich_LA, type = "dens_overlay", ndraws = 100)
# saveRDS(rich_LA, file = "final_versions/model_outputs/la_richardsonii.rds")
# rich_LA <- readRDS("final_versions/model_outputs/la_richardsonii.rds")
rich_LA.pred <- ggpredict(rich_LA, terms = c('population'))

### S. pulchra ----
pulchra_LA <- brms::brm((LA_cm2) ~ population + (1|year), data = pulchra_all_traits, 
                            family = gaussian(), chains = 3,
                            iter = 3000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(pulchra_LA)
plot(pulchra_LA)
pp_check(pulchra_LA, type = "dens_overlay", ndraws = 100) # okay
# saveRDS(pulchra_LA, file = "final_versions/model_outputs/la_pulchra.rds")
# pulchra_LA <- readRDS(final_versions/model_outputs/la_pulchra.rds")
pulchra_LA.pred <- ggpredict(pulchra_LA, terms = c('population'))

### S. arctica ----
arctica_LA <- brms::brm((LA_cm2)  ~ population, data = arctica_all_traits, 
                        family = gaussian(), chains = 3,
                            iter = 3000, warmup = 1000, 
                            control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LA)
plot(arctica_LA)
pp_check(arctica_LA, type = "dens_overlay", ndraws = 100) 
# saveRDS(arctica_LA, file = "final_versions/model_outputs/la_arctica.rds")
# arctica_LA <- readRDS("final_versions/model_outputs/la_arctica.rds")
arctica_LA.pred <- ggpredict(arctica_LA, terms = c('population'))

## 3.4 leaf length ----
arctica_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix arctica")
pulchra_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix pulchra")
richardsonii_all_growth <- all_CG_source_growth %>% 
  filter(Species == "Salix richardsonii")

### S. richardsonii ----
rich_LL <- brms::brm(mean_leaf_length ~ population + (1|year), 
                     data = richardsonii_all_growth, family = gaussian(), chains = 3,
                     iter = 3000, warmup = 1000, 
                     control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(rich_LL)
plot(rich_LL)
pp_check(rich_LL, type = "dens_overlay", ndraws = 100) 
# saveRDS(rich_LL, file = "final_versions/model_outputs/ll_richardsonii.rds")
# rich_LL <- readRDS("final_versions/model_outputs/ll_richardsonii.rds")
rich_LL.pred <- ggpredict(rich_LL, terms = c('population'))

### S. pulchra ----
pulchra_LL <- brms::brm(mean_leaf_length ~ population + (1|year), data = pulchra_all_growth, family = gaussian(), chains = 3,
                        iter = 3000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99)) 
summary(pulchra_LL)
plot(pulchra_LL)
pp_check(pulchra_LL, type = "dens_overlay", ndraws = 100) 
# saveRDS(pulchra_LL, file = "final_versions/model_outputs/ll_pulchra.rds")
# pulchra_LL <- readRDS("final_versions/model_outputs/ll_pulchra.rds")
pul_LL.pred <- ggpredict(pulchra_LL, terms = c('population'))

### S. arctica ----
arctica_LL_CG <- brms::brm((mean_leaf_length) ~ population + (1|year) + (1|SampleID_standard), data = arctica_all_growth, family = gaussian(), chains = 3,
                           iter = 3000, warmup = 1000, 
                           control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(arctica_LL_CG)
plot(arctica_LL_CG)
pp_check(arctica_LL_CG, type = "dens_overlay", ndraws = 100)
# saveRDS(arctica_LL_CG, file = "final_versions/model_outputs/ll_arctica.rds")
# arctica_LL_CG <- readRDS("final_versions/model_outputs/ll_arctica.rds")
arc_LL.pred <- ggpredict(arctica_LL_CG, terms = c('population'))



