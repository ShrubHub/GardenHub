# BAYESIAN phenology script ----
# BY Erica 
# Last update:

# Libraries----
library(brms)

# Load data ----
all_phenocam_data_salix <- read_csv("data/phenology/phenocam_pics/all_phenocam_data_salix.csv")
all_growing_season <- read_csv("data/phenology/phenocam_pics/all_growing_season.csv")

# Wrangle data ------
# ordering levels so source and garden populations side by side
all_phenocam_data_salix$population <- plyr::revalue(all_phenocam_data_salix$population, 
                                                    c("QHI"="Northern Garden",
                                                      "Kluane"="Southern Garden",
                                                      "Southern_source"="Southern Source",
                                                      "Northern_source"="Northern Source"))

all_phenocam_data_salix$population <- ordered(all_phenocam_data_salix$population, 
                                              levels = c("Northern Source", 
                                                         "Northern Garden", 
                                                         "Southern Source",
                                                         "Southern Garden"))

all_phenocam_data_salix$Year <- as.factor(all_phenocam_data_salix$Year)

all_growing_season$population <- plyr::revalue(all_growing_season$population, 
                                               c("Northern"="Northern Garden",
                                                 "Southern"="Southern Garden",
                                                 "KP"="Southern Source",
                                                 "QHI"="Northern Source"))

all_growing_season$population <- ordered(all_growing_season$population, 
                                         levels = c("Northern Source", 
                                                    "Northern Garden", 
                                                    "Southern Source",
                                                    "Southern Garden"))
# SPECIES SPECIFIC datasets -----
all_phenocam_rich <- all_phenocam_data %>%
  filter(Species == "Salix richardsonii")

all_phenocam_pulchra <- all_phenocam_data %>%
  filter(Species == "Salix pulchra")

all_phenocam_arctica <- all_phenocam_data %>%
  filter(Species == "Salix arctica")

# SOURCE POP ONLY models -----
# keeping only source pops

# Salix richardsonii -----
all_phenocam_rich_source <- all_phenocam_rich %>%
  filter(population %in% c("Northern_source", "Southern_source"))

hist(all_phenocam_rich_source$First_bud_burst_DOY, breaks=10) # defo not normal

# model leaf emergence
garden_rich_emerg <- brms::brm(First_bud_burst_DOY ~ population + (1|Year),
                               data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)


summary(garden_rich_emerg)
plot(garden_rich_emerg)
pp_check(garden_rich_emerg) # not too happy with that....

# model leaf yellowing
garden_rich_yellow <- brms::brm(First_leaf_yellow_DOY ~ population + (1|Year),
                               data = all_phenocam_rich_source, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)


summary(garden_rich_emerg)
plot(garden_rich_emerg)
pp_check(garden_rich_emerg) # not too happy with that....

# CG vs SOURCES models ------
# model bud burst doy

# growing seasonn length diffferences
growing_season_mod <- lm(growing_season ~ population*Species, data = all_growing_season)

# CG ONLY MODELS ------
all_phenocam_data_cg <- all_phenocam_data_salix %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))

bud_burst_cg_mod_1 <- lmer(First_bud_burst_DOY ~ population + (1|Species), data = all_phenocam_data_cg)
yellow_cg_mod_1 <- lmer(First_leaf_yellow_DOY ~ population + (1|Species) , data = all_phenocam_data_cg)
