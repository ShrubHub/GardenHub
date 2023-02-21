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


# SOURCE POP ONLY models -----
# keeping only source pops
all_phenocam_data_salix_sources <- all_phenocam_data_salix %>%
  filter(population %in% c("Northern Source", "Southern Source"))

# MAKE SPECIES SPECIFIC datasets -----

#Leaf emergence (make ONE per species)
bud_burst_mod_source <- lm(First_bud_burst_DOY ~ population*Species, data = all_phenocam_data_salix_sources)

# Leaf yellow
yellow_mod_source <- lm(First_leaf_yellow_DOY ~ population*Species, data = all_phenocam_data_salix_sources)

# CG vs SOURCES models ------
# model bud burst doy
bud_burst_mod <- lmer(First_bud_burst_DOY ~ population + (1|Species) + (1|Year), data = all_phenocam_data_salix)

# model bud burst doy
bud_burst_mod_2 <- lmer(First_bud_burst_DOY ~ population*Species + (1|Year), data = all_phenocam_data_salix)

# growing seasonn length diffferences
growing_season_mod <- lm(growing_season ~ population*Species, data = all_growing_season)

# CG ONLY MODELS ------
all_phenocam_data_cg <- all_phenocam_data_salix %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))
# Separate species -----

bud_burst_cg_mod_1 <- lmer(First_bud_burst_DOY ~ population + (1|Species), data = all_phenocam_data_cg)
yellow_cg_mod_1 <- lmer(First_leaf_yellow_DOY ~ population + (1|Species) , data = all_phenocam_data_cg)
