# BAYESIAN growth results models -----
# Script by Erica
# Last update: 21/02/2023
# Code adapted from coding club tutorial by Louise Litrico:
# https://ourcodingclub.github.io/tutorials/brms/ 

# Loading libraries ----
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# reclassing variables
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

# filter dataset to retain only population "northern" and "southern"
all_CG_source_growth_garden_only <- all_CG_source_growth %>%
  filter(Site == "Common_garden") %>%
  filter(population %in% c("Northern", "Southern"))

all_CG_source_growth_garden_only$population <- as.character(all_CG_source_growth_garden_only$population)
str(all_CG_source_growth_garden_only$population)
unique(all_CG_source_growth_garden_only$population)
all_CG_source_growth_garden_only$population <- as.factor(all_CG_source_growth_garden_only$population)


# ordering levels so source and garden populations side by side
all_CG_source_growth_garden_only$population <- plyr::revalue(all_CG_source_growth_garden_only$population , 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden"))

# MODELLING -------
# one model per species

# 1. Canopy height-----

#Richardsonii -----
all_CG_source_growth_garden_rich_height <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix richardsonii") %>%
  dplyr::select(Canopy_Height_cm, Year, population, Sample_age)%>%
  na.omit()

unique(all_CG_source_growth_garden_rich_height$Year)
range(all_CG_source_growth_garden_rich_height$Canopy_Height_cm)

hist(all_CG_source_growth_garden_rich_height$Canopy_Height_cm) # not gaussian, not poisson?
# scaling 
all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale <- scale(all_CG_source_growth_garden_rich_height$Canopy_Height_cm, center = T)  # scaling time
hist(all_CG_source_growth_garden_rich_height$Canopy_Height_cm_scale ) # still not gaussian?

# model
garden_rich_height <- brms::brm(Canopy_Height_cm_scale ~ population + (1|Sample_age) + (1|Year),
                                data = all_CG_source_growth_garden_rich_height, family = gaussian(), chains = 3,
                                iter = 3000, warmup = 1000)

summary(garden_rich_height)
plot(garden_rich_height)
pp_check(garden_rich_height) # i think maybe the family () is wrong 

# 2. Stem elongation ------
# Richardsonii
all_CG_source_growth_garden_rich <- all_CG_source_growth_garden_only %>%
  filter (Species == "Salix richardsonii") %>%
  dplyr::select(mean_stem_elong, Year, population, Sample_age)%>%
  na.omit()

unique(all_CG_source_growth_garden_rich$population)
range(all_CG_source_growth_garden_rich$mean_stem_elong)

hist(all_CG_source_growth_garden_rich$mean_stem_elong) # not gaussian, not poisson?
# scaling 
all_CG_source_growth_garden_rich$mean_stem_elong_scale <- scale(all_CG_source_growth_garden_rich$mean_stem_elong, center = T)  # scaling time
hist(all_CG_source_growth_garden_rich$mean_stem_elong_scale) # still not gaussian?
all_CG_source_growth_garden_rich$population<-as.factor(all_CG_source_growth_garden_rich$population)

# model
garden_rich_elong <- brms::brm(mean_stem_elong ~ population + (1|Sample_age) + (1|Year),
                               data = all_CG_source_growth_garden_rich, family = gaussian(), chains = 3,
                               iter = 3000, warmup = 1000)


summary(garden_rich_elong)
plot(garden_rich_elong)
pp_check(garden_rich_elong) # i think maybe the family () is wrong 

# PLOTTING -----

# plot model output
# plotted as "regression" / slope lines but prob not what we want

(model_height <- all_CG_source_growth_garden_rich_height %>%
   add_predicted_draws(garden_rich_height) %>%  # adding the posterior distribution
   ggplot(aes(x = population, y = Canopy_Height_cm_scale)) +  
   stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                   alpha = 0.5, colour = "black") +
   geom_point(data = all_CG_source_growth_garden_rich_height, colour = "darkseagreen4", size = 3) +   # raw data
   scale_fill_brewer(palette = "Greys") +
   ylab("Canopy height (cm)\n") +  # latin name for red knot
   xlab("\nPopulation") +
   theme_bw() +
   theme(legend.title = element_blank(),
         legend.position = c(0.15, 0.85)))

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



