# Results models for trait differences comparing northern and southern willows
# by Madi 13/12/2022
# Last updated: 08/02/2022

# TRAITS: SLA, LDMC, leaf area (LA), leaf length, leaf mass per area (LMA)

# model, where population is CG northern, CG southern, source northern (QHI), source southern (Kluane alpine)
# lmer(TRAIT ~ population + (1|sample_year/species/sample_ID))
# need to make sample_year column, although not critical for leaf traits? 
# separate nested random effects if it doesn't converge 

# LIBRARIES ---- 
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(sjPlot)
library(ggpubr)
library(GGally)

# DATA ----
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") # most traits
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv") # leaf length

all_source_pop_2022 <- read.csv("data/all_source_pop_2022.csv")

str(all_CG_source_traits)
str(all_CG_source_growth)

# reclass variables 
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)

all_CG_source_traits$Species <- as.factor(all_CG_source_traits$Species)
all_CG_source_traits$plant_tag_id <- as.factor(all_CG_source_traits$plant_tag_id)
all_CG_source_traits$population <- as.factor(all_CG_source_traits$population)
all_CG_source_traits$date_sampled <- as.POSIXct(all_CG_source_traits$date_sampled, format = '%Y-%m-%d')
all_CG_source_traits$year <- as.factor(all_CG_source_traits$year)
# currently doesn't exist! all_CG_source_traits$Sample_age <- as.factor(all_CG_source_traits$Sample_age)

# filter out two extreme LDMC values from 2014 
# filter out one extreme LMA value from 2021 
all_CG_source_traits <- all_CG_source_traits %>% 
  filter(LDMC_g_g < 1 | is.na(LDMC_g_g)) %>% 
  filter(leaf_mass_per_area_g_m2 < 130 | is.na(leaf_mass_per_area_g_m2)) %>% 
  filter(SLA > 2 | is.na(SLA))

# to run separate models per species filter out species: 
arctica_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix arctica")

pulchra_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix pulchra")

richardsonii_all_traits <- all_CG_source_traits %>% 
  filter(Species == "Salix richardsonii")


# SLA ----
#SLA_mod_1 <- lmer(SLA ~ population + (1|year/Species/plant_tag_id), 
                 data = all_CG_source_traits)
#summary(SLA_mod_1)
#tab_model(SLA_mod_1) 
# dropping plant_tag_id because shrubs weren't repeatedly measured either in source pop or garden
# in 2021 and 2022, same shrubs were sampled in garden, but also diff than in 2017 
SLA_mod_2 <- lmer(SLA ~ population + (1|year/Species), 
                  data = all_CG_source_traits)
# fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients
summary(SLA_mod_2)
tab_model(SLA_mod_2)

# species as interaction term 
SLA_mod_spp <- lmer(SLA ~ population*Species + (1|year), 
                  data = all_CG_source_traits)
summary(SLA_mod_spp)
tab_model(SLA_mod_spp)
# overwhelming with 4 populations and 3 species 

# Species specific: 
SLA_mod_arctica <- lmer(SLA ~ population + (1|year), 
                    data = arctica_all_traits)
summary(SLA_mod_arctica)
tab_model(SLA_mod_arctica)

SLA_mod_pulchra <- lmer(SLA ~ population + (1|year), 
                        data = pulchra_all_traits)
summary(SLA_mod_pulchra)
tab_model(SLA_mod_pulchra)

SLA_mod_richardsonii <- lmer(SLA ~ population + (1|year), 
                        data = richardsonii_all_traits)
summary(SLA_mod_richardsonii)
tab_model(SLA_mod_richardsonii)

# LDMC ----

# same as SLA so not including plant_tag_id because shrubs weren't repeatedly measured either in source pop or garden
# in 2021 and 2022, same shrubs were sampled in garden, but also diff than in 2017 
LDMC_mod_1 <- lmer(LDMC_g_g ~ population + (1|year/Species), 
                  data = all_CG_source_traits)
summary(LDMC_mod_1)
tab_model(LDMC_mod_1)

# Species specific: 
LDMC_mod_arctica <- lmer(LDMC_g_g ~ population + (1|year), 
                        data = arctica_all_traits)
summary(LDMC_mod_arctica)
tab_model(LDMC_mod_arctica)

LDMC_mod_pulchra <- lmer(LDMC_g_g ~ population + (1|year), 
                        data = pulchra_all_traits)
summary(LDMC_mod_pulchra)
tab_model(LDMC_mod_pulchra)

LDMC_mod_richardsonii <- lmer(LDMC_g_g ~ population + (1|year), 
                             data = richardsonii_all_traits)
summary(LDMC_mod_richardsonii)
tab_model(LDMC_mod_richardsonii)

# LEAF LENGTH ---- 

# somethig weird is happening with leaf length where some CG values are increased by a factor of 10, omit for now 
# filter only last two years to run models on 
CG_source_growth_ll <- all_CG_source_growth %>% 
  filter(Year %in% c(2021, 2022))

# to run separate models per species filter out species: 
arctica_ll_growth <- CG_source_growth_ll %>% 
  filter(Species == "Salix arctica")

pulchra_ll_growth <- CG_source_growth_ll %>% 
  filter(Species == "Salix pulchra")

richardsonii_ll_growth <- CG_source_growth_ll %>% 
  filter(Species == "Salix richardsonii")

# not including sample age as fixed effect (or random) for leaf traits 
# unsure about including year because only one year for source populations 
ll_mod_1 <- lmer(mean_leaf_length ~ population + (1|Species), 
               data = CG_source_growth_ll)
summary(ll_mod_1)
tab_model(ll_mod_1)

# rerunning model with all years data now that dataset is fixed 

ll_mod_2 <- lmer(mean_leaf_length ~ population + (1|Species) + (1|Year), 
                 data = all_CG_source_growth)
summary(ll_mod_2)
tab_model(ll_mod_2)

# species specific 
# don't currently have ll for kluane source 
ll_mod_arctica <- lmer(mean_leaf_length ~ population  + (1|Year), 
                 data = arctica_ll_traits)
summary(ll_mod_2)
tab_model(ll_mod_2)

ll_mod_pulchra <- lmer(mean_leaf_length ~ population  + (1|Year), 
                       data = pulchra_ll_growth)
summary(ll_mod_pulchra)
tab_model(ll_mod_pulchra)

ll_mod_richardsonii <- lmer(mean_leaf_length ~ population  + (1|Year), 
                       data = richardsonii_ll_growth)
summary(ll_mod_richardsonii)
tab_model(ll_mod_richardsonii)

# LEAF AREA ----
# data is a mess because of inconsistent unit reporting, 
# I'm going to try to sort it out but here are just data I collected in 2021 and 2022 
# in the common garden and source populations 
all_CG_source_traits_2022 <- all_CG_source_traits %>% 
  dplyr::filter(year %in% c("2021", "2022")) 

# to run separate models per species filter out species: 
arctica_2022_traits <- all_CG_source_traits_2022 %>% 
  filter(Species == "Salix arctica")

pulchra_2022_traits <- all_CG_source_traits_2022 %>% 
  filter(Species == "Salix pulchra")

richardsonii_2022_traits <- all_CG_source_traits_2022 %>% 
  filter(Species == "Salix richardsonii")

# same model srtucture as above. No sample age or ID 
# removing year as random effect because only two years worth of data 
LA_mod_1 <- lmer(LA ~ population + (1|Species), 
                   data = all_CG_source_traits_2022)
# small sample size 
summary(LA_mod_1)
tab_model(LA_mod_1)

# LMA ---- 
# same model as SLA, LDMC, leaf length  
LMA_mod_1 <- lmer(leaf_mass_per_area_g_m2 ~ population + (1|year/Species), 
                 data = all_CG_source_traits)
# boundary (singular) fit: see help('isSingular')

# removing year as random effect because only 2 years worth of data:
LMA_mod_2 <- lmer(leaf_mass_per_area_g_m2 ~ population + (1|Species), 
                  data = all_CG_source_traits)
summary(LMA_mod_2)
tab_model(LMA_mod_2)

# species specific 
# not including year bc only two years worth of data 
LMA_mod_arctica <- lm(leaf_mass_per_area_g_m2 ~ population, 
                       data = arctica_2022_traits)
summary(LMA_mod_arctica)
tab_model(LMA_mod_arctica)

LMA_mod_pulchra <- lm(leaf_mass_per_area_g_m2 ~ population, 
                       data = pulchra_2022_traits)
summary(LMA_mod_pulchra)
tab_model(LMA_mod_pulchra)

LMA_mod_richardsonii <- lm(leaf_mass_per_area_g_m2 ~ population, 
                            data = richardsonii_2022_traits)
summary(LMA_mod_richardsonii)
tab_model(LMA_mod_richardsonii)


# correlation matrix ----
# filter traits in the CG
traits_variables_CG <- all_CG_source_traits_2022 %>%
  filter(population %in% c("Northern Garden", "Southern Garden")) %>%
  dplyr::select(SLA, LDMC_g_g, LA, leaf_mass_per_area_g_m2)%>%
  na.omit()

# visualise correlation matrix
ggcorr(traits_variables_CG, method = c("everything", "pearson")) 

# FIGURES ----
# rename levels in garden for figures 
all_CG_source_traits$population <- plyr::revalue(all_CG_source_traits$population, 
                                                 c("Northern"="Northern Garden",
                                             "Southern"="Southern Garden",
                                             "source_south"="Southern Source",
                                             "source_north"="Northern Source"))
all_CG_source_traits$population <- ordered(all_CG_source_traits$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden", 
                                                      "Southern Source",
                                                      "Southern Garden"))

all_CG_source_traits_2022$population <- plyr::revalue(all_CG_source_traits_2022$population, 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden",
                                                   "source_south"="Southern Source",
                                                   "source_north"="Northern Source"))
all_CG_source_traits_2022$population <- ordered(all_CG_source_traits_2022$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden",
                                                      "Southern Source", 
                                                      "Southern Garden"))

CG_source_growth_ll$population <- plyr::revalue(CG_source_growth_ll$population, 
                                                      c("Northern"="Northern Garden",
                                                        "Southern"="Southern Garden",
                                                        "source_south"="Southern Source",
                                                        "source_north"="Northern Source"))
CG_source_growth_ll$population <- ordered(CG_source_growth_ll$population, 
                                                levels = c("Northern Source", 
                                                           "Northern Garden",
                                                           "Southern Source", 
                                                           "Southern Garden"))

all_CG_source_growth$population <- plyr::revalue(all_CG_source_growth$population, 
                                                c("Northern"="Northern Garden",
                                                  "Southern"="Southern Garden",
                                                  "source_south"="Southern Source",
                                                  "source_north"="Northern Source"))
all_CG_source_growth$population <- ordered(all_CG_source_growth$population, 
                                          levels = c("Northern Source", 
                                                     "Northern Garden",
                                                     "Southern Source", 
                                                     "Southern Garden"))


# SLA 
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

# LDMC
(ldmc_plot <- ggplot(all_CG_source_traits) +
    geom_boxplot(aes(x= population, y = LDMC_g_g, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("leaf dry matter content ()") +
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

# LMA
(lma_plot <- ggplot(all_CG_source_traits) +
    geom_boxplot(aes(x= population, y = leaf_mass_per_area_g_m2, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("leaf mass per area (g/m2)") +
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

# LA - need to fix values post merge 
(la_plot <- ggplot(all_CG_source_traits_2022) +
    geom_boxplot(aes(x= population, y = LA, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("leaf area ()") +
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

# leaf length - also need to fix units 
(ll_plot <- ggplot(all_CG_source_growth) +
    geom_boxplot(aes(x= population, y = mean_leaf_length, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("leaf length (mm)") +
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
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# panel figures 

(traits_panel <- ggarrange(sla_plot, ldmc_plot, la_plot, ll_plot, 
                          labels = c("A", "B", "C", "D"), common.legend = TRUE, legend = "bottom",
                          ncol = 2, nrow = 2))
  
  
  
  
