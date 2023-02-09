# methods models for trait differences comparing northern and southern willows
# Created 29 Nov 2022 Madelaine 
# last updated: 08/02/2023

# traits: SLA, LDMC, LA, leaf length, stem diameter, shrub width
# model structure: 
#  lmer(SLA ~ site*species +  (1|sample_year/field_sample_ID))
#  OR, having species as a random effect: 
# lmer(SLA ~ site + (1|sample_year) + (1|species))
# where site is Kluane (southern) or Qikiqtaruk (northern)


# libraries ----
library(lme4)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)

# data ---- 
# SLA, LDMC, LA: 
all_source_area_traits <- read.csv("data/source_pops/all_source_area_traits.csv")
# leaf length, stem diameter, width 
unique_source_mother <-  read.csv("data/source_pops/unique_source_mother.csv")

str(all_source_area_traits)
# reclass variables 
all_source_area_traits$Species <- as.factor(all_source_area_traits$Species)
all_source_area_traits$Site <- as.factor(all_source_area_traits$Site)
all_source_area_traits$year <- as.factor(all_source_area_traits$year)

str(unique_source_mother) 
# reclass variables 
unique_source_mother$Species <- as.factor(unique_source_mother$Species)
unique_source_mother$Site <- as.factor(unique_source_mother$Site)
unique_source_mother$SampleYear <- as.factor(unique_source_mother$SampleYear)

# SLA ----
# not including field_sample_id because those don't exist for any values prior to 2021 ? and not repeated measures anyways 
SLA_method_mod <- lmer(SLA ~ Site*Species + (1|year), data = all_source_area_traits) 
summary(SLA_method_mod)
plot(SLA_method_mod)
qqnorm(resid(SLA_method_mod))
qqline(resid(SLA_method_mod)) 
tab_model(SLA_method_mod)

(SLA_p <- ggplot(all_source_area_traits, aes(Site, SLA)) +
    geom_boxplot() +
    facet_wrap(vars(Species)))

(SLA_p_source <- ggplot(all_source_area_traits) +
    geom_boxplot(aes(x = Site, y = SLA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("SLA") +
    xlab("\n") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# okay, do we want species as an interaction term? not our main q and our other models aren't including it
# here, as random effect: 
SLA_method_mod_spp <- lmer(SLA ~ Site + (1|year) + (1|Species), data = all_source_area_traits) 
summary(SLA_method_mod_spp)
plot(SLA_method_mod_spp)
qqnorm(resid(SLA_method_mod_spp))
qqline(resid(SLA_method_mod_spp))
tab_model(SLA_method_mod_spp)

# LDMC ---- 
# same comments as above (SLA)
LDMC_method_mod <- lmer(LDMC_g_g ~ Site*Species + (1|year), data = all_source_area_traits) 
summary(LDMC_method_mod)
plot(LDMC_method_mod)
qqnorm(resid(LDMC_method_mod))
qqline(resid(LDMC_method_mod)) 
tab_model(LDMC_method_mod)

(LDMC_p <- ggplot(all_source_area_traits, aes(Site, LDMC_g_g)) +
    geom_boxplot() +
    facet_wrap(vars(Species)))

(LDMC_p_source <- ggplot(all_source_area_traits) +
    geom_boxplot(aes(x = Site, y = LDMC_g_g, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("LDMC_g_g") +
    xlab("\n") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# species as random effect 
LDMC_method_mod_spp <- lmer(LDMC_g_g ~ Site + (1|year) + (1|Species), data = all_source_area_traits) 
summary(LDMC_method_mod_spp)
plot(LDMC_method_mod_spp)
qqnorm(resid(LDMC_method_mod_spp))
qqline(resid(LDMC_method_mod_spp)) 
tab_model(LDMC_method_mod_spp)

# leaf length (LL) ----
# use mean value from 3 leaf lengths 
# note: we only have two years of data for leaf length in the source populations, omitted as random effect for now? 
# also note: we don't have arctica leaf length data for Kluane so making new df without arctica 
ll_data <-  unique_source_mother %>% 
  filter(Species != "Salix arctica")

LL_method_mod <- lm(mean_leaf_length ~ Site*Species, data = unique_source_mother) 

summary(LL_method_mod) 
plot(LL_method_mod)
qqnorm(resid(LL_method_mod))
qqline(resid(LL_method_mod))
tab_model(LL_method_mod)

# species as random effect 
# although only 2 levels so I'm not inclined to do this, see above model 
LL_method_mod_spp <- lmer(mean_leaf_length ~ Site + (1|Species), data = unique_source_mother) 
summary(LL_method_mod_spp) 
plot(LL_method_mod_spp)
qqnorm(resid(LL_method_mod_spp))
qqline(resid(LL_method_mod_spp))
tab_model(LL_method_mod_spp)

(ll_p <- ggplot(unique_source_mother, aes(Site, mean_leaf_length)) +
    geom_boxplot() +
    facet_wrap(vars(Species))) 

(Ll_p_source <- ggplot(unique_source_mother) +
    geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("leaf length") +
    xlab("\n") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# LA ---- 
LA_method_mod <- lmer(LA ~ Site*Species + (1|year), data = all_source_area_traits) 
summary(LA_method_mod)
plot(LA_method_mod)
qqnorm(resid(LA_method_mod))
qqline(resid(LA_method_mod)) 
tab_model(LA_method_mod)

# species as random effect 
LA_method_mod_spp <- lmer(LA ~ Site + (1|year) + (1|Species), data = all_source_area_traits) 
summary(LA_method_mod_spp)
plot(LA_method_mod_spp)
qqnorm(resid(LA_method_mod_spp))
qqline(resid(LA_method_mod_spp)) 
tab_model(LA_method_mod_spp)

(LA_p <- ggplot(all_source_area_traits, aes(Site, LA)) +
    geom_boxplot() +
    facet_wrap(vars(Species)))


# something weird is going on with leaf area data -- not sure if maybe in past years units were not consistent? 
# filter only data collected by Madi in 2021 and 2022
# note this reduces sample size 

mad_traits <- all_source_area_traits %>% 
  filter(year %in% c("2021", "2022")) %>% 
  select(-X)

LA_method_mod_mad <- lmer(LA ~ Site + (1|Species), data = mad_traits) # omitted sample year bc only 2 levels 
summary(LA_method_mod_mad)
tab_model(LA_method_mod_mad)

LA_method_mod <- lm(LA ~ Site*Species, data = mad_traits) # omitted sample year bc only 2 levels 
summary(LA_method_mod)
tab_model(LA_method_mod)

(LA_p_source <- ggplot(mad_traits) +
    geom_boxplot(aes(x = Site, y = LA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("LA") +
    xlab("\n") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))


(LA_p <- ggplot(mad_traits, aes(Site, LA)) + 
    geom_boxplot() +
    facet_wrap(vars(Species))) # this makes sense to me, for what it's worth 

(SLA_p_mad <- ggplot(mad_traits, aes(Site, SLA)) + 
    geom_boxplot() +
    facet_wrap(vars(Species)))

(LDMC_p_mad <- ggplot(mad_traits, aes(Site, LDMC_g_g)) + 
    geom_boxplot() +
    facet_wrap(vars(Species)))


# quick arrange 
(traits_plots <- ggarrange(SLA_p_source, LA_p_source, LDMC_p_source, Ll_p_source, nrow = 2, ncol = 2))

# visualize traits by year  ---- 

(LDMC_p_year <- ggplot(all_source_area_traits, aes(Site, LDMC_g_g)) + 
   geom_boxplot() +
   facet_wrap(vars(year)))

(SLA_p_year <- ggplot(all_source_area_traits, aes(Site, SLA)) + 
    geom_boxplot() +
    facet_wrap(vars(year)))

(LL_p_year <- ggplot(ll_data, aes(Site, mean_leaf_length)) + 
    geom_boxplot() +
    facet_wrap(vars(SampleYear)))

(LA_p_year <- ggplot(all_source_area_traits, aes(Site, LA)) + # the unnits must be reported wrong in the 2017 data 
    geom_boxplot() +
    facet_wrap(vars(year)))
