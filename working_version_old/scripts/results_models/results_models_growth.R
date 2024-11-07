# Results models for growth differences comparing northern and southern willows
# in the garden to their source populations
# by Erica Zaja, created on 13/12/2022
# Last update: 05/01/2023 by Madi 

# growth: canopy height, shrub width, stem elongation, stem diameter over time 
# attempted model structure: 
# lmer(growth_variable ~ population + (1|Year/Species/Sample_ID))
# population: is (a) common garden northern, (b) common garden southern, (c) source northern (QHI),
# (d) source southern (Kluane)

# if triple nested structure doesn't work, next attempted structure: 
# lmer(growth_variable ~ population + (1|Year/SampleID_standard) + (1|species))

# Loading libraries ----
library(lme4)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(GGally)
library(readr)

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# 1. DATA WRANGLING -----
str(all_CG_source_growth)

# reclassing variables
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)
all_CG_source_growth$SampleID_standard <- as.factor(all_CG_source_growth$SampleID_standard)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$Sample_Date <- as.POSIXct(all_CG_source_growth$Sample_Date, format = '%Y/%m/%d')
all_CG_source_growth$Year <- as.factor(all_CG_source_growth$Year)
all_CG_source_growth$Sample_age <- as.factor(all_CG_source_growth$Sample_age)
unique(all_CG_source_growth$population)
unique(all_CG_source_growth$Site)
unique(all_CG_source_growth$Species)
#view(all_CG_source_growth)

# 2. MODELLING -----

# 1. Canopy height (compare all) -----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below doesn't converge: boundary (singular) fit: see help('isSingular')
# height_growth_mod_1 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 below doesn't converge: boundary (singular) fit: see help('isSingular')
# height_growth_mod_2 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species), data = all_CG_source_growth)
# model 3 runs: species random effect
height_growth_mod_3 <- lmer(Canopy_Height_cm ~ population + (1|Year/SampleID_standard)+ (1|Species), data = all_CG_source_growth)
summary(height_growth_mod_3)
tab_model(height_growth_mod_3) 
# model 4 also runs, but we are keeping model 3
# height_growth_mod_4 <- lmer(Canopy_Height_cm ~ population + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)

# model 5 runs: species interacting
height_growth_mod_5 <- lmer(Canopy_Height_cm ~ population*Species + (1|Year/SampleID_standard), data = all_CG_source_growth)
tab_model(height_growth_mod_5) 

# 1.1. Canopy height only in garden -----
# filter dataset to retain only population "northern" and "southern"
all_CG_source_growth_garden_only <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))
all_CG_source_growth_garden_only$population <- as.factor(all_CG_source_growth_garden_only$population)

str(all_CG_source_growth_garden_only)
view(all_CG_source_growth_garden_only)

# model 1 below doesn't converge: boundary (singular) fit: see help('isSingular')
# height_garden_growth_mod_1 <- lmer(Canopy_Height_cm ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model 2 below converges: Spp random
height_garden_growth_mod_2 <- lmer(Canopy_Height_cm ~ population + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_source_growth_garden_only)
tab_model(height_garden_growth_mod_2)
# model 3 below does converge but keeping model 2
# height_garden_growth_mod_3 <- lmer(Canopy_Height_cm ~ population + Sample_age + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
# canopy height significatly taller for southern population

# model 4 below converges: spp interaction
height_garden_growth_mod_4 <- lmer(Canopy_Height_cm ~ population*Species + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_source_growth_garden_only)
tab_model(height_garden_growth_mod_4)
summary(height_garden_growth_mod_4)


# 2. Width (compare all)-----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below runs but not keeping for consistency with other model structures
# width_growth_mod_1 <- lmer(mean_width ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 runs: spp random
width_growth_mod_2 <- lmer(mean_width~ population + (1|Year/SampleID_standard) + (1|Species), data = all_CG_source_growth)
tab_model(width_growth_mod_2)
# model 3 runs 
# width_growth_mod_3 <- lmer(mean_width ~ population +  (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)

# model 4: spp interact
width_growth_mod_4 <- lmer(mean_width~ population*Species + (1|Year/SampleID_standard) , data = all_CG_source_growth)
tab_model(width_growth_mod_4)

# 2.1. Width only in garden -----
# model 1 below doesnt converge: boundary (singular) fit: see help('isSingular')
# width_garden_growth_mod_1 <- lmer(mean_width ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model below runs: spp random 
width_garden_growth_mod_2 <- lmer(mean_width ~ population + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_source_growth_garden_only)
tab_model(width_garden_growth_mod_2 )
# southern shrubs in the garden have wider canopies

width_garden_growth_mod_3 <- lmer(mean_width ~ population*Species + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_source_growth_garden_only)
tab_model(width_garden_growth_mod_3)
summary(width_garden_growth_mod_3)

# 3. Stem elongation (compare all) ----
# trying comparison with all (CG, KP, QHI)
# nb can't include sample_age because not all the source pop shrubs have age
# model 1 below runs but not keeping for consistency?
# elong_growth_mod_1 <- lmer(mean_stem_elong ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 below runs: spp random
elong_growth_mod_2 <- lmer(mean_stem_elong ~ population + (1|Year/SampleID_standard)+ (1|Species), data = all_CG_source_growth)
tab_model(elong_growth_mod_2)
# model 3 below runs but not using
# elong_growth_mod_3 <- lmer(mean_stem_elong ~ population + (1|Year)+ (1|Species) + (1|SampleID_standard) , data = all_CG_source_growth)

# model 2 below runs: spp interact
elong_growth_mod_4 <- lmer(mean_stem_elong ~ population*Species + (1|Year/SampleID_standard)+ (1|Species), data = all_CG_source_growth)
tab_model(elong_growth_mod_4)

# 3.1. Stem elongation only in garden -----
# model below converges but not using for consistency?
# elong_garden_growth_mod_1 <- lmer(mean_stem_elong ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model below converges: spp random  
elong_garden_growth_mod_2 <- lmer(mean_stem_elong ~ population  + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_source_growth_garden_only)
tab_model(elong_garden_growth_mod_2)
# higher stem elongation for southern shrubs in garden

# model below converges: spp interact
elong_garden_growth_mod_3 <- lmer(mean_stem_elong ~ population*Species  + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_source_growth_garden_only)
tab_model(elong_garden_growth_mod_3)
# higher stem elongation for southern shrubs in garden


# 4. Stem diameter (compare all) ----
# models 1 below doesnt converge: boundary (singular) fit: see help('isSingular')
# diam_growth_mod_1 <- lmer(Stem_diameter ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
# model 2 below does converge: spp random 
diam_growth_mod_2 <- lmer(Stem_diameter ~ population + (1|Year/SampleID_standard) + (1|Species), data = all_CG_source_growth)
tab_model(diam_growth_mod_2)
# model 3 below converges but not using
# diam_growth_mod_3 <- lmer(Stem_diameter ~ population + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)

# model 4 below does converge: spp interact
diam_growth_mod_4 <- lmer(Stem_diameter ~ population*Species + (1|Year/SampleID_standard), data = all_CG_source_growth)
tab_model(diam_growth_mod_4)

# 4.1. Stem diameter only in garden ----
# model 1 below doesnt converge: boundary (singular) fit: see help('isSingular')
# diam_garden_growth_mod_1 <- lmer(Stem_diameter ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model 2 below runs (doens't converge if I add sample age): spp random
diam_garden_growth_mod_2 <- lmer(Stem_diameter ~ population + (1|Year/SampleID_standard) + (1|Species), data = all_CG_source_growth_garden_only)
tab_model(diam_garden_growth_mod_2)
# model 3 below does converge but not using 
# diam_garden_growth_mod_3 <- lmer(Stem_diameter ~ population + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth_garden_only)
# larger diameters for southern shrubs

# model 4 below runs: spp interact
diam_garden_growth_mod_4 <- lmer(Stem_diameter ~ population*Species + (1|Year/SampleID_standard) + (1|Sample_age), data = all_CG_source_growth_garden_only)
tab_model(diam_garden_growth_mod_4)


# 5. Biovolume (compare all)----
# try centering biovolume
all_CG_source_growth$biovolume <- scale(all_CG_source_growth$biovolume, center = TRUE, scale = TRUE)
all_CG_source_growth_garden_only$biovolume <- scale(all_CG_source_growth_garden_only$biovolume, center = TRUE, scale = TRUE)

# models 1 does converge but not using for consistency
#biovol_mod_1 <- lmer(biovolume ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth)
#tab_model(biovol_mod_1)
# model 2 below doesnt  converge 
# biovol_mod_2 <- lmer(biovolume ~ population + (1|Year/SampleID_standard) + (1|Species), data = all_CG_source_growth)
# model 3 below converges: species random
biovol_mod_3 <- lmer(biovolume ~ population + (1|Year) + (1|Species) + (1|SampleID_standard), data = all_CG_source_growth)
tab_model(biovol_mod_3)

# model 4 below converges: spp interact
biovol_mod_4 <- lmer(biovolume ~ population*Species + (1|Year) + (1|SampleID_standard), data = all_CG_source_growth)
tab_model(biovol_mod_4)

# 5.1. Biovolume only in garden ----
# model 1 below doesnt converge: boundary (singular) fit: see help('isSingular')
#biovol_garden_growth_mod_1 <- lmer(biovolume ~ population + (1|Year/Species/SampleID_standard), data = all_CG_source_growth_garden_only)
# model 2 below does not run 
#biovol_garden_growth_mod_2 <- lmer(biovolume ~ population + (1|Year/SampleID_standard) + (1|Species/Sample_age), data = all_CG_source_growth_garden_only)
# model 3 below does converge: spp random 
biovol_garden_growth_mod_3 <- lmer(biovolume ~ population + (1|Species) , data = all_CG_source_growth_garden_only)
tab_model(biovol_garden_growth_mod_3)

# model 4 below does converge: spp random 
biovol_garden_growth_mod_4 <- lm(biovolume ~ population*Species, data = all_CG_source_growth_garden_only)
tab_model(biovol_garden_growth_mod_4)

# 4. DATA VISUALISATION ------

# ordering levels so source and garden populations side by side
all_CG_source_growth$population <- plyr::revalue(all_CG_source_growth$population, 
                                                 c("Northern"="Northern Garden",
                                                   "Southern"="Southern Garden",
                                                   "source_south"="Southern Source",
                                                   "source_north"="Northern Source"))

all_CG_source_growth$population <- ordered(all_CG_source_growth$population, 
                                           levels = c("Northern Source", 
                                                      "Northern Garden", 
                                                      "Southern Garden", 
                                                      "Southern Source"))

# scatter Canopy height CG and source (2013-2022) ----
(plot_canopy_height_all <- ggplot(all_CG_source_growth) +
   geom_smooth(aes(x = Year, y = Canopy_Height_cm, colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Year, y= Canopy_Height_cm, colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~ Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
   xlab("\nYear") +
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

# boxplot Canopy height CG and source (2013-2022) ----
(plot_canopy_height_all <- ggplot(all_CG_source_growth) +
   geom_boxplot(aes(x = population, y = Canopy_Height_cm, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
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

# Canopy height only CG (2013-2022) ----
(plot_canopy_height_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_smooth(aes(x = Sample_age, y = Canopy_Height_cm, colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
   xlab("\n Sample age (n years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.8) +
   scale_fill_viridis_d(begin = 0.1, end = 0.8) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))

# Canopy height CG only (2013-2022) ----
(plot_height_box_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_boxplot(aes(x= population, y = Canopy_Height_cm, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
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
         axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# Shrub width CG + source (2013-2022) ----
(plot_width_all <- ggplot(all_CG_source_growth) +
     geom_boxplot(aes(x= population, y = mean_width, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
     # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
     ylab("Width (cm)") +
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
           axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12, colour = "black"),
           axis.text.y = element_text(size = 12, colour = "black")))

# Shrub width CG only (2013-2022) ----
(plot_width_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_boxplot(aes(x= population, y = mean_width, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Width (cm)") +
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
         axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# Shrub width only CG (2013-2022) ----
(scatter_width_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_smooth(aes(x = Sample_age, y = mean_width, colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= mean_width, colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Width (cm)") +
   xlab("\n Sample age (n years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.8) +
   scale_fill_viridis_d(begin = 0.1, end = 0.8) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))

# Stem elongation CG + source (2013-2022) ----
(plot_stem_all <- ggplot(all_CG_source_growth) +
   geom_boxplot(aes(x= population, y = mean_stem_elong, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Elongation (mm)") +
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

# Stem elongation CG only (2013-2022) ----
(plot_stem_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_boxplot(aes(x= population, y = mean_stem_elong, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Elongation (mm)") +
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

# Stem elong only CG (2013-2022) ----
(scatter_elong_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_smooth(aes(x = Sample_age, y = mean_stem_elong, colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= mean_stem_elong, colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Elongation (mm)") +
   xlab("\n Sample age (n years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.8) +
   scale_fill_viridis_d(begin = 0.1, end = 0.8) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))

# Stem diameter CG + source (2013-2022) ----
(plot_stem_diam_all <- ggplot(all_CG_source_growth) +
   geom_boxplot(aes(x= population, y = Stem_diameter, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Diameter (mm)") +
   xlab("\n ") +
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

# Stem diameter CG only (2013-2022) ----
(plot_stem_diam_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_boxplot(aes(x= population, y = Stem_diameter, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Diameter (mm)") +
   xlab("\n Year") +
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


# Stem diam only CG (2013-2022) ----
(scatter_diam_CG <- ggplot(all_CG_source_growth_garden_only) +
   geom_smooth(aes(x = Sample_age, y = Stem_diameter, colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= Stem_diameter, colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem diameter (mm)") +
   xlab("\n Sample age (n years)") +
   scale_colour_viridis_d(begin = 0.1, end = 0.8) +
   scale_fill_viridis_d(begin = 0.1, end = 0.8) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))

# Biovolume  CG + source (2013-2022) ----
(plot_biovol_all <- ggplot(all_CG_source_growth) +
   geom_smooth(aes(x = Sample_age, y = log(biovolume), colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= log(biovolume), colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free") +
   ylab("Biovolume (cm3)") +
   xlab("\n ") +
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

# Biovolume  CG only (2013-2022) ----
(plot_biovol_garden <- ggplot(all_CG_source_growth_garden_only) +
   geom_smooth(aes(x = Sample_age, y = log(biovolume), colour = population, fill = population, group = population, method = "glm")) +
   geom_point(aes(x = Sample_age, y= log(biovolume), colour = population, group = population), size = 1.5, alpha = 0.5) +
   # facet_grid(cols = vars(Species)) +   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free") +
   ylab("Biovolume (log, cm3)") +
   xlab("\n ") +
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

# 3. CORRELATION -----
# Correlation matrix growth ----

# filter growth in the CG
growth_variables_CG <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern")) %>%
  dplyr::select(Canopy_Height_cm, Stem_diameter, mean_stem_elong, mean_width)%>%
  na.omit()
view(growth_variables_CG)

res <- cor(growth_variables_CG)
round(res, 2)
# Visualising correlation matrix with ggcorr
ggcorr(growth_variables_CG, method = c("everything", "pearson")) 

# Correlation matrix growth AND traits ----
# import all data (CG growth + traits)
all_cg_growth_traits_data <- read_csv("data/all_cg_growth__traits_data.csv")
view(all_cg_growth_traits_data)

# keep columns of interest for correlation
all_corr_variables_CG <- all_cg_growth_traits_data %>%
  dplyr::select(-"...1",- Bed, -SampleID, -Year_planted, -Species, -Site, -Year,
                - SampleID_standard, - Sample_age, - population, - Lat,-Lon, 
                -Elevation_m, -Width_cm, -Width_2_cm,
                -Length_1_mm, -Length_2_mm, -Length_3_mm, -Stem_Elongation_1_mm,
                -Stem_Elongation_2_mm, -Stem_Elongation_3_mm)

res_1 <- cor(all_corr_variables_CG)
round(res_1, 2)

ggcorr(all_corr_variables_CG, method = c("everything", "pearson")) 
# NAs...! 
# abandoning this. We don't really need it.
