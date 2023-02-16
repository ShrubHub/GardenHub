#### MATERNAL EFFECTS SCRIPT 
### Data wrangling and visualisation script
### By Erica Zaja, created on 31/10/2022
## Last updated: 31/01/2023 by Madi

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(dplyr)
library(lme4)
library(sjPlot)

# 2. LOADING DATA -----

# Datasets with mother data (2013-2017) and source pop data
mother_data <- read_csv("data/source_pops/mother_data.csv")

# maximum height and width data from CG 2013-2022 
max_cg_heights <- read.csv("data/common_garden_data_2022/max_heights_cg.csv")
max_cg_widths <- read.csv("data/common_garden_data_2022/max_widths_cg.csv")
max_cg_biovol <-  read.csv("data/common_garden_data_2022/max_biovol_cg.csv")
all_cg <- read_csv("data/common_garden_data_2022/all_merged_data_2022.csv") 
cg_2022 <- all_cg %>%
  dplyr::select(-...1)

# 3. DATA WRANGLING ---- 

# Match mother heights with heights in common garden

# Making variables in right format
str(mother_data)
mother_data$Site <- as.factor(mother_data$Site)
mother_data$SampleID <- as.factor(mother_data$SampleID)
mother_data$SampleYear <- as.factor(mother_data$SampleYear)

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

# reclassing variables
str(mother_data_merge)
mother_data_merge$Site <- as.factor(mother_data_merge$Site)

mother_data_merge <- mother_data_merge %>% 
  mutate(Species = ifelse(grepl("SA", mother_data_merge $SampleID_standard), "Salix arctica",
                            ifelse(grepl("SR", mother_data_merge $SampleID_standard), "Salix richardsonii", 
                                   ifelse(grepl("SP", mother_data_merge $SampleID_standard), "Salix pulchra", NA))))

mother_data_merge$Species <- as.factor(mother_data_merge$Species)

# Summarising means -----
# make cg means using only 2022 data 
#cg_means_2022 <- all_cg %>%
#  filter(Year == "2022") %>% 
#  group_by(Species, Site) %>%
#  dplyr::summarise(n = n(),  # Calculating sample size n
#    mean_Canopy_Height_cm = mean(Canopy_Height_cm, na.rm = TRUE),
#     mean_mean_width = mean(mean_width, na.rm = TRUE),
#      mean_mean_stem_elong = mean(mean_stem_elong, na.rm = TRUE),
#       mean_Stem_diameter = mean(Stem_diameter, na.rm = TRUE), 
#        mean_mean_leaf_length = mean(mean_leaf_length, na.rm = TRUE), 
#         sd_Canopy_Height_cm = sd(Canopy_Height_cm, na.rm = TRUE),
#          sd_width = sd(mean_width, na.rm = TRUE),
#           sd_stem_elong = sd(mean_stem_elong, na.rm = TRUE),
#            sd_leaf_length = sd(mean_leaf_length, na.rm = TRUE),
#             sd_Stem_diameter = sd(Stem_diameter, na.rm = TRUE),
#              se_Canopy_Height_cm = sd(Canopy_Height_cm, na.rm = TRUE)/sqrt(n), 
#               se_mean_mother_width = sd(mean_width, na.rm = TRUE)/sqrt(n),
#                se_mean_mother_elong = sd(mean_stem_elong, na.rm = TRUE)/sqrt(n),
#                 se_mean_leaf_length = sd(mean_leaf_length, na.rm = TRUE)/sqrt(n),
#                  se_Stem_diameter = sd(Stem_diameter, na.rm = TRUE)/sqrt(n))

#cg_means_2022$Species <- as.factor(cg_means_2022$Species)
#cg_means_2022$Site <- as.factor(cg_means_2022$Site) 

# also calculate standard error and standard deviation 
mother_cg_means <- mother_data_merge %>%
  group_by(Species, Site) %>%
  dplyr::summarise(n = n(),  # Calculating sample size n
            mean_mother_height = mean(Mother_Canopy_Height_cm, na.rm = TRUE),
            mean_mother_width = mean(Mother_mean_width, na.rm = TRUE),
            mean_mother_elong = mean(Mother_mean_stem_elong, na.rm = TRUE),
            mean_mother_diam = mean(Cutting_diameter, na.rm = TRUE), 
            sd_mother_height = sd(Mother_Canopy_Height_cm, na.rm = TRUE),
            sd_mother_width = sd(Mother_mean_width, na.rm = TRUE),
            sd_mother_elong = sd(Mother_mean_stem_elong, na.rm = TRUE),
            sd_mother_diam = sd(Cutting_diameter, na.rm = TRUE),
            se_mother_height = sd(Mother_Canopy_Height_cm, na.rm = TRUE)/sqrt(n), 
            se_mean_mother_width = sd(Mother_mean_width, na.rm = TRUE)/sqrt(n),
            se_mean_mother_elong = sd(Mother_mean_stem_elong, na.rm = TRUE)/sqrt(n),
            se_mean_mother_diam = sd(Cutting_diameter, na.rm = TRUE)/sqrt(n))

# same as above for common garden data 
# note, unlike for maternal data, there are multiple years of data here
str(all_cg)
cg_means <- all_cg %>%
  group_by(Species, Site) %>%
  dplyr::summarise(n = n(),  # Calculating sample size n
                   mean_Canopy_Height_cm = mean(Canopy_Height_cm, na.rm = TRUE),
                   mean_mean_width = mean(mean_width, na.rm = TRUE),
                   mean_mean_stem_elong = mean(mean_stem_elong, na.rm = TRUE),
                   mean_Stem_diameter = mean(Stem_diameter, na.rm = TRUE), 
                   mean_mean_leaf_length = mean(mean_leaf_length, na.rm = TRUE), 
                   sd_Canopy_Height_cm = sd(Canopy_Height_cm, na.rm = TRUE),
                   sd_width = sd(mean_width, na.rm = TRUE),
                   sd_stem_elong = sd(mean_stem_elong, na.rm = TRUE),
                   sd_leaf_length = sd(mean_leaf_length, na.rm = TRUE),
                   sd_Stem_diameter = sd(Stem_diameter, na.rm = TRUE),
                   se_Canopy_Height_cm = sd(Canopy_Height_cm, na.rm = TRUE)/sqrt(n), 
                   se_mean_mother_width = sd(mean_width, na.rm = TRUE)/sqrt(n),
                   se_mean_mother_elong = sd(mean_stem_elong, na.rm = TRUE)/sqrt(n),
                   se_mean_leaf_length = sd(mean_leaf_length, na.rm = TRUE)/sqrt(n),
                   se_Stem_diameter = sd(Stem_diameter, na.rm = TRUE)/sqrt(n))


# merge mother data with max height -----
# instead of taking only 2022 data, use max height data 
str(max_cg_heights)

max_cg_heights_merge <- max_cg_heights %>% 
  dplyr::select(c(Species, max_canopy_height_cm, population, Site, SampleID_standard)) 

max_cg_biovol_merge <- max_cg_biovol %>% 
  dplyr::select((c(Species, max_biovol, population, Site, SampleID_standard)))

max_cg_widths_merge <- max_cg_widths %>% 
  dplyr::select(c(Species, max_mean_width_cm, population, Site, SampleID_standard))

mother_data_merge_1 <- mother_data_merge %>% 
  dplyr::select(-c(SampleDate, Date_propagated))

mother_cg_height <- full_join(mother_data_merge_1, max_cg_heights_merge, 
                      by = c("SampleID_standard" = "SampleID_standard", 
                             "Species" = "Species",
                            "Site" = "Site"))

mother_cg_widths <- full_join(mother_cg_height, max_cg_widths_merge, 
                              by = c("SampleID_standard" = "SampleID_standard", 
                                     "Species" = "Species",
                                     "Site" = "Site"))

mother_cg <- full_join(mother_cg_widths, max_cg_biovol_merge, 
                                    by = c("SampleID_standard" = "SampleID_standard", 
                                           "Species" = "Species",
                                           "Site" = "Site"))

# add a mother or child column
mother_cg_edit <- mother_cg %>%
  mutate(mother_or_child = ifelse(population %in% c("Northern", "Southern"), "child", "mother"))

mother_cg_edit$mother_or_child <- as.factor(mother_cg_edit$mother_or_child)  

# MODELS -----
# model structure we want:
#lmer(mother_height ~ child_height + Site + (1|species))

# HEIGHT MODEL ------
maternal_height_mod <-  lmer(Mother_Canopy_Height_cm ~ max_canopy_height_cm + Site + (1|Species), data = mother_cg)
summary(maternal_height_mod)
tab_model(maternal_height_mod)

maternal_height_mod_nosite <-  lmer(Mother_Canopy_Height_cm ~ max_canopy_height_cm + (1|Species), data = mother_cg)
tab_model(maternal_height_mod_nosite)

# make species specific dfs
mother_cg_arctica <- mother_cg %>% 
  dplyr::filter(Species == "Salix arctica")

mother_cg_richardsonii <- mother_cg %>% 
  dplyr::filter(Species == "Salix richardsonii")

mother_cg_pulchra <- mother_cg %>% 
  dplyr::filter(Species == "Salix pulchra")

# Species specific height models -----
maternal_height_pulchra_mod <-  lm(Mother_Canopy_Height_cm ~ max_canopy_height_cm + Site, data = mother_cg_pulchra)
summary(maternal_height_pulchra_mod)
tab_model(maternal_height_pulchra_mod)
anova(maternal_height_pulchra_mod)

maternal_height_richardsonii_mod <-  lm(Mother_Canopy_Height_cm ~ max_canopy_height_cm + Site, data = mother_cg_richardsonii)
summary(maternal_height_richardsonii_mod)
tab_model(maternal_height_richardsonii_mod)
anova(maternal_height_richardsonii_mod)

maternal_height_arctica_mod <-  lm(Mother_Canopy_Height_cm ~ max_canopy_height_cm + Site, data = mother_cg_arctica)
summary(maternal_height_arctica_mod)
tab_model(maternal_height_arctica_mod)
anova(maternal_height_arctica_mod)

ggplot(mother_cg, aes(x = max_canopy_height_cm, y = Mother_Canopy_Height_cm, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

# make long format of data 
mother_cg_long <- mother_cg_edit %>% 
  gather(key = mother_or_child, value = height, c(Mother_Canopy_Height_cm, max_canopy_height_cm), factor_key=TRUE) 

levels(mother_cg_long$mother_or_child) <- list(Mother  = "Mother_Canopy_Height_cm", Child = "max_canopy_height_cm")

(plot_mother_height_model <- ggplot() +
    geom_point(aes(x = max_canopy_height_cm, y = Mother_Canopy_Height_cm, color = mother_or_child), size = 3, alpha = 0.5, data = mother_cg_edit) +
    geom_smooth(aes(x = max_canopy_height_cm, y = Mother_Canopy_Height_cm), method = "lm", data = mother_cg_edit) +
    # facet_wrap(~Species + Site, scales = "free") +
    facet_grid(Species ~ Site, scales = "free") +
    ylab("Mother shrub canopy height in sources (cm)") +
    xlab("\nMax child canopy height in common garden (cm)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

(plot_mother_height_model <- ggplot(mother_cg_edit, aes(x = max_canopy_height_cm, y = Mother_Canopy_Height_cm)) +
    geom_point(aes(color = mother_or_child), size = 3, alpha = 0.5) +
    facet_wrap(vars(Species)) +
    ylab("Mother shrub canopy height in sources (cm)") +
    xlab("\nMax child canopy height in common garden (cm)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

plot(mother_cg_edit$max_canopy_height_cm, mother_cg_edit$Mother_Canopy_Height_cm, col=as.factor(mother_cg_edit$mother_or_child))

test <- mother_cg_edit %>% select(c(mother_or_child, max_mean_width_cm, Species, 
                                    Mother_mean_width))

# WIDTH MODEL  -----
maternal_width_mod <-  lmer(Mother_mean_width ~ max_mean_width_cm + Site + (1|Species), data = mother_cg)
summary(maternal_width_mod)
tab_model(maternal_width_mod)

maternal_width_mod_nosite <-  lmer(Mother_mean_width ~ max_mean_width_cm + (1|Species), data = mother_cg)
tab_model(maternal_width_mod_nosite)

# Species specific width models -----
maternal_width_pulchra_mod <-  lm(Mother_mean_width ~ max_mean_width_cm + Site, data = mother_cg_pulchra)
summary(maternal_width_pulchra_mod)
tab_model(maternal_width_pulchra_mod) # no effects
anova(maternal_width_pulchra_mod)

maternal_width_rich_mod <-  lm(Mother_mean_width ~ max_mean_width_cm + Site, data = mother_cg_richardsonii)
summary(maternal_width_rich_mod)
tab_model(maternal_width_rich_mod) # yes effects
anova(maternal_width_rich_mod)

maternal_width_arc_mod <-  lm(Mother_mean_width ~ max_mean_width_cm + Site, data = mother_cg_arctica)
summary(maternal_width_arc_mod)
tab_model(maternal_width_arc_mod) # no effects
anova(maternal_width_arc_mod)

(plot_mother_width_model <- ggplot() +
    geom_point(aes(x = max_mean_width_cm, y = Mother_mean_width, color = mother_or_child), size = 3, alpha = 0.5, data = mother_cg_edit) +
    geom_smooth(aes(x = max_mean_width_cm, y = Mother_mean_width), method = "lm", data = mother_cg_edit) +
   # facet_wrap(~Species + Site, scales = "free") +
    facet_grid(Species ~ Site, scales = "free") +
    ylab("Mother shrub width in sources (cm)") +
    xlab("\nMax child width in common garden (cm)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

(plot_mother_width_model <- ggplot(mother_cg_edit, aes(x = max_mean_width_cm, y = Mother_mean_width)) +
    geom_point(aes(color = mother_or_child), size = 3, alpha = 0.5) +
    facet_wrap(vars(Species)) +
    ylab("Mother shrub canopy width in sources (cm)") +
    xlab("\nMax child canopy width in common garden (cm)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 


# BIOVOLUME ----
# log transforming biovolume and cutting length 
mother_cg_edit_biovol <- mother_cg_edit %>%
  mutate(mother_biovol_log = log(Mother_biovolume),
         child_max_biovol_log = log(max_biovol),
         log_cutting_length = log(Cutting_length))

mother_cg_edit_biovol_arctica <- mother_cg_edit_biovol %>%
  filter(Species == "Salix arctica")

mother_cg_edit_biovol_rich <- mother_cg_edit_biovol %>%
  filter(Species == "Salix richardsonii")

mother_cg_edit_biovol_pulchra <- mother_cg_edit_biovol %>%
  filter(Species == "Salix pulchra")

# all species model
maternal_biovol_mod <-  lmer(mother_biovol_log ~ child_max_biovol_log + Site + (1|Species), data = mother_cg_edit_biovol)
summary(maternal_biovol_mod)
tab_model(maternal_biovol_mod) # nothing significant

# run model without site 
maternal_biovol_nosite_mod <-  lmer(mother_biovol_log ~ child_max_biovol_log + (1|Species), data = mother_cg_edit_biovol)
summary(maternal_biovol_nosite_mod)
tab_model(maternal_biovol_nosite_mod) # still nothing significant

# Species specific biovolume models -----
maternal_biovol_pulchra_mod <-  lm(mother_biovol_log ~ child_max_biovol_log + Site, data = mother_cg_edit_biovol_pulchra)
summary(maternal_biovol_pulchra_mod)
tab_model(maternal_biovol_pulchra_mod) # yes effects?or just QHI?
anova(maternal_biovol_pulchra_mod)

maternal_biovol_rich_mod <-  lm(mother_biovol_log ~ child_max_biovol_log + Site, data = mother_cg_edit_biovol_rich)
summary(maternal_biovol_rich_mod)
tab_model(maternal_biovol_rich_mod) # no effects
anova(maternal_biovol_rich_mod)

maternal_biovol_arc_mod <-  lm(mother_biovol_log ~ child_max_biovol_log + Site, data = mother_cg_edit_biovol_arctica)
summary(maternal_biovol_arc_mod)
tab_model(maternal_biovol_arc_mod) # no effects
anova(maternal_biovol_arc_mod)

(plot_biovol_maternal_model <- ggplot() +
    geom_point(aes(x = child_max_biovol_log, y= mother_biovol_log, color =mother_or_child), size = 3, alpha = 0.5, data = mother_cg_edit_biovol) +
    geom_smooth(aes(x = child_max_biovol_log, y= mother_biovol_log), method = "lm", data = mother_cg_edit_biovol) +
    # facet_wrap(~Species + Site, scales = "free") +
    facet_grid(Species ~ Site, scales = "free") +    
    ylab("Mother biovolume (cm3)") +
    xlab("\nMax child biovolume in common garden (cm3)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# PROPOGATION DATA MODELS ----
#a. Height vs Cutting length -----
cutting_length_mod <-  lmer(Cutting_length ~ max_canopy_height_cm + Site + (1|Species), data = mother_cg_edit)
summary(cutting_length_mod)
tab_model(cutting_length_mod)

cutting_length_mod_nosite <-  lmer(Cutting_length ~ max_canopy_height_cm + (1|Species), data = mother_cg_edit)
tab_model(cutting_length_mod_nosite)

# Species specific propagation models -----
maternal_prop_pulchra_mod <-  lm(Cutting_length ~ max_canopy_height_cm + Site, data = mother_cg_pulchra)
summary(maternal_prop_pulchra_mod)
tab_model(maternal_prop_pulchra_mod) #no effects
anova(maternal_prop_pulchra_mod)

maternal_prop_rich_mod <-  lm(Cutting_length ~ max_canopy_height_cm + Site, data = mother_cg_richardsonii)
summary(maternal_prop_rich_mod)
tab_model(maternal_prop_rich_mod) # no effects
anova(maternal_prop_rich_mod)

maternal_prop_arc_mod <-  lm(Cutting_length ~ max_canopy_height_cm + Site, data = mother_cg_arctica)
summary(maternal_prop_arc_mod)
tab_model(maternal_prop_arc_mod) # yes effects??
anova(maternal_prop_arc_mod)

(plot_cutting_length_model <- ggplot() +
    geom_point(aes(x = max_canopy_height_cm, y= Cutting_length, color =mother_or_child), size = 3, alpha = 0.5, data = mother_cg_edit) +
    geom_smooth(aes(x = max_canopy_height_cm, y= Cutting_length), method = "lm", data = mother_cg_edit) +
    # facet_wrap(~Species + Site, scales = "free") +
    facet_grid(Species ~ Site, scales = "free") +    
    ylab("Cutting length (cm)") +
    xlab("\nMax child canopy height in common garden (cm)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"))) 

# b.Biovolume vs cutting length model -----
cutting_bio_mod <-  lmer(child_max_biovol_log ~ log_cutting_length + Site + (1|Species), data = mother_cg_edit_biovol)
summary(cutting_bio_mod)
tab_model(cutting_bio_mod) # site term significant

cutting_bio_mod_nosite <-  lmer(child_max_biovol_log ~ log_cutting_length + (1|Species), data = mother_cg_edit_biovol)
tab_model(cutting_bio_mod_nosite)

# Species specific propagation models -----
maternal_prop_bio_pulchra_mod <-  lm(child_max_biovol_log ~ log_cutting_length + Site, data = mother_cg_edit_biovol_pulchra)
summary(maternal_prop_bio_pulchra_mod)
tab_model(maternal_prop_bio_pulchra_mod) #yes?
anova(maternal_prop_bio_pulchra_mod)

maternal_prop_bio_arc_mod <-  lm(child_max_biovol_log ~ log_cutting_length + Site, data = mother_cg_edit_biovol_arctica)
summary(maternal_prop_bio_arc_mod)
tab_model(maternal_prop_bio_arc_mod) #yes?
anova(maternal_prop_bio_arc_mod)

maternal_prop_bio_rich_mod <-  lm(child_max_biovol_log ~ log_cutting_length + Site, data = mother_cg_edit_biovol_rich)
summary(maternal_prop_bio_rich_mod)
tab_model(maternal_prop_bio_rich_mod) #yes?
anova(maternal_prop_bio_rich_mod)


(plot_biovol_cutting_model <- ggplot() +
    geom_point(aes(x = child_max_biovol_log, y= log_cutting_length, color =mother_or_child), size = 3, alpha = 0.5, data = mother_cg_edit_biovol) +
    geom_smooth(aes(x = child_max_biovol_log, y= log_cutting_length, colour =mother_or_child), method = "lm", data = mother_cg_edit_biovol) +
    # facet_wrap(~Species + Site, scales = "free") +
    facet_grid(Species ~ Site, scales = "free") +    
    ylab("Maternal cutting length (cm)") +
    xlab("\nMax child biovolume in common garden (cm3)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

