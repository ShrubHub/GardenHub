#### MATERNAL EFFECTS SCRIPT 
### Data wrangling and visualisation script
### By Erica Zaja, created on 31/10/2022
## Last updated: 08/12/2022 by Madi

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

# All 2022 data from the common garden
all_cg_2022 <- read_csv("data/common_garden_data_2022/all_merged_data_2022.csv") 


# 3. DATA WRANGLING ---- 

# Match mother heights with heights in common garden
cg_2022 <- all_cg_2022 %>%
  select(-...1)

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
         "Mother_mean_width" = "mean_width") %>% 
  dplyr::select(-Match, -...1, -...2) %>%   # these columns are useless and will only cause anger and pain
  dplyr::select(-SampleID, - Site) %>%
  dplyr::mutate(Site = case_when(SampleSite %in% c("Kluane", "Kluane Plateau", "Pika Camp", "Printers Pass") ~ 'Kluane', 
                          SampleSite %in% c("Qikiqtaruk","QHI") ~ 'Qikiqtaruk'))%>%
  dplyr::select(-SampleSite)
# also drop this because it's weird inconsistent and will cause merging issues 

# reclassing variables
str(mother_data_merge)
mother_data_merge$Site <- as.factor(mother_data_merge$Site)

mother_data_merge <- mother_data_merge %>% 
  mutate(Species = ifelse(grepl("SA", mother_data_merge $SampleID_standard), "Salix arctica",
                            ifelse(grepl("SR", mother_data_merge $SampleID_standard), "Salix richardsonii", 
                                   ifelse(grepl("SP", mother_data_merge $SampleID_standard), "Salix pulchra", NA))))

mother_data_merge$Species <- as.factor(mother_data_merge$Species)

# Summarising means -----
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

# madi checking means a slightly way to compare values  
mother_cg_means_MA <- mother_data_merge %>%
  group_by(Species, Site) %>%
  summarise_at(c("Mother_Canopy_Height_cm", "Mother_mean_width", 
                 "Mother_mean_stem_elong", "Cutting_diameter"
                 ), mean, na.rm = TRUE) # woohoo they match! 
# and sd 
mother_cg_sd_MA <-  mother_data_merge %>% 
  group_by(Species, Site) %>%
  summarise_at(c("Mother_Canopy_Height_cm", "Mother_mean_width", 
                 "Mother_mean_stem_elong", "Cutting_diameter"),
               sd, na.rm = TRUE) 

# same as above for common garden data 
# note, unlike for maternal data, there are multiple years of data here
str(all_cg_2022)
cg_means <- all_cg_2022 %>%
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

# make cg means using only 2022 data 
cg_means_2022 <- all_cg_2022 %>%
  filter(Year == "2022") %>% 
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

cg_means_2022$Species <- as.factor(cg_means_2022$Species)
cg_means_2022$Site <- as.factor(cg_means_2022$Site) 

# Erica tries to put data in long format  -----
cg_means_2022_long <- cg_means_2022 %>%
group_by(Species, Site) %>%              
 pivot_longer(cols = starts_with("mean"), names_to = "trait", names_prefix = "mean_", values_to = "mean_value") %>%
 pivot_longer(cols = starts_with("sd"), names_to = "SD_trait", names_prefix = "sd_", values_to = "sd_value") %>% 
  pivot_longer(cols = starts_with("se"), names_to = "SE_trait", names_prefix = "se_", values_to = "se_value")
# same as above but for means for mothers   
mother_means_long <- mother_cg_means %>%
  group_by(Species, Site) %>%              
  pivot_longer(cols = starts_with("mean"), names_to = "trait", names_prefix = "mean_", values_to = "mean_value") %>%
  pivot_longer(cols = starts_with("sd"), names_to = "SD_trait", names_prefix = "sd_", values_to = "sd_value") %>% 
  pivot_longer(cols = starts_with("se"), names_to = "SE_trait", names_prefix = "se_", values_to = "se_value")
# merge together 
means_long_all <- rbind(mother_means_long, cg_means_2022_long) # not perfect, will fiddle with 

# checking format of CG data
str(cg_2022)
cg_2022$Site <- as.factor(cg_2022$Site)
cg_2022$Species <- as.factor(cg_2022$Species)

# merging means datasets and making into long format
means_all <- rbind(cg_means_2022, mother_cg_means) 

means_long <- means_all 
  
# merging datasets
mother_cg <- full_join(mother_data_merge, cg_2022, 
                       by = c("SampleID_standard" = "SampleID_standard", 
                              "Year_planted" = "Year_planted", 
                               "SampleDate" = "Sample_Date",
                                "Species" = "Species",
                                "Site" = "Site"))

# HEIGHTS: making one single column for each trait and a "treatment" column for mother/child
mother_cg_long_heights <- mother_cg %>%
  select(SampleDate,  Year_planted, Mother_Canopy_Height_cm, Canopy_Height_cm,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Mother_Canopy_Height_cm","Canopy_Height_cm"),
               names_to = "treatment", values_to = "Height_cm")


# reclassing variables
mother_cg_long_heights$treatment <- as.factor(mother_cg_long_heights$treatment)
mother_cg_long_heights$Sample_age <- as.factor(mother_cg_long_heights$Sample_age)
mother_cg_long_heights$SampleID_standard <- as.factor(mother_cg_long_heights$SampleID_standard)

# renaming 
levels(mother_cg_long_heights$treatment) <- list(Mother  = "Mother_Canopy_Height_cm", Child = "Canopy_Height_cm")

# WIDTHS: making one single column for each trait and a "treatment" column for mother/child
mother_cg_long_widths <- mother_cg %>%
  select(SampleDate,  Year_planted, Mother_mean_width, mean_width,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Mother_mean_width","mean_width"),
               names_to = "treatment", values_to = "Width")

# reclassing variables
mother_cg_long_widths$treatment <- as.factor(mother_cg_long_heights$treatment)
mother_cg_long_widths$Sample_age <- as.factor(mother_cg_long_heights$Sample_age)
mother_cg_long_widths$SampleID_standard <- as.factor(mother_cg_long_heights$SampleID_standard)

# renaming 
levels(mother_cg_long_widths$treatment) <- list(Mother  = "Mother_mean_width", Child = "mean_width")

# ELONG: making one single column for each trait and a "treatment" column for mother/child
mother_cg_long_elong <- mother_cg %>%
  select(SampleDate,  Year_planted, Mother_mean_stem_elong, mean_stem_elong,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Mother_mean_stem_elong","mean_stem_elong"),
               names_to = "treatment", values_to = "Stem_elongation")

# reclassing variables
mother_cg_long_elong$treatment <- as.factor(mother_cg_long_elong$treatment)
mother_cg_long_elong$Sample_age <- as.factor(mother_cg_long_elong$Sample_age)
mother_cg_long_elong$SampleID_standard <- as.factor(mother_cg_long_elong$SampleID_standard)

# renaming 
levels(mother_cg_long_elong$treatment) <- list(Mother  = "Mother_mean_stem_elong", Child = "mean_stem_elong")

# DIAM: making one single column for each trait and a "treatment" column for mother/child
mother_cg_long_diam <- mother_cg %>%
  select(SampleDate,  Year_planted, Cutting_diameter, Stem_diameter,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Cutting_diameter","Stem_diameter"),
               names_to = "treatment", values_to = "Stem_diam")

# reclassing variables
mother_cg_long_diam$treatment <- as.factor(mother_cg_long_diam$treatment)
mother_cg_long_diam$Sample_age <- as.factor(mother_cg_long_diam$Sample_age)
mother_cg_long_diam$SampleID_standard <- as.factor(mother_cg_long_diam$SampleID_standard)

# renaming 
levels(mother_cg_long_diam$treatment) <- list(Mother  = "Cutting_diameter", Child = "Stem_diameter")

# 4. DATA VISUALISATION ----
# N.B. to make figures like Gergana, create MEAN (one value) Mother height and MEAN child height (one value)
# and then plot one line per shrub 

# Heights
(plot_mother_compare_heights <- ggplot() +
   geom_point(aes(x = treatment, y= Height_cm, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.1, data = mother_cg_long_heights) +
   #geom_point(aes(x = treatment, y= Height_cm, colour = Site, group = SampleID_standard), size = 3, alpha = 0.7, data = mother_cg_sd_MA) +
   geom_smooth(aes(x = treatment, y= Height_cm, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.1, data = mother_cg_long_heights)) +
  #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
   xlab("\nTreatment") +
   scale_colour_viridis_d(begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(begin = 0.1, end = 0.85) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.text = element_text(size = 15, color = "black", face = "italic"),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black"))


# same as above but with only 2022 and mother data 
mother_long_height_22 <- mother_cg_long_heights %>% 
  filter(treatment == "Mother" | SampleYear == "2022")
# 

# Widths
(plot_mother_compare_widths <- ggplot(mother_cg_long_widths) +
   geom_point(aes(x = treatment, y= Width, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.5) +
  geom_smooth(aes(x = treatment, y= Width, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.2)) +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Width (cm)") +
  xlab("\nTreatment") +
  scale_colour_viridis_d(begin = 0.1, end = 0.85) +
  scale_fill_viridis_d(begin = 0.1, end = 0.85) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=12),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"))

# Stem elongation
(plot_mother_compare_elong <- ggplot(mother_cg_long_elong) +
    geom_point(aes(x = treatment, y= Stem_elongation, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = treatment, y= Stem_elongation, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.2)) +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Stem elongation (mm)") +
  xlab("\nTreatment") +
  scale_colour_viridis_d(begin = 0.1, end = 0.85) +
  scale_fill_viridis_d(begin = 0.1, end = 0.85) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=12),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"))

# Stem diameter
(plot_mother_compare_diam <- ggplot(mother_cg_long_diam) +
    geom_point(aes(x = treatment, y= Stem_diam, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = treatment, y= Stem_diam, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.2)) +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Stem diameter (mm)") +
  xlab("\nTreatment") +
  scale_colour_viridis_d(begin = 0.1, end = 0.85) +
  scale_fill_viridis_d(begin = 0.1, end = 0.85) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=12),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"))


# 5. DATA ANALYSIS -----

# 1. HEIGHTS: effect of mother heights on canopy heights in the CG
# N.B. gives me errror message when i run it with sample_age random effect
maternal_height <- lmer(Height_cm ~ treatment + (1|Species) + (1|SampleID_standard), data = mother_cg_long_heights)
summary(maternal_height)
tab_model(maternal_height)
plot(maternal_height)
qqnorm(resid(maternal_height))
qqline(resid(maternal_height)) 

# 2. WIDTHS: effect of mother widths on widths in the CG
# N.B. gives me error message when i run it with sample_age random effect
maternal_width <- lmer(Width~treatment + (1|SampleID_standard) + (1|Species), data = mother_cg_long_widths)
summary(maternal_width )
tab_model(maternal_width )
plot(maternal_width )
qqnorm(resid(maternal_width ))
qqline(resid(maternal_width )) 

# 3. ELONG: effect of mother elongation on elongation in the CG
# N.B. gives me error message when i run it with sample_age random effect
maternal_elong<- lmer(Stem_elongation ~ treatment + (1|SampleID_standard) + (1|Species), data = mother_cg_long_elong)
summary(maternal_elong)
tab_model(maternal_elong)
plot(maternal_elong)
qqnorm(resid(maternal_elong))
qqline(resid(maternal_elong)) 

# 4. DIAMETER: effect of mother diameters on diameters in the CG
maternal_diam <- lmer(Stem_diam~treatment + (1|SampleID_standard) + (1|Species), data = mother_cg_long_diam)
summary(maternal_diam)
tab_model(maternal_diam)
plot(maternal_diam)
qqnorm(resid(maternal_diam))
qqline(resid(maternal_diam)) 



