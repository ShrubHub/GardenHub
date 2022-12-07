#### MATERNAL EFFECTS SCRIPT 
### Data wrangling and visualisation script
### By Erica Zaja, created on 31/10/2022
## Last updated: 31/10/2022

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

## @Madi, ALSO note for code below: When i started this script i only wanted to look
# at mother heights vs child heights, but I think now we want to look at other
# mother variables too (height, width, stem diam)? So we should match mother and child based on SampleID.
# SO we can probably delete the chunk where I keep only the heights column

# Match mother heights with heights in common garden
# Keeping only relevant columns in Common Garden dataset 2022
cg_2022 <- all_cg_2022 %>%
  select(-...1)

# Making variables in right format
str(mother_data)
mother_data$Site <- as.factor(mother_data$Site)
mother_data$SampleID <- as.factor(mother_data$SampleID)
mother_data$SampleYear <- as.factor(mother_data$SampleYear)

# rename variables to make clear mother metrics before merge
mother_data_merge <-  mother_data %>% 
  rename("Mother_Canopy_Height_cm" = "Canopy_Height_cm", 
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
         "Mother_mother_mean_width" = "mean_width") %>% 
  select(-Match, -...1, -...2) %>%   # these columns are useless and will only cause anger and pain
  select(-SampleID, -site) # also drop this because it's weird inconsistent and will cause merging issues 
# note we don't have species but when we merge by standard sample ID these will appear 
str(mother_data_merge)
str(cg_2022)

mother_cg <- full_join(mother_data_merge, cg_2022, by = c("SampleID_standard" = "SampleID_standard", 
                                                          "Year_planted" = "Year_planted", 
                                                          "SampleDate" = "Sample_Date"))

# Making variables in right format


# Merging source pop dataset with mothers dataset
# Keeping relevant columns

# 4. DATA VISUALISATION ----
(plot_mother_compare <- ggplot(mother_cg_2022_july) +
   geom_point(aes(x = Mother_height, y= Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   geom_smooth(aes(x = Mother_height, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "lm")) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy Height (cm)") +
   xlab("\nMother Height (cm)") +
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
         axis.text.y = element_text(size = 15, colour = "black")))

# Comparing mother heights with source pop heights collected in 2022
(plot_mother_height_compare <- ggplot() +
      geom_boxplot(data = mother_heights_2017_01, aes(x = Site, y = Mother_height, fill = Site, group = Site), colour = "lightgrey", size = 0.5, alpha = 0.3) +
      geom_boxplot(data = source_pop_heights_2022, aes(x = Site, y = (Canopy_Height_cm), fill = Site, group = Site), colour = "black", size = 0.5, alpha = 0.8) +
      facet_wrap(~Species, scales = "free_y") +
      ylab("Canopy height (cm)") +
      xlab("") +
      scale_colour_viridis_d(begin = 0.85, end = 0.4) +
      scale_fill_viridis_d(begin = 0.85, end = 0.4) +
      # ylim(-0.01, 2.0) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 14),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
            axis.text.y = element_text(size = 12, colour = "black")))

str(mother_cg_2022_july)

# 5. DATA ANALYSIS -----

# 1. Lmer: effect of mother heights on canopy heights in the CG
maternal_height <- lmer(Canopy_Height_cm~Mother_Canopy_Height_cm + (1|sample_age) + (1|SampleID) + (1|Species), data = mother_cg)
summary(maternal_height)
tab_model(maternal_height)
plot(maternal_height)
qqnorm(resid(maternal_height))
qqline(resid(maternal_height)) 


# 2. Lmer: effect of mother widths on widths in the CG
maternal_width <- lmer(mean_width~Mother_width + (1|sample_age) + (1|SampleID) + (1|Species), data = mother_cg_2022)
summary(maternal_width )
tab_model(maternal_width )
plot(maternal_width )
qqnorm(resid(maternal_width ))
qqline(resid(maternal_width )) 

# 3. Lmer: effect of mother diameters on diameters in the CG
maternal_diam <- lmer(Stem_diameter~Mother_diam + (1|sample_age) + (1|SampleID) + (1|Species), data = mother_cg_2022)
summary(maternal_diam)
tab_model(maternal_diam)
plot(maternal_diam)
qqnorm(resid(maternal_diam))
qqline(resid(maternal_diam)) 



