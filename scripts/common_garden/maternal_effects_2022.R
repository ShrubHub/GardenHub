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

# Datasets with mother heights (2013-2017)
Common_garden_2017 <- read_csv("data/common_garden_data_2017/Common_garden_2017.csv")
Common_garden_2017_sampling_date <- read_csv("scripts/common_garden/common_garden_data_2017/Common_garden_2017_sampling_date.csv")

# All 2022 data from the common garden
all_cg_2022 <- read_csv("data/common_garden_2022/wrangled_ALL_combined_Common_Garden_Measurements_2022.csv")

# All 2022 data from the source populations
all_source_pop_2022 <- read_csv("data/source_pops/all_source_pop_2022.csv")

# 3. DATA WRANGLING ----

# Match mother heights with heights in common garden

# Keeping only relevant columns in Common Garden dataset 2022
cg_heights_2022 <- all_cg_2022 %>%
  select(SampleID, Year_planted, Species, Site, Sample_Date,
         Month, Day, Year, Canopy_Height_cm, Length_1_mm, Length_2_mm,
         Length_3_mm)

unique(Common_garden_2017$Sample_location)

# Keeping only relevant columns in Common Garden dataset 2017
mother_heights_2017 <- Common_garden_2017 %>%
  select(Sample_ID, Match, Sample_location, Date_sampled,
         Date_propagated, Date_planted, Year_planted, Mother_height,
         Mother_CW_1, Mother_CW_2, Mother_LS, Mother_LL1, Mother_LL2, 
         Mother_LL3, Mother_SE1, Mother_SE2, Mother_SE3, Cutting_length, 
         Cutting_diameter) %>%
  rename("SampleID" = "Sample_ID")

unique(Common_garden_2017$Species)

# Joining datasets by Sample ID
mother_cg_2022 <- merge(mother_heights_2017, cg_heights_2022, by ="SampleID", "Match")

# Only keeping peak greeness measure (supposedly time in season when shrubs are tallest)
mother_cg_2022_july <- mother_cg_2022 %>%
  filter(Sample_Date == "23/07/2022")

str(mother_cg_2022_july)

# Making variables in right format
mother_cg_2022_july$Site <- as.factor(mother_cg_2022_july$Site)
mother_cg_2022_july$Species <- as.factor(mother_cg_2022_july$Species)
mother_cg_2022_july$Canopy_Height_cm <- as.numeric(mother_cg_2022_july$Canopy_Height_cm)
mother_cg_2022_july$Mother_height <- as.numeric(mother_cg_2022_july$Mother_height)

mother_cg_2022$Site <- as.factor(mother_cg_2022$Site)
mother_cg_2022$Species <- as.factor(mother_cg_2022$Species)
mother_cg_2022$Canopy_Height_cm <- as.numeric(mother_cg_2022$Canopy_Height_cm)
mother_cg_2022$Mother_height <- as.numeric(mother_cg_2022$Mother_height)

# Merging source pop dataset with mothers dataset
# Keeping relevant columns
source_pop_heights_2022 <-  all_source_pop_2022 %>% 
   select(Species, SampleDate, Site, Canopy_Height_cm)

mother_heights_2017_01 <- Common_garden_2017 %>%
   select(Sample_ID, Species, Sample_location, Date_sampled, Mother_height, Species) %>%
   mutate(Site = case_when(Sample_location %in% c("QHI", "Qikiqtaruk") ~ "Qikiqtaruk",
                           Sample_location %in% c("Pika Camp", "Kluane Plateau", "Printers Pass") ~ "Kluane")) %>%
filter(Species %in% c("Salix pulchra", "Salix arctica", "Salix richardsonii")) 


#source_pop_mother_compare <- merge(mother_heights_2017_01, source_pop_heights_2022, by ="Site","Species")


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

# Lmeri: effect of mother heights on canopy heights in the CG
model <- lmer(Canopy_Height_cm~Mother_height + (1|sample_age) + (1|SampleID) + (1|Species), data = mother_cg_2022)
summary(model)
tab_model(model)
plot(model)
qqnorm(resid(model))
qqline(resid(model)) 



