# Common garden data 2023 
# By Madelaine Anderson 
# last updated 06 September 2023 

# libraries ----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

# data 
data_2023 <- read.csv("data/common_garden_data_2023/Common_Garden_data_Jul2023_final.csv")

data_2023_wrangle <- data_2023 %>% 
  dplyr::rename("Sample_ID" = "Name", 
                "Sample_Date" = "Date..dd.mm.yyyy.", 
                "Canopy_Height_Aug" = "Height_Aug2022..cm.",
                 "Canopy_Height_cm" = "Height..cm.",  
                "Width_cm" = "Width..cm.", 
                "Width_2_cm" = "Width.2..cm.", 
                "Stem_Elongation_1_mm" = "Stem.Elongation.1..mm.", 
                "Stem_Elongation_2_mm" = "Stem.Elongation.2..mm.", 
                'Stem_Elongation_3_mm' = "Stem.Elongation.3..mm.",
                "Length_1_mm" = "Length.1..mm.", 
                "Length_2_mm" = "Length.2..mm.", 
                "Length_3_mm" = "Length.3..mm.", 
                "Stem_diameter" = "Stem.diameter", 
                "July_2023_notes" = "X11.July.2023.notes") %>% 
  dplyr::select(-c(Canopy_Height_Aug, Sample_Date))

str(data_2023_wrangle)
# reclass variables as numeric
data_2023_wrangle$Canopy_Height_cm <- as.numeric(data_2023_wrangle$Canopy_Height_cm)
data_2023_wrangle$Stem_Elongation_1_mm <- as.numeric(data_2023_wrangle$Stem_Elongation_1_mm)
data_2023_wrangle$Stem_Elongation_2_mm <- as.numeric(data_2023_wrangle$Stem_Elongation_2_mm)
data_2023_wrangle$Stem_Elongation_3_mm <- as.numeric(data_2023_wrangle$Stem_Elongation_3_mm)
data_2023_wrangle$Length_1_mm <- as.numeric(data_2023_wrangle$Length_1_mm)
data_2023_wrangle$Length_2_mm <- as.numeric(data_2023_wrangle$Length_2_mm)
data_2023_wrangle$Length_3_mm <- as.numeric(data_2023_wrangle$Length_3_mm)
data_2023_wrangle$Stem_diameter <- as.numeric(data_2023_wrangle$Stem_diameter)

# make mean leaf length, mean stem elongation, and biovolume columns 
data_2023_wrangle <- data_2023_wrangle %>% 
  dplyr::mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2)) %>% 
  dplyr::mutate(biovolume = (Canopy_Height_cm* Width_cm* Width_2_cm))

# make standard sample ID column to avoid issue of dashes, spaces, etc. 
data_2023_wrangle$SampleID_standard <- toupper(data_2023_wrangle$Sample_ID) # make all uppercase characters 
data_2023_wrangle$SampleID_standard<-gsub("-","",as.character(data_2023_wrangle$SampleID_standard)) # remove "-"
data_2023_wrangle$SampleID_standard<-gsub("_","",as.character(data_2023_wrangle$SampleID_standard)) # remove "-"
data_2023_wrangle$SampleID_standard<-gsub(" ","",as.character(data_2023_wrangle$SampleID_standard)) # remove spaces " " 

# date column is reading wrong, manually make day month year columns bc measurements were collected 23 July 2023
# also add site column
data_2023_wrangle$Year <- "2023"
data_2023_wrangle$Month <- "07"
data_2023_wrangle$Day <- "23"
data_2023_wrangle$Site <- "Common_garden"
data_2023_wrangle$Year <- as.numeric(data_2023_wrangle$Year)
data_2023_wrangle$Month <- as.numeric(data_2023_wrangle$Month)
data_2023_wrangle$Day <- as.numeric(data_2023_wrangle$Day)

# try to merge with complete dataset from 2022 
# wish me luck 
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv")
# to get year planted and species, extract from 2022 dataset 
spp_year <- all_CG_source_growth %>% 
  dplyr::select(c(Species, Year_planted, SampleID_standard, population, Cutting_diameter, Cutting_length, Mother_LS)) %>% 
  distinct()

# merge spp_year with 2023 data 
spp_2023_merge <- full_join(spp_year, data_2023_wrangle, by = c("SampleID_standard"))
data_2023_wrangle_to_merge <- spp_2023_merge %>% 
  dplyr::filter(Year == "2023")

all_data_merge <- full_join(all_CG_source_growth, data_2023_wrangle_to_merge, 
                           by = c("Bed", "Site",
                                  "Year_planted", "Species",
                                  "Canopy_Height_cm",
                                  "Width_cm", "Width_2_cm", 
                                  "Stem_Elongation_1_mm", "Stem_Elongation_2_mm", "Stem_Elongation_3_mm", 
                                  "Length_1_mm", "Length_2_mm", "Length_3_mm",
                                  "Stem_diameter", "population",
                                  "mean_stem_elong", 
                                  "mean_leaf_length", 
                                  "biovolume", "mean_width", 
                                  "SampleID_standard", 
                                  "Year", "Month", "Day", 
                                  "Cutting_diameter", "Cutting_length", "Mother_LS")) 

all_data_wrangle <- all_data_merge %>% 
  dplyr::select(-Sample_age) %>% 
  mutate(Sample_age = Year - Year_planted)

# import leaf lengths for S. arctica collected up Kluane Plateau 

S_arc_leaf_lengths <- read.csv("data/source_pops/S_arctica_leaf_lengths_Jul2023.csv")
S_arc_leaf_lengths <- S_arc_leaf_lengths %>% 
  mutate("Site" = "Kluane") %>% 
  mutate("population" = "source_south") %>% 
  mutate("Species" = "Salix arctica") %>% 
  mutate(mean_leaf_length = ((Leaf_length_1_mm + Leaf_length_2_mm + Leaf_length_3_mm)/3)) %>% 
  select(-c(Date, GPS_name)) %>% 
  dplyr::rename("Length_1_mm" = "Leaf_length_1_mm", 
                "Length_2_mm" = "Leaf_length_2_mm", 
                "Length_3_mm" = "Leaf_length_3_mm", 
                "Year" = "year", 
                "Month" = "month", 
                "Day" = "day", 
                "SampleID_standard" = "Sample")

data_merge <- full_join(S_arc_leaf_lengths, all_data_wrangle, 
                            by = c("Site", "Species",
                                   "Length_1_mm", "Length_2_mm", "Length_3_mm",
                                  "population", "mean_leaf_length", 
                                   "SampleID_standard", 
                                   "Year", "Month", "Day")) 

# load source pop trait data from 2023
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") # most traits

trait_2023 <- read.csv("data/source_pops/willow_2021_2022_2023_sla.csv")

trait_2023$year <-  format(as.Date(trait_2023$date_sampled, format="%Y-%m-%d"),"%Y")
trait_2023$month <-  format(as.Date(trait_2023$date_sampled, format="%Y-%m-%d"),"%m")
trait_2023$DOY <-  yday(as.POSIXct(trait_2023$date_sampled, format = "%Y-%m-%d"))
# reclass for merge 
trait_2023$DOY <- as.character(trait_2023$DOY)
trait_2023$year <- as.integer(trait_2023$year)
trait_2023$month <- as.integer(trait_2023$month)

trait_2023 <- trait_2023 %>% 
  dplyr::filter(year == "2023") %>% # previous data is already merged 
  dplyr::filter(leaf_status == "green") %>%  # only keep healthy, green leaves
  mutate(Site = recode(Site, "Kluane Plateau" = 'Kluane', "Qikiqtaruk" = 'Qikiqtaruk')) %>% 
  mutate(total_leaf_dry_mass_mg = total_leaf_dry_mass_g*1000) %>% # convert leaf mass per area to mg instead of g 
  mutate(LA = LA*100) %>%  #convert leaf area to mm2 instead of cm2
  mutate(LDMC_g_g = LDMC/1000) %>% # covert LDMC from mg g-1 to g g-1
  dplyr::select(-c(X, LDMC, rehydrated_leaf_sample_remarks, sample_remarks, dried_leaf_sample_remarks)) %>% 
  mutate(population = case_when(Site %in% c("Qikiqtaruk") ~ "Northern",
                                 Site == "Kluane" ~ "Southern")) %>% 
  dplyr::filter(Species %in% c("Salix richardsonii", "Salix pulchra", "Salix arctica"))

all_source_traits_2023 <- full_join(trait_2023, all_CG_source_traits, by = c("Site" = "Site", 
                                                                             "plant_tag_id" = "plant_tag_id", 
                                                                             "site_id" = "site_id",
                                                                             "sample_id" = "sample_id", 
                                                                             "month" = "month",
                                                                             "date_sampled" = "date_sampled",
                                                                             "population" = "population",
                                                                         "Species" = "Species", 
                                                                         "leaf_mass_per_area_g_m2" = "leaf_mass_per_area_g_m2",
                                                                         "actual_leaf_dry_matter_content_perc" = "actual_leaf_dry_matter_content_perc",
                                                                         "equivalent_water_thickness_cm" = "equivalent_water_thickness_cm",
                                                                         "total_rehydrated_leaf_mass_g" = "total_rehydrated_leaf_mass_g",
                                                                         "DOY" = "DOY",
                                                                         "year" = "year",
                                                                         "LA" = "LA", 
                                                                         "SLA" = "SLA", 
                                                                         "LDMC_g_g" = "LDMC_g_g", 
                                                                         "total_leaf_dry_mass_mg" = "total_leaf_dry_mass_mg", 
                                                                         "total_leaf_dry_mass_g" = "total_leaf_dry_mass_g",
                                                                         "leaf_fresh_mass_g" = "leaf_fresh_mass_g"))

# save as csv 
write.csv(all_source_traits_2023, "data/all_CG_source_traits_2023.csv")

# quick figs ----
# load data 
all_data_2023 <- read.csv("data/common_garden_data_2023/all_data_2023.csv")

cg_2023 <- all_data_2023 %>% 
  dplyr::filter(Site == "Common_garden") %>% 
  dplyr::filter(population %in% c("Northern", "Southern"))

(plot_canopy_2023 <- ggplot(cg_2023) +
    geom_smooth(aes(x = Sample_age, y = Canopy_Height_cm, colour = population, fill = population, group = population, method = "glm")) +
    geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = population, group = population), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Canopy Height (cm)") +
    xlab("\nAge (years)") +
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

(plot_canopy_2023 <- ggplot(cg_2023) +
    geom_line(aes(x = Sample_age, y = Canopy_Height_cm, colour = population, fill = population) +
    geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = SampleID_standard), size = 1.5, alpha = 0.5) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Canopy Height (cm)") +
    xlab("\nAge (years)") +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 15, color = "black", face = "italic"),
          legend.position = "none",
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))
