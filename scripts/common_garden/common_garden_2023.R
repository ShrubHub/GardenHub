# Common garden data 2023 
# By Madelaine Anderson 
# 23 July 2023 

# libraries ----
library(tidyr)
library(ggplot2)

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
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2)) %>% 
  mutate(biovolume = (Canopy_Height_cm* Width_cm* Width_2_cm))

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
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")
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
  dplyr::select(-c(Sample_age, ...1)) %>% 
  mutate(Sample_age = Year - Year_planted)
  

