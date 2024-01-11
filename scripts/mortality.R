# mortality and survival of shrubs in the garden 
# by Madelaine Anderson
# created 11 January 2024 

# libraries ----
library(dplyr)
library(ggplot2)
library(tidyverse)

# data ----
all_CG_source_growth <- read.csv("data/common_garden_data_2023/all_data_2023.csv") # 2023 data
# survival data (last updated 2021)
all_survival_2021 <- read_csv("data/common_garden_data_2021/all_survival_2021.csv")

# subset 2022 and 2023 separately 
survival_2022 <- all_CG_source_growth %>% 
  filter(Year == "2022") %>% 
  filter(population %in% c("Northern", "Southern")) %>% # only CG shrubs
  select(c(Year_planted, SampleID_standard, population, Species,
           Canopy_Height_cm)) %>% 
  mutate(Survived_2022 = case_when(!is.na(Canopy_Height_cm) ~ "1",
                                       TRUE ~ "0")) %>% 
  select(-Canopy_Height_cm) # check to make sure it works then drop height column for next merge 

survival_2023 <- all_CG_source_growth %>% 
  filter(Year == "2023") %>%  
  filter(population %in% c("Northern", "Southern")) %>% # only CG shrubs 
  select(c(Year_planted, SampleID_standard, population, Species,
          Canopy_Height_cm)) %>% 
  mutate(Survived_2023 = case_when(!is.na(Canopy_Height_cm) ~ "1",
                                   TRUE ~ "0")) %>% 
  select(-Canopy_Height_cm) # check to make sure it works then drop height column for next merge

# merge 2022 and 2023 before doing the whole thing to avoid issues down the road 

survival_2022_2023 <- full_join(survival_2023, survival_2022, 
                                by = c("SampleID_standard", "population", 
                                       "Species", "Year_planted"))

# change any 0's in 2022 to 1's if marked 1 in 2023 
survival_2022_2023_final <- survival_2022_2023 %>% 
  mutate(Survived_2022_final = case_when((Survived_2023 == "1") ~ "1",
                                   TRUE ~ "0")) %>% 
  select(-Survived_2022) %>% 
  filter(SampleID_standard != "HERSR4") %>% 
  filter(SampleID_standard != "H18SA29") 

# make same standard ID column in 2021 data 
# but first, sample IDs were changed in 2021 e.g. the letter F was removed 
all_growth_2021 <- read_csv("data/common_garden_data_2021/all_growth_2021.csv")
samples_2021 <- all_growth_2021 %>% 
  select(c(SampleID, SampleID_old))
# merge with all_survival_2021
# match old sample IDs with new (2021) versions
all_survival_2021_samples <- full_join(all_survival_2021, samples_2021, by = c("Sample_ID" = "SampleID_old")) %>% 
  select(-Sample_ID)
# now make standard ID column 
all_survival_2021_samples$SampleID_standard <- toupper(all_survival_2021_samples$SampleID) # make all uppercase characters 
all_survival_2021_samples$SampleID_standard<-gsub("-","",as.character(all_survival_2021_samples$SampleID_standard)) # remove "-"
all_survival_2021_samples$SampleID_standard<-gsub(" ","",as.character(all_survival_2021_samples$SampleID_standard)) # remove spaces " " 

all_survival <- full_join(all_survival_2021_samples, survival_2022_2023_final, 
                          by = c("SampleID_standard",  
                                 "Species", "Year_planted")) %>% 
  dplyr::rename("Survived_2022" = "Survived_2022_final")

# save 
write.csv(all_survival, "data/common_garden_data_2023/all_survival_2023.csv")

