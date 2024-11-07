#### PHENOCAMS: common garden, Kluane and QHI
#### Script by Erica Zaja, created 14/12/22
### Last updated: 19/10/23 by Madi 

#  LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)
library(gridExtra)

# 2023 ----
# data ----
# import source data (QHI, KP)
source_pheno_2023 <- read.csv("data/phenology/Phenocams_2023.csv")

# clean up data sheet 
source_pheno_2023$Species[source_pheno_2023$Species %in% c('ARC', 'Arctica')] <- 'Salix arctica'
source_pheno_2023$Species[source_pheno_2023$Species %in% c('RICH', 'Richarsonii', 'Richardsonii', 'richardsonii')] <- 'Salix richardsonii'
source_pheno_2023$Species[source_pheno_2023$Species %in% c('PUL', 'Pulchra', 'pulchra')] <- 'Salix pulchra'

source_pheno_2023_working <- source_pheno_2023 %>% 
  dplyr::select(-c(Observer, NOTES, Plants_first_visible_through_snow, 
                   X50_snow_coverge_end_of_season, 
                   X50_Leaves_Green, X100_Leaves_Green, X50_Leaves_Yellow, 
                   First_leaf_bud_burst, 
                   First_yellow_leaf, X100_Leaves_Yellow, 
                   X100_snow_coverage_end_of_season, 
                   Plants_first_visible_through_snow)) %>% 
  dplyr::filter(Site %in% c("Kluane", "QHI")) %>% 
  dplyr::filter(Species %in% c("Salix richardsonii", "Salix pulchra", 'Salix arctica')) %>% 
  dplyr::rename("PhenocamID" = "PLOT",
                "First_bud_burst" = "First_Leaf_Bud_Burst", 
                "First_leaf_yellow" = "First_Yellowing_of_Leaves", 
                "All_leaves_yellow" = "Last_Leaf_Turns_Yellow", 
                "First_snow_return_day_end_of_season" = "First_snow_return_day._end_of_season")

# make date columns dates 
source_pheno_2023_working$Snow_Free_Melt_Date <- as.POSIXct(source_pheno_2023_working$Snow_Free_Melt_Date, format = "%d/%m/%Y")
source_pheno_2023_working$First_snow_free_day <- as.POSIXct(source_pheno_2023_working$First_snow_free_day, format = "%d/%m/%Y")
source_pheno_2023_working$First_snow_return_day_end_of_season <- as.POSIXct(source_pheno_2023_working$First_snow_return_day_end_of_season, format = "%d/%m/%Y")
source_pheno_2023_working$First_bud_burst <- as.POSIXct(source_pheno_2023_working$First_bud_burst, format = "%d/%m/%Y")
source_pheno_2023_working$First_leaf_yellow <- as.POSIXct(source_pheno_2023_working$First_leaf_yellow, format = "%d/%m/%Y")
source_pheno_2023_working$All_leaves_yellow <- as.POSIXct(source_pheno_2023_working$All_leaves_yellow, format = "%d/%m/%Y")
# make DOY columns
source_pheno_2023_working$Snow_melt_DOY <-  lubridate::yday(as.POSIXct(source_pheno_2023_working$Snow_Free_Melt_Date, format = "%d-%m-%Y"))
source_pheno_2023_working$All_snow_free_DOY <-  lubridate::yday(as.POSIXct(source_pheno_2023_working$First_snow_free_day, format = "%d-%m-%Y"))
source_pheno_2023_working$Snow_return_EoS_DOY <-  lubridate::yday(as.POSIXct(source_pheno_2023_working$First_snow_return_day_end_of_season, format = "%d-%m-%Y"))
source_pheno_2023_working$First_bud_burst_DOY <-  lubridate::yday(as.POSIXct(source_pheno_2023_working$First_bud_burst, format = "%d-%m-%Y"))
source_pheno_2023_working$First_leaf_yellow_DOY <-  lubridate::yday(as.POSIXct(source_pheno_2023_working$First_leaf_yellow, format = "%d-%m-%Y"))
source_pheno_2023_working$All_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(source_pheno_2023_working$All_leaves_yellow, format = "%d-%m-%Y"))

# make population column 
source_pheno_2023_merge <- source_pheno_2023_working %>% 
  mutate(population = case_when(startsWith(as.character(Site), "Q") ~ "Northern Source",
                                TRUE ~ "Southern Source"))

# adding QHI S. actica data ----
qhi <- read.csv("data/phenology/QHI_phenology_plots/qiki_phen_2023.csv")

qhi$Spp[qhi$Spp == 'SALARC'] <- 'Salix arctica'

str(qhi)
unique(qhi$Spp) # only want SALARC
unique(all_phenocam_data_salix$population)

qhi_arctica <- qhi %>% 
  dplyr::filter(Spp == "Salix arctica") %>% 
  dplyr::filter(Year >= "2014") %>% 
  mutate("population" = "Northern Source") %>% 
  dplyr::rename("Species" = "Spp", 
         "All_snow_free_DOY" = "P1",
         "First_bud_burst_DOY" = "P2", 
         "First_leaf_yellow_DOY" = "P5", 
         "All_leaves_yellow_DOY" = "P6",
         "PhenocamID" = "Plot.ID") %>% 
  select(-c(P3, P4, P7))

# merge with phenocam data 
all_source_phenocam_merge <- full_join(qhi_arctica, source_pheno_2023_merge, 
                                      by = c("Year", "population", "PhenocamID", 
                                             "Species", 
                                             "All_snow_free_DOY",
                                             "First_bud_burst_DOY", 
                                            "First_leaf_yellow_DOY", 
                                            "All_leaves_yellow_DOY"))
# adding growing season column 
# adding snow free days column 
all_source_phenocam_merge <- all_source_phenocam_merge %>% 
  mutate(growing_season = First_leaf_yellow_DOY - First_bud_burst_DOY) %>% 
  mutate(snow_free_days = Snow_return_EoS_DOY - All_snow_free_DOY)
 
# saving updated data frames 
write.csv(all_source_phenocam_merge, "data/phenology/all_source_pheno_2023.csv")

# Common garden ---- 
cg_2023 <- read.csv("data/phenology/Common_garden_phenocams_2023.csv")

cg_2023$SampleID_standard <- toupper(cg_2023$Shrub) # make all uppercase characters 
cg_2023$SampleID_standard <- gsub("-","",as.character(cg_2023$SampleID_standard)) # remove "-"
cg_2023$SampleID_standard <- gsub("_","",as.character(cg_2023$SampleID_standard)) # remove "-"
cg_2023$SampleID_standard <- gsub(" ","",as.character(cg_2023$SampleID_standard)) 

cg_2023_working <- cg_2023 %>% 
  dplyr::select(-c(Observer, Notes, Plants_first_visible_through_snow, 
                   snow_coverge_end_of_season_50, Shrub)) %>% 
  dplyr::rename("PhenocamID" = "Plot",
                "First_bud_burst" = "First_leaf_bud_burst", 
                "First_leaf_yellow" = "First_yellowing_leaves", 
                "All_leaves_yellow" = "Last_leaf_starts_turning_yellow", 
                "First_snow_return_day_end_of_season" = "First_snow_return_day_end_of_season") %>% 
  mutate(Species = ifelse(grepl("SA", cg_2023$SampleID_standard), "Salix arctica",
                          ifelse(grepl("SR", cg_2023$SampleID_standard), "Salix richardsonii", 
                                 ifelse(grepl("SP", cg_2023$SampleID_standard), "Salix pulchra", NA)))) %>%  # species col
  mutate(population_1 = ifelse(grepl("HE" , cg_2023$SampleID_standard), "QHI", # working population col 1
                               ifelse(grepl("KP", cg_2023$SampleID_standard), "Kluane", 
                                      ifelse(grepl("PC", cg_2023$SampleID_standard), "Kluane", NA)))) %>% 
  mutate(population_2 = ifelse(grepl("H" , cg_2023$SampleID_standard), "QHI", # working population col 1
                               ifelse(grepl("K", cg_2023$SampleID_standard), "Kluane", 
                                      ifelse(grepl("PP", cg_2023$SampleID_standard), "Kluane", NA)))) %>%
  mutate(population = case_when(population_1 == "QHI" | population_2 == "QHI" ~ "Northern Garden", # final population col 
                                population_1  == "Kluane" | population_2 == "Kluane" ~ "Southern Garden")) %>%
  select(-population_1, -population_2) 

# make date columns dates 
cg_2023_working$Snow_Free_Melt_Date <- as.POSIXct(cg_2023_working$Snow_Free_Melt_Date, format = "%d/%m/%Y")
cg_2023_working$First_snow_free_day <- as.POSIXct(cg_2023_working$First_snow_free_day, format = "%d/%m/%Y")
cg_2023_working$First_snow_return_day_end_of_season <- as.POSIXct(cg_2023_working$First_snow_return_day_end_of_season, format = "%d/%m/%Y")
cg_2023_working$First_bud_burst <- as.POSIXct(cg_2023_working$First_bud_burst, format = "%d/%m/%Y")
cg_2023_working$First_leaf_yellow <- as.POSIXct(cg_2023_working$First_leaf_yellow, format = "%d/%m/%Y")
cg_2023_working$All_leaves_yellow <- as.POSIXct(cg_2023_working$All_leaves_yellow, format = "%d/%m/%Y")
# make DOY columns
cg_2023_working$Snow_melt_DOY <-  lubridate::yday(as.POSIXct(cg_2023_working$Snow_Free_Melt_Date, format = "%d-%m-%Y"))
cg_2023_working$All_snow_free_DOY <-  lubridate::yday(as.POSIXct(cg_2023_working$First_snow_free_day, format = "%d-%m-%Y"))
cg_2023_working$Snow_return_EoS_DOY <-  lubridate::yday(as.POSIXct(cg_2023_working$First_snow_return_day_end_of_season, format = "%d-%m-%Y"))
cg_2023_working$First_bud_burst_DOY <-  lubridate::yday(as.POSIXct(cg_2023_working$First_bud_burst, format = "%d-%m-%Y"))
cg_2023_working$First_leaf_yellow_DOY <-  lubridate::yday(as.POSIXct(cg_2023_working$First_leaf_yellow, format = "%d-%m-%Y"))
cg_2023_working$All_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(cg_2023_working$All_leaves_yellow, format = "%d-%m-%Y"))

cg_2023_merge <-  cg_2023_working %>% 
  mutate(growing_season = First_leaf_yellow_DOY - First_bud_burst_DOY) %>% 
  mutate(snow_free_days = Snow_return_EoS_DOY - All_snow_free_DOY)

# save CG phenology 
write.csv(cg_2023_merge, "data/phenology/cg_phenology_2023.csv")

# merge all phenology 2023 ----
cg_2023_merge <- read.csv("data/phenology/cg_phenology_2023.csv")
all_source_phenocam_merge <- read.csv("data/phenology/all_source_pheno_2023.csv")

all_pheno_2023 <- full_join(cg_2023_merge, all_source_phenocam_merge, 
                            by = c("PhenocamID", "Year", "Species", 
                                   "population", 
                                   "All_snow_free_DOY", 
                                   "First_bud_burst_DOY", 
                                   "First_leaf_yellow_DOY",
                                   'All_leaves_yellow_DOY',
                                   "Snow_Free_Melt_Date",
                                   'First_snow_free_day',
                                   'First_snow_return_day_end_of_season',
                                   'First_bud_burst',
                                   'First_leaf_yellow',
                                   'All_leaves_yellow',
                                   'Snow_melt_DOY', 
                                   'Snow_return_EoS_DOY', 
                                   'growing_season',
                                   'snow_free_days'))

# save ALL phenology 2023
write.csv(all_pheno_2023, "data/phenology/all_phenology_2023.csv")

# OLD ----
# data ----
KP_phenocams_2021 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2021.csv")
KP_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2022.csv")
CG_phenocams_individual_2021_2022 <- read_csv("data/phenology/phenocam_pics/CG_phenocams_individual_2021_2022.csv")
QHI_phenocams_all_final <- read_excel("data/phenology/phenocam_pics/QHI_phenocams_all_final.xlsx")
# add the generic sheets too (not the individual observations)- but might not need them
# CG_phenocams_2021_all, CG_phenocams_2022_all

# data manip -----

# to make DOY column: 
#df$DOY <-  lubridate::yday(as.POSIXct(df$Date, format = "%Y-%m-%d"))

# KP ------
KP_phenocams_2022 <-KP_phenocams_2022[1:9,] # removing loads of NAs
KP_phenocams_2021 <- KP_phenocams_2021[1:9,]

# renaming columns 
# 2022 KP data
KP_phenocams_2022_wrangle <- KP_phenocams_2022 %>%
  rename("Plot" = "PLOT", "Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "First_greening" = "First signs of greening",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "First_leaf_bud_burst" = "First leaf bud burst",
         "Half_leaves_green" ="50% Leaves Green",
         "All_leaves_green" = "100% Leaves Green",
         "First_leaf_yellow"= "First leaf yellow",
         "Half_leaves_yellow" = "50% Leaves Yellow",
         "All_leaves_yellow" = "100% Leaves Yellow",
         "Salix_pulchra_bud_burst" = "Salix pulchra First Leaf Bud Burst",
         "Salix_pulchra_first_yellow"= "Salix pulchra First Yellowing of Leaves",
         "Salix_pulchra_last_yellow" = "Salix pulchra Last Leaf Turns Yellow",
         "Salix_rich_bud_burst" = "Salix richardsonii First Leaf Bud Burst",
         "Salix_rich_first_yellow" = "Salix richardsonii First Yellowing of Leaves",
         "Salix_rich_last_yellow" = "Salix richardsonii Last Leaf Turns Yellow") %>%
  dplyr::select(Plot, Year, Viewshed, NOTES, Snow_melt, All_snow_free, First_greening, Snow_return_EoS,
                Half_snow_cover_EoS, Full_snow_cover_EoS, First_leaf_bud_burst, 
                Half_leaves_green, All_leaves_green, First_leaf_yellow, Half_leaves_yellow, 
                All_leaves_yellow, Salix_pulchra_bud_burst, Salix_pulchra_first_yellow, 
                Salix_pulchra_last_yellow, Salix_rich_bud_burst, Salix_rich_first_yellow, 
                Salix_rich_last_yellow)

# 2021 KP data
KP_phenocams_2021_wrangle <- KP_phenocams_2021 %>%
  rename("Plot" = "PLOT", "Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "First_leaf_bud_burst" = "First leaf bud burst",
         "Half_leaves_green" ="50% Leaves Green",
         "All_leaves_green" = "100% Leaves Green",
         "First_leaf_yellow"= "First leaf yellow",
         "Half_leaves_yellow" = "50% Leaves Yellow",
         "All_leaves_yellow" = "100% Leaves Yellow",
         "Salix_pulchra_bud_burst" = "Salix pulchra First Leaf Bud Burst",
         "Salix_pulchra_first_yellow"= "Salix pulchra First Yellowing of Leaves",
         "Salix_pulchra_last_yellow" = "Salix pulchra Last Leaf Turns Yellow",
         "Salix_rich_bud_burst" = "Salix richardsonii First Leaf Bud Burst",
         "Salix_rich_first_yellow" = "Salix richardsonii First Yellowing of Leaves",
         "Salix_rich_last_yellow" = "Salix richardsonii Last Leaf Turns Yellow") %>%
  dplyr::select(Plot, Year, Viewshed, NOTES, Snow_melt, All_snow_free, Snow_return_EoS,
                Half_snow_cover_EoS, Full_snow_cover_EoS, First_leaf_bud_burst, 
                Half_leaves_green, All_leaves_green, First_leaf_yellow, Half_leaves_yellow, 
                All_leaves_yellow, Salix_pulchra_bud_burst, Salix_pulchra_first_yellow, 
                Salix_pulchra_last_yellow, Salix_rich_bud_burst, Salix_rich_first_yellow, 
                Salix_rich_last_yellow)

# merge 2021 and 2022 kp datasets
KP_phenocams_2021_2022 <- bind_rows(KP_phenocams_2021_wrangle, KP_phenocams_2022_wrangle)

# write csv
write.csv(KP_phenocams_2021_2022, "data/phenology/phenocam_pics/working_data/KP_phenocams_2021_2022.csv")
# dates are A MESS. Easier/quicker to edit manually.

# load manually edited data
KP_phenocams_2021_2022_manual <- read_excel("data/phenology/phenocam_pics/working_data/KP_phenocams_2021_2022_manual_final.xlsx")
str(KP_phenocams_2021_2022_manual)

KP_phenocams_2021_2022_manual$Snow_melt <- as.POSIXct(KP_phenocams_2021_2022_manual$Snow_melt, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$All_snow_free <- as.POSIXct(KP_phenocams_2021_2022_manual$All_snow_free, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Snow_return_EoS <- as.POSIXct(KP_phenocams_2021_2022_manual$Snow_return_EoS, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Full_snow_cover_EoS <- as.POSIXct(KP_phenocams_2021_2022_manual$Full_snow_cover_EoS, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$First_leaf_bud_burst <- as.POSIXct(KP_phenocams_2021_2022_manual$First_leaf_bud_burst, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Half_leaves_green <- as.POSIXct(KP_phenocams_2021_2022_manual$Half_leaves_green, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$All_leaves_green <- as.POSIXct(KP_phenocams_2021_2022_manual$All_leaves_green, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$First_leaf_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$First_leaf_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Half_leaves_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Half_leaves_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$All_leaves_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$All_leaves_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_bud_burst <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_bud_burst, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_first_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_first_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_last_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_last_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$First_greening <- as.POSIXct(KP_phenocams_2021_2022_manual$First_greening, format = "%d/%m/%Y")

# add DOY column 
KP_phenocams_2021_2022_manual$Snow_melt_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Snow_melt, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$All_snow_free_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$All_snow_free, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Snow_return_EoS_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Snow_return_EoS, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$First_leaf_bud_burst_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$First_leaf_bud_burst, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Half_leaves_green_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Half_leaves_green, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$All_leaves_green_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$All_leaves_green, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$First_leaf_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$First_leaf_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Half_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Half_leaves_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$All_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$All_leaves_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salixbud_burst_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_bud_burst, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_first_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_first_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_last_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_last_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$First_greening_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$First_greening, format = "%Y-%m-%d"))

view(KP_phenocams_2021_2022_manual)

write.csv(KP_phenocams_2021_2022_manual, "data/phenology/phenocam_pics/KP_phenocams_2021_2022_wrangle.csv")

# Divide salix pulchra and richardsonii
KP_phenocams_2021_2022_pulchra <- KP_phenocams_2021_2022_manual %>%
  filter(Species == "Salix pulchra")

# range of green up and senescence DOY
range(KP_phenocams_2021_2022_pulchra$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 170- 175
range(KP_phenocams_2021_2022_pulchra$First_leaf_yellow_DOY, na.rm=TRUE)
# 170- 175

KP_phenocams_2021_2022_rich <- KP_phenocams_2021_2022_manual %>%
  filter(Species == "Salix richardsonii")

# range of green up and senescence DOY
range(KP_phenocams_2021_2022_rich$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 168 - 177
range(KP_phenocams_2021_2022_rich$First_leaf_yellow_DOY, na.rm=TRUE)
#199- 227

# Mean 90% snow melt DOY
mean(KP_phenocams_2021_2022_wrangle$Snow_melt_DOY, na.rm=TRUE)
# 170.375
range(KP_phenocams_2021_2022_wrangle$Snow_melt_DOY, na.rm=TRUE)
# 166 177

# Mean snow return DOY
mean(KP_phenocams_2021_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE)
# 240
range(KP_phenocams_2021_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE)
# 240 240

# Growing season length KP
# 240 - 170 = 70 days

# QHI ----

# all QHI phenocams (2016-2022). NB only yellowing in 2022
QHI_phenocams_2022_wrangle <- QHI_phenocams_all_final %>%
  filter(!Species == "Salix reticulata") %>%
  dplyr::select(-Observer, -NOTES)

# making names consistent
QHI_phenocams_2022_wrangle$Species<- as.factor(QHI_phenocams_2022_wrangle$Species)
str(QHI_phenocams_2022_wrangle)

# reclass dates
QHI_phenocams_2022_wrangle$plant_first_vis <- as.POSIXct(QHI_phenocams_2022_wrangle$plant_first_vis, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Snow_melt <- as.POSIXct(QHI_phenocams_2022_wrangle$Snow_melt, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$All_snow_free <- as.POSIXct(QHI_phenocams_2022_wrangle$All_snow_free, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Snow_return_EoS <- as.POSIXct(QHI_phenocams_2022_wrangle$Snow_return_EoS, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Half_snow_cover_EoS <- as.POSIXct(QHI_phenocams_2022_wrangle$Half_snow_cover_EoS, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Full_snow_cover_EoS <- as.POSIXct(QHI_phenocams_2022_wrangle$Full_snow_cover_EoS, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$First_leaf_bud_burst <- as.POSIXct(QHI_phenocams_2022_wrangle$First_leaf_bud_burst, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Half_leaves_green <- as.POSIXct(QHI_phenocams_2022_wrangle$Half_leaves_green, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$All_leaves_green <- as.POSIXct(QHI_phenocams_2022_wrangle$All_leaves_green, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$First_leaf_yellow <- as.POSIXct(QHI_phenocams_2022_wrangle$First_leaf_yellow, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Half_leaves_yellow <- as.POSIXct(QHI_phenocams_2022_wrangle$Half_leaves_yellow, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$All_leaves_yellow <- as.POSIXct(QHI_phenocams_2022_wrangle$All_leaves_yellow, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Salix_bud_burst <- as.POSIXct(QHI_phenocams_2022_wrangle$Salix_bud_burst, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Salix_first_yellow <- as.POSIXct(QHI_phenocams_2022_wrangle$Salix_first_yellow, format = "%d/%m/%Y")
QHI_phenocams_2022_wrangle$Salix_last_yellow <- as.POSIXct(QHI_phenocams_2022_wrangle$Salix_last_yellow, format = "%d/%m/%Y")

# add DOY column 
QHI_phenocams_2022_wrangle$Snow_melt_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Snow_melt, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$All_snow_free_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$All_snow_free, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Snow_return_EoS_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Snow_return_EoS, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Half_snow_cover_EoS_DOY <- lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Half_snow_cover_EoS, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Full_snow_cover_EoS_DOY <- lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Full_snow_cover_EoS, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$First_leaf_bud_burst_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$First_leaf_bud_burst, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Half_leaves_green_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Half_leaves_green, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$All_leaves_green_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$All_leaves_green, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$First_leaf_yellow_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$First_leaf_yellow, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Half_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Half_leaves_yellow, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$All_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$All_leaves_yellow, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Salix_first_bud_burst_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Salix_bud_burst, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Salix_first_yellow_DOY <-  lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Salix_first_yellow, format = "%Y-%m-%d"))
QHI_phenocams_2022_wrangle$Salix_last_yellow_DOY <- lubridate::yday(as.POSIXct(QHI_phenocams_2022_wrangle$Salix_last_yellow, format = "%Y-%m-%d"))

write.csv(QHI_phenocams_2022_wrangle, "data/phenology/phenocam_pics/QHI_phenocams_2022_wrangle.csv")

# dividing species
QHI_salarc_pheno <- QHI_phenocams_2022_wrangle %>%
  filter(Species == "Salix arctica")

# range of green up and senescence DOY
range(QHI_salarc_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 157 - 167
range(QHI_salarc_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# 197 - 216

QHI_salpul_pheno <- QHI_phenocams_2022_wrangle %>%
  filter(Species == "Salix pulchra")

# range of green up and senescence DOY
range(QHI_salpul_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 158- 168
range(QHI_salpul_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# 202 - 214

QHI_salric_pheno <- QHI_phenocams_2022_wrangle %>%
  filter(Species == "Salix richardsonii")
# Nb Salix richardsonii only has first yellowing from 2022

# range of green up and senescence DOY
range(QHI_salric_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 162- 175
range(QHI_salric_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# 205 - 219

# Mean DOY of 90% snow melt
mean(QHI_phenocams_2022_wrangle$Snow_melt_DOY, na.rm=TRUE) 
# 155.4
range(QHI_phenocams_2022_wrangle$Snow_melt_DOY, na.rm=TRUE)

# Mean DOY of snow return
mean(QHI_phenocams_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE) 
# 243.5
range(QHI_phenocams_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE)

# Growing season length on QHI 
# 243 - 155 = 88 days

# CG -----
CG_phenocams_individual_2021_2022 <- CG_phenocams_individual_2021_2022[,1:18] # removing extra blank cols from csv

# rename columns
CG_phenocams_individual_2021_2022_wrangle <- CG_phenocams_individual_2021_2022 %>%
  rename("Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "First_leaf_bud_burst" = "First leaf bud burst",
         "First_leaf_yellow" = "First yellowing of leaves",
         "Last_leaves_yellow" = "Last leaf starts turning yellow") %>%
  dplyr::select(-Shrub, -Observer,-Notes,-"Certainty Index", -"Certainty Index (1-5)...16",
                -"Certainty Index (1-5)...18") %>% 
  mutate(Species = ifelse(grepl("SA", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Salix arctica",
                          ifelse(grepl("SR", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Salix richardsonii", 
                                 ifelse(grepl("SP", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Salix pulchra", NA)))) %>%  # species col
  mutate(population_1 = ifelse(grepl("HE" , CG_phenocams_individual_2021_2022$ShrubID_Standard), "QHI", # working population col 1
                               ifelse(grepl("KP", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Kluane", 
                                      ifelse(grepl("PC", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Kluane", NA)))) %>% 
  mutate(population_2 = ifelse(grepl("H" , CG_phenocams_individual_2021_2022$ShrubID_Standard), "QHI", # working population col 1
                               ifelse(grepl("K", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Kluane", 
                                      ifelse(grepl("PP", CG_phenocams_individual_2021_2022$ShrubID_Standard), "Kluane", NA)))) %>%
  mutate(population = case_when(population_1 == "QHI" | population_2 == "QHI" ~ "QHI", # final population col 
                                population_1  == "Kluane" | population_2 == "Kluane" ~ "Kluane")) %>%
  select(-population_1, -population_2)

# reclassing dates
CG_phenocams_individual_2021_2022_wrangle$Snow_melt <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Snow_melt, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$Plants_first_visible_through_snow <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Plants_first_visible_through_snow, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$All_snow_free <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$All_snow_free, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$Full_snow_cover_EoS <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Full_snow_cover_EoS, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$First_leaf_bud_burst <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$First_leaf_bud_burst, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$First_leaf_yellow <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$First_leaf_yellow, format = "%d/%m/%Y")
CG_phenocams_individual_2021_2022_wrangle$Last_leaves_yellow <- as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Last_leaves_yellow, format = "%d/%m/%Y")

# add DOY column 
CG_phenocams_individual_2021_2022_wrangle$Snow_melt_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Snow_melt, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$Plants_first_visible_through_snow_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Plants_first_visible_through_snow, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$All_snow_free_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$All_snow_free, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$Full_snow_cover_EoS_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Full_snow_cover_EoS, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$First_leaf_bud_burst_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$First_leaf_bud_burst, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$First_leaf_yellow_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$First_leaf_yellow, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$Half_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Half_leaves_yellow, format = "%Y-%m-%d"))
CG_phenocams_individual_2021_2022_wrangle$Last_leaves_yellow_DOY <-  lubridate::yday(as.POSIXct(CG_phenocams_individual_2021_2022_wrangle$Last_leaves_yellow, format = "%Y-%m-%d"))

# species and pop as factors
CG_phenocams_individual_2021_2022_wrangle$Species <- as.factor(CG_phenocams_individual_2021_2022_wrangle$Species)
CG_phenocams_individual_2021_2022_wrangle$population <- as.factor(CG_phenocams_individual_2021_2022_wrangle$population)

write.csv(CG_phenocams_individual_2021_2022_wrangle, "data/phenology/phenocam_pics/CG_phenocams_individual_2021_2022_wrangle.csv")

# import 
CG_phenocams_individual_2021_2022_wrangle <- read.csv("data/phenology/phenocam_pics/CG_phenocams_individual_2021_2022_wrangle.csv")

# subsetting into species and site
CG_KP_arctica_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix arctica" & population == "Kluane")

# range of green up and senescence DOY
range(CG_KP_arctica_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 133 - 140
range(CG_KP_arctica_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# 178 - 227

CG_QHI_arctica_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix arctica" & population == "QHI")

# range or mean(change code as appropriate) of green up and senescence DOY
range(CG_QHI_arctica_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# range:  129 132
# mean: 129.75
mean (CG_QHI_arctica_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
#range: 172- 217
#mean: 194.8333

CG_KP_pulchra_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix pulchra" & population == "Kluane")

# range of green up and senescence DOY
mean(CG_KP_pulchra_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# range: 135- 147
#mean: 141.5
mean(CG_KP_pulchra_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
#  range:197 - 228
#mean:  217.1667

CG_QHI_pulchra_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix pulchra" & population == "QHI")

# range of green up and senescence DOY
mean(CG_QHI_pulchra_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# range: 128 - 140
# mean:136
mean(CG_QHI_pulchra_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# range: 174 - 224
# mean:194.4

CG_KP_rich_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix richardsonii" & population == "Kluane")

# range of green up and senescence DOY
range(CG_KP_rich_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 129- 142
range(CG_KP_rich_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# 191- 242

CG_QHI_rich_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix richardsonii" & population == "QHI")

# range of green up and senescence DOY
range(CG_QHI_rich_pheno$First_leaf_bud_burst_DOY, na.rm=TRUE)
# 126 - 142
range(CG_QHI_rich_pheno$First_leaf_yellow_DOY, na.rm=TRUE)
# 170- 218

# Mean DOY of 90% snow melt
mean(CG_phenocams_individual_2021_2022_wrangle$Snow_melt_DOY, na.rm=TRUE) 
# 113.4658
range(CG_phenocams_individual_2021_2022_wrangle$Snow_melt_DOY, na.rm=TRUE)
# 109 124

# Mean DOY of snow return
mean(CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE) 
# 270.1905
range(CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE)
# 270 276

# Growing season length on CG
# 270 - 113 = 157 days
mean_cg_gs <- (mean(CG_phenocams_individual_2021_2022_wrangle$Snow_return_EoS_DOY, na.rm=TRUE) - mean(CG_phenocams_individual_2021_2022_wrangle$Snow_melt_DOY, na.rm=TRUE))
mean_cg_gs

# mean growing season length -----
# summarise mean DOY of leaf emergence and leaf yellowing 
# to calculate species specific and site specific growing season lenghts
# CG -----
CG_pheno_summary <- CG_phenocams_individual_2021_2022_wrangle %>%
  group_by(Species, population)%>%
  summarise(mean_leaf_emergence_DOY = mean(First_leaf_bud_burst_DOY, na.rm=TRUE), 
            mean_leaf_yellow_DOY = mean(First_leaf_yellow_DOY, na.rm=TRUE)) %>%
  na.omit() %>%
  mutate(population = case_when(population == "Kluane" ~ "Southern",
                                population == "QHI" ~ "Northern"))


CG_growing_season <- CG_pheno_summary %>%
  group_by(Species, population)%>%
  summarise(growing_season_length = (mean_leaf_yellow_DOY-mean_leaf_emergence_DOY))

CG_pheno_summary_for_plot <- CG_phenocams_individual_2021_2022_wrangle %>%
  group_by(Species, population) %>%
  mutate(growing_season = First_leaf_yellow_DOY-First_leaf_bud_burst_DOY) %>%
  mutate(population = case_when(population == "Kluane" ~ "Southern",
                                population == "QHI" ~ "Northern"))



# KP -----
KP_pheno_summary <- KP_phenocams_2021_2022_wrangle %>%
  group_by(Species) %>%
  summarise(mean_leaf_emergence_DOY = mean(First_leaf_bud_burst_DOY, na.rm=TRUE), 
            mean_leaf_yellow_DOY = mean(First_leaf_yellow_DOY, na.rm=TRUE)) %>%
  na.omit()%>%
  mutate(population = rep("KP"))

KP_growing_season <- KP_pheno_summary %>%
  group_by(Species, population)%>%
  summarise(growing_season_length = (mean_leaf_yellow_DOY-mean_leaf_emergence_DOY))

KP_pheno_summary_for_plot <- KP_phenocams_2021_2022_wrangle %>%
  group_by(Species) %>%
  mutate(growing_season = First_leaf_yellow_DOY-First_leaf_bud_burst_DOY) %>%
  mutate(population = rep("KP"))

# QHI -----
QHI_pheno_summary <- QHI_phenocams_2022_wrangle %>%
  group_by(Species) %>%
  summarise(mean_leaf_emergence_DOY = mean(First_leaf_bud_burst_DOY, na.rm=TRUE), 
            mean_leaf_yellow_DOY = mean(First_leaf_yellow_DOY, na.rm=TRUE)) %>%
  na.omit()%>%
  mutate(population = rep("QHI"))

QHI_growing_season <- QHI_pheno_summary %>%
  group_by(Species, population)%>%
  summarise(growing_season_length = (mean_leaf_yellow_DOY-mean_leaf_emergence_DOY))

QHI_pheno_summary_for_plot <- QHI_phenocams_2022_wrangle %>%
  group_by(Species) %>%
  mutate(growing_season = First_leaf_yellow_DOY-First_leaf_bud_burst_DOY)%>%
  mutate(population = rep("QHI"))

# merge all growing season datasets (MEANS)
all_growing_season_means <- rbind(QHI_growing_season, KP_growing_season,
                                  CG_growing_season)

all_growing_season <-rbind(QHI_pheno_summary_for_plot,KP_pheno_summary_for_plot,
                           CG_pheno_summary_for_plot)

all_growing_season<-all_growing_season[-which(is.na(all_growing_season$Species)),]

write.csv(all_growing_season, "data/phenology/phenocam_pics/all_growing_season.csv")
all_growing_season <- read_csv("data/phenology/phenocam_pics/all_growing_season.csv")

# ordering levels so source and garden populations side by side
all_growing_season$population <- plyr::revalue(all_growing_season$population, 
                                               c("Northern"="Northern Garden",
                                                 "Southern"="Southern Garden",
                                                 "KP"="Southern Source",
                                                 "QHI"="Northern Source"))

all_growing_season$population <- ordered(all_growing_season$population, 
                                         levels = c("Northern Source", 
                                                    "Northern Garden", 
                                                    "Southern Source",
                                                    "Southern Garden"))

# OVERALL growing season means per site ----
means_growing_season <- all_growing_season_means %>%
  group_by(population) %>%
  summarise(overall_mean_growing_season = mean(growing_season_length))

# models -----
# load all data 
CG_phenocams_individual_2021_2022_wrangle <- read_csv("data/phenology/phenocam_pics/CG_phenocams_individual_2021_2022_wrangle.csv")
QHI_phenocams_2022_wrangle <- read_csv("data/phenology/phenocam_pics/QHI_phenocams_2022_wrangle.csv")
KP_phenocams_2021_2022_wrangle <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2021_2022_wrangle.csv")

# Wrangling to allow merge for salix phenophase comparison
# KP
KP_phenocams_2021_2022_wrangle <- KP_phenocams_2021_2022_wrangle[, - c(1:2)]
KP_phenocams_2021_2022_merge <- KP_phenocams_2021_2022_wrangle %>%
  dplyr::select(Plot, Year, Species, Salix_bud_burst, Salix_first_yellow,
                Salix_last_yellow, Salixbud_burst_DOY, Salix_first_yellow_DOY,
                Salix_last_yellow_DOY) %>%
  mutate(population = rep("Southern_source")) %>%
  rename("PhenocamID" = "Plot", 
         "First_bud_burst" ="Salix_bud_burst"  , 
         "First_leaf_yellow" = "Salix_first_yellow" ,
         "All_leaves_yellow" = "Salix_last_yellow" , 
         "First_bud_burst_DOY" ="Salixbud_burst_DOY", 
         "First_leaf_yellow_DOY" = "Salix_first_yellow_DOY"  ,
         "All_leaves_yellow_DOY" = "Salix_last_yellow_DOY")

# CG
CG_phenocams_individual_2021_2022_wrangle <- CG_phenocams_individual_2021_2022_wrangle[, - 1]
CG_phenocams_individual_2021_2022_merge <- CG_phenocams_individual_2021_2022_wrangle %>%
  dplyr::select(PhenocamID, Year, Species, First_leaf_bud_burst, First_leaf_yellow,
                Last_leaves_yellow, First_leaf_bud_burst_DOY, First_leaf_yellow_DOY,
                Last_leaves_yellow_DOY, population) %>% 
  rename("All_leaves_yellow" = "Last_leaves_yellow",
         "All_leaves_yellow_DOY" = "Last_leaves_yellow_DOY",
         "First_bud_burst" = "First_leaf_bud_burst",
         "First_bud_burst_DOY" = "First_leaf_bud_burst_DOY")

# removing rows where species NA
CG_phenocams_individual_2021_2022_merge <- CG_phenocams_individual_2021_2022_merge[-118, ]
CG_phenocams_individual_2021_2022_merge <- CG_phenocams_individual_2021_2022_merge[-c(124:126), ]

# QHI
QHI_phenocams_2022_wrangle <- QHI_phenocams_2022_wrangle[, -1]
QHI_phenocams_2022_wrangle <- QHI_phenocams_2022_wrangle[-c(3:4), ]

QHI_phenocams_2022_merge <- QHI_phenocams_2022_wrangle %>%
  dplyr::select(PhenocamID, Year, Species, Salix_bud_burst, Salix_first_yellow,
                Salix_last_yellow, Salix_first_bud_burst_DOY, Salix_first_yellow_DOY,
                Salix_last_yellow_DOY) %>%
  mutate(population = rep("Northern_source")) %>%
  rename( "First_bud_burst" ="Salix_bud_burst"  , 
          "First_leaf_yellow" = "Salix_first_yellow" ,
          "All_leaves_yellow" = "Salix_last_yellow" , 
          "First_bud_burst_DOY" ="Salix_first_bud_burst_DOY", 
          "First_leaf_yellow_DOY" = "Salix_first_yellow_DOY"  ,
          "All_leaves_yellow_DOY" = "Salix_last_yellow_DOY")

# merge all 

all_phenocam_data_salix <- rbind(QHI_phenocams_2022_merge, CG_phenocams_individual_2021_2022_merge,
                                 KP_phenocams_2021_2022_merge) 

all_phenocam_data_salix$Species <- as.factor(all_phenocam_data_salix$Species)
all_phenocam_data_salix$population <- as.factor(all_phenocam_data_salix$population)

# save as csv
write.csv(all_phenocam_data_salix, "data/phenology/phenocam_pics/all_phenocam_data_salix.csv")

# ordering levels so source and garden populations side by side
all_phenocam_data_salix$population <- plyr::revalue(all_phenocam_data_salix$population, 
                                                    c("QHI"="Northern Garden",
                                                      "Kluane"="Southern Garden",
                                                      "Southern_source"="Southern Source",
                                                      "Northern_source"="Northern Source"))

all_phenocam_data_salix$population <- ordered(all_phenocam_data_salix$population, 
                                              levels = c("Northern Source", 
                                                         "Northern Garden", 
                                                         "Southern Source",
                                                         "Southern Garden"))

all_phenocam_data_salix$Year <- as.factor(all_phenocam_data_salix$Year)

# source pop -----
# keeping only source pops
all_phenocam_data_salix_sources <- all_phenocam_data_salix %>%
  filter(population %in% c("Northern Source", "Southern Source"))

# bud burst: spp interact
# doesnt run with year random effect or spp random effect
bud_burst_mod_source <- lm(First_bud_burst_DOY ~ population*Species, data = all_phenocam_data_salix_sources)
tab_model(bud_burst_mod_source) 

(budburst_source_plot <- ggplot(all_phenocam_data_salix_sources) +
    geom_boxplot(aes(x= population, y = First_bud_burst_DOY, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("First leaf emergence DOY") +
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

# model first yellow leaf doy: spp interact
yellow_mod_source <- lm(First_leaf_yellow_DOY ~ population*Species, data = all_phenocam_data_salix_sources)
tab_model(yellow_mod_source)
# Model matrix is rank deficient. Parameters population.L:SpeciesSalix richardsonii were not estimable. 

(yellow_leaf_source_plot <- ggplot(all_phenocam_data_salix_sources) +
    geom_boxplot(aes(x= population, y = First_leaf_yellow_DOY, colour = population, fill = population, group = population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species) +
    ylab("First yellow leaf DOY") +
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

(pheno_source_panel <- ggarrange(budburst_source_plot, yellow_leaf_source_plot, 
                                 labels = c("A", "B"), common.legend = TRUE, legend = "bottom",
                                 ncol = 2, nrow = 1))


# CG vs sources ------
# model bud burst doy: spp random
bud_burst_mod <- lmer(First_bud_burst_DOY ~ population + (1|Species) + (1|Year), data = all_phenocam_data_salix)
tab_model(bud_burst_mod)

# model bud burst doy: spp interact
bud_burst_mod_2 <- lmer(First_bud_burst_DOY ~ population*Species + (1|Year), data = all_phenocam_data_salix)
tab_model(bud_burst_mod_2)
# fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

# model bud burst doy: spp interact omit year 
bud_burst_mod_3 <- lm(First_bud_burst_DOY ~ population*Species, data = all_phenocam_data_salix)
tab_model(bud_burst_mod_3)

(plot_bud_burst_mod <- ggplot(all_phenocam_data_salix) +
    geom_boxplot(aes(x = population, y = First_bud_burst_DOY , colour = population, fill = population, group =population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("First leaf emergence DOY\n") +
    xlab("\nPopulation") +
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

# model first yellow leaf doy: spp random
yellow_mod <- lmer(First_leaf_yellow_DOY ~ population + (1|Species) + (1|Year), data = all_phenocam_data_salix)
tab_model(yellow_mod)

# model first yellow leaf doy: spp interact
yellow_mod_2 <- lm(First_leaf_yellow_DOY ~ population*Species, data = all_phenocam_data_salix)
tab_model(yellow_mod_2)

(plot_yellow_mod <- ggplot(all_phenocam_data_salix) +
    geom_boxplot(aes(x = population, y = First_leaf_yellow_DOY , colour = population, fill = population, group =population), size = 0.5, alpha = 0.5) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("First yellowing of leaves DOY\n") +
    xlab("\nPopulation") +
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

# growing seasonn length diffferences
growing_season_mod <- lm(growing_season ~ population*Species, data = all_growing_season)
tab_model(growing_season_mod)

# plot growing season length
(plot_growing_season <- ggplot(all_growing_season) +
    geom_boxplot(aes(x = population, y = growing_season , colour = population, fill = population, group =population), size = 0.5, alpha = 0.5, na.rm=TRUE) +
    # facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Growing season length (days)\n") +
    xlab("\nPopulation") +
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

# CG only ------
all_phenocam_data_cg <- all_phenocam_data_salix %>% 
  filter(population %in% c("Northern Garden", "Southern Garden"))

# model bud burst doy common garden
#bud_burst_cg_mod <- lmer(First_bud_burst_DOY ~ population + (1|Species) + (1|Year), data = all_phenocam_data_cg)
# tab_model(bud_burst_cg_mod)
# boundary (singular) fit: see help('isSingular')
# omit year, spp random effect
bud_burst_cg_mod_1 <- lmer(First_bud_burst_DOY ~ population + (1|Species), data = all_phenocam_data_cg)
tab_model(bud_burst_cg_mod_1)

# omit year, spp interact
bud_burst_cg_mod_2 <- lm(First_bud_burst_DOY ~ population*Species, data = all_phenocam_data_cg)
tab_model(bud_burst_cg_mod_2)

(plot_bud_burst_cg_mod <- ggplot(all_phenocam_data_cg) +
    geom_boxplot(aes(x = population, y = First_bud_burst_DOY , colour = population, fill = population, group =population), size = 0.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("First leaf emergence DOY\n") +
    xlab("\nPopulation") +
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


# model first yellow leaf doy common garden 
#yellow_cg_mod <- lmer(First_leaf_yellow_DOY ~ population + (1|Species) + (1|Year), data = all_phenocam_data_cg)
# tab_model(yellow_cg_mod)
# boundary (singular) fit: see help('isSingular')
# omit year, spp random effect
yellow_cg_mod_1 <- lmer(First_leaf_yellow_DOY ~ population + (1|Species) , data = all_phenocam_data_cg)
tab_model(yellow_cg_mod_1)

# omit year, spp interact
yellow_cg_mod_2 <- lm(First_leaf_yellow_DOY ~ population*Species, data = all_phenocam_data_cg)
tab_model(yellow_cg_mod_2)

(plot_yellow_cg_mod <- ggplot(all_phenocam_data_cg) +
    geom_boxplot(aes(x = population, y = First_leaf_yellow_DOY , colour = population, fill = population, group =population), size = 0.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("First yellowing of leaves DOY") +
    xlab("\nPopulation") +
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
  
  