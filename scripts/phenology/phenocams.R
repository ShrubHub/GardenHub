#### PHENOCAMS: common garden, Kluane and QHI
#### Script by Erica Zaja, created 14/12/22
### Last updated: 18/01/23 by Madi 

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(readxl)
library(gridExtra)

# 2. LOADING DATA ----
KP_phenocams_2021 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2021.csv")
KP_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/KP_phenocams_2022.csv")
QHI_phenocams_2022 <- read_csv("data/phenology/phenocam_pics/QHI_phenocams_2022.csv")
Phenocam_Datasheet_QHI <- read_csv("data/phenology/phenocam_pics/Phenocam_Datasheet_QHI.csv")
CG_phenocams_individual_2021_2022 <- read_csv("data/phenology/phenocam_pics/CG_phenocams_individual_2021_2022.csv")
QHI_observations_2016_19 <- read_excel("data/phenology/phenocam_pics/QHI_observations_2016-19.xlsx")

# add the generic sheets too (not the individual observations)- but might not need them
# CG_phenocams_2021_all, CG_phenocams_2022_all

# 3. DATA WRANGLING-----

# to make DOY column: 
#df$DOY <-  lubridate::yday(as.POSIXct(df$Date, format = "%Y-%m-%d"))

# 3.1. KP ------
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
KP_phenocams_2021_2022_manual <- read_csv("data/phenology/phenocam_pics/working_data/KP_phenocams_2021_2022_manual.csv")
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
KP_phenocams_2021_2022_manual$Salix_pulchra_bud_burst <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_pulchra_bud_burst, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_pulchra_first_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_pulchra_first_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_pulchra_last_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_pulchra_last_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_rich_bud_burst <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_rich_bud_burst, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_rich_first_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_rich_first_yellow, format = "%d/%m/%Y")
KP_phenocams_2021_2022_manual$Salix_rich_last_yellow <- as.POSIXct(KP_phenocams_2021_2022_manual$Salix_rich_last_yellow, format = "%d/%m/%Y")
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
KP_phenocams_2021_2022_manual$Salix_pulchra_bud_burst_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_pulchra_bud_burst, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_pulchra_first_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_pulchra_first_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_pulchra_last_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_pulchra_last_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_rich_bud_burst_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_rich_bud_burst, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_rich_first_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_rich_first_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$Salix_rich_last_yellow_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$Salix_rich_last_yellow, format = "%Y-%m-%d"))
KP_phenocams_2021_2022_manual$First_greening_DOY <-  lubridate::yday(as.POSIXct(KP_phenocams_2021_2022_manual$First_greening, format = "%Y-%m-%d"))


# Divide salix pulchra and richardsonii
KP_phenocams_2021_2022_pulchra <- KP_phenocams_2021_2022_manual %>%
  select(-Salix_rich_bud_burst, -Salix_rich_first_yellow, -Salix_rich_last_yellow, 
         Salix_rich_bud_burst_DOY, Salix_rich_first_yellow_DOY, Salix_rich_last_yellow_DOY)

KP_phenocams_2021_2022_rich <- KP_phenocams_2021_2022_manual %>%
  select(-Salix_pulchra_bud_burst, -Salix_pulchra_first_yellow, -Salix_pulchra_last_yellow,
         Salix_pulchra_bud_burst_DOY, Salix_pulchra_first_yellow_DOY, Salix_pulchra_last_yellow_DOY)


# 3.2. QHI ----

# 2022 QHI phenocams. NB only yellowing 
QHI_phenocams_2022_wrangle <- QHI_phenocams_2022 %>%
  rename("Plot" = "PLOT", "Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "First_leaf_bud_burst" = "First leaf bud burst",
         "Half_leaves_green" ="50% Leaves Green",
         "All_leaves_green" = "100% Leaves Green",
         "Half_leaves_yellow" = "50% Leaves Yellow",
         "All_leaves_yellow" = "100% Leaves Yellow",
         "Salix_spp" = "SALIX SPP",
         "Salix_bud_burst" = "Salix First Leaf Bud Burst",
         "Salix_first_yellow"= "Salix First Yellowing of Leaves",
         "Salix_last_yellow" = "Salix Last Leaf Turns Yellow") %>%
  select(Plot, Year, Viewshed, NOTES, Snow_melt, All_snow_free, Snow_return_EoS,
         Half_snow_cover_EoS, Full_snow_cover_EoS, First_leaf_bud_burst, 
         Half_leaves_green, All_leaves_green, Half_leaves_yellow, 
         All_leaves_yellow, Salix_spp, Salix_bud_burst, Salix_first_yellow, 
         Salix_last_yellow)

# past years QHI 
Phenocam_Datasheet_QHI <- Phenocam_Datasheet_QHI[1:21,] # removing loads of NAs

QHI_phenocams_past_wrangle <- Phenocam_Datasheet_QHI %>%
  rename("Plot" = "PLOT", "Plants_first_visible_through_snow" = "Plants first visible through snow",
         "Snow_melt" = "Snow Free Melt Date (>90% plot free of snow)", 
         "All_snow_free" = "First 100% snow-free day",
         "Snow_return_EoS" = "First snow return day - end of season",
         "Half_snow_cover_EoS" = "50% snow coverge - end of season",
         "Full_snow_cover_EoS" = "100% snow coverage - end of season",
         "Salix_spp" = "Salix spp (ARC, PUL, or RIC)",
         "Salix_bud_burst" = "Salix First Leaf Bud Burst",
         "Salix_first_yellow"= "Salix First Yellowing of Leaves",
         "Salix_last_yellow" = "Salix Last Leaf Turns Yellow") %>%
  select(Plot, Year, NOTES, Snow_melt, All_snow_free, Snow_return_EoS,
         Half_snow_cover_EoS, Full_snow_cover_EoS, 
         Salix_spp, Salix_bud_burst, Salix_first_yellow, 
         Salix_last_yellow)

# merge 2022 and past years datasheets
QHI_phenocams_2016_2022 <- bind_rows(QHI_phenocams_2022_wrangle, QHI_phenocams_past_wrangle)

# making names consistent
QHI_phenocams_2016_2022$Salix_spp <- as.factor(QHI_phenocams_2016_2022$Salix_spp)

QHI_phenocams_2016_2022_edit <- QHI_phenocams_2016_2022 %>%
  mutate(Species = case_when(Salix_spp %in% c("ARC", "Arctica") ~ "Salix arctica",
                              Salix_spp %in% c("PUL", "Pulchra") ~ "Salix pulchra",
                              Salix_spp %in% c("RIC", "Richardsonii") ~ "Salix richardsonii")) %>%
  mutate(PhenocamID = case_when(Plot %in% c("Phenocam 1", "QHI_1") ~ "1",
                                Plot %in% c("Phenocam 2", "QHI_2") ~ "2", 
                                Plot %in% c("Phenocam 3", "QHI_3") ~ "3",                               
                                Plot %in% c("Phenocam 4", "QHI_4") ~ "4",                                
                                Plot %in% c("Phenocam 5", "QHI_5") ~ "5",                                
                                Plot %in% c("Phenocam 6", "QHI_6") ~ "6",
                                Plot == ("QHI_7") ~ "7",
                                Plot == ("QHI_8") ~ "8",
                                Plot == ("QHI_9") ~ "9",
                                Plot == ("QHI_10") ~ "10",
                                Plot == ("QHI_11") ~ "11",
                                Plot == ("QHI_12") ~ "12",
                                Plot == ("QHI_13") ~ "13",
                                Plot == ("QHI_14") ~ "14",
                                Plot == ("QHI_15") ~ "15",
                                Plot == ("QHI_16") ~ "16",
                                Plot == ("QHI_17") ~ "17",
                                Plot == ("QHI_18") ~ "18",
                                Plot == ("QHI_19") ~ "19",
                                Plot == ("QHI_20") ~ "20")) %>%
  select(Year, PhenocamID, Species, Snow_melt, All_snow_free, Snow_return_EoS, Half_snow_cover_EoS, Full_snow_cover_EoS,
         Salix_bud_burst, Salix_first_yellow, Salix_last_yellow)%>%
  drop_na(Species)

# dates are a big mess. Easier and quicker to edit manually.
write_csv(QHI_phenocams_2016_2022_edit, "data/phenology/phenocam_pics/working_data/QHI_phenocams_2016_2022_edit.csv" )

# load manual version
QHI_phenocams_2016_2022_edit_manual <- read_csv("data/phenology/phenocam_pics/working_data/QHI_phenocams_2016_2022_edit_manual.csv")
str(QHI_phenocams_2016_2022_edit_manual)

# reclass dates
QHI_phenocams_2016_2022_edit_manual$Snow_melt <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Snow_melt, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$All_snow_free <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$All_snow_free, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$Snow_return_EoS <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Snow_return_EoS, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$Half_snow_cover_EoS <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Half_snow_cover_EoS, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$Full_snow_cover_EoS <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Full_snow_cover_EoS, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$Salix_bud_burst <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Salix_bud_burst, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$Salix_first_yellow <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Salix_first_yellow, format = "%d/%m/%Y")
QHI_phenocams_2016_2022_edit_manual$Salix_last_yellow <- as.POSIXct(QHI_phenocams_2016_2022_edit_manual$Salix_last_yellow, format = "%d/%m/%Y")

# dividing species
QHI_salarc <- QHI_phenocams_2016_2022_edit_manual %>%
  filter(Species == "Salix arctica")

QHI_salpul <- QHI_phenocams_2016_2022_edit_manual %>%
  filter(Species == "Salix pulchra")

QHI_salric <- QHI_phenocams_2016_2022_edit_manual %>%
  filter(Species == "Salix richardsonii")
# Nb Salix richardsonii only has first yellowing from 2022

# 3.3. CG -----
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
  select(-Shrub, -Observer,-Notes,-"Certainty Index", -"Certainty Index (1-5)...16",
         -"Certainty Index (1-5)...18") %>% 
  mutate(Species = ifelse(grepl("SA", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Salix arctica",
                          ifelse(grepl("SR", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Salix richardsonii", 
                                 ifelse(grepl("SP", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Salix pulchra", NA)))) %>%  # species col
  mutate(population_1 = ifelse(grepl("HE" , CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "QHI", # working population col 1
                                   ifelse(grepl("KP", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Kluane", 
                                          ifelse(grepl("PC", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Kluane", NA)))) %>% 
  mutate(population_2 = ifelse(grepl("H" , CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "QHI", # working population col 1
                             ifelse(grepl("K", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Kluane", 
                                    ifelse(grepl("PP", CG_phenocams_individual_2021_2022_wrangle$ShrubID_Standard), "Kluane", NA)))) %>%
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

# species and pop as factors
CG_phenocams_individual_2021_2022_wrangle$Species <- as.factor(CG_phenocams_individual_2021_2022_wrangle$Species)
CG_phenocams_individual_2021_2022_wrangle$population <- as.factor(CG_phenocams_individual_2021_2022_wrangle$population)

# subsetting into species and site
CG_KP_arctica_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix arctica" & population == "Kluane")

CG_QHI_arctica_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix arctica" & population == "QHI")

CG_KP_pulchra_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix pulchra" & population == "Kluane")

CG_QHI_pulchra_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix pulchra" & population == "QHI")

CG_KP_rich_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix richardsonii" & population == "Kluane")

CG_QHI_rich_pheno <- CG_phenocams_individual_2021_2022_wrangle %>%
  filter(Species == "Salix richardsonii" & population == "QHI")

