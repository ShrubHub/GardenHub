#### COMMON GARDEN 2022 SCRIPT
### Data wrangling and visualisation script
### By Erica Zaja and Madelaine Anderson, created on 30/09/2022
## Adapted from Madelaine Anderson's common_garden_2021.R 
## Last updated: 27/01/2023 by Erica

# 1. LOADING LIBRARIES ----
library(tidyverse)
library(viridis)
library(readr)
library(base)
library(lubridate)
library(readxl) # reads excel files if java doesn't work for (xlsx)

# 2. LOADING DATA ----

# Data collected in the common garden in summer 2022 (June-July-August)
growth_2022 <- read.csv("data/common_garden_data_2022/wrangled_ALL_combined_Common_Garden_Measurements_2022.csv")

# Common garden data collected between 2013-2021
growth <- read.csv("data/common_garden_data_2021/all_growth_2021.csv")

# Data collected from source locations (Kluane and QHI) between 2013 – 2017
field_data <- read.csv('data/source_pops/Salix_field_trait_data.csv')

# Height data from 2021 on KP
kp_2021 <- read.csv('data/source_pops/KP_heights_11AUG2021.csv')

# Data collected from source locations (Kluane and QHI) in 2022
# kluane subsets collected weekly in 2022
X010822 <- read_excel("data/source_pops/source_pop_Kluane_shrub_data/weekly_subsets/010822_EZ_weekly_source_pop_Kluane_2022.xlsx")
X090722 <- read_excel("data/source_pops/source_pop_Kluane_shrub_data/weekly_subsets/090722_EZ_weekly_source_pop_Kluane_2022.xlsx")
X130822 <- read_excel("data/source_pops/source_pop_Kluane_shrub_data/weekly_subsets/130822_EZ_weekly_source_pop_Kluane_2022.xlsx")
X160722 <- read_excel("data/source_pops/source_pop_Kluane_shrub_data/weekly_subsets/160722_EZ_weekly_source_pop_Kluane_2022.xlsx")
X240722 <- read_excel("data/source_pops/source_pop_Kluane_shrub_data/weekly_subsets/240722_EZ_weekly_source_pop_Kluane_2022.xlsx")

# All weekly subsets merged from QHI data 2022
all_weekly_QHI_2022 <- read_csv("data/source_pops/source_pop_Qiki_shrub_data/weekly_subsets/all_weekly_QHI_2022.csv")

# Data from both Kluane and QHI 2022
all_source_pop_2022 <- read.csv("data/source_pops/all_source_pop_2022.csv")

# Dataset with mother data (2013-2017)
Common_garden_2017 <- read.csv("data/common_garden_data_2017/Common_garden_2017.csv")

# 3. DATA WRANGLING ----

# 3.1. Source pop subsets 2022 ----

# Making raw data (Kluane subsets 2022) dates into POSIXct
X010822$SampleDate <- as.POSIXct(X010822$SampleDate, format = "%d/%m/%Y")
X090722$SampleDate <- as.POSIXct(X090722$SampleDate, format = "%d/%m/%Y")
X130822$SampleDate <- as.POSIXct(X130822$SampleDate, format = "%d/%m/%Y")

# merging 2022 source pop subsets in Kluane
kluane_source_pop_2022 <- rbind(X010822,X090722,X130822,X160722, X240722)

# Creating mean stem elongation, mean leaf length and mean width columns
# remove S. reticulata 
kluane_source_pop_2022 <- kluane_source_pop_2022 %>% 
  filter(Species != "Salix reticulata") %>% # removing salix reticulata
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
         mean_width = ((Width_cm + Width_2_cm)/2)) %>% 
 dplyr::select(- Stem_diameter_2, - Stem_diameter_3) # stem diam 2 and 3 were taken for sal ret
        
# removing sort column from QHI subsets
QHI_source_pop_2022 <- all_weekly_QHI_2022[,-1]

# reclassing stem diam
kluane_source_pop_2022$Stem_diameter <- as.numeric(kluane_source_pop_2022$Stem_diameter)
QHI_source_pop_2022$Stem_diameter <- as.numeric(QHI_source_pop_2022$Stem_diameter)

# merging QHI and Kluane data from 2022
all_source_pop_2022 <- rbind(QHI_source_pop_2022, kluane_source_pop_2022)
unique(all_source_pop_2022$SampleDate)

# making variables right format
all_source_pop_2022$Species <- as.factor(all_source_pop_2022$Species)
all_source_pop_2022$Site <- as.factor(all_source_pop_2022$Site)
all_source_pop_2022$Stem_diameter <- as.numeric(all_source_pop_2022$Stem_diameter)
all_source_pop_2022$SampleDate  <- format(as.POSIXct(all_source_pop_2022$SampleDate,format='%d/%m/%Y %H:%M:%S'),format='%d-%m-%Y')
all_source_pop_2022$SampleDate <- as.POSIXct(all_source_pop_2022$SampleDate, format = "%Y-%m-%d")
unique(all_source_pop_2022$SampleDate)

# making a year column
all_source_pop_2022 <- all_source_pop_2022 %>%
  mutate(SampleYear = format(as.Date(SampleDate, format="%d/%m/%Y"),"%Y"))
all_source_pop_2022$SampleYear <- as.numeric(all_source_pop_2022$SampleYear)

# saving source pop 2022 data as csv
write.csv(all_source_pop_2022, 'data/source_pops/all_source_pop_2022.csv')

# 3.2. CG growth 2022 ----
# Keeping only relevant columns of 2022 common garden data
growth_2022 <- dplyr::select(growth_2022, Bed, SampleID, Year_planted, Species, Site, Sample_Date,
                             Month, Day, Year, Canopy_Height_cm, Width_cm, Width_2_cm, Stem_diameter,
                             Stem_Elongation_1_mm, Stem_Elongation_2_mm, Stem_Elongation_3_mm, 
                             Length_1_mm, Length_2_mm, Length_3_mm)


# Subsetting data to remove NA columns
growth_2022 <- growth_2022[1:780, ]

# make standard sample ID column to avoid issue of dashes, spaces, etc. 
growth_2022$SampleID_standard <- toupper(growth_2022$SampleID) # make all uppercase characters 
growth_2022$SampleID_standard<-gsub("-","",as.character(growth_2022$SampleID_standard)) # remove "-"
growth_2022$SampleID_standard<-gsub(" ","",as.character(growth_2022$SampleID_standard)) # remove spaces " " 

# make all variables numeric to drop written observations 
growth_2022$Canopy_Height_cm <- as.numeric(growth_2022$Canopy_Height_cm)
growth_2022$Width_cm <- as.numeric(growth_2022$Width_cm)
growth_2022$Width_2_cm <- as.numeric(growth_2022$Width_2_cm)
growth_2022$Stem_diameter <- as.numeric(growth_2022$Stem_diameter)
growth_2022$Stem_Elongation_1_mm <- as.numeric(growth_2022$Stem_Elongation_1_mm)
growth_2022$Stem_Elongation_2_mm <- as.numeric(growth_2022$Stem_Elongation_2_mm)
growth_2022$Stem_Elongation_3_mm <- as.numeric(growth_2022$Stem_Elongation_3_mm)
growth_2022$Length_1_mm <- as.numeric(growth_2022$Length_1_mm)
growth_2022$Length_2_mm <- as.numeric(growth_2022$Length_2_mm)
growth_2022$Length_3_mm <- as.numeric(growth_2022$Length_3_mm)

#filter out July observations and only keep sampleID (to match) and growth variables 
july <- growth_2022 %>% 
  filter(Month == "7") %>% 
  dplyr::select(SampleID, Canopy_Height_cm, Width_cm, Width_2_cm, 
         Stem_diameter, Stem_Elongation_1_mm, Stem_Elongation_2_mm, 
         Stem_Elongation_3_mm, Length_1_mm, Length_2_mm, Length_3_mm) %>% 
  rename("jul_height" = "Canopy_Height_cm", # rename all to july values 
         "jul_width_1" = "Width_cm", 
         "jul_width_2" = "Width_2_cm", 
         "jul_Stem_diameter" = "Stem_diameter",
         "jul_stem_Elong_1_mm" = "Stem_Elongation_1_mm",
         "jul_stem_Elong_2_mm" = "Stem_Elongation_2_mm",
         "jul_stem_Elong_3_mm" = "Stem_Elongation_3_mm",
         "jul_length_1_mm" = "Length_1_mm",
         "jul_length_2_mm" = "Length_2_mm",
         "jul_length_3_mm" = "Length_3_mm")

str(july) # all good 
# date: 23/07/2022

# same for august 
aug <-  growth_2022 %>% 
  filter(Month == "8") %>% 
  rename("aug_height" = "Canopy_Height_cm", 
         "aug_width_1" = "Width_cm", 
         "aug_width_2" = "Width_2_cm", 
         "aug_Stem_diameter" = "Stem_diameter",
         "aug_stem_Elong_1_mm" = "Stem_Elongation_1_mm",
         "aug_stem_Elong_2_mm" = "Stem_Elongation_2_mm",
         "aug_stem_Elong_3_mm" = "Stem_Elongation_3_mm",
         "aug_length_1_mm" = "Length_1_mm",
         "aug_length_2_mm" = "Length_2_mm",
         "aug_length_3_mm" = "Length_3_mm")

str(aug) # all good 

# merge the two dataframes by SampleID and merge columns for traits/growth
# use august value unless NA, in which case use 
july_aug <- left_join(aug, july, by = "SampleID")  %>% 
  mutate(Canopy_Height_cm = coalesce(aug_height, jul_height), 
         Width_cm = coalesce(aug_width_1, jul_width_1),
         Width_2_cm = coalesce(aug_width_2, jul_width_2), 
         Stem_diameter = coalesce(aug_Stem_diameter, jul_Stem_diameter), 
         Stem_Elongation_1_mm = coalesce(aug_stem_Elong_1_mm, jul_stem_Elong_1_mm), 
         Stem_Elongation_2_mm  = coalesce(aug_stem_Elong_2_mm, jul_stem_Elong_2_mm), 
         Stem_Elongation_3_mm  = coalesce(aug_stem_Elong_3_mm, jul_stem_Elong_3_mm),
         Length_1_mm = coalesce(aug_length_1_mm, jul_length_1_mm), 
         Length_2_mm = coalesce(aug_length_2_mm, jul_length_2_mm), 
         Length_3_mm = coalesce(aug_length_3_mm, jul_length_3_mm)) %>%
  dplyr::select(- Sample_Date)

sum(is.na(july_aug$aug_height)) # 76 NAs
sum(is.na(july_aug$jul_height)) # 44 NAs
sum(is.na(july_aug$Canopy_Height_cm)) # 44 aka same as July but now with some Aug observations when available 

sum(is.na(july_aug$aug_width_1)) # 76 NAs
sum(is.na(july_aug$jul_width_1)) # 44
sum(is.na(july_aug$Width_cm)) # 44

sum(is.na(july_aug$aug_width_2)) # 76 NAs
sum(is.na(july_aug$jul_width_2)) # 44
sum(is.na(july_aug$Width_2_cm)) # 44

sum(is.na(july_aug$aug_stem_Elong_1_mm)) # 78 NAs
sum(is.na(july_aug$jul_stem_Elong_1_mm)) # 48
sum(is.na(july_aug$Stem_Elongation_1_mm)) # 47  

sum(is.na(july_aug$aug_stem_Elong_2_mm)) # 86 NAs
sum(is.na(july_aug$jul_stem_Elong_2_mm)) # 53
sum(is.na(july_aug$Stem_Elongation_2_mm)) # 51

sum(is.na(july_aug$aug_stem_Elong_3_mm)) # 93 NAs
sum(is.na(july_aug$jul_stem_Elong_3_mm)) # 75
sum(is.na(july_aug$Stem_Elongation_3_mm)) # 67

sum(is.na(july_aug$aug_length_1_mm)) # 77 NAs
sum(is.na(july_aug$jul_length_1_mm)) # 44
sum(is.na(july_aug$Length_1_mm)) # 44 

sum(is.na(july_aug$aug_length_2_mm)) # 78 NAs
sum(is.na(july_aug$jul_length_2_mm)) # 45
sum(is.na(july_aug$Length_2_mm)) # 44 

sum(is.na(july_aug$aug_length_3_mm)) # 83 NAs
sum(is.na(july_aug$jul_length_3_mm)) # 46
sum(is.na(july_aug$Length_3_mm)) # 45 

sum(is.na(july_aug$aug_Stem_diameter)) # 76 NAs
sum(is.na(july_aug$jul_Stem_diameter)) # 46
sum(is.na(july_aug$Stem_diameter)) # 44 

# statement specify the date depending on which value was used 
CG_july_aug_2022_dates <- july_aug %>%
  mutate(Sample_Date = case_when(Canopy_Height_cm == jul_height & Width_cm == jul_width_1 &
                                   Width_2_cm == jul_width_2 & Stem_diameter == jul_Stem_diameter &
                                   Stem_Elongation_1_mm == jul_stem_Elong_1_mm & Stem_Elongation_2_mm == jul_stem_Elong_2_mm & 
                                 Stem_Elongation_3_mm == jul_stem_Elong_3_mm &  Length_1_mm == jul_length_1_mm & 
                                   Length_2_mm == jul_length_2_mm & Length_3_mm == jul_length_3_mm 
                                 ~ make_date(year = "2022", month = "7", day = "23"), 
                                             TRUE ~ make_date(year = "2022", month = "8", day = "17")))


# keep relevant columns for merge with 2013-2021 dataset
CG_july_aug_2022 <- CG_july_aug_2022_dates %>%
  dplyr::select(Bed, SampleID, Year_planted, Species, Site, Sample_Date, SampleID_standard,
                Canopy_Height_cm, Width_cm, Width_2_cm, Stem_diameter, 
                Stem_Elongation_1_mm, Stem_Elongation_2_mm, Stem_Elongation_3_mm, 
                Length_1_mm, Length_2_mm, Length_3_mm) %>%
  mutate(Year = lubridate::year(Sample_Date), 
          Month = lubridate::month(Sample_Date), 
          Day = lubridate::day(Sample_Date))

str(CG_july_aug_2022) 

# convert date format to merge below
CG_july_aug_2022$Sample_Date <- as.POSIXct(CG_july_aug_2022$Sample_Date)
CG_july_aug_2022$Sample_Date <- format(as.POSIXct(CG_july_aug_2022$Sample_Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')

# Keeping only relevant columns of 2013-2021 data
growth <- dplyr::select(growth, Bed, SampleID, Year_planted, Species, Site, Sample_Date,
                        Month, Day, Year, Canopy_Height_cm, Width_cm, Width_2_cm, Stem_diameter,
                        Stem_Elongation_1_mm, Stem_Elongation_2_mm, Stem_Elongation_3_mm, 
                        Length_1_mm, Length_2_mm, Length_3_mm)
str(growth)
growth$Sample_Date <- as.POSIXct(growth$Sample_Date, format = "%d/%m/%Y")

# make standard sample ID column to avoid issue of dashes, spaces, etc. 
growth$SampleID_standard <- toupper(growth$SampleID) # make all uppercase characters 
growth$SampleID_standard<-gsub("-","",as.character(growth$SampleID_standard)) # remove "-"
growth$SampleID_standard<-gsub(" ","",as.character(growth$SampleID_standard)) # remove spaces " " 

# Removing Betula nana and Betula glandulosa, keeping only month of August for consistency,
# Creating mean stem elongation, mean leaf length and mean width columns
growth_to_merge <- growth %>% 
  filter(Species != "unknown" & Species!="Betula nana" & Species!="Betula glandulosa") %>% 
  filter(Month == "8") %>%
  filter(Day != 2) 

# Merging 2022 data with 2013-2021 data 
all_growth_2022 <- rbind(growth_to_merge, CG_july_aug_2022) 
str(all_growth_2022)
unique(all_growth_2022$Sample_Date) # one NA
which(is.na(all_growth_2022$Sample_Date)) # at row 5211 

# calculate mean leaf length, mean width, mean stem elongation, sample year 
all_merged_data_2022 <- all_growth_2022 %>% 
  mutate(Sample_age = Year - Year_planted) %>%
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
       mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
       mean_width = ((Width_cm + Width_2_cm)/2)) 
#biovolume = (Width_cm*Width_2_cm*Canopy_Height_cm))

# Making variables into the right format
all_merged_data_2022$Stem_Elongation_1_mm <- as.numeric(all_merged_data_2022$Stem_Elongation_1_mm)
all_merged_data_2022$Stem_Elongation_2_mm <- as.numeric(all_merged_data_2022$Stem_Elongation_2_mm)
all_merged_data_2022$Stem_Elongation_3_mm <- as.numeric(all_merged_data_2022$Stem_Elongation_3_mm)
all_merged_data_2022$Length_1_mm <- as.numeric(all_merged_data_2022$Length_1_mm)
all_merged_data_2022$Length_2_mm <- as.numeric(all_merged_data_2022$Length_2_mm)
all_merged_data_2022$Length_3_mm <- as.numeric(all_merged_data_2022$Length_3_mm)
all_merged_data_2022$Width_cm <- as.numeric(all_merged_data_2022$Width_cm)
all_merged_data_2022$Width_2_cm <- as.numeric(all_merged_data_2022$Width_2_cm)
all_merged_data_2022$Canopy_Height_cm <- as.numeric(all_merged_data_2022$Canopy_Height_cm)

str(all_merged_data_2022)

# Saving all merged and formatted 2013-2022 data as csv file
write.csv(all_merged_data_2022, 'data/common_garden_data_2022/all_merged_data_2022.csv')
# load data 
all_merged_data_2022 <- read.csv("data/common_garden_data_2022/all_merged_data_2022.csv")

# Making variables into the right format
all_merged_data_2022$Bed <- as.factor(as.character(all_merged_data_2022$Bed))
all_merged_data_2022$Species <- as.factor(as.character(all_merged_data_2022$Species))
all_merged_data_2022$Site <- as.factor(as.character(all_merged_data_2022$Site))
all_merged_data_2022$Site <- as.factor(as.character(all_merged_data_2022$Site))
all_merged_data_2022$Month <- as.numeric(all_merged_data_2022$Month)
all_merged_data_2022$Day <- as.numeric(all_merged_data_2022$Day)
all_merged_data_2022$Canopy_Height_cm <- as.numeric(all_merged_data_2022$Canopy_Height_cm)
all_merged_data_2022$Sample_Date <- as.POSIXct(all_merged_data_2022$Sample_Date, format = "%Y-%m-%d")

# Checking all variables are right format
str(all_merged_data_2022)

# make treatment column that specifies whether shrubs are north or south 
all_cg_data_2022 <-  all_merged_data_2022 %>% 
  mutate(population = case_when(startsWith(as.character(SampleID_standard), "H") ~ "Northern",
                              TRUE ~ "Southern")) %>% 
  dplyr::select(-X) 
# save again 
# write.csv(all_cg_data_2022, "data/common_garden_data_2022/all_cg_data_2022.csv")

# 3.2.1. Leaf length - Stem elongation swap ----
# fix 2013-2020 data from CG where leaf length and stem elongation values are swapped 
# load data 
all_cg_data_2022 <-  read.csv("data/common_garden_data_2022/all_cg_data_2022.csv")
# filter out 2021 and 22 data (those values are already correct)
# doing this in small steps so it's easy to follow / not mix myself up 
cg_data_2020 <- all_cg_data_2022 %>% 
  filter(Year != "2021" & Year != "2022") 
str(cg_data_2020)
# now rename variables 
cg_data_2020_renamed <- cg_data_2020 %>% 
  dplyr::rename("Length_1_mm" = "Stem_Elongation_1_mm", 
                "Length_2_mm" = "Stem_Elongation_2_mm", 
                "Length_3_mm" = "Stem_Elongation_3_mm", 
                "Stem_Elongation_1_mm" = "Length_1_mm", 
                "Stem_Elongation_2_mm" = "Length_2_mm", 
                "Stem_Elongation_3_mm" = "Length_3_mm") %>%
  mutate(mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
         mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3))
view(cg_data_2020_renamed)
# this is correct! now add in 2021 and 2022 data 
cg_data_2022 <- all_cg_data_2022 %>% 
  filter(Year %in% c("2021", "2022"))
# merge two data frames 
all_cg_data_2022 <- bind_rows(cg_data_2020_renamed, cg_data_2022) 
# drop all X columns 
all_cg_data_2022 <- all_cg_data_2022 %>% 
  dplyr::select(-"X.1")

write.csv(all_cg_data_2022, "data/common_garden_data_2022/all_cg_data_2022.csv")

# 3.3. Field data from 2017 ----
# Keeping only relevant columns
field_data <- dplyr::select(field_data, Species, Plant_height_veg_m, Lat, Lon, 
                            Elevation_m, Site, date, Year_measured)

# Making variables into the right format
field_data$Species <- as.factor(field_data$Species)
field_data$Site <- as.factor(field_data$Site)

# multiplying by 100 to get height in cm
field_source_pop <- field_data %>%
  mutate(Canopy_Height_cm = Plant_height_veg_m*100) %>%
  dplyr::select(-Plant_height_veg_m)%>%
  na.omit()

write.csv(field_source_pop, 'data/source_pops/field_source_pop.csv')
# fixing date issue manually was quicker


# loading new dataset (modified a couple dates manually)
# field_source_pop_new <- read_csv("data/source_pops/field_source_pop_new.csv")

field_source_pop_new <- read_csv("data/source_pops/field_source_pop.csv")

# making date into right format
field_source_pop_new$date <- as.POSIXct(field_source_pop_new$date, format = "%d/%m/%Y")
str(field_source_pop_new$date) # right!

# renaming columns so they match 
field_source_pop_new <- field_source_pop_new %>%
  rename("SampleDate" = "date", 
         "Latitude" = "Lat",
         "Longitude" = "Lon", 
         "Elevation" = "Elevation_m")

# making site column
# field_source_pop_new <- field_source_pop_new %>%
#  mutate(Site = case_when(SampleSite %in% c("Kluane", "Kluane Plateau", "Pika Camp", "Printers Pass") ~ 'Kluane', 
                          # SampleSite %in% c("Qikiqtaruk","QHI") ~ 'Qikiqtaruk'))

# date into the right format
#field_source_pop_new$SampleDate <- format(as.POSIXct(field_source_pop_new$SampleDate,
  #                                                         format='%Y/%m/%d %H:%M:%S'),format='%d/%m/%Y')

# making a year column
# field_source_pop_new <- field_source_pop_new %>%
#  mutate(SampleYear = format(as.Date(SampleDate, format="%d/%m/%Y"),"%Y"))

# renaming year column 
field_source_pop_new <- field_source_pop_new %>% 
  rename("SampleYear" = "Year_measured")

# variables into right format
field_source_pop_new$SampleDate <- as.POSIXct(field_source_pop_new$SampleDate, format = "%d/%m/%Y")
field_source_pop_new$SampleYear <- as.numeric(field_source_pop_new$SampleYear)
field_source_pop_new$Site <- as.factor(field_source_pop_new$Site)
field_source_pop_new$Species <- as.factor(field_source_pop_new$Species)
str(field_source_pop_new$Site)


# Field data 2021 ----
# Kluane Plateau height data 2021 
str(kp_2021)

# drop extra columns 
kp_2021_heights <-kp_2021 %>% 
  dplyr::select(-c(X, Notes, elevation, Bulk_11_aug, CAMERA, fulcrum_Plant_ID)) %>% 
  rename("Canopy_Height_cm" =  "Height..cm.", 
         "Latitude" = "LAT", 
         "Longitude" = "LONG",
         "Elevation" = "ELEVATION",
         "Species" = "species") %>% 
  mutate(SampleYear = "2021") %>% 
  mutate(Site = "Kluane")
# merge with other field data 
str(field_source_pop_new)
kp_2021_heights$SampleYear <- as.factor(kp_2021_heights$SampleYear)
field_source_pop_new$SampleYear <- as.factor(field_source_pop_new$SampleYear)
  
field_source_pop_new <- full_join(kp_2021_heights, field_source_pop_new, 
                                  by = c("Latitude", 
                                         "Longitude",
                                         "Elevation",
                                         "SampleYear",
                                         "Canopy_Height_cm",
                                         "Site", "Species"))
# save file 
write.csv(field_source_pop_new, 'data/source_pops/field_source_pop_new.csv')

# 3.4. Mother data -----
# only keeping relevant columns of mother data
mother_data <- Common_garden_2017 %>%
  dplyr::select(Sample_ID, Match, Sample_location, Date_sampled,
         Date_propagated, Date_planted, Year_planted, Mother_height,
         Mother_CW_1, Mother_CW_2, Mother_LS, Mother_LL1, Mother_LL2, 
         Mother_LL3, Mother_SE1, Mother_SE2, Mother_SE3, Cutting_length, 
         Cutting_diameter) %>%
  rename("SampleID" = "Sample_ID", 
         "Canopy_Height_cm" = "Mother_height",
         "Length_1_mm" = "Mother_LL1",
         "Length_2_mm" = "Mother_LL2",
         "Length_3_mm" = "Mother_LL3",
         "Stem_Elongation_1_mm" = "Mother_SE1",
         "Stem_Elongation_2_mm" = "Mother_SE2",
         "Stem_Elongation_3_mm" = "Mother_SE3",
         "Width_cm" = "Mother_CW_1",
         "Width_2_cm" = "Mother_CW_2", 
         "Site" = "Sample_location",
         "SampleDate" = "Date_sampled") %>% 
  filter(Canopy_Height_cm < 600) %>% 
  filter(Width_cm < 700)

str(mother_data)

# Making variables right format
mother_data$Width_cm <- as.numeric(mother_data$Width_cm)
mother_data$Width_2_cm <- as.numeric(mother_data$Width_2_cm)
mother_data$Stem_Elongation_3_mm <- as.numeric(mother_data$Stem_Elongation_3_mm)
mother_data$Site <- as.factor(mother_data$Site)
# mother_data$SampleDate <- as.Date(mother_data$SampleDate, format = "%d/%m/%Y")
mother_data$SampleDate <- as.POSIXct(mother_data$SampleDate, format = "%d/%m/%Y")
mother_data$Date_planted <- as.POSIXct(mother_data$Date_planted, format = "%d/%m/%Y")
mother_data$Date_propagated <- as.Date(mother_data$Date_propagated, format="%d/%m/%Y")
mother_data$Date_planted <- as.Date(mother_data$Date_planted, format="%d/%m/%Y")
mother_data$Cutting_diameter<- as.numeric(mother_data$Cutting_diameter)
mother_data$Cutting_length <- as.numeric(mother_data$Cutting_length)

# function to convert "0017" to "2017"
two_dig_year_cnvt <- function(z, year=2013){
  y <- as.numeric(format(z, '%Y'))
  range <- 2013 - 2000
  year(z) <- ifelse(y >= 0 & y <= range, 2000 + y, 
                    ifelse(y > range & y <= 200, 2000 + y, y))
  z
}

# converting "0017" to "2017" for SampleDate, DatePlanted 
mother_data$SampleDate <- two_dig_year_cnvt(mother_data$SampleDate)
mother_data$Date_propagated <- two_dig_year_cnvt(mother_data$Date_propagated)
mother_data$Date_planted <- two_dig_year_cnvt(mother_data$Date_planted)
unique(mother_data$SampleDate)
# renaming site col
mother_data <- mother_data %>%
  rename("SampleSite" = "Site") 

# making site column
mother_data <- mother_data %>%
  mutate(Site = case_when(SampleSite %in% c("Kluane", "Kluane Plateau", "Pika Camp", "Printers Pass") ~ 'Kluane', 
                          SampleSite %in% c("Qikiqtaruk","QHI") ~ 'Qikiqtaruk'))

# site as factor
mother_data$Site <- as.factor(mother_data$Site)
str(mother_data)

# Making means cols
mother_data <- mother_data %>%
  mutate(SampleYear = format(as.Date(SampleDate, format="%d/%m/%Y"),"%Y"), 
         mean_stem_elong = ((Stem_Elongation_1_mm + Stem_Elongation_2_mm + Stem_Elongation_3_mm)/3), 
       mean_leaf_length = ((Length_1_mm + Length_2_mm + Length_3_mm)/3),
       mean_width = ((Width_cm + Width_2_cm)/2)) 

# year as numeric
mother_data$SampleYear <- as.numeric(mother_data$SampleYear)

# to filter out Betula nana from mother_data 
mother_data <-  mother_data[!grepl(c("b"), mother_data$SampleID),,drop = FALSE] # any b in sample id is for betula
mother_data <-  mother_data[!grepl(c("BN"), mother_data$SampleID),,drop = FALSE] # same as above but with uppercase B, must use BN because one clone is called B

# make standard ID column for maternal data 
mother_data$SampleID_standard <- toupper(mother_data$SampleID) # make all uppercase characters 
mother_data$SampleID_standard<-gsub("-","",as.character(mother_data$SampleID_standard)) # remove "-"
mother_data$SampleID_standard<-gsub(" ","",as.character(mother_data$SampleID_standard)) # remove spaces " " 

# save mother data 
write.csv(mother_data, 'data/source_pops/mother_data.csv')


# 3.5. Merging source pop plus mother ----
# Merging wrangled versions of salix_field_data, all_source_pop_2022, common_garden_2017
field_source_pop_new <- read_csv("data/source_pops/field_source_pop_new.csv")
all_source_pop_2022 <- read_csv("data/source_pops/all_source_pop_2022.csv")
mother_data <- read_csv("data/source_pops/mother_data.csv")

all_source_pop_plus_mother <- bind_rows(field_source_pop_new, all_source_pop_2022,
                        mother_data)
# formatting variables
str(all_source_pop_plus_mother$SampleDate)
unique(all_source_pop_plus_mother$SampleDate)
all_source_pop_plus_mother$SampleDate <- format(as.POSIXct(all_source_pop_plus_mother$SampleDate,
                                                          format='%Y/%m/%d %H:%M:%S'),format='%d/%m/%Y')

# making a year column
all_source_pop_plus_mother <- all_source_pop_plus_mother %>%
  mutate(SampleYear = format(as.Date(SampleDate, format="%d/%m/%Y"),"%Y")) %>%
  dplyr::select(- `2022 Notes`)
unique(all_source_pop_plus_mother$SampleDate)
# variables in right format
str(all_source_pop_plus_mother)
all_source_pop_plus_mother$Species <- as.factor(all_source_pop_plus_mother$Species)
all_source_pop_plus_mother$Site <- as.factor(all_source_pop_plus_mother$Site)
all_source_pop_plus_mother$SampleDate <- as.Date(all_source_pop_plus_mother$SampleDate, format="%d/%m/%Y")
all_source_pop_plus_mother$SampleID <- as.factor(all_source_pop_plus_mother$SampleID)
all_source_pop_plus_mother$Stem_diameter <- as.numeric(all_source_pop_plus_mother$Stem_diameter)
all_source_pop_plus_mother$Date_propagated <- as.Date(all_source_pop_plus_mother$Date_propagated, format="%d/%m/%Y")
all_source_pop_plus_mother$Date_planted <- as.Date(all_source_pop_plus_mother$Date_planted, format="%d/%m/%Y")
all_source_pop_plus_mother$Cutting_diameter <- as.numeric(all_source_pop_plus_mother$Cutting_diameter)
all_source_pop_plus_mother$SampleYear<- as.numeric(all_source_pop_plus_mother$SampleYear)

# are SampleSite and Site the same? 
all_source_pop_plus_mother$SampleSite <- as.factor(all_source_pop_plus_mother$SampleSite)
identical(all_source_pop_plus_mother[['SampleSite']],all_source_pop_plus_mother[['Site']])
all(all_source_pop_plus_mother$SampleSite == all_source_pop_plus_mother$Site)
# seems like no 
unique(all_source_pop_plus_mother$Site) # SampleSite has more site specific info 
unique(all_source_pop_plus_mother$SampleSite) # SampleSite has more site specific info 
# omitting next lines to preserve info in SampleSite 
# all_source_pop_plus_mother <- all_source_pop_plus_mother %>%
#  mutate(Site = case_when(SampleSite %in% c("Kluane", "Kluane Plateau", "Pika Camp", "Printers Pass") ~ 'Kluane', 
#                          SampleSite %in% c("Qikiqtaruk"," QHI") ~ 'Qikiqtaruk'))
# formatting site columns
# all_source_pop_plus_mother <- all_source_pop_plus_mother %>%
#  rename("SampleSite" = "Site") 
        
# making variables right format
unique(all_source_pop_plus_mother$Site)
all_source_pop_plus_mother$Site <- as.factor(all_source_pop_plus_mother$Site)
all_source_pop_plus_mother$SampleID <- as.factor(all_source_pop_plus_mother$SampleID)

# only keeping mid july dates for Kluane 2022
kluane_mid_july_2022 <- all_source_pop_plus_mother %>%
  group_by(Site, SampleYear) %>%
  filter(SampleDate == "2022-07-16")%>%
  ungroup()

# variables in right format
kluane_mid_july_2022$Date_propagated <- as.Date(kluane_mid_july_2022$Date_propagated, format="%d/%m/%Y")
kluane_mid_july_2022$Date_planted <- as.Date(kluane_mid_july_2022$Date_planted, format="%d/%m/%Y")
str(kluane_mid_july_2022)

# only keeping mid july dates for QHI 2022
QHI_mid_july_2022_a <- all_source_pop_plus_mother %>%
  group_by(Site, SampleYear) %>%
  filter(between(SampleDate, as.Date("2022-07-19"), as.Date("2022-07-21")))%>%
  distinct() %>%
  ungroup() 

QHI_mid_july_2022_b <- all_source_pop_plus_mother %>%
  group_by(Site, SampleYear, SampleID) %>%
  filter(SampleDate == "2022-07-25" & 
          SampleID %in% c("HESP07", "HESP08", "HESP09", "HESP10")) %>%
  distinct() %>%
  ungroup() 

# merging the july subsets of QHI
QHI_mid_july_2022 <- rbind(QHI_mid_july_2022_a, QHI_mid_july_2022_b)

# formatting dates
QHI_mid_july_2022$Date_propagated <- as.Date(QHI_mid_july_2022$Date_propagated, format="%d/%m/%Y")
QHI_mid_july_2022$Date_planted <- as.Date(QHI_mid_july_2022$Date_planted, format="%d/%m/%Y")

# re-merge with mother data
july_source_pop_plus_mother <- bind_rows(kluane_mid_july_2022, QHI_mid_july_2022, 
                                         mother_data, field_source_pop_new)
# checking dataset has kluane 2022
test <- july_source_pop_plus_mother %>%
  filter(SampleYear == "2022")
view(july_source_pop_plus_mother)
# formatting variables
july_source_pop_plus_mother$Species <- as.factor(july_source_pop_plus_mother$Species)
july_source_pop_plus_mother$Site <- as.factor(july_source_pop_plus_mother$Site)
july_source_pop_plus_mother$SampleID <- as.factor(july_source_pop_plus_mother$SampleID)
unique(july_source_pop_plus_mother$SampleID)
unique(july_source_pop_plus_mother$SampleDate)
length(unique(july_source_pop_plus_mother$SampleID))

# Saving july source population heights 2017-2022 data as csv file
write.csv(july_source_pop_plus_mother, 'data/source_pops/july_source_pop_plus_mother.csv')

# 3.6. Matching SampleIDs with species -----
# extract species and cg sample_ids to match to maternal data because some don't have species 
unique(july_source_pop_plus_mother$Species) # contains NAs because of mother_data 
unique(july_source_pop_plus_mother$SampleID)
# how many NAs for species? 
sum(is.na(july_source_pop_plus_mother$Species)) # 879, uh-oh that's most of them
length(unique(july_source_pop_plus_mother$SampleID)) # how many unique sampleIDs # #930 

# major issue here is a lack of consistency between uppercase, lowercase, dashes, etc. 
# pull sample_ids and species list from common garden samples 
cg_ids <- all_merged_data_2022 %>% 
  dplyr::select(SampleID, Species) %>% 
  distinct(SampleID, Species) %>% # keep each sample ID once 
  rename("Species2" = "Species") %>% # for matching 
  rename("SampleID_standard" = "SampleID")

# make all sample ids upper case 
cg_ids$SampleID_standard <- toupper(cg_ids$SampleID_standard) 
july_source_pop_plus_mother$SampleID <-toupper(july_source_pop_plus_mother$SampleID)

# make standard SampleID column for july_source_pop_plus_mother IDs 
# aka remove spaces and dashes and use uppercase letters 
july_source_pop_plus_mother$SampleID_standard<-gsub("-","",as.character(july_source_pop_plus_mother$SampleID)) # remove "-"
july_source_pop_plus_mother$SampleID_standard<-gsub(" ","",as.character(july_source_pop_plus_mother$SampleID_standard)) # remove spaces " " 

# calling this unique source mother working for merges 
unique_source_mother_working <- left_join(july_source_pop_plus_mother, cg_ids, by = c("SampleID_standard"))

unique_source_mother_working_merge <- unique_source_mother_working %>% 
  mutate(spp = case_when(Species == "Salix arctica" | Species2 == "Salix arctica" ~ "Salix arctica",
                         Species == "Salix pulchra" | Species2 == "Salix pulchra" ~ "Salix pulchra", 
                         Species == "Salix richardsonii" | Species2 == "Salix richardsonii" ~ "Salix richardsonii"))

sum(is.na(unique_source_mother_working_merge$spp)) # 101, 
unique_source_mother_working_merge$spp_test <- ifelse(grepl("SA", unique_source_mother_working_merge$SampleID_standard), "Salix arctica",
                                  ifelse(grepl("SR", unique_source_mother_working_merge$SampleID_standard), "Salix richardsonii", 
                                         ifelse(grepl("SP", unique_source_mother_working_merge$SampleID_standard), "Salix pulchra", NA)))

unique_source_mother_check <- unique_source_mother_working_merge %>% 
  mutate(spp_3 = case_when(spp == "Salix arctica" | spp_test == "Salix arctica" ~ "Salix arctica",
                           spp == "Salix pulchra" | spp_test == "Salix pulchra" ~ "Salix pulchra", 
                           spp == "Salix richardsonii" | spp_test == "Salix richardsonii" ~ "Salix richardsonii"))
sum(is.na(unique_source_mother_check$spp_3)) # 0 baby! 

# drop old Species columns and rename spp_3 column to Species
unique_source_mother <- unique_source_mother_check %>% 
  dplyr::select(-c(spp_test, Species, Species2, spp)) %>% 
  rename("Species" = "spp_3")
# check to make sure we still have no NAs for species 
sum(is.na(unique_source_mother$Species)) # 0, success! 

# investigating strange arctica values 
unique_source_mother_tall_arcticas <- unique_source_mother %>%
  filter(Species == "Salix arctica")
# tall arcticas: no arcticas above 25 cm according to literature
# Weird heights 34.0, 30.0, 28.1, 26.0, 25.0
# IDs: HE16SA1, K15PSA11, K15PSA12, K15PSA6,K15PSA9
# Weird widths (above 58, from previous years max values) IDs: KP16SA19, PC16CESA5,
# PC16PVSA3, PC16PVSA2,KP16SA10,PC17SA5,KP16SA7,
# KP16SA2,PC17SA6,PC17SA1, HE16SA9,HE16SA3, PC17SA10, HE16SA5,PC16PVSA4

# removing TALL heights values from the main dataframe using their sample IDs
unique_source_mother_edit_1_1 <- unique_source_mother[ !(unique_source_mother$SampleID_standard %in% 
                                                           c("HE16SA1", "K15PSA11", "K15PSA12", 
                                                             "K15PSA6","K15PSA9")), ]

unique_source_mother_edit_1_2 <- unique_source_mother %>%
  subset(SampleID_standard  %in% 
           c("HE16SA1", "K15PSA11", "K15PSA12", 
                                    "K15PSA6","K15PSA9"))%>%
  mutate(Canopy_Height_cm = Canopy_Height_cm/10) # convert to cm by dividing by 10

# code below to filter values out: not using it!
# unique_source_mother_edit_2 <- unique_source_mother %>%
 #filter(Species == "Salix arctica") %>%
 #subset(Canopy_Height_cm <= 25.0) %>% # based on literature
 #subset(mean_stem_elong <= 29.0) # based on mean max values from previous years

# remerge all data
unique_source_mother <- rbind(unique_source_mother_edit_1_1, 
                              unique_source_mother_edit_1_2)

view(unique_source_mother) # all goood

# removing WIDE width values from the main dataframe using their sample IDs
unique_source_mother_edit_1_3 <- unique_source_mother[ !(unique_source_mother$SampleID_standard %in% 
                                                           c("KP16SA19", "PC16CESA5",
                                                             "PC16PVSA3", "PC16PVSA2","KP16SA10","PC17SA5","KP16SA7",
                                                             "KP16SA2","PC17SA6","PC17SA1", "HE16SA9","HE16SA3", "PC17SA10", "HE16SA5","PC16PVSA4"
                                                           )), ]

unique_source_mother_edit_1_4 <- unique_source_mother %>%
  subset(SampleID_standard  %in% 
           c("KP16SA19", "PC16CESA5",
              "PC16PVSA3", "PC16PVSA2","KP16SA10","PC17SA5","KP16SA7",
              "KP16SA2","PC17SA6","PC17SA1", "HE16SA9","HE16SA3", "PC17SA10", "HE16SA5","PC16PVSA4")) %>%
  mutate(mean_width = mean_width/10) # convert to cm by dividing by 10

# remerge all data
unique_source_mother <- rbind(unique_source_mother_edit_1_3, 
                              unique_source_mother_edit_1_4)


# Saving all source population heights 2017-2022 data as csv file
write.csv(unique_source_mother, 'data/source_pops/unique_source_mother.csv')
view(unique_source_mother)
# 3.7.1. Merge source / mother / common garden data ----
# load files 
unique_source_mother <- read.csv('data/source_pops/unique_source_mother.csv') # all mother and source (growth, not traits -- that's a bigger problem to do after)
all_cg_data_2022 <-  read.csv('data/common_garden_data_2022/all_cg_data_2022.csv') # all CG (one point per year)

str(unique_source_mother)
str(all_cg_data_2022) # I'll convert variables classes after merging 
# change Site column to be Common_garden, instead of source pop location 
all_cg_data_2022_merge <- all_cg_data_2022 %>% 
  dplyr::select(-SampleID, -X) %>% 
  dplyr::mutate(Site = "Common_garden") 

unique_source_mother_merge <- unique_source_mother %>% 
  dplyr::select(-c(SampleID, Match, X, SampleSite)) %>% 
  mutate(population = case_when(Site == "Kluane" ~ "source_south", 
                                Site == "Qikiqtaruk" ~ "source_north" ))  # add population column to indicate source north or south
  
all_CG_source_growth <- full_join(all_cg_data_2022_merge, unique_source_mother_merge, 
                        by = c("Species", "Site", 
                               "Sample_Date" = "SampleDate", 
                               "Year" = "SampleYear",  
                               "SampleID_standard", "population", "Year_planted", 
                               "Width_cm", "Canopy_Height_cm", "Width_2_cm", 
                               "Stem_diameter", "Stem_Elongation_1_mm", "Stem_Elongation_2_mm", 
                               "Stem_Elongation_3_mm", "Length_1_mm", "Length_2_mm", 
                               "Length_3_mm", "mean_stem_elong", "mean_leaf_length", "mean_width"
                               ))
# checking variables
str(all_CG_source_growth)
unique(all_CG_source_growth$Site) # "Common_garden" "Kluane" "Qikiqtaruk"
unique(all_CG_source_growth$population) # "Northern"     "Southern"     "source_south" "source_north"

# reclassing variables
all_CG_source_growth$Site <- as.factor(all_CG_source_growth$Site)
all_CG_source_growth$population <- as.factor(all_CG_source_growth$population)
all_CG_source_growth$Species <- as.factor(all_CG_source_growth$Species)

# check heights of arctica in sources 
# QHI 
all_CG_source_growth_arctica_QHI <- all_CG_source_growth %>%
  filter(population == "source_north" & Species == "Salix arctica")
# view(all_CG_source_growth_arctica_QHI)
range(all_CG_source_growth_arctica_QHI$Canopy_Height_cm)
# 2.5 - 19.8 cm all good
#tall_arctica_QHI <- all_CG_source_growth_arctica_QHI %>%
 # filter(Canopy_Height_cm > 25) # filtering anything above 25cm as arcticas are rarely taller than that
# view(tall_arctica_QHI) # 0! 

# KP
all_CG_source_growth_arctica_KP <- all_CG_source_growth %>%
  filter(population == "source_south" & Species == "Salix arctica")
# view(all_CG_source_growth_arctica_KP)
range(all_CG_source_growth_arctica_KP$Canopy_Height_cm)
# 1.9 23.5 cm all good
#tall_arctica_KP <- all_CG_source_growth_arctica_KP %>%
 # filter(Canopy_Height_cm > 25) # filtering anything above 25cm as arcticas are rarely taller than that
#view(tall_arctica_KP) # 0! 

# saving data as csv
# write.csv(all_CG_source_growth, 'data/all_CG_source_growth.csv')

# 3.7.2. Merge traits from cg with source and mother data ----
# load data 
# SLA, LDMC, LA: 
all_source_area_traits <- read.csv("data/source_pops/all_source_area_traits.csv")
cg_sla <- read.csv("data/common_garden_data_2022/cg_sla_2022.csv")
traits_2017 <- read.csv("data/common_garden_data_2021/all_growth_2021.csv")

traits_2017$SampleID_standard <- toupper(traits_2017$SampleID) # make all uppercase characters 
traits_2017$SampleID_standard<-gsub("-","",as.character(traits_2017$SampleID_standard)) # remove "-"
traits_2017$SampleID_standard<-gsub(" ","",as.character(traits_2017$SampleID_standard)) # remove spaces " " 

traits_2017_merge <- traits_2017 %>% 
  dplyr::select(LA, SLA, LDMC, SampleID_standard, Species, Sample_Date, Fresh_mass, Dry_mass) %>% 
  filter(Species %in% c("Salix arctica", "Salix pulchra", "Salix richardsonii")) %>% 
  mutate(population = case_when(startsWith(as.character(SampleID_standard), "H") ~ "Northern",
                                TRUE ~ "Southern")) %>% 
  mutate(Site = "Common_garden") %>% 
  filter(SLA < 35)  # filter out NAs

# make month, day, year columns for common garden data 
traits_2017_merge$year <-  format(as.Date(traits_2017_merge$Sample_Date, format="%d/%m/%Y"),"%Y")
traits_2017_merge$month <-  format(as.Date(traits_2017_merge$Sample_Date, format="%d/%m/%Y"),"%m")
traits_2017_merge$DOY <-  lubridate::yday(as.POSIXct(traits_2017_merge$Sample_Date, format = "%Y-%m-%d"))
traits_2017_merge$DOY <- as.factor(traits_2017_merge$DOY)
traits_2017_merge$year <- as.factor(traits_2017_merge$year)

traits_2017_merge_1 <- traits_2017_merge %>% 
  filter(month == "07") %>% # keep only July values
  group_by(SampleID_standard) %>% # take mean of july values 
  mutate(mean_SLA = mean(SLA), 
         mean_LA = mean(LA), 
         mean_LDMC = mean(LDMC), 
         mean_Fresh_mass = mean(Fresh_mass), 
         mean_Dry_mass = mean(Dry_mass)) %>% 
  ungroup() 
traits_2017_merge_2 <- traits_2017_merge_1 %>% 
  dplyr::select(-c(SLA, LA, LDMC, Fresh_mass, Dry_mass)) %>% 
  dplyr::rename(SLA = mean_SLA, LA = mean_LA, LDMC = mean_LDMC, 
         Fresh_mass = mean_Fresh_mass, Dry_mass = mean_Dry_mass) %>% 
  distinct()

str(all_source_area_traits)
str(cg_sla)

# make month, day, year columns for common garden data 
cg_sla$year <-  format(as.Date(cg_sla$date_sampled, format="%Y-%m-%d"),"%Y")
cg_sla$month <-  format(as.Date(cg_sla$date_sampled, format="%Y-%m-%d"),"%m")
cg_sla$DOY <-  lubridate::yday(as.POSIXct(cg_sla$date_sampled, format = "%Y-%m-%d"))
# reclass variables after merge, but need to fix date ones first 
cg_sla$DOY <- as.factor(cg_sla$DOY)
all_source_area_traits$DOY <- as.factor(all_source_area_traits$DOY)
cg_sla$year <- as.factor(cg_sla$year)
all_source_area_traits$year <- as.factor(all_source_area_traits$year)

# make population column for cg_sla aka northern = QHI, southern = Kluane 
# make site column to be common_garden instead of Kluane v QHI
cg_sla_merge <- cg_sla %>% 
  mutate(population = case_when(Source == "Kluane" ~ "Southern", 
                                Source == "Qikiqtaruk" ~ "Northern")) %>% 
  mutate(LDMC_g_g = LDMC/1000) %>% # covert LDMC from mg g-1 to g g-1
  mutate(Site = "Common_garden") %>% 
  mutate(LA = LA*100) %>%  #convert leaf area to mm2 instead of cm2
  dplyr::select(-c(X, Source, LDMC, number_leaves, dried_leaf_sample_remarks, 
            rehydrated_leaf_sample_remarks)) %>% 
  filter(month == "06") #only keep june 2021 and 2022 observations

all_source_area_traits_merge <- all_source_area_traits %>% 
  dplyr::select(-c(X, number_leaves, ValueKindName, Data_coPIs, 
            Data_contributor, Species_sex, Seed_mass_dry_mg, 
            Plant_height_veg_m, Plant_height_repro_m)) %>% 
  mutate(population = case_when(Site == "Kluane" ~ "source_south", 
                                Site == "Qikiqtaruk" ~ "source_north" ))  # add population column to indicate source north or south
# initial merge
all_CG_traits <- full_join(cg_sla_merge, traits_2017_merge_2, 
                                  by = c("Species", "LDMC_g_g" = "LDMC", 
                                         "plant_tag_id" = "SampleID_standard", 
                                         "SLA", "LA", "population", 
                                         "DOY", "year", "Site",
                                         "leaf_fresh_mass_g" = "Fresh_mass", 
                                         "month"))
# merge all 
all_CG_source_traits <- full_join(all_CG_traits, all_source_area_traits_merge, 
                              by = c("population", "Species", "LDMC_g_g", 
                                     "plant_tag_id", "sample_id", 
                                     "SLA", "LA", "Site",
                                     "leaf_mass_per_area_g_m2", "actual_leaf_dry_matter_content_perc", 
                                     "total_rehydrated_leaf_mass_g", "DOY", "year",
                                     "equivalent_water_thickness_cm", 
                                     "leaf_fresh_mass_g", 
                                     "date_sampled"))
# also seems to have worked
# reclassing
all_CG_source_traits$Species <- as.factor(all_CG_source_traits$Species)
all_CG_source_traits$Site <- as.factor(all_CG_source_traits$Site)
all_CG_source_traits$population <- as.factor(all_CG_source_traits$population)
unique(all_CG_source_traits$population) # Northern source_north source_south Southern
unique(all_CG_source_traits$Site) # Common_garden Kluane Qikiqtaruk
unique(all_CG_source_traits$Species) # Salix arctica Salix pulchra Salix richardsonii

# save 
# write.csv(all_CG_source_traits, "data/all_CG_source_traits.csv")

# 3.8 Merge all data ----
# merge ALL data (traits and growth) into one data frame 
all_CG_source_traits <- read.csv("data/all_CG_source_traits.csv") 
all_CG_source_growth <- read.csv("data/all_CG_source_growth.csv")

# standard id column for traits 
all_CG_source_traits$SampleID_standard <- toupper(all_CG_source_traits$plant_tag_id) # make all uppercase characters 
all_CG_source_traits$SampleID_standard<-gsub("-","",as.character(all_CG_source_traits$SampleID_standard)) # remove "-"
all_CG_source_traits$SampleID_standard<-gsub(" ","",as.character(all_CG_source_traits$SampleID_standard)) # remove spaces " " 
# get rid of date columns 
all_CG_source_traits_merge <- all_CG_source_traits %>% 
  dplyr::select(-c(month, DOY, Sample_Date, date_sampled, sample_id, site_id, X, MONTH, date, plant_tag_id)) %>% 
  dplyr::rename("Year" = "year")
all_CG_source_growth_merge <- all_CG_source_growth %>% 
  dplyr::select(-c(Month, Sample_Date, Day, X, X.5, X.2, X.1, X.3, X.4))

all_CG_source_growth_merge$Year <- as.factor(all_CG_source_growth_merge$Year)
all_CG_source_traits_merge$Year <- as.factor(all_CG_source_traits_merge$Year)


all_data <- full_join(all_CG_source_growth_merge, all_CG_source_traits_merge, 
                         by = c( "Site",
                                "population", "Species", 
                                "Year", 
                                "Elevation" = "Elevation_m",
                                "SampleID_standard",
                                "Latitude" = "Lat",
                                "Longitude" = "Lon"))
NROW(na.omit(all_CG_source_traits$SLA))
NROW(na.omit(all_cg_data$SLA))
length(unique(all_CG_source_traits$SLA))
# problem because some now are repeated 
# only merging CG data now 
all_cg_data_2022 <-  read.csv('data/common_garden_data_2022/all_cg_data_2022.csv') # all CG (one point per year)
all_cg_data_2022_merge <- all_cg_data_2022 %>% 
  dplyr::select(-c(Month, Sample_Date, Day, X, X.1))

str(all_cg_data_2022_merge)
all_cg_data_2022_merge$Year <- as.factor(all_cg_data_2022_merge$Year)
str(all_CG_source_traits_merge)

all_cg_data_merge <- full_join(all_cg_data_2022_merge, all_CG_source_traits_merge, 
                      by = c( "Site",
                              "population", "Species", 
                              "Year", "SampleID_standard")) # Erica: added the sampleId to merge too

all_cg_data <- all_cg_data_merge %>% 
  dplyr::filter(population %in% c("Northern", "Southern"))
# save 
write.csv(all_cg_data, "data/all_cg_growth_traits_data.csv")

# 3.9 Max heights and widths ---- 

# 3.9.1 Common garden max heights and widths  -----
# finding max height values from common garden over the years 
all_cg_data_2022 <-  read.csv('data/common_garden_data_2022/all_cg_data_2022.csv') # all CG (one point per year)

# check how many unique sample IDs 
length(unique(all_cg_data_2022$SampleID_standard)) # 827

# out of curiosity how many of these are NAs?
sum(is.na(all_cg_data_2022$Canopy_Height_cm)) # 1755 wow, quite a few 

# drop unnecessary columns 
max_cg_extractions <-  all_cg_data_2022 %>% 
  dplyr::select(-c(SampleID))

# extract max value for height per sample bc samples have been trimmed over the years 
max_cg_heights <- all_cg_data_2022 %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(Canopy_Height_cm)) %>% 
  rename("max_canopy_height_cm" = "Canopy_Height_cm")

# do same for widths (use average width value)
max_cg_widths <- all_cg_data_2022 %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(mean_width)) %>% 
  rename("max_mean_width_cm" = "mean_width")

# save 
write.csv(max_cg_heights, "data/common_garden_data_2022/max_heights_cg.csv")
write.csv(max_cg_widths, "data/common_garden_data_2022/max_widths_cg.csv")
max_cg_widths <- read_csv("data/common_garden_data_2022/max_widths_cg.csv")
max_cg_heights <- read_csv("data/common_garden_data_2022/max_heights_cg.csv")

# reclassing Species as factor
max_cg_heights$Species <- as.factor(max_cg_heights$Species)
max_cg_widths$Species <- as.factor(max_cg_widths$Species)
class(max_cg_heights$max_canopy_height_cm)

# mean max height per population and species
max_cg_heights_spp <- max_cg_heights %>%
 group_by(population,Species) %>%
  summarise(mean_max_height_cm = mean(max_canopy_height_cm))
range(max_cg_heights_spp$mean_max_height_cm)# 4.263636 52.472549

# mean max width per population and species
max_cg_width_spp <- max_cg_widths %>%
  group_by(population,Species) %>%
  summarise(mean_max_width_cm = mean(max_mean_width_cm))
range(max_cg_width_spp$mean_max_width_cm)# 10.75625 67.72791

# 3.9.2 Source populations max heights and widths -----
# extract max value for height per sample bc samples have been trimmed over the years 
unique_source_mother <- read_csv("data/source_pops/unique_source_mother.csv")

max_source_mother_heights <- unique_source_mother %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(Canopy_Height_cm)) %>% 
  rename("max_canopy_height_cm" = "Canopy_Height_cm")

# do same for widths (use average width value)
max_source_mother_widths<- unique_source_mother %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(mean_width)) %>% 
  rename("max_mean_width_cm" = "mean_width")

# do same for stem elongation (use average value)
max_source_mother_stem_elong <- unique_source_mother %>% 
  group_by(SampleID_standard) %>%
  slice(which.max(mean_stem_elong)) %>% 
  rename("max_mean_stem_elong_cm" = "mean_stem_elong")

# save 
write.csv(max_source_mother_heights, "data/source_pops/max_source_mother_heights.csv")
write.csv(max_source_mother_widths, "data/source_pops/max_source_mother_widths.csv")

# reclassing Species as factor
max_source_mother_heights$Species <- as.factor(max_source_mother_heights$Species)
max_source_mother_widths$Species <- as.factor(max_source_mother_widths$Species)
max_source_mother_heights$Site <- as.factor(max_source_mother_heights$Site)
max_source_mother_widths$Site <- as.factor(max_source_mother_widths$Site)
max_source_mother_stem_elong$Species <- as.factor(max_source_mother_stem_elong$Species)
max_source_mother_stem_elong$Site <- as.factor(max_source_mother_stem_elong$Site)

# mean max height per population and species
max_source_mother_heights_spp <- max_source_mother_heights %>%
  group_by(Site, Species) %>%
  summarise(mean_max_height_cm = mean(max_canopy_height_cm))

# mean max width per population and species
max_source_mother_width_spp <- max_source_mother_widths %>%
  group_by(Site,Species) %>%
  summarise(mean_max_width_cm = mean(max_mean_width_cm))
# Kluane Salix arctica 58.1 
# Qikiqtaruk Salix arctica 45.55

# mean max elong per population and species
max_source_mother_stem_elong_spp <- max_source_mother_stem_elong %>%
  group_by(Site,Species) %>%
  summarise(mean_max_stem_elong_cm = mean(max_mean_stem_elong_cm))
# Qikiqtaruk Salix arctica 29.16506
# no data for stem elong arctica in Kluane

# 4. DATA VISUALISATION ----

# 4.1. COMMON GARDEN ----

# a. Canopy height(2013-2022) ----
(plot_canopy_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
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

# b. Stem elongation (2013-2022) ----
(plot_stem_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y= mean_stem_elong, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= mean_stem_elong, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Elongation (mm)") +
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

# c. Stem diameter (2013-2022) ----
(plot_diameter_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y = Stem_diameter, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= Stem_diameter, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
    ylab("Stem diameter (mm)") +
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
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# d. Shrub width (2013-2022) ----
(plot_width_2022 <- ggplot(all_merged_data_2022) +
    geom_boxplot(aes(x= Site, y = mean_width, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Width (cm)") +
    xlab("") +
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
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# Facet
facet_traits <- grid.arrange(plot_canopy_2022, plot_stem_2022, plot_diameter_2022, plot_width_2022, ncol=2)

# e. Leaf length (2013-2022) boxplot ----

(plot_leaf_2022 <- ggplot(all_merged_data_2022) +
    geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
    ylab("Leaf Length (mm)") +
    xlab("") +
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
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# e.1. Leaf length scatter (2013-2022) -----
(plot_leaf_scatter_2022 <- ggplot(all_merged_data_2022) +
    geom_smooth(aes(x = Sample_age, y = mean_leaf_length, colour = Site, fill = Site, group = Site, method = "glm")) +
    geom_point(aes(x = Sample_age, y= mean_leaf_length, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Leaf Length (mm)") +
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
  
# f. Leaf length (2022) ----
data_leaf_2022 <- all_merged_data_2022 %>% filter(Year == 2022)

(plot_leaf_2022_only <- ggplot(data_leaf_2022) +
    geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Leaf Length (mm)") +
    xlab("") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# ggsave("scripts/common_garden/common_garden_figures/plot_leaf_2022_only.png", width = 28, height = 20, units = "cm", dpi = 100, plot = plot_leaf_2022_only)

# g. Biovolume (2013-2022) ----
(plot_biovol_2022 <- ggplot(all_merged_data_2022) +
   geom_boxplot(aes(x = Site, y = biovolume, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Biovolume (cm3)") +
   xlab("") +
   scale_colour_viridis_d(begin = 0.3, end = 0.9) +
   scale_fill_viridis_d(begin = 0.3, end = 0.9) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

