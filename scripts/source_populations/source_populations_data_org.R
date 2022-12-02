#### Source populations
# Exploration of southern (Kluane) and northern (Qikiqtaruk) willow populations 
# Madelaine 28/11/22
# last updated: 

# 1. libraries ---- 
library(ggplot2)
library(viridis)
library(lubridate)
library(tidyverse)
library(dplyr)


# 2. data ---- 
SLA_2022 <- read.csv("data/common_garden_data_2022/field_sla_2022.csv") # source pop data from 2021-22
source_traits_2017 <- read.csv("data/source_pops/Salix_field_trait_data.csv") # source population from 2013-2017

str(SLA_2022)
str(source_traits_2017) # why oh why are the dates so terrible 

# 3. data re-org 
# minor org on data frames 
# make separate columns for date  
SLA_2022$year <-  format(as.Date(SLA_2022$date_sampled, format="%Y-%m-%d"),"%Y")
SLA_2022$month <-  format(as.Date(SLA_2022$date_sampled, format="%Y-%m-%d"),"%m")
SLA_2022$DOY <-  yday(as.POSIXct(SLA_2022$date_sampled, format = "%Y-%m-%d"))

SLA_2022 <- SLA_2022 %>% 
  mutate(Site = recode(Site, "Kluane Plateau" = 'Kluane', "Qikiqtaruk" = 'Qikiqtaruk')) %>% 
  filter(month != "08")  %>%   #filter out august observations to avoid any senescence 
 mutate(total_leaf_dry_mass_mg = total_leaf_dry_mass_g*1000) %>% # convert leaf mass per area to mg instead of g 
  mutate(LDMC_g_g = LDMC/1000) %>% # covert LDMC from mg g-1 to g g-1
  dplyr::select(-c(X, LDMC, rehydrated_leaf_sample_remarks, total_leaf_dry_mass_g, sample_remarks, dried_leaf_sample_remarks))  

source_traits_2017 <- source_traits_2017 %>% 
  dplyr::select(-Treatment) # honestly date info in this sheet is such a nightmare, just focus on year
  
str(source_traits_2017)
str(SLA_2022)
SLA_2022$DOY <- as.character(SLA_2022$DOY) # converting to character to merge because source pop data is so messed up
SLA_2022$year <- as.integer(SLA_2022$year) # converting to same variable type for merging 

# 4. merge! ----
all_source_traits_2022 <- full_join(SLA_2022, source_traits_2017, by = c("Site" = "Site", 
                                                           "Species" = "Species", 
                                                           "DOY" = "DOY",
                                                           "year" = "Year_measured",
                                                           "LA" = "Leaf_area_mm2", 
                                                           "SLA" = "SLA_mm2_mg", 
                                                           "LDMC_g_g" = "LDMC_g_g", 
                                                           "total_leaf_dry_mass_mg" = "Leaf_dry_mass_mg", 
                                                           "leaf_fresh_mass_g" = "Leaf_fresh_mass_g", 
                                                           "site_id" = "Subsite_name"
                                                           )) 
str(all_source_traits_2022)

all_source_traits_2022$Site <- as.factor(all_source_traits_2022$Site)
all_source_traits_2022$Species <- as.factor(all_source_traits_2022$Species)
all_source_traits_2022$year <- as.factor(all_source_traits_2022$year)
all_source_traits_2022$month <- as.factor(all_source_traits_2022$month)


all_source_traits_2022$month_1 <-  format(as.Date(all_source_traits_2022$date, format="%d/%m/%Y"),"%m")
all_source_traits_2022$month_2 <- ifelse(grepl("Jul",all_source_traits_2022$date),"7",NA)

all_source_traits_2022 <- all_source_traits_2022 %>% 
  mutate(MONTH = case_when(month == "6" | month_1 == "06" ~ "6",
         month == "7" | month_2 == "7" | month_1 == "07" ~ "7", 
         month_1 == "08" ~ "8")) %>% 
  dplyr::select(-c(month_2, month_1, month))

# 5. save data ----
write.csv(all_source_traits_2022,'data/source_pops/all_source_area_traits.csv')

# load ! 
all_source_area_traits <- read.csv('data/source_pops/all_source_area_traits.csv')

# 6. plots ----

# data from all yeares 
(sla_all_source <- ggplot(all_source_area_traits) +
   geom_boxplot(aes(x = Site, y = SLA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   facet_grid(cols = vars(Species)) +
   ylab("SLA (mm2mg-1)") +
   xlab("Source population") +
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

# Leaf area 
(la_source_all <- ggplot(all_source_traits_2022) +
    geom_boxplot(aes(x = Site, y = LA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("LA") +
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

(ldmc_source_all <- ggplot(all_source_area_traits) +
    geom_boxplot(aes(x = Site, y = LDMC_g_g, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("LDMC") +
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

# only JULY plots ---- 
july_traits <-all_source_area_traits %>% 
  filter(MONTH == "7")

(sla_2022_source <- ggplot(july_traits) +
    geom_boxplot(aes(x = Site, y = SLA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("SLA (mm2mg-1)") +
    xlab("Source population") +
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


# only 2022 plots ====
# quick plot of ALL source populations SLA 
# field data collected 2021 for Kluane and 2022 for QHI
 # but, to be fair this includes senescing leaves, too 
 # samples in Klaune were collected 4-5  times over season - should probably pick a July value
 # samples in QHI were collected in July or senescing in Aug. 
(sla_2022_source <- ggplot(SLA_2022) +
    geom_boxplot(aes(x = Site, y = SLA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("SLA (mm2mg-1)") +
    xlab("Source population") +
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

# okay filter out to only include July values 
# need to still merge with old field data 

july_sla <- SLA_2022 %>% 
  filter(date_sampled %in% c("2022-07-26", "2022-07-25", "2021-07-06", "2021-07-19"))

(sla_source_july <- ggplot(july_sla) +
    geom_boxplot(aes(x = Site, y = SLA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("SLA (mm2mg-1)") +
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


# Leaf area 
(la_source_july <- ggplot(july_sla) +
    geom_boxplot(aes(x = Site, y = LA, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("LA") +
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

# Leaf dry matter content 
(ldmc_source_july <- ggplot(july_sla) +
    geom_boxplot(aes(x = Site, y = LDMC, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    facet_grid(cols = vars(Species)) +
    ylab("LDMC") +
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
