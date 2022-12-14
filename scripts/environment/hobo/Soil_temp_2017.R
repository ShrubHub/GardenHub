#### SOIL TEMPERATURE (2015-2017) SCRIPT
### Data wrangling and visualisation script
### By Erica Zaja, created on 31/10/2022
## Last updated: 31/10/2022

# 1. LOADING LIBRARIES -----
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)

# 2. LOADING DATA ----

# read in soil temperature data (2015-2017)
Formatted_temps <- read_csv("data/hobo/Formatted_temps.csv")

# read in metadata
Metadata <- read_excel("data/hobo/Metadata.xlsx")

# 3. DATA WRANGLING ----

# add coordinates to soil temp
soil_clim <- merge(Formatted_temps, Metadata,
                   by.x=c("Plotcode"), by.y=c("Plotcode"))

# Years of sampling
unique(soil_clim$Year)
# 2015 2016 2017

# Sensors used
unique(soil_clim$Sensor_used)
# "iButton" "HOBO"  

# Latitudes
unique(soil_clim$Latitude)
# there is a typo in Latitude (not 0.97134 but 60.97134)

# Plot codes
unique(soil_clim$Plotcode)

# make date column
soil_clim <- soil_clim %>% 
  mutate('Date' = make_date(year = Year, month = Month, day = Day))
         
str(soil_clim)

# Make sensor into factor
soil_clim$Sensor_used <- as.factor(soil_clim$Sensor_used)

# Make MONTHLY means 
soil_clim_means <- soil_clim %>%
  group_by(Sensor_used, Year, Month) %>%
  mutate(mean_temp = mean(Temperature)) %>%
  slice(1) %>% # keeping only one observation from each set of year/month
  ungroup() %>%
  mutate(Site = case_when(Plotcode %in% c("QHI_phenology_plot", "HH_1", "HH_2") ~ "Qikiqtaruk",
                          Plotcode %in% c("PC16_1", "Kluane_Plateau_1", "Kluane_Plateau_2", 
                                          "Kluane_1") ~ "Kluane",
                          Plotcode == "Kluane_Lake_CG" ~ "Common Garden"))
                          
# Make site a factor 
soil_clim_means$Site <- as.factor(soil_clim_means$Site)
str(soil_clim_means$Site)

# 3. DATA VISUALISATION -----

# Plot 2 lines for different sensors
(plot_soil_temp <- ggplot(soil_clim_means, aes(x = Date, y = mean_temp, colour = Sensor_used)) +
   geom_line()+
   #geom_smooth(method="lm", colour = "black") + 
   ylab("Soil temperature (°C)") +
   xlab("Date") +
   theme_bw() +
   #scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# Plot data by site and by sensor used
soil_clim_means_ibutton <- soil_clim_means %>%
  filter(Sensor_used == "iButton")

soil_clim_means_HOBO <- soil_clim_means %>%
  filter(Sensor_used == "HOBO") # only common garden has HOBO


(plot_monthly_soil_temp <- ggplot(soil_clim_means, aes(x = Date, y = mean_temp, colour = Site)) +
    geom_line()+
    #geom_smooth(method="lm", colour = "black") + 
    ylab("Mean monthly soil temperature (°C)") +
    xlab("Date") +
    theme_bw() +
    #scale_color_manual(values = c("dark green", "blue", "red")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

