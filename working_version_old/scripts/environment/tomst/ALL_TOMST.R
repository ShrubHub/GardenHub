### ALL TOMST logger data from common garden (CG), Kluane Plateau (KP) and 
### Qikiqtaruk-Hershel Island (QHI) (2022)
### Script by Erica Zaja
### Last updated: 19/10/2022

# 1. LOADING DATA ----

# a. Surface temperature data ----
KP_mean_daily_temp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_temp.csv")
QHI_mean_daily_temp <- read_csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_temp.csv")
CG_mean_daily_temp <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_temp.csv")

# b. Above-surface temperature data ----
KP_mean_daily_top_sensor <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_top_sensor.csv")
QHI_mean_daily_top_sensor <- read_csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_top_sensor.csv")
CG_mean_daily_top_sensor <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_top_sensor.csv")

# c. Soil temperature data ----
KP_mean_daily_soil_temp <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_soil_temp.csv")
QHI_mean_daily_soil_temp <- read_csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_soil_temp.csv")
CG_mean_daily_soil_temp <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_soil_temp.csv")

# d. Soil moisture data ----
KP_mean_daily_soil_moist <- read_csv("data/tomst/Kluane_Plateau_TOMST_15August2022/KP_mean_daily_soil_moist.csv")
QHI_mean_daily_soil_moist <- read_csv("data/tomst/QHI_TOMST_August2022/QHI_mean_daily_soil_moist.csv")
CG_mean_daily_soil_moist <- read_csv("data/tomst/Common_garden_TOMST_17August2022/CG_mean_daily_soil_moist.csv")

# 2. DATA MANIPULATION -----

# Adding 'Site'

# Kluane Plateau (KP)
KP_mean_daily_temp <- KP_mean_daily_temp %>%
  mutate(Site = c("KP"))

KP_mean_daily_top_sensor <- KP_mean_daily_top_sensor %>%
  mutate(Site = c("KP"))

KP_mean_daily_soil_temp <- KP_mean_daily_soil_temp %>%
  mutate(Site = c("KP"))

KP_mean_daily_soil_moist <- KP_mean_daily_soil_moist %>%
  mutate(Site = c("KP"))

# Qikiqtaruk Island (QHI)
QHI_mean_daily_temp <- QHI_mean_daily_temp %>%
  mutate(Site = c("QHI"))

QHI_mean_daily_top_sensor <- QHI_mean_daily_top_sensor %>%
  mutate(Site = c("QHI"))

QHI_mean_daily_soil_temp <- QHI_mean_daily_soil_temp %>%
  mutate(Site = c("QHI"))

QHI_mean_daily_soil_moist <- QHI_mean_daily_soil_moist %>%
  mutate(Site = c("QHI"))

# Common Garden (CG)
CG_mean_daily_temp <- CG_mean_daily_temp %>%
  mutate(Site = c("CG"))

CG_mean_daily_top_sensor <- CG_mean_daily_top_sensor %>%
  mutate(Site = c("CG"))

CG_mean_daily_soil_temp <- CG_mean_daily_soil_temp %>%
  mutate(Site = c("CG"))

CG_mean_daily_soil_moist <- CG_mean_daily_soil_moist %>%
  mutate(Site = c("CG"))

# Merging data

# a. Surface temp 
all_mean_daily_temp <- rbind(KP_mean_daily_temp, QHI_mean_daily_temp, CG_mean_daily_temp)
all_mean_daily_temp$Site <- as.factor(all_mean_daily_temp$Site)
str(all_mean_daily_temp)

# a. Above-surface temp 
all_mean_daily_top_sensor <- rbind(KP_mean_daily_top_sensor, QHI_mean_daily_top_sensor, CG_mean_daily_top_sensor)
all_mean_daily_top_sensor$Site <- as.factor(all_mean_daily_top_sensor$Site)

# c. Soil temp
all_mean_daily_soil_temp <- rbind(KP_mean_daily_soil_temp, QHI_mean_daily_soil_temp, CG_mean_daily_soil_temp)
all_mean_daily_soil_temp$Site <- as.factor(all_mean_daily_soil_temp$Site)

# d. Soil moist
all_mean_daily_soil_moist <- rbind(KP_mean_daily_soil_moist, QHI_mean_daily_soil_moist, CG_mean_daily_soil_moist)
all_mean_daily_soil_moist$Site <- as.factor(all_mean_daily_soil_moist$Site)

# Mean summer air temperatures ----

# Mean summer temperature Kluane 2022
mean(CG_mean_daily_temp$mean_temp)
# 13.79942

# Mean summer temperature Kluane 2022
mean(KP_mean_daily_temp$mean_temp)
# 5.694995
range(KP_mean_daily_temp$mean_temp)
# -0.3227679 11.8287760

# Mean summer temperature QHI 2022 
mean(QHI_mean_daily_temp$mean_temp)
# 2.488281 12.311627
range(QHI_mean_daily_temp$mean_temp)

# 3. DATA VISUALISATION ----

# a. ALL surface temperature ----
(plot_all_mean_daily_temp <- ggplot(all_mean_daily_temp, aes(x = Date, y = mean_temp)) +
   geom_line(aes(color = Site)) + 
   ylab("Daily mean surface temperature (°C)") +
   xlab("\nDate (2022)") +
  theme_bw() +
   scale_color_manual(values = c("dark orange","purple", "green")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title = element_text(size=15), #change legend title font size
         legend.text = element_text(size=12),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))


# b. ALL above-surface temperature ----
(plot_all_mean_daily_top_sensor <- ggplot(all_mean_daily_top_sensor, aes(x = Date, y = mean_temp)) +
   geom_line(aes(color = Site)) + 
   ylab("Daily mean above-surface temperature (°C)") +
   xlab("Date (2022)") +
   theme_bw() +
    scale_color_manual(values = c("dark orange","purple", "green")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# c. ALL soil temperature ----
(plot_all_mean_daily_soil_temp <- ggplot(all_mean_daily_soil_temp, aes(x = Date, y = mean_temp)) +
   geom_line(aes(color = Site)) + 
   ylab("Daily mean soil temperature (°C)") +
   xlab("\nDate (2022)") +
   theme_bw() +
    scale_color_manual(values = c("dark orange","purple", "green")) +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size=15), #change legend title font size
          legend.text = element_text(size=12),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# d. ALL soil moist ----
(plot_all_mean_daily_soil_moist <- ggplot(all_mean_daily_soil_moist, aes(x = Date, y = mean_moist)) +
   geom_line(aes(color = Site)) + 
   ylab("Daily mean soil moisture ()") +
   xlab("Date (2022)") +
   theme_bw() +
   scale_color_manual(values = c("dark green", "blue", "red")) +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 18),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15, colour = "black"),
         axis.text.y = element_text(size = 15, colour = "black")))

# PANEL ----
facet_ALL_env <- grid.arrange(plot_all_mean_daily_soil_moist, plot_all_mean_daily_soil_temp, 
                             plot_all_mean_daily_temp, plot_all_mean_daily_top_sensor, ncol=2)


