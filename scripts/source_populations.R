#### Source populations
# Exploration of southern (Kluane) and northern (Qikiqtaruk) willow populations 
# Madelaine 16/11/22
# last updated: 

# libraries ---- 
library(ggplot2)
library(viridis)
library(dplyr)


# data ---- 
SLA_2022 <- read.csv("data/common_garden_2022/field_sla_2022.csv")

str(SLA_2022)

# quick plot of ALL source populations SLA 
# field data collected 2021 for Kluane and 2022 for QHI
 # but, to be fair this includes senescing leaves, too 
 # samples in Klaune were collected 4-5  times over season - should probably pick a July value
 # samples in QHI were collected in July or senescing in Aug. 
(sla_source <- ggplot(SLA_2022) +
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
(sla_source_july <- ggplot(july_sla) +
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
