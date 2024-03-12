# Created 28 February 2024
# Madelaine Anderson 

theme_shrub <- function(){ theme(legend.position = "bottom",
  axis.title.x = element_text(face="bold", family = "Helvetica Light", size=14),
                                 axis.text.x  = element_text(vjust=0.5, size=14, family = "Helvetica Light", colour = "black"), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=14),
                                 axis.text.y  = element_text(vjust=0.5, size=14, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 20, family = "Helvetica Light", face = "italic", hjust = 0.5),
  legend.title = element_text(size=14, family = "Helvetica Light"),
  legend.text = element_text(size=14, family = "Helvetica Light"),
                                 strip.text.x = element_text(
                                   size = 15, color = "black", face = "italic", family = "Helvetica Light"))
                                 }


# map of cutting locations for common garden

library(raster)
library(ggplot2)
library(tidyr)
library(dplyr)

provinces <- getData(country="Canada", level=1)
states <- getData(country="USA", level=1)
crs(provinces) # CRS = World Geodetic System 1984 ("EPSG",6326)
e <- extent(-144, -123, 59, 70.5) # Define extent (long_min, long_max, lat_min, lat_max)
t <- crop(provinces, e) # Crop provincial/territorial spatial polygon dataframe to extent
t2 <- crop(states, e) # Crop states spatial polygon dataframe to extent

alp_klu <- c(lat = 60.97, long = -138.4, site = "Source")
qhi <- c(lat = 69.6, long = -138.98, "Source")
field_sites <- as.data.frame(rbind(alp_klu, qhi))
cg <- data.frame(lat = 61.03, long = -138.41, site = "Common garden")
sites <- as.data.frame(rbind(alp_klu, qhi, cg))
sites$lat <- as.numeric(sites$lat)
sites$long <- as.numeric(sites$long)

main_map_shapes <- c(16, 0)

yukon_map <-  ggplot() +
  coord_map() +
  geom_polygon(data = t, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = t2, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = subset(t, NAME_1 == "Yukon"), aes(x = long, y =lat, group = group), fill = 'grey85', colour = 'black') +
  geom_point(data = sites, aes(x = long, y = lat, shape = site, size = site), colour = "black") +
  annotate('text', y = 65, x = -137, label = 'Yukon', fontface =1, size = 6) +
  annotate('text', y = 69.75, x = -139.6, label = 'a', fontface =1, size = 5) + 
  annotate('text', y = 61.05, x = -139.2, label = 'b', fontface =1, size = 5) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ylab("") +
  scale_shape_manual(values = main_map_shapes) +
  scale_size_manual(values = c(2, 4), guide = "none") +
  theme_bw() +
  guides(shape=guide_legend(title = "Site", override.aes = list(size = 4))) +
  theme_shrub()
yukon_map

# QHI MAP ====

qhi_gps <- read.csv("data/map/qhi_cuttings_gps.csv")

e_qhi <- extent(-139.25, -138, 69.55, 70.5) #Define extent (long_min, long_max, lat_min, lat_max)
t_qhi <- crop(provinces, e_qhi) # Crop provincial/territorial spatial polygon dataframe to extent
#t2_qhi <- crop(states, e_qhi) # Crop states spatial polygon dataframe to extent

qhi_map <- ggplot() +
  coord_map() +
  geom_polygon(data = t_qhi, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  #geom_polygon(data = t2_qhi, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = subset(t_qhi, NAME_1 == "Yukon"), aes(x = long, y =lat, group = group), fill = 'grey85', colour = 'black') +
  geom_point(data = qhi_gps, aes(x = lon, y = lat), shape=2, size=2, colour = "black") +
  annotate('text', y = 69.57, x = -138.95, label = 'Pauline Cove', fontface = 1, size = 4) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(colour = 'black'))+
          theme_shrub()
qhi_map

# KLUANE MAP ====

# import cg data with GPS points  
mother_gps <- read.csv("data/source_pops/all_source_pop_plus_mother.csv")

cg_gps <- mother_gps %>% 
  dplyr::select(Latitude, Longitude, Species, Site, SampleID, Elevation) %>% 
  drop_na(SampleID) %>% 
  drop_na(Latitude) %>% 
  mutate(Longitude_neg = Longitude*-1)
# the above appear only to be from Kluane but alas use for now 


e_klu <- extent(-140, -138, 60.5, 62.5) #Define extent (long_min, long_max, lat_min, lat_max)
t_klu <- crop(provinces, e_klu) # Crop provincial/territorial spatial polygon dataframe to extent
#t2_qhi <- crop(states, e_qhi) # Crop states spatial polygon dataframe to extent

kluane_map <- ggplot() +
  coord_map() +
  geom_polygon(data = t_klu, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  #geom_polygon(data = t2_qhi, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = subset(t_klu, NAME_1 == "Yukon"), aes(x = long, y =lat, group = group), fill = 'grey85', colour = 'black') +
  geom_point(data = cg_gps, aes(x = Longitude_neg, y = Latitude), shape = 2, size = 2, colour = "black") +
  #annotate('text', y = 65, x = -137, label = 'Yukon', fontface =2, size = 8) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(colour = 'black'))
kluane_map
