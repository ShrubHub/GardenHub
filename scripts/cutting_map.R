# Created 28 February 2024
# Madelaine Anderson 

theme_map <- function(){ theme(legend.position = "bottom",
  axis.title.x = element_text(face="bold", family = "Helvetica Light", size=14),
                                 axis.text.x  = element_text(vjust=0.5, size=14, family = "Helvetica Light", colour = "black", angle = 45), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=14),
                                 axis.text.y  = element_text(vjust=0.5, size=14, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 20, family = "Helvetica Light", face = "italic", hjust = 0.5),
  legend.title = element_text(size=14, family = "Helvetica Light"),
  legend.text = element_text(size=14, family = "Helvetica Light"))
}
theme_map_source <- function(){ theme(legend.position = "bottom",
                               axis.title.x = element_text(face="bold", family = "Helvetica Light", size=14),
                               axis.text.x  = element_text(vjust=0.5, size=18, family = "Helvetica Light", colour = "black", angle = 45), 
                               axis.title.y = element_text(face="bold", family = "Helvetica Light", size=14),
                               axis.text.y  = element_text(vjust=0.5, size=18, family = "Helvetica Light", colour = "black"),
                               panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                               panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                               panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                               plot.title = element_text(color = "black", size = 20, family = "Helvetica Light", face = "italic", hjust = 0.5),
                               legend.title = element_text(size=14, family = "Helvetica Light"),
                               legend.text = element_text(size=14, family = "Helvetica Light"))
}

# map of cutting locations for common garden

library(raster)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggspatial)
library(sf)

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

(yukon_map <-  ggplot() +
  coord_map() +
  geom_polygon(data = t, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = t2, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = subset(t, NAME_1 == "Yukon"), aes(x = long, y =lat, group = group), fill = 'grey85', colour = 'black') +
  geom_point(data = sites, aes(x = long, y = lat, shape = site, size = site), colour = "black", stroke = 2) +
  annotate('text', y = 65, x = -137, label = 'Yukon', fontface =1, size = 6) +
  annotate('text', y = 69.75, x = -139.75, label = 'a', fontface =1, size = 6) + 
  annotate('text', y = 61.05, x = -139.2, label = 'b', fontface =1, size = 6) + 
  scale_x_continuous(expand = c(0,0), limits = c(-144.0, -123), 
                     breaks = c(-140.0, -135.00, -130.00, -125.00), 
                     labels = c("140.00ºW", "135.00ºW", "130.00ºW", "125.00ºW")) +
  scale_y_continuous(expand = c(0,0), limits = c(59, 70.5), 
                     breaks = c(60, 62.5, 65.0, 67.5, 70.0), 
                     labels = c("60.00ºN", "62.50ºN", "65.00ºN", "67.50ºN","70.00ºN"))+
  scale_shape_manual(values = main_map_shapes) +
  scale_size_manual(values = c(2, 4), guide = "none") +
  theme_bw() +
  guides(shape=guide_legend(title = "Site", override.aes = list(size = 4))) +
  theme_map()+
  theme(legend.position = "bottom", axis.title.x = element_blank(), 
        axis.title.y = element_blank()))
yukon_map

ggsave("output/figures/map/yukon_map.png", 
       height = 18, width = 18, unit = "cm", dpi = 500, device = png)

# QHI MAP ====

qhi_gps <- read.csv("data/map/qhi_cuttings_gps.csv")
qhi_gps <- qhi_gps %>% 
  mutate(lon_pos = lon*-1)

qhi_sf <- st_read("~/Desktop/Ecological_classification_Herschel_Island/Ecological_classification_Herschel_Island.shp")

e_qhi <- extent(-139.25, -138, 69.55, 70.5) #Define extent (long_min, long_max, lat_min, lat_max)
t_qhi <- crop(provinces, e_qhi) # Crop provincial/territorial spatial polygon dataframe to extent
#t2_qhi <- crop(states, e_qhi) # Crop states spatial polygon dataframe to extent


# qhi_map <- ggplot() +
#   geom_sf(data = qhi_sf, fill = 'grey95', colour = 'grey') +
#   coord_sf() +
#   #geom_point(data = qhi_gps, aes(x = lon, y = lat), shape=2, size=2, colour = "black") +
#   annotate('text', -Inf, Inf, label = 'Pauline Cove', fontface = 1, size = 3, 
#            hjust = -5.5, vjust = 15) +
#   #scale_x_continuous(expand = c(0,0)) +
#   #scale_y_continuous(expand = c(0,0)) +
#   xlab("") +
#   ylab("") +
#   annotation_scale()+
#   theme_classic() +
#   theme(axis.text = element_text(colour = 'black'))+
#   theme_map()
# qhi_map

#qhi_zoom <- st_crop(qhi_sf, c(xmin=572248, xmax=583782, ymin=7715140, ymax=7734833))

qhi_zoom <- st_crop(qhi_sf, c(xmin=580500, xmax=583782, ymin=7710140, ymax=7721600))

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

qhi_gps_UTM <- LongLatToUTM(qhi_gps$lon,qhi_gps$lat,7)
qhi_gps_UTM$type <- "point"

qhi_points_1 <- (c(x1 = 581050.8, x2 = 7720105, type = "point"))
qhi_points_7 <- (c(x1 = 581872.0, x2 = 7719887, type = "point"))
qhi_points_9 <- (c(x1 = 582052.1, x2 = 7719405, type = "point"))
qhi_points_10 <- (c(x1 = 582851.0, x2 = 7720448, type = "point"))
qhi_points_11 <- (c(x1 = 583120.9, x2 = 7719931, type = "point"))

qhi_point <- as.data.frame(rbind(qhi_points_1,
                    qhi_points_7, qhi_points_9, qhi_points_10,
                   qhi_points_11))
qhi_point$x1 <- as.numeric(qhi_point$x1)
qhi_point$x2 <- as.numeric(qhi_point$x2)


# -139.25, -138, 69.55, 70.5
qhi_zoom_map <- ggplot() +
  geom_sf(data = qhi_zoom, fill = 'grey95', colour = 'grey95',  inherit.aes = FALSE) +
  coord_sf(label_graticule = "SW", expand = T) +
  geom_point(data = qhi_point, aes(x = x1, y = x2, shape = type), shape=2, size=6, 
              colour = "black", alpha = 0.8) +
  annotate('text', -Inf, Inf, label = 'Pauline Cove', fontface = 1, size = 4, 
           hjust = -1.6, vjust = 43) +
  #annotate('text', -Inf, Inf, label = 'Qikiqtaruk', fontface = 1, size = 5, 
  #         hjust = -3.3, vjust = 20.5) +
  ylab("") +
  #annotation_scale()+
  theme_classic() +
  scale_size_manual(values = 1) +
  theme_map_source()+
  theme(axis.text = element_text(colour = 'black'), 
                                 legend.position = "bottom", 
                                 axis.title.x = element_blank(), 
                                 axis.title.y = element_blank()) 

qhi_zoom_map
ggsave("output/figures/map/qhi_map.png", 
       height = 18, width = 18, unit = "cm", dpi = 500, device = png)

# qhi_lat_long_sf <- qhi_sf %>% st_transform(4979)

# qhi_sp <- as(qhi_lat_long_sf, 'Spatial')
# e_qhi <- extent(-139.25, -138.83, 69.55, 70.5) #Define extent (long_min, long_max, lat_min, lat_max)
# t_qhi <- crop(qhi_sp, e_qhi) # Crop provincial/territorial spatial polygon dataframe to extent
# 
# qhi_map <- ggplot() +
#   coord_map() +
#   geom_polygon(data = qhi_sp, aes(x = long, y = lat), fill = 'grey95', colour = 'grey95') +
#   #geom_polygon(data = t2_qhi, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
#   geom_point(data = qhi_gps, aes(x = lon, y = lat), shape=1, size=2, colour = "black") +
#   #annotate('text', y = 69.6, x = 138.93, label = 'Pauline Cove', fontface = 1, size = 4) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   xlab("") +
#   ylab("") +
#   theme_bw() +
#   theme(axis.text = element_text(colour = 'black'), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank())+ 
#           theme_map()
# qhi_map
# 
# qhi_zoom_lat_long_sf <- qhi_zoom %>% st_transform(4979)
# 
# qhi_sp_zoom_test <- as(qhi_zoom, 'Spatial')

# qhi_sp_zoom <- as(qhi_zoom_lat_long_sf, 'Spatial')
# 
# qhi_zoom_sp_map <- ggplot() +
#   coord_map() +
#   geom_polygon(data = qhi_sp_zoom, aes(x = long, y = lat), fill = 'grey95', colour = 'grey95') +
#   #geom_polygon(data = t2_qhi, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
#   geom_point(data = qhi_gps, aes(x = lon, y = lat), shape=1, size=2, colour = "black") +
#   #annotate('text', y = 69.6, x = 138.93, label = 'Pauline Cove', fontface = 1, size = 4) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   xlab("") +
#   ylab("") +
#   theme_bw() +
#   theme(axis.text = element_text(colour = 'black'), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank())+ 
#   theme_map()
# qhi_zoom_sp_map


# KLUANE MAP ====

# import cg data with GPS points  
mother_gps <- read.csv("data/source_pops/all_source_pop_plus_mother.csv")

printers_pass <- c(Latitude = 61.3, Longitude_neg = -139.1, Site = "Printers Pass")
pika_camp <- c(Latitude = 61.12, Longitude_neg = -138.16, Site = "Pika Camp")
kluane_regions <- as.data.frame(rbind(printers_pass, pika_camp))
kluane_regions$Latitude <- as.numeric(kluane_regions$Latitude)
kluane_regions$Longitude_neg <- as.numeric(kluane_regions$Longitude_neg)

cg_gps <- mother_gps %>% 
  dplyr::select(Latitude, Longitude, Species, Site, SampleID, Elevation) %>% 
  drop_na(SampleID) %>% 
  drop_na(Latitude) %>% 
  mutate(Longitude_neg = Longitude*-1)
# the above appear only to be from Kluane but alas use for now 

kluane_gps <- full_join(cg_gps, kluane_regions, by = c("Latitude", "Longitude_neg", "Site"))

e_klu <- extent(-139.25, -138, 60.9, 61.45) #Define extent (long_min, long_max, lat_min, lat_max)
t_klu <- crop(provinces, e_klu) # Crop provincial/territorial spatial polygon dataframe to extent
#t2_qhi <- crop(states, e_qhi) # Crop states spatial polygon dataframe to extent

kluane_map <- ggplot() +
  coord_map() +
  geom_polygon(data = t_klu, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  #geom_polygon(data = t2_qhi, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  #geom_polygon(data = subset(t_klu, NAME_1 == "Yukon"), aes(x = long, y =lat, group = group), fill = 'grey85', colour = 'black') +
  geom_point(data = kluane_regions, aes(x = Longitude_neg, y = Latitude), shape = 2, size = 5, colour = "black") +
  annotate('text', y = 61.34, x = -139.1, label = "Printer's Pass", fontface =1, size = 5) +
  annotate('text', y = 61.16, x = -138.16, label = "Pika Camp", fontface =1, size = 5) +
  annotate('text', y = 60.95, x = -138.41, label = "Kluane Plateau", fontface =1, size = 5) +
  annotate('point', y = 60.97020, x = -138.41, shape = 2, size = 5) +
  annotate('text', y = 61.06, x = -138.41, label = "Common Garden", fontface =1, size = 5) +
  annotate('point', y = 61.03, x = -138.41,  shape =16, size = 5) +
  scale_x_continuous(expand = c(0,0), limits = c(-139.25, -138), 
                     breaks = c(-139.20, -139.00, -138.80, -138.60, -138.40, -138.2), 
                     labels = c("139.20ºW", "139.00ºW", "138.80ºW", "138.60ºW", "138.40ºW", "138.20ºW")) +
  scale_y_continuous(expand = c(0,0), limits = c(60.92, 61.38), 
                     breaks = c(60.94, 61.02, 61.10, 61.18, 61.26, 61.34), 
                     labels = c("60.94ºN", "61.02ºN", "61.10ºN", "61.18ºN","61.26ºN", "61.34ºN")) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme_map_source() +
  theme(axis.text = element_text(colour = 'black'))
kluane_map

ggsave("output/figures/map/kluane_map.png", 
       height = 18, width = 18, unit = "cm", dpi = 500, device = png)

# source map panel
source_map <- ggarrange(qhi_zoom_map, kluane_map, nrow = 2, ncol = 1,
                         labels = c("a", "b"), 
                         font.label=list(color="black", size = 12), 
                         heights = c(0.9, 1))

source_map <- ggpubr::ggarrange(qhi_zoom_map, kluane_map, nrow = 2, ncol = 1)

# big map 

map_overall <- ggarrange(yukon_map, source_map, ncol = 2)


# polar map code
data("wrld_simpl", package = "maptools")  

data("wrld_simpl", package = "maptools")                                                                            
(soil_map <- ggplot() +
    geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = "grey80", 
                 colour = "grey80", size = 0.5, alpha = 0.5) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    scale_y_continuous(breaks = seq(45, 90, by = 50), labels = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = NULL, legend = "Current Status") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = NA),
          axis.ticks = element_blank(),
          legend.title = element_blank()))

data("wrld_simpl", package = "maptools")                                                                            
(soil_map <- ggplot() +
    geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = "grey80", 
                 colour = "grey80", size = 0.5, alpha = 0.5) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    scale_y_continuous(breaks = seq(45, 90, by = 50), labels = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = NULL, legend = "Current Status") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = NA),
          axis.ticks = element_blank(),
          legend.title = element_blank())) 
# geom_point(data = soilcores_process,  
#           aes(x = lon, y = lat), size = 7, position = "jitter", alpha = 0.6, color = "#F09205") +
#geom_label_repel(data = soilcores_process,
#                 aes(x = lon, y = lat,
#                     label = site) ,
# Setting the positions of the labels
#                 box.padding = 0.9, size = 4, nudge_x = 2, nudge_y = -6, 
#                 segment.alpha = 0.8, segment.colour = "#F09205"))
