# Created 28 February 2024
# Madelaine Anderson 

# map of cutting locations for common garden

library(raster)
library(ggplot2)
provinces <- getData(country="Canada", level=1)
states <- getData(country="USA", level=1)
crs(provinces) # CRS = World Geodetic System 1984 ("EPSG",6326)
e <- extent(-144, -123, 59, 70.5) #Define extent (long_min, long_max, lat_min, lat_max)
t <- crop(provinces, e) # Crop provincial/territorial spatial polygon dataframe to extent
t2 <- crop(states, e) # Crop states spatial polygon dataframe to extent

alp_klu <- c(lat = 61.3, long = -138.8)
qhi <- c(lat = 69.6, long = -138.9)
cg <- c(lat = 61.03, long = -138.41)
sites <- as.data.frame(rbind(alp_klu, qhi, cg))

yukon_map <-  ggplot() +
  coord_map() +
  geom_polygon(data = t, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = t2, aes(x = long, y =lat, group = group), fill = 'grey95', colour = 'grey50') +
  geom_polygon(data = subset(t, NAME_1 == "Yukon"), aes(x = long, y =lat, group = group), fill = 'grey85', colour = 'black') +
  geom_point(data = sites, aes(x = long, y = lat), shape="\u2605", size=10, colour = "black") +
  annotate('text', y = 65, x = -137, label = 'Yukon', fontface =2, size = 8) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.text = element_text(colour = 'black'))
yukon_map

