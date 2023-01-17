#### Conceptual diagram topographic map script
### By Erica Zaja using online tutorial
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
# created on 17/01/2023
## Last updated: 17/01/2023 by Erica

# Loading libraries -----
library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)

# if checkpoint is not yet installed, install it (for people using this
# system for the first time)
if (!require(checkpoint)) {
  if (!require(devtools)) {
    install.packages("devtools", repos = "http://cran.us.r-project.org")
    require(devtools)
  }
  devtools::install_github("RevolutionAnalytics/checkpoint",
                           ref = "v0.3.2", # could be adapted later,
                           # as of now (beginning of July 2017
                           # this is the current release on CRAN)
                           repos = "http://cran.us.r-project.org")
  require(checkpoint)
}
# nolint start
if (!dir.exists("~/.checkpoint")) {
  dir.create("~/.checkpoint")
}
# nolint end
# install packages for the specified CRAN snapshot date
checkpoint(snapshotDate = package_date,
           project = path_to_wd,
           verbose = T,
           scanForPackages = T,
           use.knitr = F,
           R.version = R_version)
rm(package_date)
# Error in path.expand(path) : object 'path_to_wd' not found

source("manifest.R")
unlink("manifest.R")
sessionInfo()

# shapefile of Canada borders from Statistics Canada
# read borders
canada_geo <- read_sf("data/map/lpr_000b21a_e.shp")
projection(canada_geo)
#  "+proj=lcc +lat_0=63.390675 +lon_0=-91.8666666666667 +lat_1=49 +lat_2=77 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs"

# and topographic map from University of Texas Austin
# read in raster of relief
relief <- raster("data/map/stanford-xs501gm7232-geotiff.tif") # check CRS

%>%
  # hide relief outside of Switzerland by masking with country borders
  mask(canada_geo) 


plot(relief)
