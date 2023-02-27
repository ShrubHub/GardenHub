# BAYESIAN maternal effects / propagation effects ------
# script by Erica and Madi
# last update: 27/02/2023

# 1. Loading libraries ----
library(brms)
library(tidyverse)

# 2. Loading data ---- 
mother_cg <- read_csv("data/source_pops/mother_cg.csv")

# 3. Wrangle ------
# make species specific dfs
mother_cg_arctica <- mother_cg %>% 
  dplyr::filter(Species == "Salix arctica")

mother_cg_richardsonii <- mother_cg %>% 
  dplyr::filter(Species == "Salix richardsonii")

mother_cg_pulchra <- mother_cg %>% 
  dplyr::filter(Species == "Salix pulchra")

# explore 

# 4. MODELLING ------

#4.1. MATERNAL EFFECTS 

# HEIGHT -------
# Salix richardsonii ------
# Salix pulchra -------
# Salix arctica --------

# WIDTH ------
# Salix richardsonii ------
# Salix pulchra -------
# Salix arctica --------

# BIOVOLUME -------
# Salix richardsonii ------
# Salix pulchra -------
# Salix arctica --------

# 4.2. PROPAGATION EFFECTS ------

# Height vs Cutting length ------
# Salix richardsonii ------
# Salix pulchra -------
# Salix arctica --------

# Biovolume vs Cutting length ------
# Salix richardsonii ------
# Salix pulchra -------
# Salix arctica --------

# Cutting length vs mother canopy height  ------
# Salix richardsonii ------
# Salix pulchra -------
# Salix arctica --------