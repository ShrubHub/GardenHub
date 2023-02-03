#### Common garden HOBO and TOMST summary SCRIPT
### By Erica Zaja and Madi Anderson, created on 24/01/2023
## Last updated: 26/01/2023 by Erica

# HOBO JULY means across 2017-2021 ----
# Soil moisture = 0.05968028 i.e. 5.9%
# Ground temp = 13.11494
# Soil temp = 13.64918
# Air temp = 14.01731

# TOMST: surface temp -----
# 2021: 17.97439
# 2022: 14.54956

# TOMST: top sensor ----
# 2022: 9.570722
# 2021: 18.29926

# TOMST: soil temp ----
# 2021-2022 mean: 13.80938

# TOMST soil moist -----
# 2021-2022 mean: 39.10862

# SUMMARY MEANS with TOMST -----

# ground temp HOBO + TOMST -----
mean_ground_temp_CG <- mean(17.97439, 14.54956, 13.11494,
                            9.570722, 18.29926) # 17.97439

mean_soil_temp_CG <- mean(13.80938, 13.64918) # 13.72928

mean_soil_moist_CG <- mean(39.10862,5.968028) # 22.53832
