#### Common garden HOBO and TOMST summary SCRIPT
### By Erica Zaja and Madi Anderson, created on 24/01/2023
## Last updated: 26/01/2023 by Erica

# HOBO JULY means across 2017-2021 ----
# Soil moisture 
 # 2017: 0.06248524 = 6.2%
 # 2019: 0.08079126 = 8%
 # 2020: 0.06237782 = 6.2%
 # 2021: 0.03306678 = 3.3%

# Ground temp = 
  # 2017: 12.71875
  # 2019: 15.69672
  # 2020: 10.05658
  # 2021: 13.98771

# Soil temp = 
 # 2017: 12.07936
 # 2019: 14.57302
 # 2020: 13.40346
 # 2021: 14.54086


# Air temp = 
 # 2017: 
 # 2019: 
 # 2020: 
 # 2021: 

# TOMST: surface temp -----
# 2021: 17.97439
# 2022: 14.54956

# TOMST: top sensor ----
# 2022: 9.570722
# 2021: 18.29926

# TOMST: soil temp ----
# 2022:  13.63691
# 2021: 15.62174

# TOMST soil moist -----
# 2021: 24.63718
# 2022: 40.04226

# SUMMARY MEANS with TOMST -----

# ground temp HOBO + TOMST -----
mean_ground_temp_CG <- c(17.97439, 14.54956, 12.71875, 15.69672,  10.05658,
                            9.570722, 18.29926, 13.98771) 
mean(mean_ground_temp_CG) # 14.10671
sd(mean_ground_temp_CG) # 3.255325

mean_soil_temp_CG <- c(13.63691, 15.62174, 12.07936, 14.57302, 13.40346, 14.54086) 
mean(mean_soil_temp_CG) # 13.97589
sd(mean_soil_temp_CG) #  1.218523

mean_soil_moist_CG <- c(40.04226, 24.63718, 6.2, 6.2, 3.3, 8) 
mean(mean_soil_moist_CG) # 14.72991
sd(mean_soil_moist_CG) # 17.09821
