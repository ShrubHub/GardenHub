#### Common garden HOBO and TOMST summary SCRIPT
### By Erica Zaja and Madi Anderson, created on 24/01/2023
## Last updated: 24/01/2023 by Erica

# HOBO ground temp----
# 2019: 15.69672
# 2020:10.05658
# 2021: 13.98771

# HOBO air temp ----
# 2019: 15.70248
# 2020:13.36663
# 2021: 15.27245

# HOBO soil temp: ----
# 2019:14.57302
# 2020: 13.40346
# 2021: 14.54086

# TOMST: surface temp -----
# 2021: 17.97439
# 2022: 14.54956

# TOMST: top sensor ----
# 2022: 9.570722
# 2021: 18.29926

# SUMMARY MEANS with TOMST -----

# ground temp HOBO + TOMST. NOt final, MISSING 2015-2017 data!! 
mean_ground_temp_CG <- mean(15.69672,10.05658,13.98771,17.97439, 14.54956,
                            9.570722, 18.29926) # 15.69672

