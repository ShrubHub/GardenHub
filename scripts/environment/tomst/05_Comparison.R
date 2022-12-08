
#### 1 - LOAD PACKAGES  ##### 
library(readr)
library(tidyverse) 
library(esquisse)
library(lubridate)
library(gridExtra)

#### 2 - LOAD AND CLEAN DATA ####

# load both datasets and metadata
tms <- read_csv("users/egallois/groundtruthing/data/QHI_FullTOMST_2022.csv")
hobo <- read_csv("users/egallois/groundtruthing/data/QHI_FullHOBO_2022.csv")
micro <- read_csv("users/egallois/groundtruthing/data/QHI_FullMICROCLIMA_2022.csv")
meta_qhi <- read_xlsx("users/egallois/groundtruthing/data/COMPLETE_Microclimate_Metadata_2022.xlsx")

# get serial numbers from serial_ID
# for hobo we want everything starting 53 to 61
hobo$Serial_No <- substr(hobo$SerialID, 54, 61)
# for tomst we want everything 50 to 28
#users/egallois/groundtruthing/data/TOMST/data_94217204_2022_08_10_0.csv	
tms$Serial_No <- substr(tms$SerialID, 47, 54)

# for the initial comparison we only want the tms matches for microclima too
micro <- micro[grepl("Tomst", micro$Field_ID), ]

# new columns with type of logger added
hobo$logger <- "hobo"
tms$logger <- "tomst"
micro$logger <- "micro"

# add one hour to microclimate datetime!!! (did it wrong before)
micro$Datetime_UTC <- micro$Datetime_UTC + lubridate::hours(2)


# get date only column
hobo <- hobo %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  mutate(hour = lubridate::hour(Datetime_UTC))

tms <- tms %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  mutate(hour = lubridate::hour(Datetime_UTC))

micro <- micro %>% 
  mutate(Date = lubridate::date(Datetime_UTC)) %>% 
  mutate(hour = lubridate::hour(Datetime_UTC))


# calculate daily averages, max and min temps
hobo_summary <- hobo %>%
  #filter(Variable %in% "Temperature") %>% 
  filter(Date > lubridate::ymd("2022-07-27"))%>% 
  group_by(Date, hour, Serial_No, Variable) %>% 
  mutate(mean_value = mean(Value)) %>% 
  select(-Value) %>% 
  ungroup()

tms_summary <- tms %>%
  #filter(Variable %in% "T2: Surface sensor") %>% 
  filter(Date > lubridate::ymd("2022-07-27"))%>% 
  group_by(Date, hour,  Serial_No, Variable) %>% 
  mutate(mean_value = mean(Value)) %>% 
  select(-Value) %>% 
  ungroup()


micro_summary <- micro %>%
  filter(Date > lubridate::ymd("2022-07-27")) %>% 
  group_by(Date, hour, Field_ID) %>% 
  mutate(mean_value = mean(meantemp)) %>% 
  select(-meantemp) %>% 
  ungroup() 


# plot individual time series
tms_plot <- ggplot(tms_summary) +
  aes(x = Datetime_UTC, y = mean_value, colour = Serial_No) +
  geom_line(size = 1L) +
  scale_color_hue() +
  facet_wrap(vars(Variable), scales = "free") +
  theme_minimal()

tms_plot + theme(legend.position = "none")


hobo_plot <- ggplot(hobo_summary) +
  aes(x = Datetime_UTC, y = mean_value, colour = Serial_No) +
  geom_line(size = 1L) +
  scale_color_hue() +
  facet_wrap(vars(Variable), scales = "free") +
theme_minimal()

hobo_plot + theme(legend.position = "none")

micro_plot <- ggplot(micro_summary) +
  aes(x = Datetime_UTC, y = mean_value, colour = Field_ID) +
  geom_line(size = 1L) +
  scale_color_hue() +
  theme_minimal()

micro_plot + theme(legend.position = "none")

# concatenate date - hour - serial number
hobo_summary$concat1 <- paste(hobo_summary$Date, "_", hobo_summary$hour,"_",hobo_summary$Serial_No, hobo_summary$Variable)
tms_summary$concat1 <- paste(tms_summary$Date, "_", tms_summary$hour,"_",tms_summary$Serial_No, tms_summary$Variable) 
micro_summary$concat1 <- paste(micro_summary$Date, "_", micro_summary$hour,"_",micro_summary$Serial_No) 


# remove duplicates
hobo_clean <- hobo_summary[!duplicated(hobo_summary$concat1),]
tms_clean <- tms_summary[!duplicated(tms_summary$concat1),]
micro_clean <- micro_summary[!duplicated(micro_summary$concat1),]

colnames(hobo_clean)
colnames(tms_clean)
colnames(micro_clean)
str(hobo_clean) # Temperature Lux
str(tms_clean) # T1: Soil sensor  # T2: Surface sensor # T3: Top sensor # SoilMoistureCount

# change column titles
micro_clean$Serial_ID <- micro_clean$Serial_No
micro_clean$Variable <- 'SurfaceTemp'

# rename level titles to make them match

tms_clean$Variable <- recode_factor(tms_clean$Variable, 'T1: Soil sensor' = "SoilTemp", 
                                     'T2: Surface sensor' = "SurfaceTemp",
                                     'T3: Top sensor' = "AirTemp",
                                     'SoilMoistureCount' = "Soilmoist")
hobo_clean$Variable <- recode_factor(hobo_clean$Variable, Temperature = "SurfaceTemp", 
                                     Lux = "Lux")



# keep only useful columns
hobo_merge <- hobo_clean %>% 
  select(Serial_No, logger, Datetime_UTC, Date, hour, mean_value, Variable)
tms_merge <- tms_clean %>% 
  select(Serial_No, logger, Datetime_UTC, Date, hour, mean_value, Variable)
micro_merge <- micro_clean %>% 
  select(Serial_No, logger, Datetime_UTC, Date, hour, mean_value, Variable) 

# merge datasets
full_loggers <- rbind(hobo_merge, tms_merge, micro_merge)
View(full_loggers)


# plot dataset for surface temp
full_loggers %>%
  filter(Variable %in% "SurfaceTemp") %>% 
  filter(Date >= "2022-07-28" & Date <= "2022-08-10") %>%
  ggplot() +
  aes(x = Datetime_UTC, y = mean_value, colour = logger) +
  geom_line(size = 1L) +
  scale_color_hue() +
  labs(y = "Surface temperature", color = "Microclimate type") +
  theme_minimal()


# add nearest phenocam based on serial number
meta_qhi$Serial_No <- as.character(meta_qhi$Serial_No)
full_loggers <- left_join(full_loggers, meta_qhi)
colnames(full_loggers)

# keep only useful columns
full_loggers <- full_loggers %>% 
  select(Serial_No,'Meta-site', logger,  Date, hour, Variable, mean_value,
         Field_ID, 'Nearest Phenocam', Lat, Lon) 

# write as csv for other projects
write.csv(full_loggers, file = "users/egallois/groundtruthing/data/QHI_Abiotic_2022.csv", row.names = FALSE)




# remove values wth no phenocam for now
#full_loggers$phenocam <- full_loggers$'Nearest Phenocam' 
#full_loggers <- full_loggers[!is.na(full_loggers$phenocam),]

# concat date and hour
full_loggers$datesig <- paste(full_loggers$Date,"_",
                              full_loggers$hour, "_",
                              full_loggers$'Meta-site')

# surf temp similarities - need to make a wide column
full_loggers2 <- full_loggers %>% 
  filter(Variable %in% "SurfaceTemp") %>% 
  select('Meta-site',datesig, mean_value, logger)

full_logger_wide <- pivot_wider(full_loggers2, 
                                names_from = logger, 
                                values_from = mean_value) 

full_loggers2 %>%
  dplyr::group_by(Lat, Lon, datesig, logger) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 


# plot comparisons
(comp1 <- full_logger_wide %>%
    filter(`Meta-site` %in% c("P1","P2","P3", "P4","P5","P6","P7", "P8", "P9",
                              "P10")) %>%
  ggplot() +
  aes(x = hobo, y = tomst, colour = `Meta-site`) +
  geom_point(size = 1L, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_hue() +
  labs(title = "a) TOMST vs HOBO") +
  labs(x = "HOBO Hourly Mean Surface Temp (\u00B0C)", y = "TOMST Hourly Mean Surface Temp (\u00B0C)", color = "Phenocam") +
  theme_classic())

comp1 <- comp1 + theme(legend.position = "none")

(comp2 <- full_logger_wide %>%
    filter(`Meta-site` %in% c("P1","P2","P3", "P4","P5","P6","P7", "P8", "P9",
                               "P10")) %>%
    ggplot() +
    aes(x = hobo, y = micro, colour = `Meta-site`) +
    geom_point(size = 1L,, alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_hue() +
    labs(title = "b) Microclima vs HOBO") +
    labs(x = "HOBO Hourly Mean Surface Temp (\u00B0C)", y = "Microclima Hourly Mean Surface Temp (\u00B0C)", color = "Phenocam") +
    theme_classic())

comp2 <- comp2 + theme(legend.position = "none")


(comp3 <- full_logger_wide %>%
    ggplot() +
    aes(x = tomst, y = micro, colour = `Meta-site`) +
    geom_point(size = 1L, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
    scale_color_hue() +
    labs(title = "c) Microclima vs TOMST") +
    labs(x = "Tomst Hourly Mean Surface Temp (\u00B0C)", y = "Microclima Hourly Mean Surface Temp (\u00B0C)", color = "Phenocam") +
    theme_classic())

comp3 <- comp3 + theme(legend.position = "none")

# make panel
(all_comp <- grid.arrange(comp1,comp2,comp3, nrow = 1))

# save panel
ggsave(all_comp, file = "users/egallois/groundtruthing/figures/logger_comparison_panel.png", height = 4, width = 12)

