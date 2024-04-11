
# Loading libraries ----
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(knitr) # For kable tables
library(kableExtra) # For kable tables
library(gridExtra)
library(ggpubr)
library(ggeffects)

# Loading data ---- 
#all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv") # 2022 data 
all_CG_source_growth <- read.csv("data/common_garden_data_2023/all_data_2023.csv") # 2023 data


# 1. scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# 2. extract model result function =====

# funciton to extract model summary
model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Sample_age # change name of random effect here
  random = sum$random$SampleID_standard # change name of random effect here
  obs = sum$nobs
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  # fixed$effect <- "population"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  # random$effect <- "SampleID_standard"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "random" # could change rowname here of random effect if you'd like
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

model_summ_growth <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Sample_age
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "Sample_age"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge it all together
}

model_summ_time <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$Year
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "Year"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge it all together
}

# WRANGLE ------
# Filtering data for height over time model 
all_CG_growth <- all_CG_source_growth %>%
  filter(population %in% c("Northern", "Southern"))

# making a growth rate column
all_CG_height_growth_rates <- all_CG_growth %>%
  group_by(SampleID_standard) %>% 
  arrange(Sample_age) %>%
  mutate(height_growth_diff = Canopy_Height_cm-lag(Canopy_Height_cm)) %>%
  mutate(height_growth_diffpercent = (height_growth_diff/Canopy_Height_cm)*100) %>%
  mutate(biovol_growth_diff = biovolume-lag(biovolume)) %>%
  mutate(biovol_growth_diffpercent = (biovol_growth_diff/biovolume)*100)

# Species specific 
all_CG_growth_ric <- all_CG_height_growth_rates %>%
  filter(Species == "Salix richardsonii")

all_CG_growth_pul<-  all_CG_height_growth_rates %>%
  filter(Species == "Salix pulchra")

all_CG_growth_arc <-all_CG_height_growth_rates %>%
  filter(Species == "Salix arctica")

rich_alive_2023 <- all_CG_growth_ric %>% 
  filter(Year == "2023") %>% # 70 total 
  drop_na(Canopy_Height_cm) %>% 
  filter(population == "Northern") # 31 northern 

pul_alive_2023 <- all_CG_growth_pul %>% 
  filter(Year == "2023") %>% 
  drop_na(Canopy_Height_cm) %>%  # 89 total 
  filter(population == "Northern") # 42 northern 

arc_alive_2023 <- all_CG_growth_arc %>% 
  filter(Year == "2023") %>% 
  drop_na(Canopy_Height_cm) %>%  # total 51
  filter(population == "Northern") # 24 northern 

# HEIGHT 2023 ----
pal_garden <- c("#332288", "#7ad151")


theme_shrub <- function(){ theme(legend.position = "bottom",
                                 axis.title.x = element_text(face="bold", family = "Helvetica Light", size=14),
                                 axis.text.x  = element_text(vjust=0.5, size=14, family = "Helvetica Light", colour = "black", angle = 270), 
                                 axis.title.y = element_text(face="bold", family = "Helvetica Light", size=14),
                                 axis.text.y  = element_text(vjust=0.5, size=14, family = "Helvetica Light", colour = "black"),
                                 panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y = element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 14, family = "Helvetica Light", face = "italic", hjust = 0.5),
                                 legend.title = element_text(size=14, family = "Helvetica Light"),
                                 legend.key=element_blank(),
                                 strip.text.x = element_text(
                                   size = 12, color = "black", face = "italic", family = "Helvetica Light"),
                                 strip.background = element_blank(),
                                 legend.text=element_text(size = 14, family = "Helvetica Light"))}

# Salix richardsonii -------
height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(height_rich, file = "output/models/height_rich_2023.rds")
height_rich <- readRDS("output/models/height_rich_2023.rds")
summary(height_rich)

ggpred_height_ric <- ggpredict(height_rich, terms = c("Sample_age", "population"))
colnames(ggpred_height_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")
ggpred_height_ric$species <- "Salix richardsonii"

(ggpred_height_rich_plot <-ggplot(ggpred_height_ric) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5, size = 1)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n" ) +
    scale_color_manual(values=pal_garden) +
    scale_fill_manual(values=pal_garden) +
    ggtitle(expression(italic("Salix richardsonii"))) +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
    theme_shrub()+ 
    theme(text=element_text(family="Helvetica
                                           Light")) +
    scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 25)) +
    theme( axis.text.x  = element_text(angle = 0)) +
    labs(title = "Salix richardsonii", size = 14, family = "Helvetica Light")) 

# estimate for northern sample age: 1.46+0.10*1 = exp(1.56) = 4.758821 cm, in year 1
# estimate for southern sample age: (1.46+1.06)+(0.10*1+0.11*1) = exp(2.73) = 15.33289 in year 1
# estimate for southern sample age in year 10: (1.46+1.06)+(0.10*10 +0.11*10) = exp(4.62) = 101.494 in year 10 --> 101.494/10 = 10.1494 cm/year
# estimate for n sample age in year 10: 1.46+0.10*10 = exp(2.46) = 11.70481 in year 10 --> 11.70481/10= 1.170481 cm/year

# extracting model summary
height_rich_summ <- model_summ(height_rich)
height_rich_summ$Species <- "Salix richardsonii"
height_rich_summ <- height_rich_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")

# # change estimates by adding estimate to other rows 
# height_rich_summ[3,1] <- height_rich_summ[3,1] + height_rich_summ[1,1]
# height_rich_summ[4,1] <- height_rich_summ[4,1] + height_rich_summ[2,1]
# 
# # change lower CI by adding
# height_rich_summ[3,3] <- height_rich_summ[3,3] + height_rich_summ[1,3]
# height_rich_summ[4,3] <- height_rich_summ[4,3] + height_rich_summ[2,3]
# 
# # change upper CI
# height_rich_summ[3,4] <- height_rich_summ[3,4] + height_rich_summ[1,4]
# height_rich_summ[4,4] <- height_rich_summ[4,4] + height_rich_summ[2,4]

#height_rich_summ <- height_rich_summ[c(1:4),] # this removes the random effects

rownames(height_rich_summ) <- c("Intercept", "Sample age", "Southern population"
                                , "Sample age:Southern population", "Random intercept", 
                                "sd(Sample age)", "cor(Intercept, Sample age)", "sigma")

height_rich_summ$Rhat <- as.character(formatC(height_rich_summ$Rhat, digits = 2, format = 'f'))

height_rich_summ <- height_rich_summ %>%
  #mutate(Species = "Salix richardsonii")%>%
  relocate("Species", .before = "Estimate")

# Salix pulchra -----
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(height_pul, file = "output/models/height_pul_2023.rds")
height_pul <- readRDS("output/models/height_pul_2023.rds")

summary(height_pul)
# estimate for northern sample age: 1.96-0.00=1.96 --> exp(1.96)= 7.099327 cm in year 1
# estimate for s sample age: (1.96+0.99) + (-0.00+0.01)= 2.96, exp(2.96)= 19.29797 cm per sample age
# estimate for s. sample age at year 10: (1.99+0.95) + (-0.02*9+0.03*9)= 3.03, exp(3.03)= 20.69 in year 9 --> 2.299692 per year


# estimate for s. sample age at year 9: (1.99+0.95) + (-0.02*9+0.03*9)= 3.03, exp(3.03)= 20.69 in year 9 --> 2.299692 per year
# estimate for northern sample age at year 9: 1.99-0.02*9=1.97= 3.03, exp(3.03)= 6.110447 in year 9 --> 0.6789386 per year

ggpred_height_pul <- ggpredict(height_pul, terms = c("Sample_age", "population"))
colnames(ggpred_height_pul) = c('Sample_age','fit', 'lwr', 'upr',"population")
ggpred_height_pul$species <- "Salix pulchra"

(ggpred_height_pul_plot <-ggplot(ggpred_height_pul) +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5, size = 1)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n" ) +
    scale_color_manual(values=pal_garden) +
    scale_fill_manual(values=pal_garden) +
    ggtitle(expression(italic("Salix pulchra"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica
                                           Light")) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 90, by = 15)) +
    theme( axis.text.x  = element_text(angle = 0)) +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
    labs(title = "Salix pulchra", size = 14, family = "Helvetica Light"))

height_pul_summ <- model_summ(height_pul)
height_pul_summ$Species <- "Salix pulchra"
height_pul_summ <- height_pul_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")

# change estimates by adding estimate to other rows 
# height_pul_summ[3,1] <- height_pul_summ[3,1] + height_pul_summ[1,1]
# height_pul_summ[4,1] <- height_pul_summ[4,1] + height_pul_summ[2,1]
# 
# # change lower CI by adding 
# height_pul_summ[3,3] <- height_pul_summ[3,3] + height_pul_summ[1,3]
# height_pul_summ[4,3] <- height_pul_summ[4,3] + height_pul_summ[2,3]
# 
# # change upper CI
# height_pul_summ[3,4] <- height_pul_summ[3,4] + height_pul_summ[1,4]
# height_pul_summ[4,4] <- height_pul_summ[4,4] + height_pul_summ[2,4]

rownames(height_pul_summ) <- c("Intercept ", "Sample age ", "Southern population "
                               , "Sample age:Southern population ", "Random intercept ", 
                               "sd(Sample age) ", "cor(Intercept, Sample age) ", "sigma ")

height_pul_summ$Rhat <- as.character(formatC(height_pul_summ$Rhat, digits = 2, format = 'f'))

height_pul_summ <- height_pul_summ %>%
#mutate(Species = "Salix pulchra")%>%
relocate("Species", .before = "Estimate")

# Salix arctica  -------
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))
saveRDS(height_arc, file = "output/models/height_arc_2023.rds")
height_arc <- readRDS("output/models/height_arc_2023.rds")

summary(height_arc) # significant growth over time
# estimate for northern sample age 9: 0.86+0.08*9= exp(1.58) = 4.854956 -->0.5394396
# estimate for s. sample age: 0.86-0.28 + 0.08*9 + 0.11*9 = 2.29= exp(2.29)=  9.874938 --> 1.097215

ggpred_height_arc <- ggpredict(height_arc, terms = c("Sample_age", "population"))
colnames(ggpred_height_arc) = c('Sample_age','fit', 'lwr', 'upr',"population")
ggpred_height_arc$species <- "Salix arctica"

(ggpred_height_arc_plot <-ggplot(ggpred_height_arc) +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5, size = 1)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n" ) +
    scale_color_manual(values=pal_garden) +
    scale_fill_manual(values=pal_garden) +
    theme_shrub() + 
    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3)) +
    scale_x_continuous(limits = c(1, 8), breaks = seq(1, 8, by = 1)) +
    theme( axis.text.x  = element_text(angle = 0)) + 
    labs(title = "Salix arctica", size = 14, family = "Helvetica Light")) 

(ggpred_CG_height_panel <- ggarrange(ggpred_height_rich_plot,
                                    ggpred_height_pul_plot, 
                                    ggpred_height_arc_plot, nrow = 1,
                                    common.legend = TRUE, 
                                    labels = c("A)", "B)", "C)"),
                                    legend="none", 
                                    font.label=list(color="black",size = 12)))

ggsave(ggpred_CG_height_panel, filename ="output/figures/ggpred_CG_height_panel_2023.png",
       width = 18, height = 8, units = "cm", device = png)

height_arc_summ <- model_summ(height_arc)

height_arc_summ$Species <- "Salix arctica"
height_arc_summ <- height_arc_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")

# # change estimates by adding estimate to other rows 
# height_arc_summ_2[3,1] <- height_arc_summ_2[3,1] + height_arc_summ_2[1,1]
# height_arc_summ_2[4,1] <- height_arc_summ_2[4,1] + height_arc_summ_2[2,1]
# 
# ## change lower CI by adding 
# height_arc_summ_2[3,3] <- height_arc_summ_2[3,3] + height_arc_summ_2[1,3]
# height_arc_summ_2[4,3] <- height_arc_summ_2[4,3] + height_arc_summ_2[2,3]
# 
# # change upper CI
# height_arc_summ_2[3,4] <- height_arc_summ_2[3,4] + height_arc_summ_2[1,4]
# height_arc_summ_2[4,4] <- height_arc_summ_2[4,4] + height_arc_summ_2[2,4]

rownames(height_arc_summ) <- c(" Intercept ", " Sample age ", " Southern population "
                               , " Sample age:Southern population ", " Random intercept ", 
                               " sd(Sample age) ", " cor(Intercept, Sample age) ", " sigma ")
height_arc_summ$Rhat <- as.character(formatC(height_arc_summ$Rhat, digits = 2, format = 'f'))

height_arc_summ <- height_arc_summ %>%
  relocate("Species", .before = "Estimate")

#height_arc_summ <- height_arc_summ[c(1:4),]

# binding all summaries
all_height_summ <- rbind(height_rich_summ,height_pul_summ,height_arc_summ)

all_height_summ_back <- all_height_summ %>% 
  dplyr::mutate("Estimate (back)" = exp(Estimate)) %>% 
  dplyr::mutate("Lower 95% CI (back)" = exp(l_95_CI_log))%>%
  dplyr::mutate("Upper 95% CI (back)" = exp(u_95_CI_log)) %>%
  dplyr::rename("Est.Error (log)" = "Est.Error") %>%
  dplyr::rename("Lower 95% CI (log)" = "l_95_CI_log") %>%
  dplyr::rename("Upper 95% CI (log)" = "u_95_CI_log")

write.csv(all_height_summ_back, "outputs/tables/all_height_time_output.csv")
all_height_summ_back <- read.csv("outputs/tables/all_height_time_output.csv")
#all_height_summ_table <- all_height_summ_back %>% 
#  kbl(caption="Table. Heights over time of northern and southern shrubs in the common garden. ", 
  #    col.names = c("Species", "Estimate (log)", "Error (log)", "Lower 95% CI (log)", "Upper 95% CI (log)",
    #                "Rhat", "Bulk effective sample size", "Tail effective sample size",
       #             "Effect", "Sample size", "Estimate (back)", "Lower 95% CI (back)", "Upper 95% CI (back)"), # give the column names you want making sure you have one name per column!
      #digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
 # kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

all_height_summ_table <- all_height_summ %>% 
  kbl(caption="Table. Heights over time of northern and southern shrubs in the common garden. ", 
      col.names = c("Species", "Estimate (log)", "Error (log)", "Lower 95% CI (log)", "Upper 95% CI (log)",
                    "Rhat", "Bulk effective sample size", "Tail effective sample size",
                    "Effect", "Sample size"), # give the column names you want making sure you have one name per column!
      digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=FALSE, html_font="Helvetica") # can change fonts

# optional: making specific column text in italics
column_spec(all_height_summ_table, 2, width = NULL, bold = FALSE, italic = TRUE) # 2 is my example column number 

save_kable(all_height_summ_table,file = "outputs/tables/kable_heights.pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)

# WDITH 2023
# S. richardsonii width
# width_ric <- brms::brm(log(mean_width) ~ Sample_age*population+(Sample_age|SampleID_standard),
#                        data = all_CG_growth_ric,  family = gaussian(), chains = 3,
#                        iter = 5000, warmup = 1000, 
#                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# summary(width_ric)
# saveRDS(width_ric, file = "output/models/width_ric_2023.rds")
# width_ric <- readRDS("output/models/width_ric_2023.rds")
# 
# ggpred_width_ric <- ggpredict(width_ric, terms = c("Sample_age", "population"))
# colnames(ggpred_width_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")
# 
# (ggpred_height_arc_plot <-ggplot(ggpred_width_ric) +
#     geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = mean_width, colour = population),
#                alpha = 0.5)+ # raw data
#     geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
#     geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
#                 alpha = 0.2) +
#     ylab("Shrub width (cm)\n") +
#     xlab("\n Sample age " ) +
#     scale_colour_viridis_d(begin = 0.1, end = 0.85) +
#     scale_fill_viridis_d(begin = 0.1, end = 0.85) +
#     theme_shrub() + 
#     theme( axis.text.x  = element_text(angle = 0)) + 
#     labs(title = "Salix richardsonii", size = 20, family = "Helvetica Light"))
# 
# # S. pulchra width
# width_pul <- brms::brm(log(mean_width) ~ Sample_age*population+(Sample_age|SampleID_standard),
#                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
#                        iter = 5000, warmup = 1000, 
#                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# summary(width_pul)
# saveRDS(width_pul, file = "output/models/width_pul_2023.rds")
# width_pul <- readRDS("output/models/width_pul_2023.rds")
# 
# ggpred_width_pul <- ggpredict(width_pul, terms = c("Sample_age", "population"))
# colnames(ggpred_width_pul) = c('Sample_age','fit', 'lwr', 'upr',"population")
# 
# (ggpred_height_arc_plot <-ggplot(ggpred_width_pul) +
#     geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = mean_width, colour = population),
#                alpha = 0.5)+ # raw data
#     geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
#     geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
#                 alpha = 0.2) +
#     ylab("Shrub width (cm)\n") +
#     xlab("\n Sample age " ) +
#     scale_colour_viridis_d(begin = 0.1, end = 0.85) +
#     scale_fill_viridis_d(begin = 0.1, end = 0.85) +
#     theme_shrub() + 
#     theme( axis.text.x  = element_text(angle = 0)) + 
#     labs(title = "Salix pulchra", size = 20, family = "Helvetica Light"))
# 
# # # S arctica width
# width_arc <- brms::brm(log(mean_width) ~ Sample_age*population+(Sample_age|SampleID_standard),
#                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
#                        iter = 5000, warmup = 1000, 
#                        control = list(max_treedepth = 15, adapt_delta = 0.99))
# summary(width_arc)
# saveRDS(width_arc, file = "output/models/width_arc_2023.rds")
# width_arc <- readRDS("output/models/width_arc_2023.rds")
# 
# ggpred_width_arc <- ggpredict(width_arc, terms = c("Sample_age", "population"))
# colnames(ggpred_width_arc) = c('Sample_age','fit', 'lwr', 'upr',"population")
# 
# (ggpred_width_arc_plot <-ggplot(ggpred_width_arc) +
#     geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = mean_width, colour = population),
#                alpha = 0.5)+ # raw data
#     geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
#     geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
#                 alpha = 0.2) +
#     ylab("Shrub width (cm)\n") +
#     xlab("\n Sample age " ) +
#     scale_colour_viridis_d(begin = 0.1, end = 0.85) +
#     scale_fill_viridis_d(begin = 0.1, end = 0.85) +
#     theme_shrub() + 
#     theme( axis.text.x  = element_text(angle = 0)) + 
#     labs(title = "Salix arctica", size = 20, family = "Helvetica Light"))


# STEM ELONGATION 2023 ----
# S. richardsonii ---- 
stem_ric <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(Sample_age|SampleID_standard),
                       data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(stem_ric)
saveRDS(stem_ric, file = "output/models/stem_elong_ric_2023.rds")
stem_ric <- readRDS("output/models/stem_elong_ric_2023.rds")

ggpred_stem_ric <- ggpredict(stem_ric, terms = c("Sample_age", "population"))
colnames(ggpred_stem_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")

ggpred_stem_ric$population <- ordered(ggpred_stem_ric$population, 
                                        levels = c("Northern", "Southern"))

all_CG_growth_ric$population <- ordered(all_CG_growth_ric$population, 
                                        levels = c("Northern", "Southern"))

(ggpred_stem_ric_plot <-ggplot(ggpred_stem_ric) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = mean_stem_elong, colour = population),
               alpha = 0.5, size = 1) + # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Stem elongation (mm)\n") +
    xlab("Sample age \n" ) +
    scale_colour_manual(values=pal_garden) +
    scale_fill_manual(values=pal_garden) +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
    scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 25)) +
    theme_shrub() + 
    theme( axis.text.x  = element_text(angle = 0)))

stem_elong_rich_summ <- model_summ(stem_ric)
stem_elong_rich_summ$Species <- "Salix richardsonii"
stem_elong_rich_summ <- stem_elong_rich_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")
rownames(stem_elong_rich_summ) <- c("Intercept", "Sample age", "Southern population"
                                , "Sample age:Southern population", "Random intercept", 
                                "sd(Sample age)", "cor(Intercept, Sample age)", "sigma")

stem_elong_rich_summ$Rhat <- as.character(formatC(stem_elong_rich_summ$Rhat, digits = 2, format = 'f'))

stem_elong_rich_summ <- stem_elong_rich_summ %>%
  #mutate(Species = "Salix richardsonii")%>%
  relocate("Species", .before = "Estimate")

# S. pulchra ----
all_CG_growth_pul_elong <- all_CG_growth_pul %>% 
  filter(mean_stem_elong < 550) # filter some extreme values that don't make biological sense 

stem_pul <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(Sample_age|SampleID_standard),
                      data = all_CG_growth_pul_elong,  family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(stem_pul)
saveRDS(stem_pul, file = "output/models/stem_elong_pul_2023.rds")
stem_pul <- readRDS("output/models/stem_elong_pul_2023.rds")

ggpred_stem_pul <- ggpredict(stem_pul, terms = c("Sample_age", "population"))
colnames(ggpred_stem_pul) = c('Sample_age','fit', 'lwr', 'upr',"population")

ggpred_stem_pul$population <- ordered(ggpred_stem_pul$population, 
                                      levels = c("Northern", "Southern"))

all_CG_growth_pul_elong$population <- ordered(all_CG_growth_pul_elong$population, 
                                        levels = c("Northern", "Southern"))

(ggpred_stem_pul_plot <-ggplot(ggpred_stem_pul) +
    geom_point(data = all_CG_growth_pul_elong, aes(x = Sample_age, y = mean_stem_elong, colour = population),
               alpha = 0.5, size = 1)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Stem elongation (mm)\n") +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
    scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 15)) +
    xlab("Sample age \n" ) +
    scale_colour_manual(values=pal_garden) +
    scale_fill_manual(values=pal_garden) +
    theme_shrub() + 
    theme( axis.text.x  = element_text(angle = 0)))

stem_elong_pul_summ <- model_summ(stem_pul)
stem_elong_pul_summ$Species <- "Salix pulchra"
stem_elong_pul_summ <- stem_elong_pul_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")
rownames(stem_elong_pul_summ) <- c("Intercept", "Sample age", "Southern population"
                                    , "Sample age:Southern population", "Random intercept", 
                                    "sd(Sample age)", "cor(Intercept, Sample age)", "sigma")

stem_elong_pul_summ$Rhat <- as.character(formatC(stem_elong_pul_summ$Rhat, digits = 2, format = 'f'))

stem_elong_pul_summ <- stem_elong_pul_summ %>%
  relocate("Species", .before = "Estimate")

# S. arctica ----
stem_arc <- brms::brm(log(mean_stem_elong) ~ Sample_age*population+(Sample_age|SampleID_standard),
                      data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                      iter = 5000, warmup = 1000, 
                      control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(stem_arc)
saveRDS(stem_arc, file = "output/models/stem_elong_arc_2023.rds")
stem_arc <- readRDS("output/models/stem_elong_arc_2023.rds")

ggpred_stem_arc <- ggpredict(stem_arc, terms = c("Sample_age", "population"))
colnames(ggpred_stem_arc) = c('Sample_age','fit', 'lwr', 'upr',"population")

ggpred_stem_arc$population <- ordered(ggpred_stem_arc$population, 
                                      levels = c("Northern", "Southern"))

all_CG_growth_arc$population <- ordered(all_CG_growth_arc$population, 
                                              levels = c("Northern", "Southern"))

(ggpred_stem_arc_plot <-ggplot(ggpred_stem_arc) +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = mean_stem_elong, colour = population),
               alpha = 0.5, size = 1) + # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Stem elongation (mm)\n") +
    xlab("Sample age \n" ) +
    scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 8)) +
    scale_colour_manual(values=pal_garden) +
    scale_x_continuous(limits = c(1, 8), breaks = seq(1, 8, by = 1)) +
    scale_fill_manual(values=pal_garden) +
    theme_shrub() + 
    theme( axis.text.x  = element_text(angle = 0)))

(ggpred_CG_stem_panel <- ggarrange(ggpred_stem_ric_plot,
                                   ggpred_stem_pul_plot, 
                                   ggpred_stem_arc_plot, nrow = 1,
                                     common.legend = TRUE, 
                                     labels = c("D)", "E)", "F)"),
                                     legend="none", 
  font.label=list(color="black",size = 12)))

ggsave(ggpred_CG_stem_panel, filename ="output/figures/ggpred_CG_stem_panel_2023.png",
       width = 18, height = 8, units = "in", device = png)

(ggpred_growth_panel <- ggarrange(ggpred_CG_height_panel,
                                  ggpred_CG_stem_panel, 
                                   nrow = 2,
                                   common.legend = TRUE,
                                   legend="none", heights = c(0.9, 1)))

ggsave(ggpred_growth_panel, filename ="output/figures/ggpred_CG_growth_panel_2023.png",
       width = 18, height = 14, units = "in", device = png)

(growth_panel <- ggarrange(ggpred_growth_panel, # run all figrues with theme from trait figure to reduce font size
                           growth_maxwidth, 
                                  nrow = 2,
                           heights = c(2, 0.9)))

(growth_panel <- ggarrange(ggpred_CG_height_panel,
                           ggpred_CG_stem_panel, # run all figrues with theme from trait figure to reduce font size
                           growth_maxwidth, 
                           nrow = 3,
                           heights = c(0.95, 0.9, 0.95)))

ggsave(growth_panel, filename ="output/figures/CG_growth_panel_2023.png",
       height = 22, width = 19, unit = "cm", dpi = 500, device = png)

stem_elong_arc_summ <- model_summ(stem_arc)
stem_elong_arc_summ$Species <- "Salix arctica"
stem_elong_arc_summ <- stem_elong_arc_summ %>% 
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI")
rownames(stem_elong_arc_summ) <- c("Intercept", "Sample age", "Southern population"
                                   , "Sample age:Southern population", "Random intercept", 
                                   "sd(Sample age)", "cor(Intercept, Sample age)", "sigma")

stem_elong_arc_summ$Rhat <- as.character(formatC(stem_elong_arc_summ$Rhat, digits = 2, format = 'f'))

stem_elong_arc_summ <- stem_elong_arc_summ %>%
  relocate("Species", .before = "Estimate")

# binding all summaries
all_stem_elong_summ <- rbind(stem_elong_rich_summ, stem_elong_pul_summ, stem_elong_arc_summ)

all_stem_elong_summ_back <- all_stem_elong_summ %>% 
  dplyr::mutate("Estimate (back)" = exp(Estimate)) %>% 
  dplyr::mutate("Lower 95% CI (back)" = exp(l_95_CI_log))%>%
  dplyr::mutate("Upper 95% CI (back)" = exp(u_95_CI_log)) %>%
  dplyr::rename("Est.Error (log)" = "Est.Error") %>%
  dplyr::rename("Lower 95% CI (log)" = "l_95_CI_log") %>%
  dplyr::rename("Upper 95% CI (log)" = "u_95_CI_log")

write.csv(all_stem_elong_summ_back, "outputs/tables/all_stem_elong_time_output.csv")
all_stem_elong_summ_back <- read.csv("outputs/tables/all_stem_elong_time_output.csv")

# BIOVOLUME 2023 ----
# S. richardsonii biovolume ----
biovol_ric <- brms::brm(biovolume ~ Sample_age*population+(Sample_age), # removed sample_id as nester RE bc bulk effective sample size was too low
                       data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                       iter = 5000, warmup = 1000, 
                       control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_ric)
saveRDS(biovol_ric, file = "output/models/biovol_ric_2023.rds")
biovol_ric <- readRDS("output/models/biovol_ric_2023.rds")

ggpred_biovol_ric <- ggpredict(biovol_ric, terms = c("Sample_age", "population"))
colnames(ggpred_biovol_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_height_arc_plot <-ggplot(ggpred_biovol_ric) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = biovolume, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) + 
    ylab("Shrub biovolume (UNIT)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    theme_shrub() + 
    theme( axis.text.x  = element_text(angle = 0)) + 
    labs(title = "Salix richardsonii", size = 20, family = "Helvetica Light"))

# DATA VISUALISATION -----
# color palette for garden only
# pal_garden <- c("#440154FF", "#7AD151FF")

# theme_shrub <- function(){ theme(legend.position = "right",
#                                  axis.title.x = element_text(face="bold", family = "Helvetica Light", size=20),
#                                  axis.text.x  = element_text(vjust=0.5, size=20, family = "Helvetica Light", colour = "black", angle = 270), 
#                                  axis.title.y = element_text(face="bold", family = "Helvetica Light", size=20),
#                                  axis.text.y  = element_text(vjust=0.5, size=20, family = "Helvetica Light", colour = "black"),
#                                  panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
#                                  panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
#                                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
#                                  plot.title = element_text(color = "black", size = 20, family = "Helvetica Light", face = "italic", hjust = 0.5),
#                                  legend.title=element_text(size=16, family = "Helvetica Light"),
#                                  legend.text=element_text(size = 15, family = "Helvetica Light"))}
# 
# theme_shrub_e <- function(){ theme(legend.position = "right",
#                                 axis.title.x = element_text(size=18),
#                                 axis.text.x  = element_text(angle = 35, vjust=0.5, size=14, colour = "black"), 
#                                 axis.title.y = element_text(size=18),
#                                 axis.text.y  = element_text(vjust=0.5, size=14, colour = "black"),
#                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
#                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
#                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
#                                 plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
#                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

# reorder levels to be consistent

# HEIGHT OVER TIME PLOTS-----
pp_draws <- posterior_predict(height_rich, draws = 500)
back_transformed_draws <- exp(pp_draws)

# Salix rich -----
(rich_heights_plot_new <- all_CG_growth_ric %>%
   group_by(population) %>%
   add_predicted_draws(height_rich, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = (Canopy_Height_cm), color = population, fill = population)) +
   stat_lineribbon(aes(y = exp(.prediction)), .width = c(.5), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_color_manual(values=pal_garden) +
   scale_fill_manual(values=pal_garden) +
   theme_shrub() +
   ylab("Canopy height (cm)\n") +
  labs(title = "Salix richardsonii") +
   xlab("\nSample age") +
   scale_x_continuous(breaks = seq(0, 9, by = 1)))

# Salix pulchra ------

(pul_heights_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(height_pul, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = Canopy_Height_cm, color = population, fill = population)) +
   stat_lineribbon(aes(y = exp(.prediction)), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_pul) +
   scale_color_manual(values=pal_garden) +
   scale_fill_manual(values=pal_garden) +
   theme_shrub() +
   labs(title = "Salix pulchra") +
   ylab("Canopy height (cm)\n") +
   xlab("\nSample age") +
   scale_x_continuous(breaks = seq(0, 9, by = 1)))

# Salix arctica------
(arc_heights_plot_new <- all_CG_growth_arc %>%
   group_by(population) %>%
   add_predicted_draws(height_arc,  allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = Canopy_Height_cm, color = population, fill = population)) +
   stat_lineribbon(aes(y = exp(.prediction)), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc) +
   scale_color_manual(values=pal_garden) +
   scale_fill_manual(values=pal_garden) +
   theme_shrub() +
   labs(title = "Salix arctica") +
   ylab("Canopy height (cm)\n") +
   xlab("\nSample age") + scale_x_continuous(breaks = seq(0, 9, by = 1)))


library(ggpubr)

(panel_heights_age <- ggarrange(rich_heights_plot_new, pul_heights_plot_new, arc_heights_plot_new, 
                               common.legend = TRUE, legend="bottom",
                               nrow = 1))
panel_heights_age
# save 
ggsave("figures/height_time_panel.png", height = 10, width = 12, dpi = 300)

# BIOVOLUME OVER TIME PLOTS-----
# Salix rich -----
(rich_biovol_plot_new <- all_CG_growth_ric %>%
   group_by(population) %>%
   add_predicted_draws(garden_rich_biovol_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(biovolume), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix richardsonii biovolume (log cm3)\n") +
   xlab("\nSample age"))

# Salix pulchra ------

(pul_biovol_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(garden_pul_biovol_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(biovolume), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_pul) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix pulchra biovolume (log cm3)\n") +
   xlab("\nSample age"))


# Salix arctica------
(arc_biovol_plot_new <- all_CG_growth_arc %>%
   group_by(population) %>%
   add_predicted_draws(garden_arc_biovol_time,  allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(biovolume), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc) +
   scale_colour_viridis_d(name = "Garden population", begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix arctica biovolume (log cm)\n") +
   xlab("\nSample age")) 

(panel_biovol_age <- ggarrange(rich_biovol_plot_new, pul_biovol_plot_new, arc_biovol_plot_new, 
                               common.legend = TRUE, legend="bottom",
                               nrow = 1))
panel_biovol_age

# HEIGHT GROWTH RATE ------

# Salix rich -----
(rich_rate_plot_new <- all_CG_growth_ric %>%
   group_by(population) %>%
   add_predicted_draws(height_rich_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = height_growth_diff, color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Height growth rate (cm/year)\n") +
   xlab("\nSample age"))

# Salix pul -----
(pul_rate_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(height_pul_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = height_growth_diff, color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Height growth rate (cm/year)\n") +
   xlab("\nSample age"))

# Salix arctica -----
(arc_rate_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(height_arc_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = height_growth_diff, color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix arctica height growth rate (cm/year)\n") +
   xlab("\nSample age"))

(panel_growth_rate_height <- ggarrange(rich_rate_plot_new, pul_rate_plot_new, arc_rate_plot_new, 
                                       common.legend = TRUE, legend="bottom",
                                       nrow = 1))


# BIOVOLUME GROWTH RATE -------
# Salix rich -----
(rich_rate_biovol_plot_new <- all_CG_growth_ric %>%
   group_by(population) %>%
   add_predicted_draws(biovol_rich_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = biovol_growth_diff, color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix richardsonii biovolume growth rate (cm/year)\n") +
   xlab("\nSample age"))

# Salix pul -----
(pul_rate_biovol_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(biovol_pul_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = biovol_growth_diff, color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix pulchra biovolume growth rate (cm/year)\n") +
   xlab("\nSample age"))

# Salix arctica -----
(arc_rate_biovol_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(biovol_arc_time, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = biovol_growth_diff, color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_ric) +
   scale_colour_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("Salix arctica biovolume growth rate (cm/year)\n") +
   xlab("\nSample age"))

(panel_growth_rate_biovol <- ggarrange(rich_rate_biovol_plot_new, pul_rate_biovol_plot_new, arc_rate_biovol_plot_new, 
                                       common.legend = TRUE, legend="bottom",
                                       nrow = 1))


# OVER TIME MODELS ----
(ric_rate_plot <-ggplot() +
   geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = height_growth_diff, colour = population),
              alpha = 0.5)+ # raw data
   geom_smooth(data = all_CG_growth_ric, aes(x = Sample_age, y = height_growth_diff, colour = population,  fill = population),
               alpha = 0.5)+ # raw data
   ylab("Salix richardsonii growth rate (cm/year) \n") +
   xlab("\n Sample age" ) +
   scale_colour_viridis_d(begin = 0.1, end = 0.95) +
   scale_fill_viridis_d(begin = 0.1, end = 0.95) +
   theme_shrub()) 

(pul_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = height_growth_diff, colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_pul, aes(x = Sample_age, y = height_growth_diff, colour = population,  fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix pulchra growth rate (cm/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

(arc_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = height_growth_diff, colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_arc, aes(x = Sample_age, y = height_growth_diff, colour = population, fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix arctica growth rate (cm/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

grid.arrange(ric_rate_plot, pul_rate_plot, arc_rate_plot, 
             nrow=1)

(ric_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_ric, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population,  fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix richardsonii biovolume growth rate (log, cm3/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

(pul_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_pul, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population,  fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix pulchra biovolume growth rate (log, cm3/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

(arc_rate_plot <-ggplot() +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = log(biovol_growth_diff), colour = population),
               alpha = 0.5)+ # raw data
    geom_smooth(data = all_CG_growth_arc, aes(x = Sample_age, y =log(biovol_growth_diff), colour = population, fill = population),
                alpha = 0.5)+ # raw data
    ylab("Salix arctica biovolume growth rate (log, cm3/year) \n") +
    xlab("\n Sample age" ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.95) +
    scale_fill_viridis_d(begin = 0.1, end = 0.95) +
    theme_shrub()) 

grid.arrange(ric_rate_plot, pul_rate_plot, arc_rate_plot, 
             nrow=1)


# by 05 Aug 2023
# 1. HEIGHT OVER TIME MODEL -------
# Salix rich ------
height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population + (1|Year),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich) # significant height growth over time
plot(height_rich)
pp_check(height_rich, type = "dens_overlay", nsamples = 100) 

# extract outputs
height_rich_extract <- model_summ_time(height_rich)

# extraction for model output table
rownames(height_rich_extract) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma")
height_rich_extract_df <- height_rich_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix pulchra -------
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+ (1|Year),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_pul) # significant height growth over time
plot(height_pul)
pp_check(height_pul, type = "dens_overlay", nsamples = 100) 

# extract outputs
height_pul_extract <- model_summ_time(height_pul)

# extraction for model output table
rownames(height_pul_extract) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma")
height_pul_extract_df <- height_pul_extract %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# Salix arctica -------
height_arc <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+ (1|Year),
                        data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_arc) # significant growth over time
plot(height_arc)
pp_check(height_arc, type = "dens_overlay", nsamples = 100) 

# extract outputs
height_arc_extract <- model_summ_time(height_arc)

# extraction for model output table
rownames(height_arc_extract) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma")
height_arc_extract_df <- height_arc_extract %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

garden_heights_interact <- rbind(height_rich_extract_df,height_pul_extract_df,height_arc_extract_df)

# back transforming from log
garden_heights_interact_back <- garden_heights_interact %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# madi thinks to back transform you exp(#) ?
garden_heights_interact_back_mad <- garden_heights_interact %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = exp(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = exp(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = exp(Estimate), 
         Est.Error_trans = exp(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_heights_interact_back, "output/garden_heights_interact_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_heights_interact_back) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma", 
                                            " Intercept", " Sample age", " Southern Garden", " Sample age*Southern Garden", " Year", " sigma",
                                            "Intercept ", "Sample age ", "Southern Garden ", "Sample age*Southern Garden ", "Year ", "sigma ")


# making sure Rhat keeps the .00 
garden_heights_interact_back$Rhat <- as.character(formatC(garden_heights_interact_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_time_interact <- garden_heights_interact_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: canopy height (log cm) over time of northern vs southern shrubs in the common garden. 
      Model structure per species:log(Canopy_Height_cm) ~ Sample_age*population+ (1|Year)", 
      col.names = c("Species","Estimate",
                    "Est. Error",
                    "Lower 95% CI (log)",
                    "Upper 95% CI (log)", 
                    "Rhat", 
                    "Bulk Effective Sample Size",
                    "Tail Effective Sample Size", 
                    "Sample Size",
                    "Effect",
                    "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                    "Estimate transformed", 
                    "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_time_interact, 2, width = NULL, bold = FALSE, italic = TRUE)

# 1.1 HEIGHT GROWTH RATE  ------
# Salix richardsonii ------
hist(all_CG_growth_ric$height_growth_diff) # normal 
height_rich_time <- brms::brm(height_growth_diff ~ population + (1|SampleID_standard) + (1|Sample_age),
                              data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))

summary(height_rich_time) # faster height growth rate over time
pp_check(height_rich_time, type = "dens_overlay", nsamples = 100) 
z <- model_summ_RE(height_rich_time_test)
# extract output with function
rich_extract_time <- model_summ_RE(height_rich_time)

# extraction for model output table
rownames(rich_extract_time) <- c("Intercept", "Southern Garden", "Sample ID", "Sample age", "sigma")
rich_extract_time_df <- rich_extract_time %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S.pulchra -----
hist(all_CG_growth_pul$height_growth_diff) # normal 
height_pul_time <- brms::brm(height_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                             data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(height_pul_time) # no diff
pp_check(height_pul_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
pul_extract_time <- model_summ_RE(height_pul_time)

# extraction for model output table
rownames(pul_extract_time) <-  c("Intercept", "Southern Garden", "Sample ID", "Sample age", "sigma")
pul_extract_time_df <- pul_extract_time %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. arctica -----
hist(all_CG_growth_arc$height_growth_diff) # normal 
height_arc_time <- brms::brm(height_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                             data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(height_arc_time) # faster for southern 
pp_check(height_arc_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
arc_extract_time <- model_summ_RE(height_arc_time)

# extraction for model output table
rownames(arc_extract_time) <- c("Intercept", "Southern Garden", "Sample ID", "Sample age", "sigma")
arc_extract_time_df <- arc_extract_time %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

garden_heights_time <- rbind(rich_extract_time_df,pul_extract_time_df,arc_extract_time_df)

# save df of results 
write.csv(garden_heights_time, "output/garden_heights_time.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_heights_time) <- c("Intercept", "Southern Garden", "Sample ID", "Sample age",
                                   "Sigma", " Intercept", " Southern Garden", " Sample ID", " Sample age",
                                   " Sigma", "Intercept ", "Southern Garden ", "Sample ID ", "Sample age ",
                                   "Sigma ")

# making sure Rhat keeps the .00 
garden_heights_time$Rhat <- as.character(formatC(garden_heights_time$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_heights_time <- garden_heights_time %>% 
  kbl(caption="Table.xxx BRMS model outputs: growth rate (height change per year) northern vs southern shrubs in the common garden. 
      Model structure per species: (height_difference ~ population + (1|Year).", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI ",
                     "Upper 95% CI ", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_heights_time, 2, width = NULL, bold = FALSE, italic = TRUE)


# Loading data ---- 
all_2023_growth <- read_csv("data/common_garden_data_2023/all_data_2023.csv")

# # Salix richardsonii -------
height_rich <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                         data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                         iter = 5000, warmup = 1000, 
                         control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(height_rich, file = "output/models/height_rich_2023.rds")
height_rich <- readRDS("output/models/height_rich_2023.rds")

ggpred_height_ric <- ggpredict(height_rich, terms = c("Sample_age", "population"))
colnames(ggpred_height_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_height_rich_plot <-ggplot(ggpred_height_ric) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    ggtitle(expression(italic("Salix richardsonii"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) 

# just raw data
(richardsonii_height_2023 <- ggplot(all_CG_growth_ric) +
    geom_smooth(aes(x = Sample_age, y = Canopy_Height_cm, colour = population, fill = population, group = population, method = "glm")) +
    geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = population, group = population), size = 1.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
    ylab("Canopy Height (cm)") +
    xlab("\nAge (years)") +
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
          axis.title = element_text(size = 18),
          axis.text.x = element_text(vjust = 0.5, size = 15, colour = "black"),
          axis.text.y = element_text(size = 15, colour = "black")))

# Salix pulchra -----
height_pul <- brms::brm(log(Canopy_Height_cm) ~ Sample_age*population+(Sample_age|SampleID_standard),
                        data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                        iter = 5000, warmup = 1000, 
                        control = list(max_treedepth = 15, adapt_delta = 0.99))

saveRDS(height_pul, file = "output/models/height_pul_2023.rds")
height_pul <- readRDS("output/models/height_pul_2023.rds")

summary(height_pul)

ggpred_height_pul <- ggpredict(height_pul, terms = c("Sample_age", "population"))
colnames(ggpred_height_pul) = c('Sample_age','fit', 'lwr', 'upr',"population")


(ggpred_height_pul_plot <-ggplot(ggpred_height_pul) +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = Canopy_Height_cm, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Canopy height (cm)\n") +
    xlab("\n Sample age " ) +
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    ggtitle(expression(italic("Salix pulchra"))) +
    theme_shrub()+ theme(text=element_text(family="Helvetica Light")) +
    theme(axis.text.x  = element_text(angle = 0)))

# OLDER models from 2022 ----
# Biovolume over time ------
# S. Richardsonii -----
# model
garden_rich_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (Sample_age|SampleID_standard),
                                     data = all_CG_growth_ric, family = gaussian(), chains = 3,
                                     iter = 5000, warmup = 1000, 
                                     control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_biovol_time) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol_time) # fine
pp_check(garden_rich_biovol_time,  type = "dens_overlay", ndraws = 100) # fine

ggpred_biovol_ric <- ggpredict(garden_rich_biovol_time, terms = c("Sample_age", "population"))
colnames(ggpred_biovol_ric) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_biovol_ric_plot <-ggplot(ggpred_biovol_ric) +
    geom_point(data = all_CG_growth_ric, aes(x = Sample_age, y = biovolume, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Biovolume (cm3)\n") +
    xlab("\n Sample age " ) +
    ylim(0, 2376000.000)+
    xlim(3, 9)+
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    ggtitle(expression(italic("Salix richardsonii"))) +
    theme_shrub_e()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

# extract outputs
# extract outputs
rich_biovol_time_extract <- model_summ_time(garden_rich_biovol_time)

# extraction for model output table
rownames(rich_biovol_time_extract) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma")
rich_biovol_time_extract_df <- rich_biovol_time_extract %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. Pulchra -----
garden_pul_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (Sample_age|SampleID_standard),
                                    data = all_CG_growth_pul, family = gaussian(), chains = 3,
                                    iter = 5000, warmup = 1000, 
                                    control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_biovol_time) # significantly larger biovolume for southern shrubs in garden
plot(garden_pul_biovol_time) # fine
pp_check(garden_pul_biovol_time,  type = "dens_overlay", nsamples = 100) # fine

ggpred_biovol_pul <- ggpredict(garden_pul_biovol_time, terms = c("Sample_age", "population"))
colnames(ggpred_biovol_pul) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_biovol_pul_plot <-ggplot(ggpred_biovol_pul) +
    geom_point(data = all_CG_growth_pul, aes(x = Sample_age, y = biovolume, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Biovolume (cm3)\n") +
    xlab("\n Sample age " ) +
    ylim(0, 466096.890)+
    xlim(3, 9)+
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    ggtitle(expression(italic("Salix pulchra"))) +
    theme_shrub_e()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

# extract outputs
pul_biovol_time_extract <- model_summ_time(garden_pul_biovol_time)

# extraction for model output table
rownames(pul_biovol_time_extract) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma")
pul_biovol_time_extract_df <- pul_biovol_time_extract %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. Arctica -----
garden_arc_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population +  (Sample_age|SampleID_standard),
                                    data = all_CG_growth_arc, family = gaussian(), chains = 3,
                                    iter = 5000, warmup = 1000, 
                                    control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_biovol_time) # NOT significant diff. 
plot(garden_arc_biovol_time) # fine
pp_check(garden_arc_biovol_time,  type = "dens_overlay", nsamples = 100) # fine

library(ggeffects)
ggpred_biovol_arc <- ggpredict(garden_arc_biovol_time, terms = c("Sample_age", "population"))
colnames(ggpred_biovol_arc) = c('Sample_age','fit', 'lwr', 'upr',"population")

(ggpred_biovol_arc_plot <-ggplot(ggpred_biovol_arc) +
    geom_point(data = all_CG_growth_arc, aes(x = Sample_age, y = biovolume, colour = population),
               alpha = 0.5)+ # raw data
    geom_line(aes(x = Sample_age , y = fit, colour = population), linewidth = 1)+
    geom_ribbon(aes(x = Sample_age, ymin = lwr, ymax = upr,  fill = population),
                alpha = 0.2) +
    ylab("Biovolume (cm3)\n") +
    xlab("\n Sample age " ) +
    xlim(3, 7)+
    scale_colour_viridis_d(begin = 0.1, end = 0.85) +
    scale_fill_viridis_d(begin = 0.1, end = 0.85) +
    ggtitle(expression(italic("Salix arctica"))) +
    theme_shrub_e()+ theme(text=element_text(family="Helvetica Light")) +
    theme( axis.text.x  = element_text(angle = 0))) # if i log everything it's exactly the same plot as with conditional effects! 

ggpred_CG_biovol_panel <- ggarrange(ggpred_biovol_ric_plot,
                                    ggpred_biovol_pul_plot, 
                                    ggpred_biovol_arc_plot, nrow = 1,
                                    common.legend = TRUE, legend="none")

ggsave("output/figures/ggpred_CG_biovol_panel.png", width = 14.67, height = 6.53, units = "in")

# extract outputs
arc_biovol_time_extract <- model_summ_time(garden_arc_biovol_time)

# extraction for model output table
rownames(arc_biovol_time_extract) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma")
arc_biovol_time_extract_df <- arc_biovol_time_extract %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

garden_biovol_interact <- rbind(rich_biovol_time_extract_df,pul_biovol_time_extract_df,arc_biovol_time_extract_df)

# back transforming from log
garden_biovol_interact_back <- garden_biovol_interact %>%
  dplyr::rename("l_95_CI_log" = "l-95% CI", 
                "u_95_CI_log" = "u-95% CI") %>%
  mutate(CI_range = (Estimate - l_95_CI_log)) %>% 
  mutate(CI_low_trans = 10^(Estimate - CI_range)) %>% 
  mutate(CI_high_trans = 10^(Estimate + CI_range)) %>% 
  mutate(Estimate_trans = 10^(Estimate), 
         Est.Error_trans = 10^(Est.Error)) %>% 
  select(-CI_range)

# save df of results 
write.csv(garden_biovol_interact_back, "output/garden_biovol_interact_back.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_biovol_interact_back) <- c("Intercept", "Sample age", "Southern Garden", "Sample age*Southern Garden", "Year", "sigma", 
                                           " Intercept", " Sample age", " Southern Garden", " Sample age*Southern Garden", " Year", " sigma",
                                           "Intercept ", "Sample age ", "Southern Garden ", "Sample age*Southern Garden ", "Year ", "sigma ")


# making sure Rhat keeps the .00 
garden_biovol_interact_back$Rhat <- as.character(formatC(garden_biovol_interact_back$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_time_interact_biovol <- garden_biovol_interact_back %>% 
  kbl(caption="Table.xxx BRMS model outputs: biovolume (log cm3) over time of northern vs southern shrubs in the common garden. 
      Model structure per species:log(biovolume) ~ Sample_age*population+ (1|Year)", 
      col.names = c("Species","Estimate",
                    "Est. Error",
                    "Lower 95% CI (log)",
                    "Upper 95% CI (log)", 
                    "Rhat", 
                    "Bulk Effective Sample Size",
                    "Tail Effective Sample Size", 
                    "Sample Size",
                    "Effect",
                    "Lower 95% CI 
                    (back transformed)", "Upper 95% CI
                    (back transformed)", 
                    "Estimate transformed", 
                    "Error transformed"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_time_interact_biovol, 2, width = NULL, bold = FALSE, italic = TRUE)

# BIOVOL GROWTH RATE -------

# S.richardsonii ------
hist(all_CG_growth_ric$biovol_growth_diff) # normal 
biovol_rich_time <- brms::brm(biovol_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                              data = all_CG_growth_ric,  family = gaussian(), chains = 3,
                              iter = 5000, warmup = 1000, 
                              control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_rich_time) # no diff
pp_check(biovol_rich_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
rich_extract_timeb <- model_summ_RE(biovol_rich_time)

# extraction for model output table
rownames(rich_extract_timeb) <- c("Intercept", "Southern Garden", "Sample ID", "Sample age", "Sigma")
rich_extract_timeb_df <- rich_extract_timeb %>% 
  mutate(Species = rep("Salix richardsonii")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. pulchra -----
hist(all_CG_growth_pul$biovol_growth_diff) # normal 
biovol_pul_time <- brms::brm(biovol_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                             data = all_CG_growth_pul,  family = gaussian(), chains = 3,
                             iter = 5000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_pul_time) # no diff
pp_check(biovol_pul_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
pul_extract_timeb <- model_summ_RE(biovol_pul_time)

# extraction for model output table
rownames(pul_extract_timeb) <-  c("Intercept", "Southern Garden", "Sample ID", "Sample age", "Sigma")
pul_extract_timeb_df <- pul_extract_timeb %>% 
  mutate(Species = rep("Salix pulchra")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

# S. arctica-----
hist(all_CG_growth_arc$biovol_growth_diff) # normal 
biovol_arc_time <- brms::brm(biovol_growth_diff ~ population  + (1|SampleID_standard) + (1|Sample_age),
                             data = all_CG_growth_arc,  family = gaussian(), chains = 3,
                             iter = 10000, warmup = 1000, 
                             control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(biovol_arc_time) # no diff
pp_check(biovol_arc_time, type = "dens_overlay", nsamples = 100) 

# extract output with function
arc_extract_timeb <- model_summ_RE(biovol_arc_time)

# extraction for model output table
rownames(arc_extract_timeb) <-c("Intercept", "Southern Garden", "Sample ID", "Sample age", "Sigma")
arc_extract_timeb_df <- arc_extract_timeb %>% 
  mutate(Species = rep("Salix arctica")) %>% 
  # "Sample Size" = rep(105)) %>% 
  relocate("Species", .before = "Estimate")%>%
  relocate("nobs", .before = "effect")

garden_heights_timeb <- rbind(rich_extract_timeb_df,pul_extract_timeb_df,arc_extract_timeb_df)

# save df of results 
write.csv(garden_heights_timeb, "output/garden_heights_timeb.csv")

# adding spaces before/after each name so they let me repeat them in the table
rownames(garden_heights_timeb) <- c("Intercept", "Southern Garden", "Sample ID", "Sample age",
                                    "Sigma", " Intercept", " Southern Garden", " Sample ID", " Sample age",
                                    " Sigma", "Intercept ", "Southern Garden ", "Sample ID ", "Sample age ",
                                    "Sigma ")


# making sure Rhat keeps the .00 
garden_heights_timeb$Rhat <- as.character(formatC(garden_heights_timeb$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_biovol_time <- garden_heights_timeb %>% 
  kbl(caption="Table.xxx BRMS model outputs: growth rate (biovolume change per year) northern vs southern shrubs in the common garden. 
      Model structure per species: (biovolume_difference ~ population + (1|Year).", 
      col.names = c( "Species","Estimate",
                     "Est. Error",
                     "Lower 95% CI ",
                     "Upper 95% CI ", 
                     "Rhat", 
                     "Bulk Effective Sample Size",
                     "Tail Effective Sample Size", 
                     "Sample Size",
                     "Effect"), digits=2, align = "c") %>% 
  kable_classic(full_width=FALSE, html_font="Cambria")

# making species column in cursive
column_spec(kable_biovol_time, 2, width = NULL, bold = FALSE, italic = TRUE)

