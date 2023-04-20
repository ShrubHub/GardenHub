
# Loading libraries ----
library(brms)
library(tidyverse)
library(tidybayes)
library(dplyr)
library(knitr) # For kable tables
library(kableExtra) # For kable tables
library(gridExtra)
library(ggpubr)

# Loading data ---- 
all_CG_source_growth <- read_csv("data/all_CG_source_growth.csv")

# 1. scale function =====
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# 2. extract model result function =====

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

# making a growth rate column
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


# 1. BIOVOLUME over time ------
# S. Richardsonii -----
# model
garden_rich_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (1|Year) ,
                                     data = all_CG_growth_ric, family = gaussian(), chains = 3,
                                     iter = 5000, warmup = 1000, 
                                     control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_rich_biovol_time) # significantly larger biovolume for southern shrubs in garden
plot(garden_rich_biovol_time) # fine
pp_check(garden_rich_biovol_time,  type = "dens_overlay", ndraws = 100) # fine

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
garden_pul_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (1|Year),
                                    data = all_CG_growth_pul, family = gaussian(), chains = 3,
                                    iter = 5000, warmup = 1000, 
                                    control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_pul_biovol_time) # significantly larger biovolume for southern shrubs in garden
plot(garden_pul_biovol_time) # fine
pp_check(garden_pul_biovol_time,  type = "dens_overlay", nsamples = 100) # fine

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
garden_arc_biovol_time <- brms::brm(log(biovolume) ~ Sample_age*population + (1|Year),
                                    data = all_CG_growth_arc, family = gaussian(), chains = 3,
                                    iter = 5000, warmup = 1000, 
                                    control = list(max_treedepth = 15, adapt_delta = 0.99))


summary(garden_arc_biovol_time) # NOT significant diff. 
plot(garden_arc_biovol_time) # fine
pp_check(garden_arc_biovol_time,  type = "dens_overlay", nsamples = 100) # fine

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

#2.1. BIOVOL GROWTH RATE -------

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

# DATA VISUALISATION -----
# color palette for garden only
pal_garden <- c("#440154FF", "#7AD151FF")


theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=12),
                                 axis.text.x  = element_text(vjust=0.5, size=12, colour = "black", angle = 45), 
                                 axis.title.y = element_text(face="bold", size=12),
                                 axis.text.y  = element_text(vjust=0.5, size=12, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),
                                 plot.margin = unit(c(1,1,1,1), units = , "cm"))}

theme_shrub <- function(){ theme(legend.position = "right",
                                 axis.title.x = element_text(face="bold", size=16),
                                 axis.text.x  = element_text(vjust=0.5, size=16, colour = "black", angle = 60), 
                                 axis.title.y = element_text(face="bold", size=16),
                                 axis.text.y  = element_text(vjust=0.5, size=16, colour = "black"),
                                 panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                                 panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                 plot.title = element_text(color = "black", size = 16, face = "bold.italic", hjust = 0.5),
                                 legend.title=element_text(size=16),
                                 legend.text=element_text(size = 15))}
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
   xlab("\nSample age"))

# Salix pulchra ------

(pul_heights_plot_new <- all_CG_growth_pul %>%
   group_by(population) %>%
   add_predicted_draws(height_pul, allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = (Canopy_Height_cm), color = population, fill = population)) +
   stat_lineribbon(aes(y = exp(.prediction)), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_pul) +
   scale_color_manual(values=pal_garden) +
   scale_fill_manual(values=pal_garden) +
   theme_shrub() +
   labs(title = "Salix pulchra") +
   coord_cartesian(ylim=c(0, 200)) +
   ylab("\n") +
   xlab("\nSample age"))

# Salix arctica------
(arc_heights_plot_new <- all_CG_growth_arc %>%
   group_by(population) %>%
   add_predicted_draws(height_arc,  allow_new_levels = TRUE) %>%
   ggplot(aes(x = Sample_age, y = log(Canopy_Height_cm), color = ordered(population), fill = ordered(population))) +
   stat_lineribbon(aes(y = .prediction), .width = c(.50), alpha = 1/4) +
   geom_point(data = all_CG_growth_arc) +
   scale_colour_viridis_d(name = "Garden population", begin = 0.1, end = 0.85) +
   scale_fill_viridis_d(name = "Garden population",begin = 0.1, end = 0.85) +
   theme_shrub() +
   ylab("\n") +
   xlab("\nSample age"))

library(ggpubr)

panel_heights_age <- ggarrange(rich_heights_plot_new, pul_heights_plot_new, arc_heights_plot_new, 
                               common.legend = TRUE, legend="bottom",
                               nrow = 1)
panel_heights_age

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
