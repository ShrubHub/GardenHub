# Old code
# code from working scripts that was removed because no longer needed, but keeping for good measure 
# omitting from working scripts to make easier to read/run 

# common_garden_2022 ----
# aka BIG FAT SCRIPT (if you know you know)
# old data viz 
# 4. DATA VISUALISATION ----
# these data are wrong 
all_merged_data_2022 <- read.csv("data/common_garden_data_2022/all_merged_data_2022.csv")

# 4.1. COMMON GARDEN ----

# a. Canopy height(2013-2022) ----
(plot_canopy_2022 <- ggplot(all_merged_data_2022) +
   geom_smooth(aes(x = Sample_age, y = Canopy_Height_cm, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_age, y= Canopy_Height_cm, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
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

# b. Stem elongation (2013-2022) ----
(plot_stem_2022 <- ggplot(all_merged_data_2022) +
   geom_smooth(aes(x = Sample_age, y= mean_stem_elong, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_age, y= mean_stem_elong, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem Elongation (mm)") +
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

# c. Stem diameter (2013-2022) ----
(plot_diameter_2022 <- ggplot(all_merged_data_2022) +
   geom_smooth(aes(x = Sample_age, y = Stem_diameter, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_age, y= Stem_diameter, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Stem diameter (mm)") +
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
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# d. Shrub width (2013-2022) ----
(plot_width_2022 <- ggplot(all_merged_data_2022) +
   geom_boxplot(aes(x= Site, y = mean_width, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Width (cm)") +
   xlab("") +
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
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# Width scatter (2013-2022) ----
(plot_width_scatter <- ggplot(all_merged_data_2022) +
   geom_smooth(aes(x = Sample_age, y = mean_width, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_age, y= mean_width, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Canopy width (cm)") +
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


# Facet
facet_traits <- grid.arrange(plot_canopy_2022, plot_stem_2022, plot_diameter_2022, plot_width_2022, ncol=2)

# e. Leaf length (2013-2022) boxplot ----

(plot_leaf_2022 <- ggplot(all_merged_data_2022) +
   geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Leaf Length (mm)") +
   xlab("") +
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
         axis.title = element_text(size = 14),
         axis.text.x = element_text(vjust = 0.5, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))

# e.1. Leaf length scatter (2013-2022) -----
(plot_leaf_scatter_2022 <- ggplot(all_merged_data_2022) +
   geom_smooth(aes(x = Sample_age, y = mean_leaf_length, colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_age, y= mean_leaf_length, colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   #facet_grid(cols = vars(Species)) +
   facet_wrap(~Species, scales = "free_y") +
   ylab("Leaf Length (mm)") +
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

# f. Leaf length (2022) ----
data_leaf_2022 <- all_merged_data_2022 %>% filter(Year == 2022)

(plot_leaf_2022_only <- ggplot(data_leaf_2022) +
    geom_boxplot(aes(x = Site, y = mean_leaf_length, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
    #facet_grid(cols = vars(Species)) +
    facet_wrap(~Species, scales = "free_y") +
    ylab("Leaf Length (mm)") +
    xlab("") +
    scale_colour_viridis_d(begin = 0.3, end = 0.9) +
    scale_fill_viridis_d(begin = 0.3, end = 0.9) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black")))

# ggsave("scripts/common_garden/common_garden_figures/plot_leaf_2022_only.png", width = 28, height = 20, units = "cm", dpi = 100, plot = plot_leaf_2022_only)

# g. Biovolume (2013-2022) ----
(plot_biovol_2022 <- ggplot(all_merged_data_2022) +
   geom_smooth(aes(x = Sample_age, y = (biovolume/1e+6), colour = Site, fill = Site, group = Site, method = "glm")) +
   geom_point(aes(x = Sample_age, y= (biovolume/1e+6), colour = Site, group = Site), size = 1.5, alpha = 0.5) +
   # geom_boxplot(aes(x = Site, y = biovolume, colour = Site, fill = Site, group = Site), size = 0.5, alpha = 0.5) +
   facet_wrap(~Species, scales = "free") +
   ylab("Biovolume (m3)") +
   xlab("Sample age") +
   scale_colour_viridis_d(begin = 0.3, end = 0.9) +
   scale_fill_viridis_d(begin = 0.3, end = 0.9) +
   theme_bw() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, colour = "black"),
         axis.text.y = element_text(size = 12, colour = "black")))






# Sample size ----
# Need to figure out how to remove NA rows of DEAD shurbs, not fully sen shrubs

# How many shrubs of each type (Arctic vs Alpine?)
count_alpine_shrubs <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane")
# n = 134

count_arctic_shrubs <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk")
# n = 126
# total n = 260

# How many Salix richardsonii from Kluane?
count_salric_kluane <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane",
         Species == "Salix richardsonii")
# n = 44 -3 NA rows = 41

# How many Salix richardsonii from QHI?
count_salric_QHI <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk",
         Species == "Salix richardsonii")
# n = 39 -6 NA rows = 33

# How many Salix pulchra from Kluane?
count_salpul_kluane <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane",
         Species == "Salix pulchra")
# n = 60

# How many Salix pulchra from QHI?
count_salpul_QHI <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk",
         Species == "Salix pulchra")
# n = 56

# How many Salix arctica from QHI?
count_salarct_QHI <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Qikiqtaruk",
         Species == "Salix arctica")
# n = 31

# How many Salix arctica from Kluane?
count_salarct_kluane <- growth_2022 %>%
  filter(Sample_Date == "17/08/2022", 
         Site == "Kluane",
         Species == "Salix arctica")
# n = 30



# MATERNAL EFFECTS ---- 

# previous data manipulation when using only 2022 values, replaced by using max height/widths

# Erica tries to put data in long format  
#cg_means_2022_long <- cg_means_2022 %>%
#group_by(Species, Site) %>%              
# pivot_longer(cols = starts_with("mean"), names_to = "trait", names_prefix = "mean_", values_to = "mean_value") %>%
# pivot_longer(cols = starts_with("sd"), names_to = "SD_trait", names_prefix = "sd_", values_to = "sd_value") %>% 
#  pivot_longer(cols = starts_with("se"), names_to = "SE_trait", names_prefix = "se_", values_to = "se_value")
# same as above but for means for mothers   
# mother_means_long <- mother_cg_means %>%
#  group_by(Species, Site) %>%              
#  pivot_longer(cols = starts_with("mean"), names_to = "trait", names_prefix = "mean_", values_to = "mean_value") %>%
#  pivot_longer(cols = starts_with("sd"), names_to = "SD_trait", names_prefix = "sd_", values_to = "sd_value") %>% 
#  pivot_longer(cols = starts_with("se"), names_to = "SE_trait", names_prefix = "se_", values_to = "se_value")
# merge together 
#means_long_all <- rbind(mother_means_long, cg_means_2022_long) # not perfect, will fiddle with 

# checking format of CG data
#str(all_cg)
#all_cg$Site <- as.factor(all_cg$Site)
#all_cg$Species <- as.factor(all_cg$Species)

# merging means datasets and making into long format
# means_all <- rbind(cg_means_2022, mother_cg_means) 

# merging datasets

# but first keep only 2022 data from common garden 
# cg_2022 <- all_cg %>% 
#  filter(Year == "2022")

# mother_cg <- full_join(mother_data_merge, cg_2022, 
#                       by = c("SampleID_standard" = "SampleID_standard", 
#                             "Year_planted" = "Year_planted", 
#                             "SampleDate" = "Sample_Date",
#                             "Species" = "Species",
#                            "Site" = "Site", 
#                         "Sample_age" = "Sample_age"))

# # madi checking means a slightly way to compare values  
#mother_cg_means_MA <- mother_data_merge %>%
#  group_by(Species, Site) %>%
#  summarise_at(c("Mother_Canopy_Height_cm", "Mother_mean_width", 
#                 "Mother_mean_stem_elong", "Cutting_diameter"
#                 ), mean, na.rm = TRUE) # woohoo they match! 
# and sd 
#mother_cg_sd_MA <-  mother_data_merge %>% 
#  group_by(Species, Site) %>%
#  summarise_at(c("Mother_Canopy_Height_cm", "Mother_mean_width", 
#                 "Mother_mean_stem_elong", "Cutting_diameter"),
#               sd, na.rm = TRUE) 

# previous figures and data manipulation to create figures 
# HEIGHTS: making one single column for each trait and a "treatment" column for mother/child
mother_cg_long_heights <- mother_cg %>%
  select(SampleDate,  Year_planted, Mother_Canopy_Height_cm, Canopy_Height_cm,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Mother_Canopy_Height_cm","Canopy_Height_cm"),
               names_to = "mother_or_child", values_to = "Height_cm")


# reclassing variables
mother_cg_long_heights$mother_or_child <- as.factor(mother_cg_long_heights$mother_or_child)
mother_cg_long_heights$Sample_age <- as.factor(mother_cg_long_heights$Sample_age)
mother_cg_long_heights$SampleID_standard <- as.factor(mother_cg_long_heights$SampleID_standard)

# renaming 
levels(mother_cg_long_heights$mother_or_child) <- list(Mother  = "Mother_Canopy_Height_cm", Child = "Canopy_Height_cm")

# WIDTHS: making one single column for each trait and a "mother_or_child" column for mother/child
mother_cg_long_widths <- mother_cg %>%
  select(SampleDate,  Year_planted, Mother_mean_width, mean_width,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Mother_mean_width","mean_width"),
               names_to = "mother_or_child", values_to = "Width")

# reclassing variables
mother_cg_long_widths$mother_or_child <- as.factor(mother_cg_long_heights$mother_or_child)
mother_cg_long_widths$Sample_age <- as.factor(mother_cg_long_heights$Sample_age)
mother_cg_long_widths$SampleID_standard <- as.factor(mother_cg_long_heights$SampleID_standard)

# renaming 
levels(mother_cg_long_widths$mother_or_child) <- list(Mother  = "Mother_mean_width", Child = "mean_width")

# ELONG: making one single column for each trait and a "mother_or_child" column for mother/child
mother_cg_long_elong <- mother_cg %>%
  select(SampleDate,  Year_planted, Mother_mean_stem_elong, mean_stem_elong,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Mother_mean_stem_elong","mean_stem_elong"),
               names_to = "mother_or_child", values_to = "Stem_elongation")

# reclassing variables
mother_cg_long_elong$mother_or_child <- as.factor(mother_cg_long_elong$mother_or_child)
mother_cg_long_elong$Sample_age <- as.factor(mother_cg_long_elong$Sample_age)
mother_cg_long_elong$SampleID_standard <- as.factor(mother_cg_long_elong$SampleID_standard)

# renaming 
levels(mother_cg_long_elong$mother_or_child) <- list(Mother  = "Mother_mean_stem_elong", Child = "mean_stem_elong")

# DIAM: making one single column for each trait and a "mother_or_child" column for mother/child
mother_cg_long_diam <- mother_cg %>%
  select(SampleDate,  Year_planted, Cutting_diameter, Stem_diameter,
         Species, SampleYear, SampleID_standard, Site, Year, Sample_age) %>%
  pivot_longer(cols=c ("Cutting_diameter","Stem_diameter"),
               names_to = "mother_or_child", values_to = "Stem_diam")

# reclassing variables
mother_cg_long_diam$mother_or_child <- as.factor(mother_cg_long_diam$mother_or_child)
mother_cg_long_diam$Sample_age <- as.factor(mother_cg_long_diam$Sample_age)
mother_cg_long_diam$SampleID_standard <- as.factor(mother_cg_long_diam$SampleID_standard)

# renaming 
levels(mother_cg_long_diam$mother_or_child) <- list(Mother  = "Cutting_diameter", Child = "Stem_diameter")

# 4. DATA VISUALISATION ----

# filter out means only for height 
# but note: these means are not what the figure lines are  
height_means <- means_long_all %>% 
  filter(trait %in% c("mother_height", "Canopy_Height_cm")) %>% 
  mutate(mother_or_child = case_when(trait == "mother_height" ~ "Mother", 
                                     trait == "Canopy_Height_cm" ~ "Child"))

# Heights
(plot_mother_compare_heights <- ggplot() +
    geom_point(aes(x = mother_or_child, y= Height_cm, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.1, data = mother_cg_long_heights)) +
  geom_smooth(aes(x = mother_or_child, y= Height_cm, colour = Site, fill = Site, group = SampleID_standard, alpha = 0.01), method = "lm", se = F, alpha = 0.01, data = mother_cg_long_heights) +
  geom_point(aes(x = mother_or_child, y= mean_value, colour = Site), size = 3, alpha = 0.7, data = height_means, colour = "red") +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Canopy Height (cm)") +
  xlab("\nTreatment") +
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
        axis.text.y = element_text(size = 15, colour = "black"))

# width means
width_means <- means_long_all %>% 
  filter(trait %in% c("mother_width", "mean_width")) %>% 
  mutate(mother_or_child = case_when(trait == "mother_width" ~ "Mother", 
                                     trait == "mean_width" ~ "Child"))

# Widths
(plot_mother_compare_widths <- ggplot(mother_cg_long_widths) +
    geom_point(aes(x = mother_or_child, y= Width, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.5)) +
  geom_smooth(aes(x = mother_or_child, y= Width, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.2) +
  geom_point(aes(x = mother_or_child, y= mean_value, colour = Site), size = 3, alpha = 0.7, data = width_means, colour = "red") +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Width (cm)") +
  xlab("\nTreatment") +
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
        axis.text.y = element_text(size = 15, colour = "black"))

# elongation means
elong_means <- means_long_all %>% 
  filter(trait %in% c("mean_mother_elong", "mean_stem_elong")) %>% 
  mutate(mother_or_child = case_when(trait == "mean_mother_elong" ~ "Mother", 
                                     trait == "mean_stem_elong" ~ "Child"))

# Stem elongation
(plot_mother_compare_elong <- ggplot(mother_cg_long_elong) +
    geom_point(aes(x = mother_or_child, y= Stem_elongation, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.5)) +
  geom_smooth(aes(x = mother_or_child, y= Stem_elongation, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.2) +
  geom_point(aes(x = mother_or_child, y= mean_value, colour = Site), size = 3, alpha = 0.7, data = elong_means, colour = "red") +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Stem elongation (mm)") +
  xlab("\nTreatment") +
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
        axis.text.y = element_text(size = 15, colour = "black"))

# diameter means
diam_means <- means_long_all %>% 
  filter(trait %in% c("mother_diam", "Stem_diameter")) %>% 
  mutate(mother_or_child = case_when(trait == "mother_diam" ~ "Mother", 
                                     trait == "Stem_diameter" ~ "Child"))

# Stem diameter
(plot_mother_compare_diam <- ggplot(mother_cg_long_diam) +
    geom_point(aes(x = mother_or_child, y= Stem_diam, colour = Site, group = SampleID_standard), size = 1.5, alpha = 0.5)) +
  geom_smooth(aes(x = mother_or_child, y= Stem_diam, colour = Site, fill = Site, group = SampleID_standard), method = "lm", se = F, alpha = 0.2) +
  geom_point(aes(x = mother_or_child, y= mean_value, colour = Site), size = 3, alpha = 0.7, data = diam_means, colour = "red") +
  #facet_grid(cols = vars(Species)) +
  facet_wrap(~Species, scales = "free_y") +
  ylab("Stem diameter (mm)") +
  xlab("\nTreatment") +
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
        axis.text.y = element_text(size = 15, colour = "black"))




# previous data analyses 

# 1. HEIGHTS: effect of mother heights on canopy heights in the CG
#maternal_height <- lmer(Height_cm ~ mother_or_child + (1|Species) + (1|SampleID_standard) + (1|Sample_age), data = mother_cg_long_heights)
#summary(maternal_height)
#tab_model(maternal_height)
#plot(maternal_height)
#qqnorm(resid(maternal_height))
#qqline(resid(maternal_height)) 

# 2. WIDTHS: effect of mother widths on widths in the CG
# N.B. boundary (singular) fit: see help('isSingular') --> not converging
#maternal_width <- lmer(Width~mother_or_child  + (1|Sample_age) + (1|Species) + (1|Sample_age), data = mother_cg_long_widths)
#summary(maternal_width )
#tab_model(maternal_width )
#plot(maternal_width )
#qqnorm(resid(maternal_width ))
#qqline(resid(maternal_width )) 

# 3. ELONG: effect of mother elongation on elongation in the CG
#maternal_elong<- lmer(Stem_elongation ~ mother_or_child + (1|SampleID_standard) + (1|Species) + (1|Sample_age), data = mother_cg_long_elong)
#summary(maternal_elong)
#tab_model(maternal_elong)
#plot(maternal_elong)
#qqnorm(resid(maternal_elong))
#qqline(resid(maternal_elong)) 

# 4. DIAMETER: effect of mother diameters on diameters in the CG
#maternal_diam <- lmer(Stem_diam~mother_or_child + (1|SampleID_standard) + (1|Species), data = mother_cg_long_diam)
#summary(maternal_diam)
#tab_model(maternal_diam)
#plot(maternal_diam)
#qqnorm(resid(maternal_diam))
#qqline(resid(maternal_diam)) 



