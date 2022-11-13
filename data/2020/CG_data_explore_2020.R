# exploring common garden data from 2013 to 2020
# mostly based on Haydn's code exploring data from 2013 to 2017 'Common_garden_explore.R'

# libraries 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(readr)
library(gridExtra)
library(tidyr)
library(chron)
library(reshape2)
library(lmerTest)
library(effects)
library(lmerTest)
library(stringr)
library(ggpubr)

`%notin%` <- function(x,y) !(x %in% y)
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

# import data 
survival_2020 <- read.csv("scripts/common_garden/common_garden_data_2020/all_survival_2020.csv")
str(survival_2020)
traits <- read.csv("scripts/common_garden/common_garden_data_2020/all_traits_2020.csv")
str(traits)

# Remove Betula nana 
survival_2020<-subset(survival_2020, Species != "Betula nana")

# simplify sampling locations to Kluane vs Qikiqutaruk 
survival_2020$Sample_location<-as.character(survival_2020$Sample_location)
survival_2020[survival_2020$Sample_location == "QHI",]$Sample_location<-"Qikiqtaruk"
survival_2020[survival_2020$Sample_location != "Qikiqtaruk",]$Sample_location<-"Kluane"

#filter to only include individuals alive as of 2020
survival_2020$Survived_2020 <- as.numeric(survival_2020$Survived_2020)
alive_2020 <- filter(survival_2020, Survived_2020 == 1) 

(alive_2020_p <- ggplot(alive_2020, aes(x = Species, fill = Sample_location)) +
  geom_bar() + 
    scale_fill_manual(values = c("forestgreen","blue4"), 
                      labels = c("Kluane", "Qikiqtaruk"),
                      name = "Origin Site\n") +
  labs(x = "Species", y = "number of individuals", fill = "source location") +
  labs(x = "Sample Age", y = "Canopy Height (cm)") +
  theme_bw() +
  theme(legend.justification = "top")) 

# TRAITS 

# Reorganize data
traits$Height<-as.numeric(as.character(traits$Height))
traits$LL1<-as.numeric(as.character(traits$LL1))
traits$LL2<-as.numeric(as.character(traits$LL2))
traits$LL3<-as.numeric(as.character(traits$LL3))
traits$SE1<-as.numeric(as.character(traits$SE1))
traits$SE2<-as.numeric(as.character(traits$SE2))
traits$SE3<-as.numeric(as.character(traits$SE3))
traits$Fresh_mass<-as.numeric(as.character(traits$Fresh_mass))
traits$Dry_mass<-as.numeric(as.character(traits$Dry_mass))
traits$LDMC<-as.numeric(as.character(traits$LDMC))
traits$LA<-as.numeric(as.character(traits$LA))
traits$SLA<-as.numeric(as.character(traits$SLA))

# Add species and other sampling characteristics from survival data 
traits$Species<-survival_2020$Species[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits<-subset(traits, !is.na(Species))

traits$Year_planted<-survival_2020$Year_planted[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Sample_location<-survival_2020$Sample_location[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_height<-survival_2020$Mother_height[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_CW_1<-survival_2020$Mother_CW_1[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_CW_2<-survival_2020$Mother_CW_2[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_LS<-survival_2020$Mother_LS[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_LL1<-survival_2020$Mother_LL1[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_LL2<-survival_2020$Mother_LL2[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_LL3<-survival_2020$Mother_LL3[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_SE1<-survival_2020$Mother_SE1[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_SE2<-survival_2020$Mother_SE2[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_SE3<-survival_2020$Mother_SE3[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Mother_Sex<-survival_2020$Mother_Sex[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Cutting_length<-survival_2020$Cutting_length[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Cutting_diameter<-survival_2020$Cutting_diameter[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]
traits$Cutting_age<-survival_2020$Cutting_age[match(tolower(traits$Sample_ID), tolower(survival_2020$Sample_ID))]

traits$Sample_Age<-traits$Year - traits$Year_planted

# simplify source locations 
traits$Sample_location<-as.character(traits$Sample_location)
traits[traits$Sample_location == "QHI",]$Sample_location<-"Qikiqtaruk"
traits[traits$Sample_location != "Qikiqtaruk",]$Sample_location<-"Kluane"

# Height plots
# Exclude Salix arctica
supa_sari_heights<-subset(traits, Species !="Salix arctica" & Height>5)
# make copy of heights subset with all three species to group by location
plant_heights <-traits

supa_sari_heights$Sample_location<-as.factor(supa_sari_heights$Sample_location)
height.mod<-lmer(Height ~ log(Sample_Age) * Sample_location * Species + (1|Year_planted/Sample_Date) + (1|Mother_height) + (1|Mother_LS), data = supa_sari_heights, na.action = na.omit)
sapu_sari_out_height<-as.data.frame(effect(c("log(Sample_Age)","Sample_location", "Species"),height.mod, xlevels = list(Sample_Age = seq(1,7, .1))))

(sapu_sari_height<-ggplot(supa_sari_heights)+
    geom_point(aes(Sample_Age,Height, colour = factor(Sample_location):factor(Species)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = sapu_sari_out_height, mapping = aes(x = Sample_Age, ymin = lower, ymax = upper, fill = factor(Sample_location):factor(Species)), alpha=0.5, show.legend = F) +
    geom_line(data = sapu_sari_out_height, mapping = aes(x = Sample_Age, y = fit, colour = factor(Sample_location):factor(Species)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Canopy Height (cm)")+
    scale_fill_manual(values = c("forestgreen","darkgreen","blue2","blue4"))+
    scale_colour_manual(values = c("forestgreen","darkgreen","blue2","blue4"),
                        labels = c("Kluane: Salix pulchra", "Kluane: Salix richardsonii",
                                   "Qikiqtaruk: Salix pulchra", "Qikiqtaruk: Salix richardsonii"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))

# Heights with Salix richardsonii and salix pulchra 
height.mod_all<-lmer(Height ~ log(Sample_Age) * Sample_location + Species+  (1|Year_planted/Sample_Date) + (1|Mother_height) + (1|Mother_LS), data = plant_heights, na.action = na.omit)
out_height_all<-as.data.frame(effect(c("log(Sample_Age)","Sample_location"),height.mod_all, xlevels = list(Sample_Age = seq(1,7, .1))))

(heights_all<-ggplot(plant_heights)+
    geom_point(aes(Sample_Age,Height, colour = factor(Sample_location)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = out_height_all, mapping = aes(x = Sample_Age, ymin = lower, ymax = upper, fill = factor(Sample_location)), alpha=0.5, show.legend = F) +
    geom_line(data = out_height_all, mapping = aes(x = Sample_Age, y = fit, colour = factor(Sample_location)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Canopy Height (cm)")+
    scale_fill_manual(values = c("forestgreen","blue2"))+
    scale_colour_manual(values = c("forestgreen","blue2"),
                        labels = c("Kluane", "Qikiqtaruk"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))

#####################
# Heights, same as above with including Salix arctica 
# make copy of data set
all_heights<-traits

all_heights$Sample_location<-as.factor(all_heights$Sample_location)
height.mod_all<-lmer(Height ~ log(Sample_Age) * Sample_location * Species + (1|Year_planted/Sample_Date) + (1|Mother_height) + (1|Mother_LS), data = all_heights, na.action = na.omit)
all_out_height_all<-as.data.frame(effect(c("log(Sample_Age)","Sample_location", "Species"),height.mod_all, xlevels = list(Sample_Age = seq(1,7, .1))))

(full_heights<-ggplot(all_heights)+
    geom_point(aes(Sample_Age,Height, colour = factor(Sample_location):factor(Species)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = all_out_height_all, mapping = aes(x = Sample_Age, ymin = lower, ymax = upper, fill = factor(Sample_location):factor(Species)), alpha=0.5, show.legend = F) +
    geom_line(data = all_out_height_all, mapping = aes(x = Sample_Age, y = fit, colour = factor(Sample_location):factor(Species)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Canopy Height (cm)")+
    scale_fill_manual(values = c("darkolivegreen2", "forestgreen","darkgreen","cornflowerblue","blue2","blue4"))+
    scale_colour_manual(values = c("darkolivegreen2", "forestgreen","darkgreen","cornflowerblue","blue2","blue4"),
                        labels = c("Kluane: Salix arctica", "Kluane: Salix pulchra", "Kluane: Salix richardsonii",
                                   "Qikiqtaruk: Salix arctica", "Qikiqtaruk: Salix pulchra", "Qikiqtaruk: Salix richardsonii"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))


#Leaf Lengths####
exclude<-c("Height","SE1","SE2","SE3","Fresh_mass","Dry_mass","LA","LDMC","SLA","Notes")

leaf_lengths<-traits[,colnames(traits)%notin%exclude] %>%
  gather(key = Leaf, value = Leaf_length, 4:6)

# Individual species leaf length
leaf_lengths$Sample_location<-as.factor(leaf_lengths$Sample_location)
leaf.mod<-lmer(Leaf_length ~ log(Sample_Age) * Sample_location * Species + (1|Year_planted/Sample_ID/Sample_Date) + (1|Mother_height) + (1|Mother_LS), data = leaf_lengths, na.action = na.omit)
out_LL<-as.data.frame(effect(c("log(Sample_Age)","Sample_location", "Species"),leaf.mod, xlevels = list(Sample_Age = seq(1,7, .1))))

(LL_indiv<-ggplot(leaf_lengths)+
    geom_point(aes(Sample_Age,Leaf_length, colour = factor(Sample_location):factor(Species)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = out_LL, mapping = aes(x = Sample_Age, ymin = lower, ymax = upper, fill = factor(Sample_location):factor(Species)), alpha=0.4, show.legend = F) +
    geom_line(data = out_LL, mapping = aes(x = Sample_Age, y = fit, colour = factor(Sample_location):factor(Species)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Leaf Length (mm)")+
    scale_fill_manual(values = c("darkolivegreen2", "forestgreen","darkgreen","cornflowerblue","blue2","blue4"))+
    scale_colour_manual(values = c("darkolivegreen2", "forestgreen","darkgreen","cornflowerblue","blue2","blue4"),
                        labels = c("Kluane: Salix arctica", "Kluane: Salix pulchra", "Kluane: Salix richardsonii",
                                   "Qikiqtaruk: Salix arctica", "Qikiqtaruk: Salix pulchra", "Qikiqtaruk: Salix richardsonii"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))
# All species
leaf_lengths$Sample_location<-as.factor(leaf_lengths$Sample_location)
leaf.mod_all<-lmer(Leaf_length ~ log(Sample_Age) * Sample_location + Species + (1|Year_planted/Sample_Date/Sample_ID) + (1|Mother_height) + (1|Mother_LS), data = leaf_lengths, na.action = na.omit)
out_LL_all<-as.data.frame(effect(c("log(Sample_Age)","Sample_location"),leaf.mod_all, xlevels = list(Sample_Age = seq(1,7, .1))))

(LL_all<-ggplot(leaf_lengths)+
    geom_point(aes(Sample_Age,Leaf_length, colour = factor(Sample_location)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = out_LL_all, mapping = aes(x = Sample_Age, ymin = lower, ymax = upper, fill = factor(Sample_location)), alpha=0.4, show.legend = F) +
    geom_line(data = out_LL_all, mapping = aes(x = Sample_Age, y = fit, colour = factor(Sample_location)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Leaf Length (mm)")+
    scale_fill_manual(values = c("forestgreen","blue2"))+
    scale_colour_manual(values = c("forestgreen","blue2"),
                        labels = c("Kluane", "Qikiqtaruk"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))


# Stem Elongation####
exclude<-c("Height","LL1","LL2","LL3","Fresh_mass","Dry_mass","LA","LDMC","SLA","Notes")

stem_elon<-traits[,colnames(traits)%notin%exclude] %>%
  gather(key = Stem, value = Stem_elongation, 4:6)

# Individual species stem elongation 
stem_elon$Sample_location<-as.factor(stem_elon$Sample_location)
stem.mod<-lmer(Stem_elongation ~ log(Sample_Age) * Sample_location * Species + (1|Year_planted/Sample_ID/Sample_Date) + (1|Mother_height) + (1|Mother_LS), data = stem_elon, na.action = na.omit)
out_SE<-as.data.frame(effect(c("log(Sample_Age)","Sample_location", "Species"),stem.mod, xlevels = list(Sample_Age = seq(1,7, .1))))

(SE_indiv<-ggplot(stem_elon)+
    geom_point(aes(Sample_Age,Stem_elongation/10, colour = factor(Sample_location):factor(Species)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = out_SE, mapping = aes(x = Sample_Age, ymin = lower/10, ymax = upper/10, fill = factor(Sample_location):factor(Species)), alpha=0.4, show.legend = F) +
    geom_line(data = out_SE, mapping = aes(x = Sample_Age, y = fit/10, colour = factor(Sample_location):factor(Species)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Stem Elongation (cm)")+
    scale_fill_manual(values = c("darkolivegreen2", "forestgreen","darkgreen","cornflowerblue","blue2","blue4"))+
    scale_colour_manual(values = c("darkolivegreen2", "forestgreen","darkgreen","cornflowerblue","blue2","blue4"),
                        labels = c("Kluane: Salix arctica", "Kluane: Salix pulchra", "Kluane: Salix richardsonii",
                                   "Qikiqtaruk: Salix arctica", "Qikiqtaruk: Salix pulchra", "Qikiqtaruk: Salix richardsonii"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))

# All species stem elongation 
stem.mod_all<-lmer(Stem_elongation ~ log(Sample_Age) * Sample_location + Species + (1|Year_planted/Sample_ID/Sample_Date) + (1|Mother_height) + (1|Mother_LS), data = stem_elon, na.action = na.omit)
out_SE_all<-as.data.frame(effect(c("log(Sample_Age)","Sample_location"),stem.mod_all, xlevels = list(Sample_Age = seq(1,7, .1))))

(SE_all<-ggplot(stem_elon)+
    geom_point(aes(Sample_Age,Stem_elongation/10, colour = factor(Sample_location)), alpha = 0.1)+
    theme_classic()+
    geom_ribbon(data = out_SE_all, mapping = aes(x = Sample_Age, ymin = lower/10, ymax = upper/10, fill = factor(Sample_location)), alpha=0.4, show.legend = F) +
    geom_line(data = out_SE_all, mapping = aes(x = Sample_Age, y = fit/10, colour = factor(Sample_location)), lwd = 1.5)+
    labs(x = "Sample Age", y = "Stem Elongation (cm)")+
    scale_fill_manual(values = c("forestgreen","blue2"))+
    scale_colour_manual(values = c("forestgreen","blue2"),
                        labels = c("Kluane", "Qikiqtaruk"),
                        name = "Origin Site\n")+
    theme(legend.justification = "top"))


# arrange canopy height, leaf length and stem elongation plots 

ggarrange(full_heights, LL_indiv, SE_indiv,heights_all, LL_all, SE_all,
          nrow=2, ncol = 3, common.legend = TRUE, legend="bottom")

