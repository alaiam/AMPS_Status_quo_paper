library(dplyr)
library(ggplot2)
library(reshape2)
library(FactoMineR)
library(here)
library(rphylopic)
library(cowplot)

### Variables
name_baseline = "Yr2011"
yr_min = 40
###

data <- read.csv("data/biom_comp_run_status_quo.csv")
fg <- read.csv(("data/PugetSoundAtlantisFunctionalGroups_2024_V1_SQ.csv"))


data <- data[data$Year>yr_min, ]



#


data_mean <- left_join(fg, data, by = "longname")%>%
  group_by(Large_group_alt3, run_name) %>%
  mutate(biomass = mean(biomass)) %>%
  distinct(longname, run_name, biomass, IsHeatMap, IsLTL, IsBenthos, IsFish, IsBiggerthanfish,Large_group, Large_group_alt, Large_group_alt2,Subgroup) %>%
  ungroup() %>%
  group_by(longname) %>%
  mutate(biomass_relative = (biomass-biomass[run_name == name_baseline])*100/ biomass[run_name == name_baseline])%>%
  ungroup()

data_mean$biomass_relative[data_mean$biomass_relative>105] <-105
data_mean <- data_mean[data_mean$longname!="Labile detritus",]
data_mean <- data_mean[data_mean$run_name!=name_baseline,]
data_mean$Large_group_alt3 <- factor(data_mean$Large_group_alt3,
                                     levels = rev(unique(data_mean$Large_group_alt3)),
                                     ordered = TRUE)
data_mean$Subgroup <- factor(data_mean$Subgroup,
                                     levels = unique(data_mean$Subgroup),
                                     ordered = TRUE)

sumup <- ggplot(data_mean, aes(x = run_name, y = Large_group_alt3, fill = biomass_relative)) +
  geom_tile() +
  facet_grid(Subgroup ~ ., scales = "free", space = "free") + 
  scale_fill_gradient2(low = "red3", mid = "white", high = "cyan3", midpoint = 0) +  # Palette de couleurs optimisée
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),  # Supprime les grilles
        panel.background = element_rect(fill = "white")) +
  labs(x = "", y = "", fill = "Relative \nBiomass Change (%)")
sumup


# 

data_mean <- left_join(fg, data, by = "longname") %>%
  group_by(longname, run_name) %>%
  mutate(biomass = mean(biomass)) %>%
  distinct(longname, run_name, biomass, IsHeatMap, IsLTL, IsBenthos, IsFish, IsBiggerthanfish,Large_group, Large_group_alt, Large_group_alt2) %>%
  ungroup() %>%
  group_by(longname) %>%
  mutate(biomass_relative = (biomass-biomass[run_name == name_baseline])*100/ biomass[run_name == name_baseline])%>%
  ungroup()
data_mean <- data_mean[!is.na(data_mean$longname),]
data_mean <- data_mean[data_mean$longname!="Carrion",] #Carrion not activated in the model
data_mean$biomass_relative[data_mean$biomass_relative>105] <-105
data_mean <- data_mean[data_mean$longname!="Labile detritus",]
data_mean <- data_mean[data_mean$run_name!=name_baseline,]

data_mean_LTL <- data_mean[data_mean$IsLTL==1,]


# Zoom on groups
data_mean_LTL$longname <- factor(data_mean_LTL$longname,
                             levels = rev(unique(data_mean_LTL$longname)),
                             ordered = TRUE)



LTL <- ggplot(data_mean_LTL, aes(x = run_name, y = longname, fill = biomass_relative)) +
  geom_tile() +
  scale_fill_gradient2(low = "red3", mid = "white", high = "cyan3", midpoint = 0) +  # Palette de couleurs optimisée
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),  # Supprime les grilles
        panel.background = element_rect(fill = "white"), 
        plot.title = element_text(size = 12), 
        plot.margin = unit(c(0,0,0.1,0), "cm")) +
  labs(x = "", y = "", fill = "Relative \nBiomass Change")+
  ggtitle("A. Low Trophic Level (LTL)") 

#
data_mean_Benthos <- data_mean[data_mean$IsBenthos==1,]
data_mean_Benthos$longname <- factor(data_mean_Benthos$longname,
                                 levels = rev(unique(data_mean_Benthos$longname)),
                                 ordered = TRUE)
Benthos <- ggplot(data_mean_Benthos, aes(x = run_name, y = longname, fill = biomass_relative)) +
  geom_tile() +
  scale_fill_gradient2(low = "red3", mid = "white", high = "cyan3", midpoint = 0) +  # Palette de couleurs optimisée
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),  # Supprime les grilles
        panel.background = element_rect(fill = "white"), 
        plot.title = element_text(size = 12), 
        plot.margin = unit(c(0,0,0.1,0), "cm")) +
  labs(x = "", y = "", fill = "Relative \nBiomass Change")+
  ggtitle("B. Benthos") 


#
data_mean_fish <- data_mean[data_mean$IsFish==1,]
data_mean_fish$longname <- factor(data_mean_fish$longname,
                                     levels = rev(unique(data_mean_fish$longname)),
                                     ordered = TRUE)

fish <- ggplot(data_mean_fish, aes(x = run_name, y = longname, fill = biomass_relative)) +
  geom_tile() +
  scale_fill_gradient2(low = "red3", mid = "white", high = "cyan3", midpoint = 0) +  # Palette de couleurs optimisée
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),  # Supprime les grilles
        panel.background = element_rect(fill = "white"), 
        plot.title = element_text(size = 12), 
        plot.margin = unit(c(0,0,0.1,0), "cm")) +
  labs(x = "", y = "", fill = "Relative \nBiomass Change")+
  ggtitle("C. Fish") 


#
data_mean_big <- data_mean[data_mean$IsBiggerthanfish==1,]
data_mean_big$longname <- factor(data_mean_big$longname,
                                  levels = rev(unique(data_mean_big$longname)),
                                  ordered = TRUE)
HTL <- ggplot(data_mean_big, aes(x = run_name, y = longname, fill = biomass_relative)) +
  geom_tile() +
  scale_fill_gradient2(low = "red3", mid = "white", high = "cyan3", midpoint = 0) +  # Palette de couleurs optimisée
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank(),  # Supprime les grilles
        panel.background = element_rect(fill = "white"), 
        plot.title = element_text(size = 12), 
        plot.margin = unit(c(0,0,0.1,0), "cm")) +
  labs(x = "", y = "", fill = "Relative \nBiomass Change")+
  ggtitle("D. High Trophic Level (HTL)") 
 

#
plot_grid(LTL,NULL, Benthos, fish, NULL,HTL, align = "v", ncol = 3, rel_widths = c(1, -0.1, 1), 
          rel_heights = c(.33, .66))

