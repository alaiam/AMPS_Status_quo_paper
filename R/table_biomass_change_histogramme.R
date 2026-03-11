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
fg <- read.csv(("data/PugetSoundAtlantisFunctionalGroups_2024_V1_SQ.csv"))

# Vertebrate vs invert (without detritus and phytoplankton)

data <- read.csv("data/biom_comp_run_status_quo.csv")

data <- data[data$Year>yr_min, ]
data <- data[data$longname!="Pelagic bacteria",]
data <- data[data$longname!="Benthic bacteria",]
data <- data[data$longname!="Refractory detritus",]
data <- data[data$longname!="Labile detritus",]

data <- data[data$longname!="Large phytoplankton",]
data <- data[data$longname!="Small phytoplankton",]



data_mean <- left_join(fg, data, by = "longname")%>%
  group_by(isVertebrate, run_name) %>%
  mutate(biomass = sum(biomass)/50) %>%
  distinct(isVertebrate, run_name, biomass) %>%
  ungroup() 
data_mean <- data_mean[!is.na(data_mean$biomass),]
data_mean <- data_mean%>%
  group_by(isVertebrate) %>%
  mutate(biomass_relative = (biomass-biomass[run_name == name_baseline])*100/ biomass[run_name == name_baseline])%>%
  ungroup()
data_mean

# Detritus
data <- read.csv("data/biom_comp_run_status_quo.csv")
data <- data[data$longname%in% c("Refractory detritus","Labile detritus"),]
data <- data[data$Year>yr_min, ]

data_mean <- left_join(fg, data, by = "longname")%>%
  group_by(isVertebrate, run_name) %>%
  mutate(biomass = sum(biomass)/50) %>%
  distinct(isVertebrate, run_name, biomass) %>%
  ungroup() 
data_mean <- data_mean[!is.na(data_mean$biomass),]
data_mean <- data_mean%>%
  group_by(isVertebrate) %>%
  mutate(biomass_relative = (biomass-biomass[run_name == name_baseline])*100/ biomass[run_name == name_baseline])%>%
  ungroup()
data_mean


# PP
data <- read.csv("data/biom_comp_run_status_quo.csv")
data <- data[data$Year>yr_min, ]

data <- data[data$longname%in% c("Small phytoplankton","Large phytoplankton", "Seagrass", "Macroalgae"),]

data_mean <- left_join(fg, data, by = "longname")%>%
  group_by(isVertebrate, run_name) %>%
  mutate(biomass = sum(biomass)/50) %>%
  distinct(isVertebrate, run_name, biomass) %>%
  ungroup() 
data_mean <- data_mean[!is.na(data_mean$biomass),]
data_mean <- data_mean%>%
  group_by(isVertebrate) %>%
  mutate(biomass_relative = (biomass-biomass[run_name == name_baseline])*100/ biomass[run_name == name_baseline])%>%
  ungroup()
data_mean



# Median
data <- read.csv("data/biom_comp_run_status_quo.csv")
data <- data[data$Year>yr_min, ]

data <- data[data$longname!="Pelagic bacteria",]
data <- data[data$longname!="Benthic bacteria",]
data <- data[data$longname!="Refractory detritus",]
data <- data[data$longname!="Labile detritus",]

data <- data[data$longname!="Large phytoplankton",]
data <- data[data$longname!="Small phytoplankton",]


data_mean <- left_join(fg, data, by = "longname") %>%
  group_by(longname, run_name) %>%
  summarise(
    isVertebrate = first(isVertebrate),
    biomass = sum(biomass) / 50,
    .groups = "drop"
  ) %>%
  group_by(longname) %>%
  mutate(
    biomass_baseline = biomass[run_name == name_baseline][1],
    biomass_relative = (biomass - biomass_baseline) * 100 / biomass_baseline
  ) %>%
  ungroup()%>%
  group_by(isVertebrate, run_name)%>%
  mutate(median = median(biomass_relative, na.rm = T))%>%
  distinct(isVertebrate, run_name, isVertebrate, median) %>%
  ungroup() 

data_mean



par(mfrow = c(3,1),
    mar = c(4,4,2,1),
    oma = c(0,0,2,0))

runs <- c("Yr2020s", "Yr2030s", "Yr2040")

for(r in runs){
  
  x <- data_mean$biomass_relative[data_mean$run_name == r]
  
  hist(x,
       xlim = c(-80,150),
       breaks = 40,
       col = rgb(0.2, 0.4, 0.7, 0.6),
       border = "white",
       main = r,
       xlab = "Relative biomass change (%)",
       ylab = "Number of species",
       cex.main = 1.2,
       cex.lab = 1.2)
  box()
  abline(v = c(-5, 5),
         col = "red3",
         lwd = 2,
         lty = 2)
}

mtext("Distribution of Biomass Changes relative to 2011",
      outer = TRUE,
      cex = 1)



mean(abs(data_mean$biomass_relative[data_mean$run_name == runs[1]]) <= 5, na.rm = T)
mean(abs(data_mean$biomass_relative[data_mean$run_name == runs[2]]) <= 5, na.rm = T)
mean(abs(data_mean$biomass_relative[data_mean$run_name == runs[3]]) <= 5, na.rm = T)
