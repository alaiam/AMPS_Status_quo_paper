library(dplyr)
library(ggplot2)
library(reshape2)
library(here)
library(rphylopic)
library(cowplot)

### Variables
name_baseline = "2011"
yr_min = 40
###

pdf("WAA-NAA.pdf", height = 3, width = 10)
# Weight-at-age
data_row <- read.csv("data/NAA-WAA_sim.csv")
fg <- read.csv(("data/PugetSoundAtlantisFunctionalGroups_2024_V1_SQ.csv"))


data_row <- data_row[data_row$Year>yr_min, ]

for (j in c("Wage", "Nums")){
  data <- data_row[data_row$variable_type==j, ]


# Merge Wage data with FG 

data_mean <- left_join(fg, data, by = "longname")%>%
  group_by(atlantis_group, scenario) %>%
  mutate(variable = mean(variable)) %>%
  distinct(longname, scenario, variable, IsHeatMap, IsLTL, IsBenthos, IsFish, IsBiggerthanfish,Large_group, Large_group_alt, Large_group_alt2,Subgroup) %>%
  ungroup() %>%
  group_by(longname) %>%
  mutate(variable_relative = (variable-variable[scenario == name_baseline])*100/ variable[scenario == name_baseline])%>%
  ungroup()


# Data preparation before the plot (label order, Age class renamed...)
data_mean$scenario <- factor(data_mean$scenario,
                                     levels = rev(unique(data_mean$scenario)),
                                     ordered = TRUE)
data_mean <- data_mean[!is.na(data_mean$longname),]
data_mean$Age <- 0
for (i in 1:10){
  data_mean$Age[regexpr(i, data_mean$atlantis_group)>0] <- paste0("Age ", i)
}

data_mean$Age <- factor(data_mean$Age,
                             levels = unique(data_mean$Age),
                             ordered = TRUE)

data_mean <- data_mean[data_mean$longname!="Labile detritus",]
data_mean <- data_mean[data_mean$scenario!=name_baseline,]
data_mean <- data_mean[!is.na(data_mean$longname),]


# A plot per group type


for (i in unique(data_mean$Large_group_alt2)){
  data_mean_sub <- data_mean[data_mean$Large_group_alt2 == i,]
  data_mean_sub <- data_mean_sub[!is.na(data_mean_sub$longname),]
  sumup <- ggplot(data_mean_sub, aes(x = Age, y = scenario, fill = variable_relative)) +
    geom_tile() +
    facet_grid(.~longname, scales = "free", space = "free") + 
    scale_fill_gradient2(low = "red3", mid = "white", high = "cyan3", midpoint = 0) +  # Palette de couleurs optimisée
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid = element_blank(),  # Supprime les grilles
          panel.background = element_rect(fill = "white")) +
    labs(x = "", y = "", fill = "Relative \nvariable Change (%)")
  print(sumup)
}

}
dev.off()

