
# title: SSM outputs comparison between decades
# Date = 26/01/2026
# Author = AM


# Library management
library(ncdf4)
library(rbgm)
library(sf)
library(here)
library(ggplot2)
library(ggpubr)
# remotes::install_github("r-spatial/sf")
# remotes::install_github("atlantis-amps/atlantisplotter")
library("atlantiscalib")
library("atlantisplotter")

# Polygon weight estimation
setwd("/home/atlantis/Status_quo")

## Load area
thisbgmfile <- "/home/atlantis/Status_quo/data/PugetSound_89b_NAD83.bgm"
## Thickness
box_area <- rbgm::box_sf(rbgm::read_bgm(thisbgmfile))$area
layer_limit <- c(0,5,25,50,100,150,350,351)
layer_thickness <- layer_limit[-1] - layer_limit[-8]

{layer_volume <- array(rep(layer_thickness, 89), dim = c(7,89))
layer_volume <- t(t(layer_volume)*box_area)}

## NA layer
setwd("/home/atlantis/psatlantismodel/2011")
nc <- nc_open("pugetsound_SSM_Atlantis_NH4_status_quo_2011.nc")
var <-  ncvar_get(nc, varid = "NH3")
na.mask <- var[,,1]
na.mask[!is.na(na.mask)] <- 0


# Open polygon geometry before plot
a <- rbgm::box_sf(rbgm::read_bgm(thisbgmfile))
geom_new <- lapply(st_geometry(a), function(g) {
  st_polygon(list(st_coordinates(g)[, 1:2]))
})

a_clean <- st_sf(
  a[, setdiff(names(a), "geometry")],
  geometry = geom_new,
  crs = 32610
)
nc2020 <- nc_open(paste0("/home/atlantis/psatlantismodel/2020/pugetsound_SSM_Atlantis_temperature_status_quo_2020.nc"))
pos.surface <- apply(!is.na(ncvar_get(nc2020, varid = "temperature")[,,1]), FUN = sum, MARGIN = 2)
pos.surface[pos.surface==0]<-1

plot_SSM_spatial <- function(variable.nc, variable.Atlantis, 
                             title1 = "", title2 = "", title3 = ""){
  
  nc2020 <- nc_open(paste0("/home/atlantis/psatlantismodel/2020/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2020.nc"))
  nc2011 <- nc_open(paste0("/home/atlantis/psatlantismodel/2011/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2011.nc"))
  nc2030 <- nc_open(paste0("/home/atlantis/psatlantismodel/2030/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2030.nc"))
  nc2040 <- nc_open(paste0("/home/atlantis/psatlantismodel/2040/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2040.nc"))
  
  var2020 <- ncvar_get(nc2020, varid = variable.Atlantis)
  var2040 <- ncvar_get(nc2040, varid = variable.Atlantis)
  var2030 <- ncvar_get(nc2030, varid = variable.Atlantis)
  var2011 <- ncvar_get(nc2011, varid = variable.Atlantis)
  
  var2020 <- apply(var2020[,,], FUN = function(x){mean(x,na.rm = T)}, MARGIN = c(1,2))
  var2040 <- apply(var2040[,,], FUN = function(x){mean(x,na.rm = T)}, MARGIN = c(1,2))
  var2030 <- apply(var2030[,,], FUN = function(x){mean(x,na.rm = T)}, MARGIN = c(1,2))
  var2011 <- apply(var2011[,,], FUN = function(x){mean(x,na.rm = T)}, MARGIN = c(1,2))
  var2011[var2011==0] <- NA
  var2020surface <- var2011surface <- var2030surface <- var2040surface <- c()
  for (i in 1:89){
    var2020surface <- c(var2020surface, var2020[pos.surface[i],i])
    var2011surface <- c(var2011surface, var2011[pos.surface[i],i])
    var2030surface <- c(var2030surface, var2030[pos.surface[i],i])
    var2040surface <- c(var2040surface, var2040[pos.surface[i],i])
  }
  
  a_clean$D2020s <- var2020surface - var2011surface
  a_clean$D2030s <- var2030surface - var2011surface
  a_clean$D2040s <- var2040surface - var2011surface
  
  p1 <- ggplot(a_clean, aes(fill = D2020s)) +
    geom_sf(data = a_clean[,"D2020s"])+
    scale_fill_gradient2(midpoint=0,  low="blue", mid="white",
                         high="red")+theme_bw()+
    ggtitle(title1)+ labs(fill = Delta ~ "(2025 - 2011)")
  
  p2 <- ggplot(a_clean, aes(fill = D2030s)) +
    geom_sf(data = a_clean[,"D2030s"])+
    scale_fill_gradient2(midpoint=0,  low="blue", mid="white",
                         high="red")+theme_bw()+
    ggtitle(title2) + labs(fill = Delta ~ "(2035 - 2011)")
  
  p3 <- ggplot(a_clean, aes(fill = D2040s)) +
    geom_sf(data = a_clean[,"D2040s"])+
    scale_fill_gradient2(midpoint=0,  low="blue", mid="white",
                         high="red")+theme_bw()+
    ggtitle(title3) + labs(fill = Delta ~ "(2045 - 2011)")
  ggpubr::ggarrange(p1, p2, p3, 
            labels = c("A", "B", "C"),
            ncol = 3, nrow = 1)
  
  
}
  
pdf("/home/atlantis/Status_quo/Spatial_surface.pdf", height = 8, width = 16)
plot_SSM_spatial(variable.nc = "temperature", variable.Atlantis = "temperature", 
                 title1 = "Surface temperature difference \nbetween 2025 and 2011",
                 title2 = "Surface temperature difference \nbetween 2035 and 2011",
                 title3 = "Surface temperature difference \nbetween 2045 and 2011")
plot_SSM_spatial(variable.nc = "salinity", variable.Atlantis = "salinity", 
                 title1 = "Surface salinity difference \nbetween 2025 and 2011",
                 title2 = "Surface salinity difference \nbetween 2035 and 2011",
                 title3 = "Surface salinity difference \nbetween 2045 and 2011")
plot_SSM_spatial(variable.nc = "oxygen", variable.Atlantis = "Oxygen", 
                 title1 = "Surface oxygen difference \nbetween 2025 and 2011",
                 title2 = "Surface oxygen difference \nbetween 2035 and 2011",
                 title3 = "Surface oxygen difference \nbetween 2045 and 2011")
plot_SSM_spatial(variable.nc = "NH4", variable.Atlantis = "NH3", 
                 title1 = "Surface NH4 difference \nbetween 2025 and 2011",
                 title2 = "Surface NH4 difference \nbetween 2035 and 2011",
                 title3 = "Surface NH4 difference \nbetween 2045 and 2011")
plot_SSM_spatial(variable.nc = "NO3", variable.Atlantis = "NO3", 
                 title1 = "Surface NO3 difference \nbetween 2025 and 2011",
                 title2 = "Surface NO3 difference \nbetween 2035 and 2011",
                 title3 = "Surface NO3 difference \nbetween 2045 and 2011")
plot_SSM_spatial(variable.nc = "SP", variable.Atlantis = "Sm_Phyto_N", 
                 title1 = "Surface s.phytoplankton difference \nbetween 2025 and 2011",
                 title2 = "Surface s.phytoplankton difference \nbetween 2035 and 2011",
                 title3 = "Surface s.phytoplankton difference \nbetween 2045 and 2011")
plot_SSM_spatial(variable.nc = "LP", variable.Atlantis = "Lrg_Phyto_N", 
                 title1 = "Surface l.phytoplankton difference \nbetween 2025 and 2011",
                 title2 = "Surface l.phytoplankton difference \nbetween 2035 and 2011",
                 title3 = "Surface l.phytoplankton difference \nbetween 2045 and 2011")


dev.off()

