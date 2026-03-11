
# title: SSM outputs comparison between decades
# Date = 26/01/2026
# Author = AM


# Library management
library(ncdf4)
library(rbgm)
library(sf)
library(here)
# remotes::install_github("r-spatial/sf")
# remotes::install_github("atlantis-amps/atlantisplotter")
library("atlantiscalib")
library("atlantisplotter")

# Polygon weight estimation
setwd("/home/atlantis/Status_quo")

## Load area
thisbgmfile <- "data/PugetSound_89b_NAD83.bgm"
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
na.mask[1,] <- 0
na.mask[2:7,] <- NA
## Final weight calculation
layer_volume <-  (na.mask + layer_volume)
layer_volume[1,]<-box_area
layer_volume[,61:89]<-NA
weight_layer <- (na.mask + layer_volume)/sum(layer_volume, na.rm = T)
weight_layer_t <- array(rep(weight_layer, 730), dim = c(7,89,730))


plot_SSM_temporal_mean <- function(variable.nc, variable.Atlantis, 
                             title = "", ylim, ylab){
  
  nc2020 <- nc_open(paste0("/home/atlantis/psatlantismodel/2020/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2020.nc"))
  nc2011 <- nc_open(paste0("/home/atlantis/psatlantismodel/2011/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2011.nc"))
  nc2030 <- nc_open(paste0("/home/atlantis/psatlantismodel/2030/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2030.nc"))
  nc2040 <- nc_open(paste0("/home/atlantis/psatlantismodel/2040/pugetsound_SSM_Atlantis_",variable.nc,"_status_quo_2040.nc"))
  
  var2020 <- ncvar_get(nc2020, varid = variable.Atlantis)
  var2040 <- ncvar_get(nc2040, varid = variable.Atlantis)
  var2030 <- ncvar_get(nc2030, varid = variable.Atlantis)
  var2011 <- ncvar_get(nc2011, varid = variable.Atlantis)
  
  var2020 <- var2020*weight_layer_t
  var2040 <- var2040*weight_layer_t
  var2030 <- var2030*weight_layer_t
  var2011 <- var2011*weight_layer_t

  plot((1:730)/2, apply(var2011, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), type = "l",
       xlab = "Days", ylab = ylab, 
       ylim = ylim,
       main = title,
       axes = FALSE)
  axis(1, c(-100,4e6))
  axis(1)
  axis(2, c(-100,4e6))
  axis(2)
  lines((1:730)/2 , apply(var2020, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "red3")
  lines((1:730)/2 , apply(var2030, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "orange2")
  lines((1:730)/2 , apply(var2040, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "cyan3")
  
  
  legend("topright", legend = c("2011","2025", "2035", "2045"),
         col = c("black", "red3", "orange2", "cyan3"), lty = c(1, 1), bty = "n")
  
  
  
}
# Plot script begins
## Composition
setwd("/home/atlantis/Status_quo")
pdf("mean-temporal-trend_bottom.pdf")
par(mfrow = c(2, 2), 
    mar = c(4.5,4,1.2,1.5))

# Plot 1 
plot_SSM_temporal_mean(variable.nc = "NO3", variable.Atlantis = "NO3", 
                       ylab = "NO3 (mgN.m^3)", ylim = c(250,550),
                       title = "NO3")

plot_SSM_temporal_mean(variable.nc = "NH4", variable.Atlantis = "NH3", 
                       ylab = "NH4 (mgN.m^3)", ylim = c(5,30),
                       title = "NH4")

plot_SSM_temporal_mean(variable.nc = "SP", variable.Atlantis = "Sm_Phyto_N", 
                       ylab = "Small phytoplankton (mgN.m^3)", ylim = c(0,10),
                       title = "Small Phytoplankton")

plot_SSM_temporal_mean(variable.nc = "LP", variable.Atlantis = "Lrg_Phyto_N", 
                       ylab = "Large phytoplankton (mgN.m^3)", ylim = c(0,40),
                       title = "Large Phytoplankton")
# Plot 2

plot_SSM_temporal_mean(variable.nc = "temperature", variable.Atlantis = "temperature", 
                       title = "Temperature", ylim = c(8,15), ylab = "Temperature (°C)")
plot_SSM_temporal_mean(variable.nc = "oxygen", variable.Atlantis = "Oxygen", 
                       title = "Oxygen", ylab = "Oxygen", ylim = c(5200,8000))

plot_SSM_temporal_mean(variable.nc = "salinity", variable.Atlantis = "salinity", 
                        ylab = "Salinity", 
     title = "Salinity",
     ylim = c(28.5, 30.8))







## Total Phytoplankton

setwd("/home/atlantis/psatlantismodel/2011")
nc <- nc_open("pugetsound_SSM_Atlantis_SP_status_quo_2011.nc")
var <-  ncvar_get(nc, varid = "Sm_Phyto_N")

nc <- nc_open("pugetsound_SSM_Atlantis_LP_status_quo_2011.nc")
var <-  var  + ncvar_get(nc, varid = "Lrg_Phyto_N")
var <- var*weight_layer_t



plot((1:730)/2, apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), type = "l",
     xlab = "Days", ylab = "Total phytoplankton (mgN.m^3)", 
     main = "Total phytoplankton",
     axes = FALSE)
axis(1, c(-100,4e6))
axis(1)
axis(2, c(-100,4e6))
axis(2)
legend("topright", legend = c("2011","2025", "2035", "2045"),
       col = c("black", "red3", "orange2", "cyan3"), lty = c(1, 1), bty = "n")


setwd("/home/atlantis/psatlantismodel/2020")
nc <- nc_open("pugetsound_SSM_Atlantis_SP_status_quo_2020.nc")
var <-  ncvar_get(nc, varid = "Sm_Phyto_N")

nc <- nc_open("pugetsound_SSM_Atlantis_LP_status_quo_2020.nc")
var <-  var  + ncvar_get(nc, varid = "Lrg_Phyto_N")
var <- var*weight_layer_t

lines((1:730)/2 , apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "red3")

setwd("/home/atlantis/psatlantismodel/2030")
nc <- nc_open("pugetsound_SSM_Atlantis_SP_status_quo_2030.nc")
var <-  ncvar_get(nc, varid = "Sm_Phyto_N")

nc <- nc_open("pugetsound_SSM_Atlantis_LP_status_quo_2030.nc")
var <-  var  + ncvar_get(nc, varid = "Lrg_Phyto_N")
var <- var*weight_layer_t

lines((1:730)/2 , apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "orange2")

setwd("/home/atlantis/psatlantismodel/2040")
nc <- nc_open("pugetsound_SSM_Atlantis_SP_status_quo_2040.nc")
var <-  ncvar_get(nc, varid = "Sm_Phyto_N")

nc <- nc_open("pugetsound_SSM_Atlantis_LP_status_quo_2040.nc")
var <-  var  + ncvar_get(nc, varid = "Lrg_Phyto_N")
var <- var*weight_layer_t

lines((1:730)/2 , apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "cyan3")


dev.off()




