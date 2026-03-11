
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
date <- as.POSIXct.Date((1:730)/2+365*41+9.5, format = "%d-%b")
date <- format(date, "%m-%d")
## NA layer
setwd("/home/atlantis/psatlantismodel/2011")
nc <- nc_open("pugetsound_SSM_Atlantis_NH4_status_quo_2011.nc")
var <-  ncvar_get(nc, varid = "NH3")
na.mask <- var[,,1]
na.mask[!is.na(na.mask)] <- 0
## Final weight calculation
layer_volume <-  (na.mask + layer_volume)
weight_layer <- (na.mask + layer_volume)/sum(layer_volume, na.rm = T)
for(i in 1:89){
  keep <- weight_layer[,i][is.na(weight_layer[,i])]
  weight_layer[,i] <- c(rev(weight_layer[,i][!is.na(weight_layer[,i])]),keep)
}
weight_layer_t <- array(rep(weight_layer, 730), dim = c(7,89,730))





plot_SSM_temporal_mean <- function(variable.nc, variable.Atlantis, 
                             title = "", ylim, ylab, hline = c(1,2,3,4)){
  
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
       xlab = "", ylab = ylab, 
       ylim = ylim,
       main = title,
       axes = FALSE, 
       col = "red4", 
       family="Calibri Light")
  axis(1, at = c(1,100,200,300), labels = date[c(1,200,400,600)], family="Calibri Light", cex.axis = 0.85)
  axis(1, at = c(-1000,4e6), cex.axis = 0.75)
  
  # axis(1)
  axis(2, c(-100,4e6))
  axis(2, cex.axis = 0.85)
  box()
  abline(v=1, col = "grey70", lwd = 0.5)
  abline(v=100, col = "grey70", lwd = 0.5)
  abline(v=200, col = "grey70", lwd = 0.5)
  abline(v=300, col = "grey70", lwd = 0.5)
  abline(h=hline[1], col = "grey70", lwd = 0.5)
  abline(h=hline[2], col = "grey70", lwd = 0.5)
  abline(h=hline[3], col = "grey70", lwd = 0.5)
  abline(h=hline[4], col = "grey70", lwd = 0.5)
  lines((1:730)/2 , apply(var2011, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "red4", lwd = 2)
  lines((1:730)/2 , apply(var2020, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "red2", lwd = 2)
  lines((1:730)/2 , apply(var2030, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "orange2", lwd = 2)
  lines((1:730)/2 , apply(var2040, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "palegoldenrod", lwd = 2)
  
  
  legend("topright", legend = c("2011","2020s", "2030s", "2040s"),
         col = c("red4", "red2", "orange2", "palegoldenrod"), lty = c(1, 1), bty = "n", ,
         cex = 0.75, box.col="black")
  
  
  
}

# Plot script begins
## Composition
setwd("/home/atlantis/Status_quo")
# pdf("mean-temporal-trend.pdf")
par(mfrow = c(2, 2), 
    mar = c(4.5,4,1.2,1.5))

par(mfrow = c(1,1), 
    mar = c(2,5,0.5,0.1))

# Plot 1 
plot_SSM_temporal_mean(variable.nc = "NO3", variable.Atlantis = "NO3", 
                       ylab = "NO3 (mgN.m^3)", ylim = c(250,600),
                       title = "", hline =c(250,350,450,550))

plot_SSM_temporal_mean(variable.nc = "NH4", variable.Atlantis = "NH3", 
                       ylab = "NH4 (mgN.m^3)", ylim = c(5,35),
                       title = "", hline =c(10,20,30,40))

plot_SSM_temporal_mean(variable.nc = "SP", variable.Atlantis = "Sm_Phyto_N", 
                       ylab = "Algae Species 2 \ndinoflagellates (mgN.m^3)", ylim = c(0,9),
                       title = "", hline =c(2,4,6,8))


plot_SSM_temporal_mean(variable.nc = "LP", variable.Atlantis = "Lrg_Phyto_N", 
                       ylab = "Algae Species 1 \ndiatoms (mgN.m^3)", ylim = c(0,30),
                       title = "", hline =c(10,20,30,40))
# Plot 2

plot_SSM_temporal_mean(variable.nc = "temperature", variable.Atlantis = "temperature", 
                       title = "", ylim = c(8,15), ylab = "Temperature (°C)", 
                       hline =c(9,11,13,15))
plot_SSM_temporal_mean(variable.nc = "oxygen", variable.Atlantis = "Oxygen", 
                       title = "", ylab = "Oxygen", ylim = c(5000,8000), 
                       hline =c(5000,6000,7000,8000))

plot_SSM_temporal_mean(variable.nc = "salinity", variable.Atlantis = "salinity", 
                        ylab = "Salinity", 
     title = "",
     ylim = c(28.5, 31), 
     hline =c(28.5,29.5,30.5,31.5))






par(mfrow = c(1,1), 
    mar = c(4,5,0.5,0.1))
## Total Phytoplankton

setwd("/home/atlantis/psatlantismodel/2011")
nc <- nc_open("pugetsound_SSM_Atlantis_SP_status_quo_2011.nc")
var <-  ncvar_get(nc, varid = "Sm_Phyto_N")

nc <- nc_open("pugetsound_SSM_Atlantis_LP_status_quo_2011.nc")
var <-  var  + ncvar_get(nc, varid = "Lrg_Phyto_N")
var <- var*weight_layer_t



plot((1:730)/2, apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), type = "l",
     xlab = "Days", ylab = "Total phytoplankton \n(mgN.m^3)", 
     main = "",
     col = "red4",
     axes = F, 
     family="Calibri Light", ylim = c(0,30))
axis(1, at = c(1,100,200,300), labels = date[c(1,200,400,600)], family="Calibri Light", cex.axis = 0.85)
axis(1, at = c(-1000,4e6), cex.axis = 0.75)

# axis(1)
axis(2, c(-100,4e6))
axis(2, cex.axis = 0.85)
box()
legend("topright", legend = c("2011","2020s", "2030s", "2040s"),
       col = c("red4", "red2", "orange2", "palegoldenrod"), lty = c(1, 1), bty = "n", cex = 0.75)
abline(v=1, col = "grey70", lwd = 0.5)
abline(v=100, col = "grey70", lwd = 0.5)
abline(v=200, col = "grey70", lwd = 0.5)
abline(v=300, col = "grey70", lwd = 0.5)
abline(h=10, col = "grey70", lwd = 0.5)
abline(h=20, col = "grey70", lwd = 0.5)
abline(h=30, col = "grey70", lwd = 0.5)
lines((1:730)/2 , apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "red4")

setwd("/home/atlantis/psatlantismodel/2020")
nc <- nc_open("pugetsound_SSM_Atlantis_SP_status_quo_2020.nc")
var <-  ncvar_get(nc, varid = "Sm_Phyto_N")

nc <- nc_open("pugetsound_SSM_Atlantis_LP_status_quo_2020.nc")
var <-  var  + ncvar_get(nc, varid = "Lrg_Phyto_N")
var <- var*weight_layer_t

lines((1:730)/2 , apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "red2")

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

lines((1:730)/2 , apply(var, FUN = function(x) sum(x, na.rm = T), MARGIN = 3), lty = 1, col = "palegoldenrod")


# dev.off()






table_SSM_temporal_mean <- function(variable.nc, variable.Atlantis, 
                                   title = "", ylim, ylab, hline = c(1,2,3,4)){
  
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
  
  
  return(c(sum(var2011, na.rm = T), 
           sum(var2020, na.rm = T),
           sum(var2030, na.rm = T),
           sum(var2040, na.rm = T))/730)
  
}
round(table_SSM_temporal_mean(variable.nc = "SP", variable.Atlantis = "Sm_Phyto_N", 
                       ylab = "Algae Species 2 \ndinoflagellates (mgN.m^3)", ylim = c(0,9),
                       title = "", hline =c(2,4,6,8)),2) + round(table_SSM_temporal_mean(variable.nc = "LP", variable.Atlantis = "Lrg_Phyto_N", 
                       ylab = "Algae Species 1 \ndiatoms (mgN.m^3)", ylim = c(0,30),
                       title = "", hline =c(10,20,30,40)),2)

round(table_SSM_temporal_mean(variable.nc = "NO3", variable.Atlantis = "NO3", 
                              ylab = "NO3 (mgN.m^3)", ylim = c(250,600),
                              title = "", hline =c(250,350,450,550)))
round(table_SSM_temporal_mean(variable.nc = "NH4", variable.Atlantis = "NH3", 
                              ylab = "NO3 (mgN.m^3)", ylim = c(250,600),
                              title = "", hline =c(250,350,450,550)))

      