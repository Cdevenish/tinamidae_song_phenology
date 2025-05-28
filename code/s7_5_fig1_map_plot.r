## Plot Figure 1


library(sf)
library(terra)


load("./data/ebd_x_mes_species.rdata") ## ebd_x_mes


### Draw supp figures for points in prec regimes, biomes and latbands
k5 <- rast("./gis/r_wgs/prec_km5.tif")
reg <- k5
reg[!is.na(reg)] <- 0
reg
plot(reg)

coords <- terra::crds(reg, na.rm = TRUE)
levels(ebd_x_mes$latBand)

bnd1 <- coords[,"y"] > -30 & coords[,"y"] <= -15
bnd2 <- coords[,"y"] > -15 & coords[,"y"] <= 0
bnd3 <- coords[,"y"] > 0 & coords[,"y"] <= 15
bnd4 <- coords[,"y"] > 15 & coords[,"y"] <= 30

## single raster
regAll <- reg
regAll[!is.na(reg)][bnd1] <- 1
regAll[!is.na(reg)][bnd2] <- 2
regAll[!is.na(reg)][bnd3] <- 3
regAll[!is.na(reg)][bnd4] <- 4
plot(regAll)

regAll[regAll == 0] <- NA
levels(regAll) <- data.frame(ID = 1:4, category = levels(ebd_x_mes$latBand))
plot(regAll)


# ebird_sf
load("gis/ebird_sf.rdata")

png("./plots/Fig1_obs_latBand_precReg_col.png", units = "mm", width = 300, height = 180, res = 100)
par(mfrow = c(1,2)) # , oma = c(0,0,0,0), mar = c(2,4,4,3)

## a. prec Regimes
plot(k5, legend = "bottomleft", col = viridisLite::viridis(5, alpha = 0.5))
plot(st_geometry(ebird_sf), pch = ".", add = T, col = "black")
title("a.", adj = 0, line = -1)

## b. Lat Bands
plot(reg, col = "grey90", colNA = "grey99", legend = FALSE)
plot(regAll, legend = "bottomleft", add = T, col = viridisLite::cividis(4, alpha = 0.8))
plot(st_geometry(ebird_sf), pch = ".", add = T, col = "black")
title("b.", adj = 0, line = -1)


dev.off()
