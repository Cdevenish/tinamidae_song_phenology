## Script 5

## Add precipitation data from worldclim to each ebird observation, 
## for appropriate month of observation

library(terra)
library(sf)
library(tmap)

getwd()

## Read in data
gis <- "C:/Users/ggc34/OneDrive - University of Keele/Documents/GIS_info"

# output_file <- file.path(dest, "filtered_tinamidae_relApr-2023.txt")
# ebird_df <- auk::read_ebd(output_file)

dir("./data/ebird", pattern = "\\.rds$")
file.exists(file.path(getwd(), "data/ebird/ebird_df.rds"))

ebird_df <- readRDS(file.path(getwd(), "data/ebird/ebird_df.rds"))


str(ebird_df)
sum(is.na(ebird_df$lat))
head(ebird_df)


## add numeric month to ebird
ebird_df$month <- factor(months(ebird_df$date, abbreviate = TRUE), levels = month.abb)
ebird_df$month_no <- match(ebird_df$month, month.abb)
sum(is.na(ebird_df$month))
str(ebird_df)


# # get world clim path
wc_fp <- file.path(gis, "Clim/wrldclm")

### Bioclim monthly precipitation -- 
# get world clim monthly precipitation file names
wc_fn <- list.files(path = wc_fp, pattern = "^wc2.*_prec_.*\\.tif$", full.names = T, recursive = TRUE)

# create raster stack
r_wc <- terra::rast(wc_fn)
r_wc

## rename to BC vars
names(r_wc) <- sub(".*_(prec_[[:digit:]]{2})", "\\1", names(r_wc))
r_wc

## obtain precipitation data for each observation, for the same month
## corresponding to that observation
res <- lapply(split(ebird_df, ebird_df$month_no), function(x){
  tmp <- terra::extract(subset(r_wc, subset = unique(x[,"month_no"])),
                        x[,c("lon", "lat")], ID =FALSE)
  # rename month col
  colnames(tmp) <- "prec"
  cbind(x, tmp)
  })

str(res,1)

# rbind resulting dataframes (ebird_df plus the prec column)
ebird_df <- do.call(rbind, res)
# tidy rownames
rownames(ebird_df) <- NULL
head(ebird_df)

rm(r_wc, res)

## check NAs
summary(ebird_df$prec)
# 138 NAs
wc_na <- st_as_sf(ebird_df[is.na(ebird_df$prec),], coords = c("lon", "lat"), crs = 4326)

# convert to sf data frame for extraction of regime data below
ebird_sf <- st_as_sf(ebird_df[, c("scientific_name", "common_name", "lon", "lat")], 
                     coords = c("lon", "lat"), 
                     crs = 4326, agr = "constant")

### Add Precipitation regime data ######

# 5 prec regimes
prec <- rast("./gis/r_wgs/prec_km5.tif")
prec
names(prec)
plot(prec)

ebird_df <- cbind(ebird_df, terra::extract(prec, ebird_sf, ID = FALSE))
head(ebird_df); gc()

table(ebird_df$k5, useNA = "always")
# 1484 NAs (off wc prec rasters)

## check nearest raster cells to all the NAs and assign that category (off coast points)
indNA <- which(is.na(ebird_df$k5))
length(indNA) # 1484
rm(prec)

# check geometry
sum(st_is_empty(ebird_sf[indNA,]))
sum(st_is_valid(ebird_sf[indNA,]))
plot(st_geometry(ebird_sf[indNA,]))

## 
out <- vector(length = length(indNA), mode = "list")
precs <- rast("./gis/r_wgs/prec_km5.tif")


## more efficient - do this...
## get cells in boundaries(), then centroid and cell IDs, then nearest to cell and find regions from this


for (i in seq(indNA)) {
  d <- terra::distance(precs$k5, ebird_sf[indNA[i],]) # get distance from NA points to raster
  names(d) <- "dist"
  tmp <- terra::zonal(d, precs, min) # get value for each region
  print(i)
  out[[i]] <- tmp[which.min(tmp[,"dist"]),]
}

head(out)
chk <- do.call(rbind, out)
colnames(chk)
chk$indNA <- indNA
summary(chk)
head(chk)

# points further than 5k from nearest raster
gt5k <- chk[(chk$dist > 5000),"indNA"]
hist(chk[(chk$dist > 5000),"indNA"])

head(chk)
plot(precs$k5)
plot(st_geometry(ebird_sf[gt5k,]), add = T, pch = 16)

# set to NA and discard
chk[chk$dist > 5000, "k5"] <- NA

# add regions (or NA) back to ebird_df
ebird_df[indNA, "k5"] <- chk[,"k5"]

str(ebird_df)
summary(ebird_df[, "k5"])

# convert to factors
ebird_df$k5 <- factor(ebird_df$k5)

## Save - 
saveRDS(ebird_df, file = "./data/ebird/ebird_df_wc.rds")

