## Script 4
## Make precipitation regimes. Cluster monthly precipitation into 
## geographic regions with similar precipitation patterns

library(terra)
library(sf)

library(ggplot2)
library(dplyr)

library(clue)

# load prec 1

## GIS path (dowload worldclim data to file.path(gis, "Clim/wrldclm"))
gis <- "PATH TO GIS STORAGE"


# "/wc2.1_30s_prec_01.tif"
dir(file.path(gis, "Clim/wrldclm"))


# v21
fns <- list.files(file.path(gis, "Clim/wrldclm/wc2.1_30s_prec"), "\\.tif$",full.names = TRUE)
precL <- terra::rast(fns)
precL
names(precL)
names(precL) <- sub("wc2\\.1_30s_", "", names(precL))

## clip to South America (use vector file of country borders here)
# download from url below to file.path(gis, "Adm/world")
## source: http://thematicmapping.org/downloads/world_borders.php
sa <- st_read(file.path(gis, "Adm/world/TM_WORLD_BORDERS_SIMPL-0.3_CA_SA.shp"))
sa

plot(st_geometry(sa))
sa <- st_make_valid(sa)

prec <- terra::crop(precL, vect(sa))
prec

plot(prec, 1, colNA = "black")


## Extract random points for kmeans cluster (to group precipitation regimes)
set.seed(99)
pts <- terra::spatSample(prec, size = 30000, 
                         xy = T, method = "random", 
                         as.df = FALSE, 
                         na.rm = TRUE, 
                         values = FALSE)

ext_pts <- terra::extract(prec, pts)
head(ext_pts)

saveRDS(ext_pts, file = "data/ext_pts.rds")

head(ext_pts)
apply(ext_pts, 2, hist)

# do distance matrix for sil widths
dist_ext <- dist(ext_pts)
saveRDS(dist_ext, file = "./data/dist_ext.rds")


## aggregate down to ~ 5 km res
prec_agg <- aggregate(prec, 4)
prec_agg
2659 * 2470

# save temporarily
writeRaster(prec_agg, filename = "gis/r_wgs/prec_agg.tif", overwrite = TRUE)
writeRaster(prec, filename = "gis/r_wgs/prec.tif", overwrite = TRUE)

# extract values from raster
vals <- values(prec_agg)
class(vals)
head(vals)
dim(vals)

# Do k means and predict
ks <- 3:7

precStck <- rast(prec_agg, nlyrs =1)

bsstss <- vector(length = length(ks))
sizes <- vector(mode = "list", length = length(ks))
withss <- vector(length = length(ks))
sil_dfs <- vector(mode = "list", length = length(ks))

set.seed(99)


for(k in ks){
  
  km <- kmeans(ext_pts, centers = k)
  
  # save between sum squares / total sum squares. As this approaches 1, more variation between clusters than within
  bsstss[which(k == ks)] <- km$betweenss/km$totss
  withss[which(k == ks)] <- km$tot.withinss
  sizes[[which(k == ks)]] <- km$size
  
  # silhouette valids
  sil_dfs[[which(k == ks)]] <- cluster::silhouette(km$cluster, dist_ext)
  
  pred <- clue::cl_predict(km, vals)
  pred_r <- terra::rast(prec_agg, nlyrs =1, vals = pred)
  add(precStck) <- pred_r
  
  rm(pred, pred_r, km)
  gc()
  
}

# check k means
plot(ks, bsstss)
plot(ks, withss)

# check variability in size of clusters
plot(ks, sapply(sizes, sd))

# with CV
plot(ks, sapply(sizes, sd)/sapply(sizes, mean))


# get sil widths into data frame
library(dplyr)
library(ggplot2)

sil_df <- data.frame(do.call(rbind, sil_dfs)) %>%
  mutate(k = rep(ks, each = nrow(ext_pts)),
         cluster = factor(cluster)) %>%
  group_by(k)%>%
  arrange(cluster, sil_width, .by_group=TRUE) %>%
  ungroup() %>%
  mutate(obs = rep(1:nrow(ext_pts), length(ks)))
  
head(sil_df)
table(sil_df$k)

mn_sil <- sil_df %>%
  group_by(k) %>%
  dplyr::summarise(mean = mean(sil_width))


p1 <- ggplot(sil_df, aes(y = sil_width, x = obs, col = cluster))+
  geom_bar(stat= "identity")+
  geom_hline(data = mn_sil, aes(yintercept = mean), col= "red", lty = 2)+
  facet_wrap(~k)

p1

ggsave(plot = p1, filename = "./plots/silhouette_plot_kmeans.png")
# no clusters below average sil width


## plot regimes with ks
plot(precStck)

precStck
names(precStck) <- paste0("k", ks)

png(filename = "./plots/prec_regimes_ks.png", width = 300, height = 300, res = 100, units = "mm")
plot(precStck)
dev.off()

writeRaster(precStck, filename = file.path("./gis/r_wgs", paste0("prec_km", ks, ".tif")), overwrite = TRUE)

save(sil_df, sizes, bsstss, withss, file = "./data/kmeans_res.rdata")

