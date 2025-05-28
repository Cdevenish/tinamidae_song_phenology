### Script 6. Check and further ebird filters

getwd()

## Read in data
ebird_df <- readRDS("./data/ebird/ebird_df_wc.rds")
colnames(ebird_df)
# 46180 observations

## Check data
ebird_df$effort_area_ha[!is.na(ebird_df$effort_area_ha)]
summary(ebird_df[, c("lon", "lat", 
                     "date", "effort_distance_km", 
                     "duration_minutes", "number_observers",
                     "observation_count")])

head(ebird_df)

## check no of observers
sum(ebird_df$number_observers >= 15, na.rm = T) # 163 + 24 NAs
hist(ebird_df$number_observers)
hist(ebird_df$number_observers[ebird_df$number_observers > 10 & ebird_df$number_observers < 100])
ebird_df[ebird_df$number_observers > 100 & !is.na(ebird_df$number_observers), c("trip_comments")]

## filter by no. observers, keep all < 15
ebird_df <- subset(ebird_df, number_observers < 15)
46180 - (163 + 24)


unique(ebird_df$country)
unique(ebird_df$protocol_type)
table(ebird_df$protocol_type)

unique(ebird_df$behavior_code)

sort(table(ebird_df$scientific_name))
barplot(sort(table(ebird_df$scientific_name)), las = 2)

## remove species with < 300 observations
spp2keep <- names(table(ebird_df$scientific_name)[table(ebird_df$scientific_name) > 300])

# how many records are lost?
nrow(subset(ebird_df, !scientific_name %in% spp2keep))
# 1178

ebird_df <- subset(ebird_df, scientific_name %in% spp2keep)
45993 - 1178

barplot(sort(table(ebird_df$scientific_name)), las = 2)


## Check observation counts
unique(ebird_df$observation_count)
table(ebird_df$observation_count)

## convert to numeric and assign all presence as 1
ebird_df$observation_count[ebird_df$observation_count == "X"] <- "1"
ebird_df$observation_count <- as.numeric(ebird_df$observation_count)

unique(ebird_df$observation_count)
table(ebird_df$observation_count)
barplot(table(ebird_df$observation_count))

## remove above 5 observation counts 
## -- likely to be more effort, or unlikely to see/hear more 5 tinamous over 5 km
sum(ebird_df$observation_count > 5) # 189

ebird_df <- subset(ebird_df, observation_count <= 5)
44815 - 189

row.names(ebird_df) <- NULL

head(ebird_df)
nrow(ebird_df) # 44626

summary(ebird_df[, c("lon", "lat", 
                     "date", "effort_distance_km", 
                     "duration_minutes", "number_observers",
                     "observation_count")])
# 
# lon               lat               date            effort_distance_km duration_minutes number_observers
# Min.   :-106.78   Min.   :-33.504   Min.   :2000-01-11   Min.   :0.000      Min.   : 1.00    Min.   : 1.000  
# 1st Qu.: -85.23   1st Qu.:-12.083   1st Qu.:2018-08-31   1st Qu.:0.386      1st Qu.:14.00    1st Qu.: 1.000  
# Median : -77.45   Median :  8.055   Median :2020-06-23   Median :0.805      Median :30.00    Median : 1.000  
# Mean   : -73.28   Mean   :  1.160   Mean   :2019-11-18   Mean   :1.102      Mean   :31.86    Mean   : 1.994  
# 3rd Qu.: -61.30   3rd Qu.: 10.723   3rd Qu.:2021-12-18   3rd Qu.:1.500      3rd Qu.:49.00    3rd Qu.: 2.000  
# Max.   : -35.03   Max.   : 25.650   Max.   :2022-12-31   Max.   :5.000      Max.   :60.00    Max.   :14.000  
# NA's   :19600                                       
#  observation_count
#  Min.   :1.000    
#  1st Qu.:1.000    
#  Median :1.000    
#  Mean   :1.276    
#  3rd Qu.:1.000    
#  Max.   :5.000    


## save subset
saveRDS(ebird_df, "./data/ebird/ebird_df_subset.rds")


# ## Check locations 
# 
# library(tmap)
# library(sf)
# ebird_sf <- st_as_sf(ebird_df[, c("scientific_name", "common_name", "lon", "lat")], coords = c("lon", "lat"), crs = 4326, agr = "constant")
# 
# tmap_mode("view")
# #qtm(ebird_sf)
# 
# spp <- names(sort(table(ebird_sf$scientific_name)))
# cols <- RColorBrewer::brewer.pal(8, "Dark2")
# 
# tm_list1 <- lapply(spp[1:8], function(sp){
#   tm <- tm_shape(subset(ebird_sf, scientific_name == sp))+
#     tm_dots(col = cols[which(sp == spp)], group = sp)
#   tm
# })
# 
# tm_list2 <- lapply(spp[9:16], function(sp){
#   tm_shape(subset(ebird_sf, scientific_name == sp))+
#     tm_dots(col = cols[which(sp == spp)], group = sp)
# })
# 
# tm1 <- Reduce(`+`, tm_list1)
# tm2 <- Reduce(`+`, tm_list2)
# 
# str(tm_list1, 1)
# str(tm1,1)
# tm1
# 
# library(htmlwidgets)
# saveWidget(tmap_leaflet(tm1), "spp1.html") # save to wd and then move... self contained only seems to work in wd
# saveWidget(tmap_leaflet(tm2), "spp2.html")
# 
# ## tmap_save(tm1, file.path(wd, "plots/spp1.html")) # not working
# 
