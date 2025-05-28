## Script 7 - Summarise data for response variable

library(dplyr)
library(ggplot2)

## Read in data
ebird_df <- readRDS("./data/ebird/ebird_df_subset.rds")

head(ebird_df)

summary(ebird_df[, c("lon", "lat", 
                     "date", "effort_distance_km", 
                     "duration_minutes", "number_observers",
                     "observation_count")])

plot(ebird_df[, c("lon", "lat" )], asp = 1, pch = ".")

## Make groups of species
sort(table(ebird_df$scientific_name))
length(unique(ebird_df$scientific_name))

ggplot(ebird_df,aes(x=reorder(scientific_name,
                              scientific_name,
                              function(x)-length(x))))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ylab("Numero de observaciones")


spp_gt_2k <- names(table(ebird_df$scientific_name)[table(ebird_df$scientific_name) > 2000])
spp_gt_4k <- names(table(ebird_df$scientific_name)[table(ebird_df$scientific_name) > 4000])


## Plot group memberships
ebd_x_mes_grps <- ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  mutate(latBand = cut(lat, 
                       breaks = c(-30, -15, 0, 15, 30), 
                       labels = c("-30 to -15", "-15 to 0", "0 to 15", "15 to 30"))) %>%
  mutate(grp = paste(k5, month, month_no, gsub(" ", "\\.", scientific_name), gsub(" ", "\\.", latBand), sep = "_"))


  
head(ebd_x_mes_grps)
length(unique(ebd_x_mes_grps$grp))
hist(table(ebd_x_mes_grps$grp))
sum(table(ebd_x_mes_grps$grp) == 0)
sort(table(ebd_x_mes_grps$grp), decreasing = TRUE)
grps_order <- names(sort(table(ebd_x_mes_grps$grp), decreasing = TRUE))

# convert to factor in decreasing order
ebd_x_mes_grps$grp <- factor(ebd_x_mes_grps$grp, levels = grps_order)
str(ebd_x_mes_grps)

plot(ebd_x_mes_grps[,c("lon", "lat")], pch = ".", col = "grey70", asp = 1)
points(ebd_x_mes_grps[ebd_x_mes_grps$grp %in% grps_order[2], c("lon", "lat")], 
       col = as.numeric(ebd_x_mes_grps$grp[ebd_x_mes_grps$grp %in% grps_order[2]]), 
       asp = 1, pch = 16)

ebd_x_mes_grps[ebd_x_mes_grps$grp %in% grps_order[1],]

ebd_x_mes <- ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  mutate(latBand = cut(lat, 
                        breaks = c(-30, -15, 0, 15, 30), 
                        labels = c("-30 to -15", "-15 to 0", "0 to 15", "15 to 30")),
         ns = lat > 0,
         eq = abs(lat) < 7.5) %>%
  group_by(k5, month, month_no, scientific_name, ns, eq, latBand) %>%
  summarise(n = n(), .groups = "drop",
            lat = mean(lat, na.rm = T),
            lon = mean(lon, na.rm = T),
            mean_prec = mean(prec, na.rm = T)
           ) %>%
  mutate(date = as.Date(sprintf("2010-%02d-15", month_no), format = "%Y-%m-%d")) # representative date

ebd_x_mes

table(ebd_x_mes$latBand, useNA = "always")
table(ebd_x_mes$k5, useNA = "always")

unique(ebd_x_mes$month)
levels(ebd_x_mes$latBand)
sum(is.na(ebd_x_mes$latBand))
levels(ebd_x_mes$k5)

## Add suncalc for summarised data (by date and position)
sun <- suncalc::getSunlightTimes(data = ebd_x_mes, tz = "UTC", keep = c("sunrise", "sunset"))
head(sun)
ebd_x_mes <- cbind(ebd_x_mes, sun[,c("sunrise", "sunset")])
ebd_x_mes$day_length <- difftime(ebd_x_mes$sunset, ebd_x_mes$sunrise)

head(ebd_x_mes)

## SAVE
save(ebd_x_mes, file = "./data/ebd_x_mes_species.rdata")





