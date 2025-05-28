## Script 3
## Add sunrise / sunset times to ebird records

getwd()

library(suncalc)

## Read in data
dest <- file.path(getwd(), "data/eBird")
output_file <- file.path(dest, "filtered_tinamidae_relApr-2023.txt")

# ebird_df <- auk::read_ebd(output_file)
ebird_df <- readRDS(file.path(dest, "ebird_df.rds"))
str(ebird_df)
sum(is.na(ebird_df$latitude))
sum(is.na(ebird_df$observation_date))

## rename columns for suncalc
colnames(ebird_df)[colnames(ebird_df) %in% c("latitude", "longitude", "observation_date")] <- c("lat", "lon", "date")
str(ebird_df)

# get sunrise/sunset times
sun <- suncalc::getSunlightTimes(data = ebird_df, tz = "UTC", keep = c("sunrise", "sunset"))
head(sun)

ebird_df <- cbind(ebird_df, sun[,c("sunrise", "sunset")])
head(ebird_df)

## Save - overwrite previous version
saveRDS(ebird_df, file = file.path(dest, "ebird_df.rds"))

