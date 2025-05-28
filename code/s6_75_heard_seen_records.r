## Script 6.75 ## Summarise seen / heard records

## Numbers of seen / heard

ebird_df <- readRDS("./data/ebird/ebird_df_subset.rds")

ebird_df <- subset(ebird_df, lat >= -30 & !is.na(k5))

#### Comments on heard vs seen
head(ebird_df$species_comments)
## get "heard", escuchado" "seen" from comments / behaviour notes
# total comments
sum(!is.na(ebird_df$species_comments))
# 5480 total species comments
sum(grepl("heard|escucha.*", ebird_df$species_comments, ignore.case = TRUE))
# 2163
sum(grepl("seen|visto|vimos", ebird_df$species_comments, ignore.case = TRUE))
# 111

## Breeding codes. eg
# S7--Singing male present 7+ days – Singing male, presumably the same individual,
# present in suitable nesting habitat during its breeding season and holding territory in
# the same area on visits at least 7 days apart. Typically considered probable.
# S--Singing male – Singing male present in suitable nesting habitat during its breeding
# season. Typically considered Possible.


# total behaviour codes
sum(!is.na(ebird_df$behavior_code))
# 1199
sum(ebird_df$behavior_code == "NA", na.rm = T)

## behaviour codes implying seen e.g. nest with eggs, feeding young
sum(grepl("NE|FS|FY|CF|FL|ON|UN|DD|NB|CN|PE|C|P", ebird_df$behavior_code)) 
# 30
sum(grepl("NE|FS|FY|CF|FL|ON|UN|DD|NB|CN|PE|C|P", ebird_df$behavior_code) & 
      sum(grepl("seen|visto", ebird_df$species_comments, ignore.case = TRUE))) 
## All seen. 30 - so all repeated. no extra

sum(grepl("S|S7|M", ebird_df$behavior_code)) ## singing male... 
# 940

# how many behaviour codes are replicated in comments
sum(grepl("S|S7|M", ebird_df$behavior_code) & grepl("heard|escucha.*", ebird_df$species_comments, ignore.case = TRUE)) ## singing male... 
# plus 940 - 46 extra Heard is 894

sum(grepl("escucha.*", ebird_df$species_comments, ignore.case = TRUE)) # 344
ebird_df$species_comments[grepl("escucha.*", ebird_df$species_comments, ignore.case = TRUE)]

# Total comments or behaviour codes (some have both)
sum(!is.na(ebird_df$species_comments) | !is.na(ebird_df$behavior_code))
# 6273

# Heard: 2163 + 894 = 3057
# Seen 111
# Out of 6273

## Behaviour codes: 940 (heard) vs 30 (seen) out of 1199 behaviour codes
## Species comments: 2163 (heard) vs 111 (seen) out of 5480 species comments
## Total behaviour + comments: 3057 (heard) vs 111 (seen) out of 6273 total comments
# 3057/6273 = 49%
# 111/6273 = 2%
