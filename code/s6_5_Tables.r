### Script 6.5 - make supp tables

library(dplyr)

ebird_df <- readRDS("./data/ebird/ebird_df_subset.rds")

load("./data/ebd_x_mes_species.rdata") ## ebd_x_mes

### Table 1

ebird_df %>%
  filter(lat < -30 | is.na(k5)) %>%
  summarise(n = n())
#555

ebird_df %>%
  filter(lat < -30) %>%
  summarise(n = n())
#466

ebird_df %>%
  filter(is.na(k5)) %>%
  summarise(n = n())
#89

## TOTAL RECORDS
ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  summarise(n = n())
  


# by species
tab1 <- ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  group_by(scientific_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

tab1

write.csv(tab1, "./plots/table_1.csv", row.names = FALSE)

ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  group_by(scientific_name) %>%
  summarise(n = n()) %>%
  summarise(max = max(n), 
            med = median(n),
            min = min(n))

# A tibble: 1 × 3
# max    med     min
# 12354  1828.   306


tab2 <- ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  mutate(latBand = cut(lat, 
                       breaks = c(-30, -15, 0, 15, 30), 
                       labels = c("-30 to -15", "-15 to 0", "0 to 15", "15 to 30"))) %>%
  group_by(latBand) %>%
  summarise(n = n())

tab2
write.csv(tab2, "./plots/table_1b.csv", row.names = FALSE)
# latBand        n
# -30 to -15  7906
# -15 to 0    8789
# 0 to 15    17847
# 15 to 30    9529

tab3 <- ebird_df %>%
  filter(lat >= -30 & !is.na(k5)) %>%
  group_by(k5) %>%
  summarise(n = n())

tab3
write.csv(tab3, "./plots/table_1c.csv", row.names = FALSE)
# k5        n
# 1     17163
# 2      3045
# 3      1785
# 4     10175
# 5     11903


head(ebd_x_mes)

