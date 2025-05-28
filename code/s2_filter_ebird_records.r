## Script 2
## Filter eBird records #####

## Filter ebird records for effort (time < 60 mins, distance < 5 km),
## dates (2000 - 2022) and species of interest (forest tinamous)

getwd()

library(dplyr)
library(auk)

## get eBird version
auk_version()
# $auk_version
# [1] "auk 0.6.0"
# $ebd_version
# [1] "2022-10-25"
# $taxonomy_version
# [1] 2022

## Species of interest
spp <- c("Nothocercus julius", "Nothocercus bonapartei", "Nothocercus nigrocapillus", "Tinamus tao", "Tinamus solitarius", "Tinamus major", "Tinamus guttatus", "Tinamus osgoodi", "Crypturellus cinereus", "Crypturellus soui", "Crypturellus obsoletus", "Crypturellus undulatus", "Crypturellus boucardi", "Crypturellus variegatus", "Crypturellus cinnamomeus", "Crypturellus parvirostris", "Crypturellus tataupa", "Crypturellus casiquiare", "Crypturellus bartletti", "Crypturellus berlepschi", "Crypturellus atrocapillus", "Crypturellus strigulosus", "Crypturellus kerriae", "Crypturellus duidae", "Crypturellus transfasciatus", "Crypturellus erythropus", "Crypturellus brevirostris", "Crypturellus ptaritepui", "Crypturellus noctivagus")

## check spp in eBird taxonomy
spp_ebird <- auk::ebird_species(spp, "all", 
                                taxonomy_version = 2022)

# check all matches
sum(is.na(spp_ebird$species_code))

## Define input and output files
dest <- file.path(getwd(), "data/eBird")
input_file <- file.path(dest, "tinamidae_relApr-2023.txt") # adjust here if using complete ebird data
output_file <- file.path(dest, "filtered_tinamidae_relApr-2023.txt")

ebird_df <- input_file %>%
  auk_ebd() %>%
  ## filters
  auk_species(species = spp_ebird$scientific_name, 
              taxonomy_version = 2022) %>%
  auk_date(date = c("2000-01-01", "2022-12-31")) %>%  # %Y-%m-$d
  auk_distance(distance = c(0,5)) %>% ## km
  auk_duration(duration = c(0, 60)) %>% ## minutes
  auk_complete() %>% # 47870 rows without complete checklists
  auk_filter(file = output_file, overwrite = TRUE) %>%
  read_ebd()

head(ebird_df)
colnames(ebird_df)

saveRDS(ebird_df, file = file.path(dest, "ebird_df.rds"))
