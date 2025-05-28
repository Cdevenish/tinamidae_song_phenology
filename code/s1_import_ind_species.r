## Script 1
#### Import individual eBird zip files ####

getwd()

### Import data from individual ebird zipped data (per species) and save 
### as a text file.  This script can be omitted if complete eBird data set is downloaded instead of 
### individual species files. 

## Individual ebird data downloaded to "data/eBird" within working directory

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

# Check AWK
auk_get_awk_path()

## Especies
spp <- c("Nothocercus julius", 
         "Nothocercus bonapartei", 
         "Nothocercus nigrocapillus", 
         "Tinamus tao", 
         "Tinamus solitarius", 
         "Tinamus major", 
         "Tinamus guttatus", 
         "Tinamus osgoodi", 
         "Crypturellus cinereus", 
         "Crypturellus soui", 
         "Crypturellus obsoletus", 
         "Crypturellus undulatus", 
         "Crypturellus boucardi", 
         "Crypturellus variegatus", 
         "Crypturellus cinnamomeus", 
         "Crypturellus parvirostris", 
         "Crypturellus tataupa", 
         "Crypturellus casiquiare", 
         "Crypturellus bartletti", 
         "Crypturellus berlepschi", 
         "Crypturellus atrocapillus", 
         "Crypturellus strigulosus", 
         "Crypturellus kerriae", 
         "Crypturellus duidae", 
         "Crypturellus transfasciatus", 
         "Crypturellus erythropus", 
         "Crypturellus brevirostris", 
         "Crypturellus ptaritepui", 
         "Crypturellus noctivagus")

## Check taxonomy with current eBird taxonomy
spp_ebird <- auk::ebird_species(spp, "all",
                                taxonomy_version = 2022)

# check all matches
sum(is.na(spp_ebird$species_code))

spp_ebird %>%
  print(n = 30)

## Unzip individual species zip files (downloaded separately from eBird)
## path to folders
dest <- file.path(getwd(), "data/eBird")
## paths to zip files
dests <- file.path(dest, paste0("ebd_", spp_ebird$species_code, "_relApr-2023.zip"))
## filenames of text files inside zips
fns <- paste0("ebd_", spp_ebird$species_code, "_relApr-2023.txt")

## list to hold individual species dataframes
dat <- vector(mode = "list", length = length(fns))

for(i in seq(fns)){
  con <- unz(dests[i], fns[i])
  dat[[i]] <- read.table(con, sep = "\t", 
                         header = TRUE, 
                         quote = "", 
                         comment = "")
}

# check connections are closed
showConnections()

str(dat, 1)
dat_df <- do.call(rbind, dat)
head(dat_df)

dat_df$X <- NULL

colnames(dat_df)
# change colnames to ebird original format -- to make it readable by auk_ebd ()
colnames(dat_df) <- gsub("\\.", " ", colnames(dat_df))

unique(dat_df$`SCIENTIFIC NAME`)


## Save as single text file in same format as ebd files.
write.table(dat_df, 
            file = file.path(dest, "tinamidae_relApr-2023.txt"), 
            row.names = FALSE,
            sep = "\t",
            quote = FALSE)

## how many in original dataset? 
# dat_df <- read.table(file.path(dest, "tinamidae_relApr-2023.txt"), sep = "\t", header = TRUE, quote = "", comment = "")
nrow(dat_df)
## 440279