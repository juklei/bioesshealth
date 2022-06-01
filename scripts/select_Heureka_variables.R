## This script loads selected and pre-selected Heureka data
##
## First edit: 2022-06-01
## Last edit: 2022-06-01
##
## Author: Julian Klein

rm(d_HK, mp, min_age, md_orig)

## 1. Load selected and pre-selected Heureka data ------------------------------

if(climate == "RCP0") file <- paste0(dir_HK, file_RCP0)
if(climate == "RCP45") file <- paste0(dir_HK, file_RCP45)
if(climate == "RCP85") file <- paste0(dir_HK, file_RCP85)

## Always Heureka output:,
presel <- c("Description", "period", "AlternativeNo", "RepresentedArea", 
            "ControlCategoryName", "Region", "County", "reserve")

## Load Heureka data:
d_HK <- fread(file, select = c(presel, HK), sep = ";", blank.lines.skip = TRUE)

## Select random share of NFI plots:
d_unique <- unique(d_HK$Description)
set.seed(1)
select <- sample(d_unique, NFI_share*length(d_unique))
d_HK <- d_HK[d_HK$Description %in% select, ]

## -------------------------------END-------------------------------------------
