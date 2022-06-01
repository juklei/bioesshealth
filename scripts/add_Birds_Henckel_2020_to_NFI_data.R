## This script adds bird models to the NFI data with one occupancy
## per NFI plot, ControlCategoryName, AlternativeNo, and period.
##
## The data for this script was produced by Laura Henckel. This script only 
## combines and reorganises the data.
## 
## Models by:
## Henckel L, Bradter U, Jönsson M, Isaac NJB, Snäll T. 
## Assessing the usefulness of citizen science data for habitat suitability 
## modelling: Opportunistic reporting versus sampling based on a systematic 
## protocol. 
## Divers Distrib. 2020;26:1276-1290. https://doi.org/10.1111/ddi.13128
##
## First edit: 2022-05-25 
## Last edit: 2022-05-25
##
## Author: Julian Klein

## 1. Clear environment, load libraries, and data ------------------------------

rm(list = ls())

library(data.table)
library(archive)

start <- Sys.time()

dir <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook"
dir.create("clean")

da

## 2. Combine the data files per climate scenario ------------------------------




## 6. Create output data -------------------------------------------------------

fwrite(out, "clean/MFO_WDF_Moor.csv", row.names = FALSE)

end <- Sys.time()

end-start ## 35min for 1%

## -------------------------------END-------------------------------------------
