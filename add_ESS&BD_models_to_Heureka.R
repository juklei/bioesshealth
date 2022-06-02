## This script adds ESS and BD models to the NFI data with one value
## per NFI plot, ControlCategoryName, AlternativeNo, and period
##
## All functions that add the models to Heureka data are sourced in this script.
## The model specific information can be found in the respective scripts.
## The model parameters, mean&SD, and other relevant info are defined inside the 
## respective script. The script "extract_model_data.r" shows which original 
## model data is used and how it is transformed.
##
## The user can select the species, ESS, and original Heureka data that should 
## be present in the output data set.
##
## ACTIVATE/DEACTIVATE ROWS WITH Ctrl/Cmd + shift + C
##
## First edit: 2022-05-26
## Last edit: 2022-06-01
##
## Author: Julian Klein

## Keep track of computing time:
start <- Sys.time()

## 1. Clear environment, load libraries, define directories, etc. --------------

rm(list = ls())

library(data.table)
library(boot)
library(VGAM)

## Directory and file names of the Heureka simulation data:
dir_HK <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook/data/"
file_RCP0 <- "MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"
file_RCP45 <- "MultiForestResults210710_20Periods_InclIntensivve_RCP45.csv"
file_RCP85 <- "MultiForestResults210830_20Periods_InclIntensivve_RCP8_5.csv"

## Load climate and topo data of NFI plots:
NFI_BP <- read.csv("data/NFI_Loebel_et_al_2021.csv")
NFI_ESS <- read.csv("data/NFI_Mazziotta_et_al_2022.csv")

## Chose climate scenario:
climate <- "RCP0"
# climate <- "RCP45"
# climate <- "RCP85"

## Select same random share of NFI plots for trial calculations:
NFI_share <- 0.0001

## Define period 0 year in Heureka:
period_0 <- 2010

## 2. Chose species and ESS ---------------------------------------------------- 

## Wood-decaying fungi:
WDF <- c(
  "A. lapponica", 
  "A. serialis", 
  "F. pinicola", 
  "F. rosea", 
  "P. centrifuga",
  "P. ferrugineofuscus", 
  "P. nigrolimitatus", 
  "T. abietinum", 
  "G. sepiarium", 
  "P. viticola",
  "P. chrysoloma"
  )

## Lichens:
LP <- "L. pulmonaria"
RL <- c( ## Reindeer model to be introduced....
  # "Cladonia...", 
  # "Alectoria..."
  ) 

## Birds:
Birds <- c(
  "T. bonasia",
  "P. infaustus",
  "P. cinctus",
  "A. caudatus",
  "F. parva",
  "P. canus",
  "D. minor",
  "P. tridactylus"
  )

## Bryophytes:
BP <- c(
  "Ana.hell", 
  "Ana.mich", 
  "Bux.vir", 
  "Cal.hald", 
  "Cal.suec",
  "Camp.som", 
  "Ceph.cat", 
  "Dicr.denu", 
  "Dicr.flag", 
  "Dicr.frag",
  "Geo.grav", 
  "Herz.sel", 
  "Herz.tur", 
  "Jam.aut", 
  "Jung.lei", 
  "Lo.asc",
  "Lo.cil", 
  "Lo.loflo", 
  "My.tay", 
  "Now.cur", 
  "Od.denu", 
  "Sca.api", 
  "Sca.car", 
  "Sca.api.DistWater", 
  "Sca.car.DistWater"
  )

## Ecosystem services:
ESS <- c(
  "Bilberry.beta", 
  "Wildfood", 
  "Richness", 
  "Bilberry.bern"
  )

## Beetles:
BTL <- c(
  # ...u
  )

##


## 3. Chose which original Heureka columns should remain in output data --------
##    Heureka ID columns, period, and represented area are kept automatically.
##    Needs updating if Heureka data changes.

HK <- c(
  # "Richness", 
  # "BilberryCover", 
  # "Wildfood",  
  "Age", 
  "StandingVolume", 
  # "NoOfStems", 
  # "Volume", 
  # "VolumeExclOverstory", 
  # "VolumeDecidous", 
  "SumVolumeCutTotal",
  # "SumTimberVolumeTotal", 
  # "SumPulpVolumeTotal", 
  # "SumHarvestResiduesTotal",
  # "SumHarvestFuelwoodTotal", 
  "AnnualIncrementNetTotal",
  "DeadWoodVolume", 
  # "DeadWoodVolumeSpruce", 
  # "DeadWoodLyingDeciduous", 
  # "DeadWoodLyingConiferous",
  # "DeadWoodStandingDeciduous", 
  # "DeadWoodStandingConiferous", 
  "RecreationIndex", 
  # "TotalSoilCarbon", 
  # "TotalCarbonStocksStumpsandRoots",
  # "TotalCarbonStockTreesAboveGround", 
  "TotalCarbon", 
  # "VolumeAspen",
  # "VolumeBeech",
  # "VolumeContorta", 
  # "VolumeOak",
  # "VolumeLarch", 
  # "VolumeBirch",
  # "VolumeOtherBroadLeaf", 
  # "VolumePine", 
  # "VolumeSouthernBroadleaf", 
  # "VolumeSpruce",
  "NPV" 
  )
  
## 4. Run model functions and export data set ----------------------------------

## Run all ESS & BD scripts:
source("scripts/select_Heureka_variables.r")
source("scripts/add_WDF_Mair_2018_to_NFI_data.r")
source("scripts/add_WDF_Moor_2021_to_NFI_data.r")
source("scripts/add_bryophytes_Lobel_2021_to_NFI_data.r")

## Combine all ESS & BD output data sets with original heureka data:
df_list <- list(d_HK, out_Mair, out_Lobel)
out <- Reduce(function(x, y){
  merge(x, y, all=TRUE, by = c("Description", "period", "AlternativeNo", "ControlCategoryName"))
  },
  df_list)

## Export to same directory where original Heureka data is stored:
fwrite(out, paste0(dir_HK, "MFO_results_ESS&BD_added_", climate, ".csv"))
  
## -------------------------------END-------------------------------------------

end <- Sys.time()
end-start ## 35min for 1%


