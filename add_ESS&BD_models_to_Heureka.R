## This script adds ESS and BD models to the NFI data with one value
## per NFI plot, ControlCategoryName, AlternativeNo, and period
##
## All functions that add the models to Heureka data are sourced in this script.
## The model specific information can be found in the respective scripts.
##
## The user can select the species, ESS, and original Heureka data that should 
## be present in the output data set.
##
## ACTIVATE/DEACTIVATE ROWS WITH Ctrl/Cmd + shift + C
##
## First edit: 2022-05-26
## Last edit: 2022-05-26
##
## Author: Julian Klein

## 1. Clear environment, load libraries, define directories, etc. --------------

rm(list = ls())

library(data.table)
library(boot)
library(VGAM)

## Keep track of computing time:
start <- Sys.time()

## Directory and file names of the Heureka simulation data:
dir_HK <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook/data/"
file_RCP0 <- "MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"
file_RCP45 <- "MultiForestResults210710_20Periods_InclIntensivve_RCP45.csv"
file_RCP85 <- "MultiForestResults210830_20Periods_InclIntensivve_RCP8_5.csv"

## Directory and file name of the climate and topo data of NFI plots:
dir_file_CT <- "data/NFI_climate_topo.csv"

## Chose climate scenario:
climate <- "RCP0"
# climate <- "RCP45"
# climate <- "RCP85"

## Select random share of NFI plots for trial calculations:
NFI_share <- 0.001

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
  # "P. chrysoloma"
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
  "Anastrophyllum hellerianum", 
  "Anastrophyllum michauxii", 
  "Buxbaumia viridis", 
  "Callicladium haldanianum", 
  "Calypogeia suecica", 
  "Campylium sommerfeltii", 
  "Cephalozia catenulata", 
  "Dicranodontium denudatum", 
  "Dicranum flagellare",
  "Dicranum fragilifolium", 
  "Geocalyx graveolens", 
  "Herzogiella seligeri",
  "Herzogiella turfacea", 
  "Jamesionella autumnalis", 
  "Jungermannia subulata leiantha",
  "Lophozia ascendens", 
  "Lophozia ciliata",
  "Lophozia longiflora", 
  "Mylia taylorii", 
  "Nowellia curvifolia", 
  "Odontoschisma denudatum", 
  "Scapania apiculata", 
  "Scapania carinthiaca", 
  "Scapania apiculata DistWater", ## Including the predictor distance to water 
  "Scapania carinthiaca DistWater" ## Including the predictor distance to water
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
  "Richness", 
  "BilberryCover", 
  "Wildfood",  
  "Age", 
  "StandingVolume", 
  "NoOfStems", 
  "Volume", 
  "VolumeExclOverstory", 
  "VolumeDecidous", 
  "SumVolumeCutTotal",
  "SumTimberVolumeTotal", 
  "SumPulpVolumeTotal", 
  "SumHarvestResiduesTotal",
  "SumHarvestFuelwoodTotal", 
  "AnnualIncrementNetTotal",
  "DeadWoodVolume", 
  "DeadWoodVolumeSpruce", 
  "DeadWoodLyingDeciduous", 
  "DeadWoodLyingConiferous",
  "DeadWoodStandingDeciduous", 
  "DeadWoodStandingConiferous", 
  "NPV", 
  "RecreationIndex", 
  "TotalSoilCarbon", 
  "TotalCarbonStocksStumpsandRoots",
  "TotalCarbonStockTreesAboveGround", 
  "TotalCarbon", 
  "VolumeAspen",
  "VolumeBeech",
  "VolumeContorta", 
  "VolumeOak",
  "VolumeLarch", 
  "VolumeBirch",
  "VolumeOtherBroadLeaf", 
  "VolumePine", 
  "VolumeSouthernBroadleaf", 
  "VolumeSpruce"
  )
  
## 4. Run model functoins and export data set ----------------------------------

add_WDF_Mair_2018_to_NFI_data()

dir_out <- "clean/"
  
dir.create("clean")

end <- Sys.time()

end-start ## 35min for 1%

## -------------------------------END-------------------------------------------

## always Heureka output:,
temp <- c("Description", "period", "AlternativeNo", "RepresentedArea", 
          "ControlCategoryName", "Region", "County", "reserve")

