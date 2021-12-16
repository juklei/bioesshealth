## This script adds wood-decaying fungi models to the NFI data with one occupancy
## per NFI plot, ControlCategoryName, AlternativeNo, and period
## 
## Models by:
## Mair L, Jönsson M, Räty M, et al.
## Land use changes could modify future negative effects of climate change on 
## old-growth forest indicator species. 
## Divers Distrib. 2018;00:1-10. https://doi.org/10.1111/ddi.12771
##
## First edit: 2021-12-16 
## Last edit: 
##
## Author: Julian Klein

## 1. Clear environment, load libraries, and data ------------------------------

rm(list = ls())

library(data.table)
library(boot)

dir <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook"
dir.create("clean")

## Define climate:
climate <- "RCP0"
# climate <- "RCP45"
# climate <- "RCP48"

model_params_wdf <- read.csv("data/Table_SummaryModelCompare-GLMnewfitting_03juli2020_TS.csv")
min_age <- c(87, 75, 64, 64, 76, 83) ## From table S4.3
names(min_age) <- as.character(model_params_wdf$Species)
  
## -----------------------------------------------------------------------------

if(climate == "RCP0") {file <- "MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"}
if(climate == "RCP45") {file <- "MultiForestResults210710_20Periods_InclIntensivve_RCP45.csv"}
if(climate == "RCP85") {file <- "MultiForestResults210830_20Periods_InclIntensivve_RCP8_5.csv"}

d_cov <- fread(paste0(dir, "/data/", file),  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumeSpruce", "DeadWoodVolumeSpruce"), 
                  sep = ";", blank.lines.skip = TRUE)

d_cov <- d_cov[d_cov$period == 0,] ## REDUCED SIZE FOR TRIAL !!!!!!!!!!!!!!!!!!!

## 2. Extract and add climate data to d_cov ------------------------------------

##...

## 3. Extract only median params for WDF ---------------------------------------

median_extract <- function(x){
  T1 <- strsplit(as.character(x), " ")
  return(as.numeric(sapply(T1, "[", 1)))
}

mp <- t(apply(model_params_wdf[, 2:11], 2, median_extract))
colnames(mp) <- as.character(model_params_wdf$Species)

## 4. Add species to d_cov according to model formulas -------------------------

## Standardise Heureka variables; those were standardised in model fitting:
## Not sure whether this is the right way to do it, since the SD changes 
## depending on which period and which management regime is chosen. ????????????
d_cov$VS_std <- d_cov$VolumeSpruce/sd(d_cov$VolumeSpruce[d_cov$period == 0])
d_cov$Age_std <- d_cov$Age/sd(d_cov$Age[d_cov$period == 0])
# d_cov$temp_std <- d_cov$temp/sd(d_cov$temp[d_cov$period == 0])
# d_cov$precip_std <- d_cov$precip/sd(d_cov$precip[d_cov$period == 0])
# d_cov$st_std <- d_cov$swe_twi/sd(d_cov$swe_twi[d_cov$period == 0])

pred_WDF <- function(x){
  DWC <- ifelse(x$DeadWoodVolumeSpruce > 0, 1, 0)
  AgC <- ifelse(x$Age > min_age, 1, 0 )
  WDF <- inv.logit(mp["Intercept", ] + 
                     mp["z.ald_max", ]*x$Age_std + 
                     mp["z.gran_max", ]*x$VS_std +
                     mp["z.ald_max.z.gran_max", ]*x$Age_std*x$VS_std #+
                     # mp["z.temp", ]*x$temp_std +
                     # mp["z.ald_max.z.tempann..09juli2020.", ]*x$Age_std*x$temp_std +
                     # mp["z.precsummjjason", ]*x$precip_std + 
                     # mp["z.tempann.z.precsummjjason", ]*x$temp_std*x$precip_std +
                     # mp["z.ald_max.z.tempann..09juli2020.", ]*x$Age_std*x$temp_std +
                     # mp["z.swe_twi", ]*x$st_std
                   )
  as.list(DWC*AgC*WDF)
}

## Predict WDF data:
d_pred <- d_cov[, pred_WDF(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

out <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                   d_pred[,-1])

write.csv(out, paste0("clean/MFO_WDF_", climate, ".csv"))
  
## -------------------------------END-------------------------------------------
