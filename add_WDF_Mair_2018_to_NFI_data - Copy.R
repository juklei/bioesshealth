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
# climate <- "RCP85"

## Period 0 year in Heureka:
period_0 <- 2010

## Climate data related to the NFI plots: SO FAR FROM MOSSES. SHOULD BE THE SAME BUT tempann MISSING !!!!!!!!!!!!
NFI <- read.csv("data/NFI.plots_ClimateData_for_mosses.txt", sep = "", header = TRUE) 
NFI[is.na(NFI$slope),] ## Identify why some are NA!!! What is the effect of this on the entire prediciton?
## Most species have at least one of the NA variables as covariate

## Model parameters for wood-decaying fungi (WDF):
model_params_wdf <- read.csv("data/Table_SummaryModelCompare-GLMnewfitting_03juli2020_TS.csv")

## Minimum age of a forest to host the species:
min_age <- c(87, 75, 64, 64, 76, 83) ## From table S4.3
names(min_age) <- as.character(model_params_wdf$Species)

## The original model fitting data set to calculate mean and SD from:
model_data_wdf <- read.table("data/DataToJulian20211217.txt", sep = "", header = TRUE)  

## -----------------------------------------------------------------------------

if(climate == "RCP0") {file <- "MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"}
if(climate == "RCP45") {file <- "MultiForestResults210710_20Periods_InclIntensivve_RCP45.csv"}
if(climate == "RCP85") {file <- "MultiForestResults210830_20Periods_InclIntensivve_RCP8_5.csv"}

d_cov <- fread(paste0(dir, "/data/", file),  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumeSpruce", "DeadWoodVolumeSpruce"), 
                  sep = ";", blank.lines.skip = TRUE)

d_cov <- d_cov[d_cov$Description == out$Description & ## out => From Moor et al. to compare
                 d_cov$AlternativeNo == out$AlternativeNo &
                 d_cov$ControlCategoryName == out$ControlCategoryName,] ## REDUCED SIZE FOR TRIAL !!!!!!!!!!!!!!!!!!!

## 2. Add climate data to d_cov ------------------------------------------------

## Harmonise style of "Description" for merging:
NFI$Description <- gsub("_", " ", NFI$Description)

## Year needs to become period and adjusted for missing years:
NFI$period <- (NFI$Year - period_0)/5

## Add period 0, 19 & 20 to the data: Discuss this step with Tord !!!!!!!!!!!!!!
## For now I use period 1 for period 0 and period 18 for 19 & 20
T1 <- NFI[NFI$period == 1, ]; T1$period <- 0
T2 <- NFI[NFI$period == 18, ]; T2$period <- 19
T3 <- NFI[NFI$period == 18, ]; T3$period <- 20
NFI <- rbind(NFI, T1, T2, T3)

## Aggregate and rename climate data for merging:
if(climate == "RCP0"){
  # colnames(NFI)[c(10, 11)] <- c("meantempann", "precsumamjjason")
  colnames(NFI)[11] <- "precsumamjjason" ## Add meantempann once available!!!!!!
}

if(climate == "RCP45"){
  # NFI$meantempmamj <- rowMeans(NFI[, c("meantempann_RCP4.5_BAU_ICHEC.EC.EARTH",
  #                                      "meantempann_RCP4.5_BAU_IPSL.IPSL.CM5A.MR",
  #                                      "meantempann_RCP4.5_BAU_MOHC.HadGEM2.ES",
  #                                      "meantempann_RCP4.5_BAU_MPI.M.MPI.ESM.LR")])
  NFI$precsumamjjason <- rowMeans(NFI[, c("precsumamjjason_RCP4.5_BAU_ICHEC.EC.EARTH",
                                          "precsumamjjason_RCP4.5_BAU_IPSL.IPSL.CM5A.MR",
                                          "precsumamjjason_RCP4.5_BAU_MOHC.HadGEM2.ES",
                                          "precsumamjjason_RCP4.5_BAU_MPI.M.MPI.ESM.LR")])
}

if(climate == "RCP85"){
  # NFI$meantempann <- rowMeans(NFI[, c("meantempann_RCP85_CNRM.CERFACS.CNRM.CM5",
  #                                      "meantempann_RCP85_ICHEC.EC.EARTH",
  #                                      "meantempann_RCP85_IPSL.IPSL.CM5A.MR",
  #                                      "meantempann_RCP85_MOHC.HadGEM2.ES",
  #                                      "meantempann_RCP85_MPI.M.MPI.ESM.LR")])
  NFI$precsumamjjason <- rowMeans(NFI[, c("precsumamjjason_RCP85_CNRM.CERFACS.CNRM.CM5",
                                          "precsumamjjason_RCP85_ICHEC.EC.EARTH",
                                          "precsumamjjason_RCP85_IPSL.IPSL.CM5A.MR",
                                          "precsumamjjason_RCP85_MOHC.HadGEM2.ES",
                                          "precsumamjjason_RCP85_MPI.M.MPI.ESM.LR")])
}

## Merge Heureka with NFI data:
d_cov <- merge(d_cov, 
               NFI[, c("Description", "period", "swe_twi_wetness", "precsumamjjason")],
               all.x = TRUE, 
               by = c("Description", "period"))

## 3. Extract only median params for WDF ---------------------------------------

median_extract <- function(x){
  T1 <- strsplit(as.character(x), " ")
  return(as.numeric(sapply(T1, "[", 1)))
}

mp <- t(apply(model_params_wdf[, 2:11], 2, median_extract))
colnames(mp) <- as.character(model_params_wdf$Species)

## 4. Add species to d_cov according to model formulas -------------------------

## Standardise Heureka variables with mean and SDs from original model data: 
## Verfiy with Louise Mair that centering was used!!! refit models using only scaled and both and evaluate parameters
d_cov$VS_std <- (d_cov$VolumeSpruce - mean(model_data_wdf$gran_max))/sd(model_data_wdf$gran_max)
d_cov$Age_std <- (d_cov$Age - mean(model_data_wdf$ald_max))/sd(model_data_wdf$ald_max)
# d_cov$temp_std <- (d_cov$meantempann - mean(model_data_wdf$tempann))/sd(model_data_wdf$tempann)
d_cov$precip_std <- (d_cov$precsumamjjason - mean(model_data_wdf$precsummjjason))/sd(model_data_wdf$precsummjjason)
d_cov$wet_std <- (d_cov$swe_twi_wetness - mean(model_data_wdf$swe_twi))/sd(model_data_wdf$swe_twi)

pred_WDF <- function(x){
  DwC <- ifelse(x$DeadWoodVolumeSpruce > 0, 1, 0)
  AgC <- ifelse(x$Age > min_age, 1, 0 )
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before!
  WDF <- inv.logit(mp["Intercept", ] + 
                     mp["z.ald_max", ]*x$Age_std + 
                     mp["z.gran_max", ]*x$VS_std +
                     mp["z.ald_max.z.gran_max", ]*x$Age_std*x$VS_std +
                     # mp["z.temp", ]*x$temp_std +
                     # mp["z.ald_max.z.tempann..09juli2020.", ]*x$Age_std*x$temp_std +
                     mp["z.precsummjjason", ]*x$precip_std +
                     # mp["z.tempann.z.precsummjjason", ]*x$temp_std*x$precip_std +
                     # mp["z.ald_max.z.tempann..09juli2020.", ]*x$Age_std*x$temp_std +
                     mp["z.swe_twi", ]*x$wet_std
                   )
  as.list(DwC*AgC*WDF) ## Multiply also with RpC once added above!
}

## Predict WDF data:
d_pred <- d_cov[, pred_WDF(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

out <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                   d_pred[,-1])

write.csv(out, paste0("clean/MFO_WDF_Mair_", climate, ".csv"), row.names = FALSE)
  
## -------------------------------END-------------------------------------------
