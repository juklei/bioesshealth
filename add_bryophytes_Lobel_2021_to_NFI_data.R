## This script adds bryophyte models to the NFI data with one occupancy
## per NFI plot, ControlCategoryName, AlternativeNo, and period
## 
## Models by:
## Löbel, S., Schröder, B., & Snäll, T. (2021). 
## Projected shifts in deadwood bryophyte communities under national climate and 
## forestry scenarios benefit large competitors and impair small species. 
## Journal of Biogeography, 48, 3170-3184. https://doi.org/10.1111/jbi.14278
##
## First edit: 2021-12-17 
## Last edit: 
##
## Author: Julian Klein

## 1. Clear environment, load libraries, and data ------------------------------

rm(list = ls())
dir.create("clean")

library(data.table)
library(boot)

## Directory to the data coming from Heureka:
dir <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook"

## Define climate:
climate <- "RCP0"
# climate <- "RCP45"
# climate <- "RCP85"

## Period 0 year in Heureka:
period_0 <- 2010

## Climate data related to the NFI plots:
NFI <- read.csv("data/NFI.plots_ClimateData_for_mosses.txt", sep = "", header = TRUE) 
NFI[is.na(NFI$slope),] ## Identify why some are NA!!! What is the effect of this on the entire prediciton?
## Most species have at least one of the NA variables as covariate

## Model parameters for bryophytes:
# model_params <- read.table("data/Bryo_CoeffLogReg_exclConn_inclFAge.txt", 
#                                sep = "", header = TRUE)
model_params <- read.csv("data/Bryo_CoeffLogReg_exclConn_inclFAge_corr.csv")

## Minimum age of a forest to host the species:
min_age <- read.table("data/ForestAgeThresholdsBryo.txt", sep = "", header = TRUE)

## The original model fitting data set to calculate mean and SD from:
model_data <- read.table("data/StandParametersBryophytes.txt", sep = "", header = TRUE)
  
## -----------------------------------------------------------------------------

if(climate == "RCP0"){file <- "MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"}
if(climate == "RCP45"){file <- "MultiForestResults210710_20Periods_InclIntensivve_RCP45.csv"}
if(climate == "RCP85"){file <- "MultiForestResults210830_20Periods_InclIntensivve_RCP8_5.csv"}

d_cov <- fread(paste0(dir, "/data/", file),  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumePine", "VolumeSpruce", "VolumeAspen",
                          "VolumeBeech", "VolumeOak", "VolumeBirch", "VolumeOtherBroadLeaf",
                          "VolumeSouthernBroadleaf"),
               sep = ";", blank.lines.skip = TRUE)

d_cov <- d_cov[d_cov$period == 0,] ## REDUCED SIZE FOR TRIAL !!!!!!!!!!!!!!!!!!!

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
  colnames(NFI)[c(10:12)] <- c("meantempmamj", "tempOctNov", "precsumamjjason")
}

if(climate == "RCP45"){
  NFI$meantempmamj <- rowMeans(NFI[, c("meantempmamj_RCP4.5_BAU_ICHEC.EC.EARTH",
                                       "meantempmamj_RCP4.5_BAU_IPSL.IPSL.CM5A.MR",
                                       "meantempmamj_RCP4.5_BAU_MOHC.HadGEM2.ES",
                                       "meantempmamj_RCP4.5_BAU_MPI.M.MPI.ESM.LR")])
  NFI$tempOctNov <- rowMeans(NFI[, c("tempOctNov_RCP4.5_BAU_ICHEC.EC.EARTH",
                                     "tempOctNov_RCP4.5_BAU_IPSL.IPSL.CM5A.MR",
                                     "tempOctNov_RCP4.5_BAU_MOHC.HadGEM2.ES",
                                     "tempOctNov_RCP4.5_BAU_MPI.M.MPI.ESM.LR")])
  NFI$precsumamjjason <- rowMeans(NFI[, c("precsumamjjason_RCP4.5_BAU_ICHEC.EC.EARTH",
                                          "precsumamjjason_RCP4.5_BAU_IPSL.IPSL.CM5A.MR",
                                          "precsumamjjason_RCP4.5_BAU_MOHC.HadGEM2.ES",
                                          "precsumamjjason_RCP4.5_BAU_MPI.M.MPI.ESM.LR")])
}

if(climate == "RCP85"){
  NFI$meantempmamj <- rowMeans(NFI[, c("meantempmamj_RCP85_CNRM.CERFACS.CNRM.CM5",
                                       "meantempmamj_RCP85_ICHEC.EC.EARTH",
                                       "meantempmamj_RCP85_IPSL.IPSL.CM5A.MR",
                                       "meantempmamj_RCP85_MOHC.HadGEM2.ES",
                                       "meantempmamj_RCP85_MPI.M.MPI.ESM.LR")])
  NFI$tempOctNov <- rowMeans(NFI[, c("tempOctNov_RCP85_CNRM.CERFACS.CNRM.CM5",
                                     "tempOctNov_RCP85_ICHEC.EC.EARTH",
                                     "tempOctNov_RCP85_IPSL.IPSL.CM5A.MR",
                                     "tempOctNov_RCP85_MOHC.HadGEM2.ES",
                                     "tempOctNov_RCP85_MPI.M.MPI.ESM.LR")])
  NFI$precsumamjjason <- rowMeans(NFI[, c("precsumamjjason_RCP85_CNRM.CERFACS.CNRM.CM5",
                                          "precsumamjjason_RCP85_ICHEC.EC.EARTH",
                                          "precsumamjjason_RCP85_IPSL.IPSL.CM5A.MR",
                                          "precsumamjjason_RCP85_MOHC.HadGEM2.ES",
                                          "precsumamjjason_RCP85_MPI.M.MPI.ESM.LR")])
}

## Merge Heureka with NFI data:
d_cov <- merge(d_cov, 
               NFI[, c("Description", "period", "swe_twi_wetness", 
                       "DistWaterCourse", "slope", "slope.asp", "meantempmamj",
                       "tempOctNov", "precsumamjjason")],
               all.x = TRUE, 
               by = c("Description", "period"))

## 3. Add species to d_cov according to model formulas -------------------------

## Make covariat*species matrix with estimates as content:
model_params <- as.data.table(model_params)
mp <- dcast(model_params, CovariateID ~ SpeciesID, value.var = "Estimate", fill = 0)
mp <- as.matrix(mp[,-1], rownames = mp$CovariateID) ## Must be a matrix

## Calculate missing columns:
d_cov$VolDecid <- rowSums(d_cov[, c("VolumeAspen", "VolumeBeech", "VolumeOak", 
                                    "VolumeBirch", "VolumeOtherBroadLeaf", 
                                    "VolumeSouthernBroadleaf")])
d_cov$VolOakBeech <- rowSums(d_cov[, c("VolumeBeech", "VolumeOak")])
d_cov$log.DistWaterCourse <- log(d_cov$DistWaterCourse)

## Standardise Heureka variables with mean and SDs from original model data:
scale <- function(x, covariate){
  (d_cov[, ..x] - model_data$Mean[model_data$Covariate == covariate])/
    model_data$SD[model_data$Covariate == covariate]
}
d_cov$Age_std <- scale("Age", "ald_max") ## _std because "Age" needed below
d_cov$VolumeSpruce <- scale("VolumeSpruce", "gran_max")
d_cov$VolumePine <- scale("VolumePine", "tall_max")
d_cov$VolumeBirch <- scale("VolumeBirch", "bjork_max")
d_cov$VolDecid <- scale("VolDecid", "lov_max")
d_cov$VolOakBeech <- scale("VolOakBeech", "OakBeech")
d_cov$meantempmamj <- scale("meantempmamj", "meantempmamj")
d_cov$tempOctNov <- scale("tempOctNov", "tempOctNov")
d_cov$precsumamjjason <- scale("precsumamjjason", "precsumamjjason")
d_cov$slope <- scale("slope", "slope")
d_cov$slope.asp <- scale("slope.asp", "slope.asp")
d_cov$swe_twi_wetness <- scale("swe_twi_wetness", "swe_twi_wetness")
d_cov$DistWaterCourse <- scale("DistWaterCourse", "DistWaterCourse")
d_cov$log.DistWaterCourse <- scale("log.DistWaterCourse", "log.DistWaterCourse")

## Create vector with minimum ages per species. if statement to check if order 
## of values correct:
if(all(min_age$Species[min_age$Species %in% colnames(mp)] == colnames(mp))){
  min_age_q2.5 <- min_age$q2.5[min_age$Species %in% colnames(mp)]
}

## Define prediction function:
pred_bry <- function(x){
  AgC <- ifelse(x$Age > min_age_q2.5, 1, 0 )
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before! How to know which controlCategor & Alternative contains a retention patch? Need to indicate this in future HEureka simulations??
  bry <- inv.logit(mp["(Intercept)", ] + 
                     mp["z.ald_max", ]*x$Age_std + 
                     mp["z.gran_max", ]*x$VolumeSpruce +
                     mp["z.tall_max", ]*x$VolumePine +
                     mp["z.bjork_max", ]*x$VolumeBirch +
                     mp["z.lov_max", ]*x$VolDecid +
                     mp["z.OakBeech", ]*x$VolOakBeech +
                     ## meantempmamj not present in model table
                     mp["z.tempOctNov", ]*x$tempOctNov +
                     mp["z.precsumamjjason", ]*x$precsumamjjason +
                     mp["z.slope", ]*x$slope +
                     mp["z.slope.asp", ]*x$slope.asp +
                     mp["z.swe_twi_wetness", ]*x$swe_twi_wetness +
                     ## DistWaterCourse not present in model table
                     mp["z.log.DistWaterCourse", ]*x$log.DistWaterCourse +
                     mp["I(z.gran_max^2)", ]*x$VolumeSpruce^2 +
                     mp["I(z.tall_max^2)", ]*x$VolumePine^2 +
                     mp["I(z.bjork_max^2)", ]*x$VolumeBirch^2 +
                     mp["I(z.lov_max^2)", ]*x$VolDec^2 +
                     mp["I(z.OakBeech^2)", ]*x$VolOakBeech^2 +
                     mp["I(z.tempOctNov^2)", ]*x$tempOctNov^2 +
                     mp["I(z.precsumamjjason^2)", ]*x$precsumamjjason^2 +
                     mp["I(z.swe_twi_wetness^2)", ]*x$swe_twi_wetness^2 +
                     mp["I(z.ald_max * z.gran_max)", ]*x$Age_std*x$VolumeSpruce +
                     mp["I(z.ald_max * z.tall_max)", ]*x$Age_std*x$VolumePine +
                     mp["I(z.tempOctNov * z.ald_max)", ]*x$Age_std*x$tempOctNov +
                     mp["I(z.gran_max * z.swe_twi_wetness)", ]*x$VolumeSpruce*x$swe_twi_wetness +
                     mp["I(z.tempOctNov * z.gran_max)", ]*x$VolumeSpruce*x$tempOctNov +
                     mp["I(z.tempOctNov * z.precsumamjjason)", ]*x$precsumamjjason*x$tempOctNov + ## Why different order in interaction?
                     mp["I(z.precsumamjjason * z.tempOctNov)", ]*x$precsumamjjason*x$tempOctNov + ## Why different order in interaction?
                     mp["I(z.slope * z.swe_twi_wetness)", ]*x$swe_twi_wetness*x$slope
                   )
  as.list(AgC*bry) ## Multiply also with RpC once added above!
}

## Predict Bryophyte data:
d_pred <- d_cov[, pred_bry(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

out <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                   d_pred[,-1])

fwrite(out, paste0("clean/MFO_bryophytes_", climate, ".csv"), row.names = FALSE)
  
## -------------------------------END-------------------------------------------
