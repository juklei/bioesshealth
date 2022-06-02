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
## Last edit: 2022-05-27
##
## Author: Julian Klein

rm(d_cov, mp, min_age, md_orig)

## 1. Define model parameters, minimum age thresholds and mean&SD --------------

## The model parameters:
mp <- structure(c(-6.112, 0.596, 1.02, -0.348, -2.567, 0.84, 0.452, 0.393, 0, 0,
                  -3.949, 0.289, 0.912, -0.181, -2.027, 0.392, -0.191, 0.136, 
                  0.146, 0.075, -3.828, 0.316, 0.59, 0, -1.473, 0.214, -0.346, 
                  -0.457, -0.124, 0.135, -2.636, 0.127, 1.103, 0, -0.765, 0.556, 
                  -0.22, -0.181, -0.295, 0, -3.384, 0.34, 0.482, 0, -1.874, 
                  0.501, -0.136, -0.389, -0.195, -0.159, -4.402, 0.517, 0.968, 
                  -0.123, -1.618, 0.563, 0, 0, 0.162, 0), 
                .Dim = c(10L, 6L), 
                .Dimnames = list(c("Intercept", "z.ald_max", "z.gran_max", 
                                   "z.ald_max.z.gran_max","z.tempann", 
                                   "z.ald_max.z.tempann..09juli2020.", 
                                   "z.precsummjjason", "z.tempann.z.precsummjjason", 
                                   "z.gran_max.z.tempann", "z.swe_twi"), 
                                 c("amylap", "fomros", "phechr", "phefer", 
                                   "phenig", "phlcen")))

## From table S4.3
min_age <- c(amylap = 87, fomros = 75, phechr = 64, phefer = 64, phenig = 76, phlcen = 83)

## The mean and SDs from the original data:
md_orig <- structure(c(142.801650760898, 105.766835314708, 106.486652050555, 
                       34.3372729850402, 3.80210300749865, 2.55693671224763, 178.42886483205, 
                       34.6229068311015, 48.4663285970236, 129.004375157319), 
                     .Dim = c(2L, 5L), 
                     .Dimnames = list(c("Mean", "SD"), 
                                      c("gran_max", "ald_max", "tempann", 
                                        "precsumson", "swe_twi")))

## 2. Load Heureka data --------------------------------------------------------

if(climate == "RCP0") file <- paste0(dir_HK, file_RCP0)
if(climate == "RCP45") file <- paste0(dir_HK, file_RCP45)
if(climate == "RCP85") file <- paste0(dir_HK, file_RCP85)

d_cov <- fread(file,  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumeSpruce", "DeadWoodVolumeSpruce"), 
                  sep = ";", blank.lines.skip = TRUE)

## Select random share of NFI plots:
d_unique <- unique(d_cov$Description)
set.seed(1)
select <- sample(d_unique, NFI_share*length(d_unique))
d_cov <- d_cov[d_cov$Description %in% select, ]

## 3. Add climate data to d_cov ------------------------------------------------

NFI <- NFI_BP ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Adjust once WDF NFI data available !!!!!!!!!!!!!!!!!!!!!111

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

## 4. Add species to d_cov according to model formulas -------------------------

## Standardise Heureka variables with mean and SDs from original model data:
scale <- function(x, covariate){
  (d_cov[, ..x] - md_orig["Mean", covariate])/md_orig["SD", covariate]
}

## Scale Heureka variables with mean and SDs from original model data: 
## Verfiy with Louise Mair that centering was used!!! refit models using only scaled and both and evaluate parameters
d_cov$VS_sc <- scale("VolumeSpruce", "gran_max")
d_cov$Age_sc <- scale("Age", "ald_max")
# d_cov$temp_sc <- scale("meantempann", "tempann")
d_cov$precip_sc <- scale("precsumamjjason", "precsumson")
d_cov$wet_sc <- scale("swe_twi_wetness", "swe_twi")

## Check if min_age and mp have same name order:
if(!all(dimnames(mp)[[2]] == names(min_age))) stop("species names not matching")

pred_WDF <- function(x){
  DwC <- ifelse(x$DeadWoodVolumeSpruce > 0, 1, 0)
  AgC <- ifelse(x$Age > min_age, 1, 0 )
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before!
  WDF <- inv.logit(mp["Intercept", ] + 
                     mp["z.ald_max", ]*x$Age_sc + 
                     mp["z.gran_max", ]*x$VS_sc +
                     mp["z.ald_max.z.gran_max", ]*x$Age_sc*x$VS_sc +
                     # mp["z.temp", ]*x$temp_sc +
                     # mp["z.ald_max.z.tempann..09juli2020.", ]*x$Age_sc*x$temp_sc +
                     mp["z.precsummjjason", ]*x$precip_sc +
                     # mp["z.tempann.z.precsummjjason", ]*x$temp_sc*x$precip_sc +
                     # mp["z.ald_max.z.tempann..09juli2020.", ]*x$Age_sc*x$temp_sc +
                     mp["z.swe_twi", ]*x$wet_sc
                   )
  as.list(DwC*AgC*WDF) ## Multiply also with RpC once added above!
}

## Predict WDF data:
d_pred <- d_cov[, pred_WDF(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

## Which wood-decaying fungi should be exported?

## Rename WDF to scientific names:
names(d_pred)[names(d_pred) == "amylap"] <- "A. lapponica"
names(d_pred)[names(d_pred) == "fomros"] <- "F. rosea"
names(d_pred)[names(d_pred) == "phechr"] <- "P. chrysoloma"
names(d_pred)[names(d_pred) == "phefer"] <- "P. ferrugineofuscus"
names(d_pred)[names(d_pred) == "phenig"] <- "P. nigrolimitatus" 
names(d_pred)[names(d_pred) == "phlcen"] <- "P. centrifuga"

## Combine selected wood-decaying fungi with original Heureka data:
out_Mair <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  d_pred[, colnames(d_pred) %in% WDF, with = FALSE])

## -------------------------------END-------------------------------------------
