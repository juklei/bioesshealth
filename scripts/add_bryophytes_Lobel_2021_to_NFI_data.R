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
## Last edit: 2022-05-30
##
## Author: Julian Klein

rm(d_cov, mp, min_age, md_orig)

## 1. Define model parameters, minimum age thresholds and mean&SD --------------

## The model parameters:
mp <- structure(c(-2.080223068, 0, 0, 0, 0, 0, -0.092786982, 0, 0, 
            -0.33890534, 0, 0, -0.11790558, 0, 0, 0, -0.202548969, 0, 0.1457314, 
            0, 0.381882839, 0, 0, 0.171962811, 0, 0, 0, 0.729944728, -0.928139554, 
            -5.035868716, 0, 0, 0, 0, 0, 0, 0, 0, -0.60039587, 0, 0, -0.303234924, 
            0, 0, 0, -1.187834625, 0, 0.659167492, 0, 0.254449594, 0, -1.129766628, 
            1.57448366, 0, 0, 0, 0.851018568, -0.91175481, -2.596785699, 
            0, 0, 0, 0, 0, -0.100553926, -0.103363058, 0, -0.274650953, 0, 
            0, -0.069723073, 0, 0, 0, -1.066855607, 0, 0, 0, 0.592127262, 
            0, 0.336719936, -0.532913935, 0.210987936, 0, -0.155115811, 0.364055169, 
            0.079144743, -4.700415087, 0, 0, 0, 0, 0, 0, 0, -0.921420382, 
            0, 0, 0, 0, 0, 0, 0, -2.187527503, 0, 0, 0, 0, 0, 0.18604747, 
            -0.76949481, 0, 0, 0.647899673, 0, -2.594247144, -3.837788685, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1.170261109, 0, 
            0, 0, 0.391959232, 0, 0, 0, 0, 0, 0.179384387, 0.317267784, -2.388544954, 
            -4.765032748, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.705233901, 
            0, 0, 0, 0, 0, 0.193867027, -1.099310661, 0, 0, 0.288736486, 
            0, -2.024304899, -4.322885573, 0, -0.513153975, 0, 0, 0, -0.446045285, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, -0.759767863, 0, 1.105051391, 0, 1.335773378, 
            0, 0, 0.672819381, 0, 0, 0, 0, -0.741085174, -5.616783009, -0.114654884, 
            0, 0, 0, 0, 0, 0, 0, 0, 0.230235163, 0, 0, 0, 0, 0.58976216, 
            0, 0.796891677, 0, 0, 0.480681756, 0, 0, 1.548096575, 0.443654206, 
            0, 0.251709346, 0, 0.367642071, -3.803990223, 0, 0, 0, 0, 0, 
            0, 0, 0, -0.247862729, 0, 0, -0.152583534, 0, 0, 0, -0.217491776, 
            0, 0, 0, 0, 0, 0, 0.053030585, 0, 0, 0, 0.858318082, -0.3149508, 
            -9.325115564, 0, 0, 0, 0, 0, -0.695054399, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, -0.583430791, 0, 0, 0, 0.692969794, 0, 0, -1.724368945, 
            0, 0, 0, 0, -3.981789513, -4.293002052, 0, 0, 0, 0, 0.284008079, 
            -0.307598164, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.429785584, 0, 0, 
            0, 0.366912276, 0, 0, -0.726609995, 0, 0, 0.199852852, 0, -1.429991746, 
            -2.187804774, 0, 0.313782355, 0, -0.108763131, 0, -0.16312285, 
            0, 0, -0.145245039, 0, 0, 0, 0, 0, 0, -0.74790618, 0.270834009, 
            0.238238038, 0.465783745, 0.208858424, 0, 0, 0.130505394, 0, 
            0, 0, 0, 0.733445944, -4.77896748, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0.502597143, 0, -0.980042974, 0, 0, 0, 0.42063688, 
            0, 0, -1.951776932, 0, 0, 0.338897811, 0, -2.088872725, -6.5701053, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.021430837, -4.209886198, 
            0, 0.684219118, 0, 0, 0, 0, -0.846413714, 0, 0, -0.272615855, 
            0, 3.845499079, -3.605418906, 0, 0, 0, 0, 0, -0.336275647, 0, 
            0, -0.793916711, 0, 0, 0, 0, 0, 0, -0.700998683, 0, 0, 0, 0.391345874, 
            0, 0.124218889, -1.430842869, 0, 0, 0.219109073, 0, -1.571419853, 
            -3.66014365, 0, 0, 0, 0, 0, -0.151165339, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, -0.449156379, 0, 0, 0, 0.619704911, 0, 0.154812272, -0.483349155, 
            0, 0, 0.134691957, 0.336327061, -1.59887445, -4.527590075, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, -0.128338175, -0.158938691, 0, 0, 
            -0.368673966, -0.546925397, 0, 0, 0, 0.389081803, 0, 0, -0.668859886, 
            0, 0, 0.077790003, 0.767897406, -2.557468889, -3.955217771, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.393637502, -0.796971101, 
            0, 0, 0, 0.444515497, 0, 0, -0.508295947, 0, 0, 0, 0.389739572, 
            -3.19723223, -5.158371996, 0, 0, 0, 0, 0, 0, 0, 0, -0.771756189, 
            0, 0, -0.185615376, 0, 0, 0, 0, 0, 0.778038233, 0, 0, 0, 0, 1.515489812, 
            0, 0.152427095, 0, 0.568058708, -0.305782271, -2.769029902, 0, 
            0, -0.272766413, 0, 0, -0.208060608, 0, 0.810027351, 0.155145208, 
            0, 0, 0, -0.714005182, 0, 0, -1.610940373, 0, 0.506077689, 0, 
            0.630985827, 0, 0, 0.37497793, 0, 0, 0, 0.290647741, 3.202214698, 
            -4.820224285, 0, 0, 0, 0, 0, -0.188607297, 0, 0, 0, 0, 0, -0.32616941, 
            0, 0, 0.704256452, -2.31255156, 0, 0, 0, 0.729864378, 0, 0, 0.110445056, 
            0, 0, 0.411995337, 0.936838876, 2.489604342, -3.777572535, 0, 
            0, 0, 0, 0, 0, 0, 0, -0.847827598, 0, 0, 0, 0, 0, 0, -1.006658291, 
            0, 0, 0, 0.305191611, 0, 0.233815096, -0.047708859, 0, 0, 0.417680453, 
            0.212166083, -1.865174802, -3.948984166, 0, 0, 0, 0, 0, 0, 0, 
            0, -0.766909146, 0, 0, 0, 0, 0, 0, -0.983311356, 0, 0, 0, 0.289040738, 
            -0.477521526, 0.231437021, -0.134552955, 0, 0, 0.365166837, 0.249956574, 
            -1.806018659, -5.636717786, 0, 0, 0, 0, 0, 0, 0, 0, -0.703195632, 
            0, 0, 0, 0, 0, 0, -1.300678657, 0, 0, 0, -0.307160706, 0, 0, 
            -1.327589438, 0, 0, 0.285609683, 0.217512679, -3.623276749, -5.740965819, 
            0, 0, 0, 0, 0, 0, 0, 0, -0.603670773, 0, 0, 0, 0, 0, 0, -1.295349372, 
            0, 0, 0, -0.336136748, -0.379417816, 0, -1.362232137, 0, 0, 0.238509486, 
            0.246660684, -3.584342937), .Dim = c(29L, 25L), .Dimnames = list(
              c("(Intercept)", "I(z.OakBeech^2)", "I(z.ald_max * z.gran_max)", 
                "I(z.ald_max * z.tall_max)", "I(z.bjork_max^2)", "I(z.gran_max * z.swe_twi_wetness)", 
                "I(z.gran_max^2)", "I(z.lov_max^2)", "I(z.precsumamjjason * z.tempOctNov)", 
                "I(z.precsumamjjason^2)", "I(z.slope * z.swe_twi_wetness)", 
                "I(z.swe_twi_wetness^2)", "I(z.tall_max^2)", "I(z.tempOctNov * z.ald_max)", 
                "I(z.tempOctNov * z.gran_max)", "I(z.tempOctNov * z.precsumamjjason)", 
                "I(z.tempOctNov^2)", "z.OakBeech", "z.ald_max", "z.bjork_max", 
                "z.gran_max", "z.log.DistWaterCourse", "z.lov_max", "z.precsumamjjason", 
                "z.slope", "z.slope.asp", "z.swe_twi_wetness", "z.tall_max", 
                "z.tempOctNov"), c("Ana.hell", "Ana.mich", "Bux.vir", "Cal.hald", 
                                   "Cal.suec", "Camp.som", "Ceph.cat", "Dicr.denu", "Dicr.flag", 
                                   "Dicr.frag", "Geo.grav", "Herz.sel", "Herz.tur", "Jam.aut", 
                                   "Jung.lei", "Lo.asc", "Lo.cil", "Lo.loflo", "My.tay", "Now.cur", 
                                   "Od.denu", "Sca.api", "Sca.api.DistWater", "Sca.car", "Sca.car.DistWater"
                )))

## Minimum ages from table X:
min_age <- c(Ana.hell = 57, Ana.mich = 65.525, Bux.vir = 50, Cal.hald = 20.2, 
             Cal.suec = 42.65, Camp.som = 18.85, Ceph.cat = 60, Dicr.denu = 44.975, 
             Dicr.flag = 51, Dicr.frag = 58, Geo.grav = 35.55, Herz.sel = 27.75, 
             Herz.tur = 19.95, Jam.aut = 51, Jung.lei = 39.65, Lo.asc = 39.6, 
             Lo.cil = 51.75, Lo.loflo = 50, My.tay = 70.3, Now.cur = 47, Od.denu = 51.75, 
             Sca.api = 38, Sca.api.DistWater = 38, Sca.car = 33.3, Sca.car.DistWater = 33.3
)

## The mean and SDs from the original data:
md_orig <- structure(c(778.6012987, 823.2160753, 20.42284569, 55.50751564, 
                       87.38287686, 34.12958317, 43.27365843, 30.90925357, 136.3485861, 
                       109.041934, 6.051026347, 1.30855079, 26.55221554, 34.5586534, 
                       6.813464708, 1.62944289, 507.1285465, 91.08155883, 8.665114595, 
                       10.07440231, -0.014846888, 9.617898524, 0.696204085, 1.558777522, 
                       113.7817858, 71.63983012, 3.279102079, 2.344672694), 
                     .Dim = c(2L, 14L), 
                     .Dimnames = list(c("Mean", "SD"), 
                                      c("DistWaterCourse", "OakBeech", "ald_max", 
                                        "bjork_max", "gran_max", "log.DistWaterCourse", 
                                        "lov_max", "meantempmamj", 
                                        "precsumamjjason", "slope", "slope.asp", 
                                        "swe_twi_wetness", "tall_max", "tempOctNov")))

## 2. Load Heureka data --------------------------------------------------------

if(climate == "RCP0") file <- paste0(dir_HK, file_RCP0)
if(climate == "RCP45") file <- paste0(dir_HK, file_RCP45)
if(climate == "RCP85") file <- paste0(dir_HK, file_RCP85)

d_cov <- fread(file,  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumePine", "VolumeSpruce", "VolumeAspen",
                          "VolumeBeech", "VolumeOak", "VolumeBirch", "VolumeOtherBroadLeaf",
                          "VolumeSouthernBroadleaf"),
               sep = ";", blank.lines.skip = TRUE)

## Select random share of NFI plots:
d_unique <- unique(d_cov$Description)
set.seed(1)
select <- sample(d_unique, NFI_share*length(d_unique))
d_cov <- d_cov[d_cov$Description %in% select, ]

## 3. Add climate data to d_cov ------------------------------------------------

## Climate data related to the NFI plots:
NFI <- NFI_BP
NFI[is.na(NFI$slope),] ## Identify why some are NA!!! What is the effect of this on the entire prediciton?
## Most species have at least one of the NA variables as covariate

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

## 4. Add species to d_cov according to model formulas -------------------------

## Calculate missing columns:
d_cov$VolDecid <- rowSums(d_cov[, c("VolumeAspen", "VolumeBeech", "VolumeOak", 
                                    "VolumeBirch", "VolumeOtherBroadLeaf", 
                                    "VolumeSouthernBroadleaf")])
d_cov$VolOakBeech <- rowSums(d_cov[, c("VolumeBeech", "VolumeOak")])
d_cov$log.DistWaterCourse <- log(d_cov$DistWaterCourse) ## DistWaterCourse != 0

## Standardise Heureka variables with mean and SDs from original model data:
scale <- function(x, covariate){
  (d_cov[, ..x] - md_orig["Mean", covariate])/md_orig["SD", covariate]
}

d_cov$Age_sc <- scale("Age", "ald_max")
d_cov$VS_sc <- scale("VolumeSpruce", "gran_max")
d_cov$VP_sc <- scale("VolumePine", "tall_max")
d_cov$VB_sc <- scale("VolumeBirch", "bjork_max")
d_cov$VD_sc <- scale("VolDecid", "lov_max")
d_cov$VOB_sc <- scale("VolOakBeech", "OakBeech")
d_cov$tMAMJ_sc <- scale("meantempmamj", "meantempmamj")
d_cov$tON_sc <- scale("tempOctNov", "tempOctNov")
d_cov$precip_sc <- scale("precsumamjjason", "precsumamjjason")
d_cov$slope_sc <- scale("slope", "slope")
d_cov$slope.asp_sc <- scale("slope.asp", "slope.asp")
d_cov$wet_sc <- scale("swe_twi_wetness", "swe_twi_wetness")
d_cov$DWC_sc <- scale("DistWaterCourse", "DistWaterCourse")
d_cov$log.DWC_sc <- scale("log.DistWaterCourse", "log.DistWaterCourse")

## Check if min_age and mp have same name order:
if(!all(dimnames(mp)[[2]] == names(min_age))) stop("species names not matching")

## Define prediction function:
pred_bry <- function(x){
  AgC <- ifelse(x$Age > min_age, 1, 0 )
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before! How to know which controlCategor & Alternative contains a retention patch? Need to indicate this in future HEureka simulations??
  bry <- inv.logit(mp["(Intercept)", ] + 
                     mp["z.ald_max", ]*x$Age_sc + 
                     mp["z.gran_max", ]*x$VS_sc +
                     mp["z.tall_max", ]*x$VP_sc +
                     mp["z.bjork_max", ]*x$VB_sc +
                     mp["z.lov_max", ]*x$VD_sc +
                     mp["z.OakBeech", ]*x$VOB_sc +
                     ## meantempmamj not present in model table
                     mp["z.tempOctNov", ]*x$tON_sc +
                     mp["z.precsumamjjason", ]*x$precip_sc +
                     mp["z.slope", ]*x$slope_sc +
                     mp["z.slope.asp", ]*x$slope.asp_sc +
                     mp["z.swe_twi_wetness", ]*x$wet_sc +
                     ## DistWaterCourse not present in model table
                     mp["z.log.DistWaterCourse", ]*x$log.DWC_sc +
                     mp["I(z.gran_max^2)", ]*x$VS_sc^2 +
                     mp["I(z.tall_max^2)", ]*x$VP_sc^2 +
                     mp["I(z.bjork_max^2)", ]*x$VB_sc^2 +
                     mp["I(z.lov_max^2)", ]*x$VD_sc^2 +
                     mp["I(z.OakBeech^2)", ]*x$VOB_sc^2 +
                     mp["I(z.tempOctNov^2)", ]*x$tON_sc^2 +
                     mp["I(z.precsumamjjason^2)", ]*x$precip_sc^2 +
                     mp["I(z.swe_twi_wetness^2)", ]*x$wet_sc^2 +
                     mp["I(z.ald_max * z.gran_max)", ]*x$Age_sc*x$VS_sc +
                     mp["I(z.ald_max * z.tall_max)", ]*x$Age_sc*x$VP_sc +
                     mp["I(z.tempOctNov * z.ald_max)", ]*x$Age_sc*x$tON_sc +
                     mp["I(z.gran_max * z.swe_twi_wetness)", ]*x$VS_sc*x$wet_sc +
                     mp["I(z.tempOctNov * z.gran_max)", ]*x$VS_sc*x$tON_sc +
                     mp["I(z.tempOctNov * z.precsumamjjason)", ]*x$precip_sc*x$tON_sc + 
                     mp["I(z.precsumamjjason * z.tempOctNov)", ]*x$precip_sc*x$tON_sc + 
                     mp["I(z.slope * z.swe_twi_wetness)", ]*x$wet_sc*x$slope_sc
                   )
  as.list(AgC*bry) ## Multiply also with RpC once added above!
}

## Predict Bryophyte data:
d_pred <- d_cov[, pred_bry(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

## Combine selected wood-decaying fungi with original Heureka data:
out_Lobel <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                   d_pred[, colnames(d_pred) %in% BP, with = FALSE])
  
## -------------------------------END-------------------------------------------
