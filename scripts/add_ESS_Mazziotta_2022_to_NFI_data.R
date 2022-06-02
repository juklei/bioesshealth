## This script adds Ecosystem services to the NFI data with oprediction per
## NFI plot, ControlCategoryName, AlternativeNo, and period
## 
## Models by:
## Mnazziotta et al. (YYYY). 
## Title.
## Journal. doi
##
## First edit: 2021-12-22 
## Last edit: 2022-06-02
##
## Author: Julian Klein

rm(d_cov, mp, md_orig)

## 1. Define model parameters, minimum age thresholds and mean&SD --------------

## The model parameters:
mp <- structure(c(0.3915, -0.09461, -0.04167, -0.0691, -0.2299, -0.1951, 
                  0.1213, 0, 0.1052, -0.1137, 0, -0.1004, 0.04323, -1.611, 9.24, 
                  0.05609, 0, 0.0212, -0.03905, -0.2806, -0.4698, 0, -0.0819, 0.2445, 
                  0, 0.04714, -0.05291, 0, -1.491, 16.2, -0.0402881, 0.010049492, 
                  0.1054936, 0, -0.185873, -0.018254734, -0.0445341, 0.0306308, 
                  0.0462491, 0.008906817, -0.012447399, -0.0281637, 0, 2.4235, 
                  0, 0.329, 0, -0.017, -0.0386, -0.3154, 0.104, 1.03, -0.22, 0.84, 
                  0, 0.361, -0.481, 0.252, 2.77, 0), 
                .Dim = c(15L, 4L), 
                .Dimnames = list(c("stand age", "stand age^2", "soil moisture", 
                                   "soil moisture^2", "peat soil (Y/N)", 
                                   "spruce biomass", "pine biomass", "birch biomass", 
                                   "spruce biomass * stand age", 
                                   "pine biomass * stand age", 
                                   "birch biomass * stand age", "temperature sum", 
                                   "precipitation sum", "trak-level intercept", "phi"), 
                    c("Bilberry.beta", "Wildfood", "Richness", "Bilberry.bern")))

## The mean and SDs from the original data:
md_orig <- structure(list(region2 = 1:5, 
                          Tsum.mean = c(1057.501155, 1316.179127, 1327.671591, 1330.158367, 1660.356502), 
                          Psum.mean = c(218.2355658, 248.4226499, 246.8727273, 251.8106122, 282.7982063), 
                          age.mean = c(74.84757506, 70.48260548, 63.28977273, 55.35836735, 64.18609865), 
                          age2.mean = c(5602.159492, 4967.797675, 4005.595332, 3064.548835, 4119.855261),
                          Smoist.mean = c(2.269141531, 2.318013343, 2.284090909, 2.348571429, 2.242630385), 
                          Smoist2.mean = c(5.149003289, 5.373185859, 5.217071281, 5.515787755, 5.029391046), 
                          peat.mean = c(0.020881671, 0.04744255, 0.070454545, 0.137142857, 0.095238095),
                          spruce.mean = c(2.502464139, 3.852246268, 4.16077216, 5.763048856, 5.146988168), 
                          pine.mean = c(2.463811265, 2.907608639, 4.35720254, 4.040140732, 3.63559998),
                          birch.mean = c(0.68804175, 0.904666388, 0.80701387, 1.122884998, 1.381757146), 
                          agexspruce.mean = c(187.3033725, 271.5163539, 263.3343244, 319.0329756, 330.3650903), 
                          agexpine.mean = c(184.4102986, 204.9358326, 275.7663585, 223.6555948, 233.354979), 
                          agexbirch.mean = c(51.49825655, 63.76324414, 51.07572439, 62.16108022, 88.68960052), 
                          Tsum.sd = c(393.3640826, 487.000218, 520.456125, 524.802653, 492.2743236),
                          Psum.sd = c(80.15450896, 94.06864974, 99.98392217, 99.90383473, 113.4506955), 
                          age.sd = c(53.84906762, 51.19406318, 44.31171397, 36.34978577, 39.5410057),
                          age2.sd = c(2899.722083, 2620.832105, 1963.527995, 1321.306925, 1563.491132),
                          Smoist.sd = c(0.580255722, 0.588321227, 0.587097983, 0.68965275, 0.641444656), 
                          Smoist2.sd = c(0.336696703, 0.346121866, 0.344684042, 0.475620915, 0.411451247),
                          peat.sd = c(0.143154372, 0.21266236, 0.256057041, 0.344138595, 0.293876907),
                          spruce.sd = c(4.408670904, 5.90637572, 6.12872267, 7.413770373, 8.988492677), 
                          pine.sd = c(3.255754982, 4.033837243, 4.698488976, 5.326598686, 4.906052206), 
                          birch.sd = c(1.384191307, 1.723282918, 1.676154815, 2.149370023, 2.755663074), 
                          agexspruce.sd = c(237.4028176, 302.3713717, 271.574206, 269.4889648, 355.4140402), 
                          agexpine.sd = c(175.3193702, 206.5085186, 208.1980996, 193.6207211, 193.9902383), 
                          agexbirch.sd = c(74.5374113, 88.22185456, 74.27329272, 78.12913989, 108.9616893)), 
                     class = "data.frame", row.names = c(NA, -5L))

## Tree densities in kg/m3: ## For now estimates from www.wood-database.com
dens_spruce <- 405
dens_pine <- 550
dens_birch <- 625

## 2. Load Heureka data --------------------------------------------------------

if(climate == "RCP0") file <- paste0(dir_HK, file_RCP0)
if(climate == "RCP45") file <- paste0(dir_HK, file_RCP45)
if(climate == "RCP85") file <- paste0(dir_HK, file_RCP85)

d_cov <- fread(file,  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumePine", "VolumeSpruce", "VolumeBirch",
                          "Richness", "BilberryCover", "Wildfood"), ## Only for testing !!!
               sep = ";", blank.lines.skip = TRUE)

## Select random share of NFI plots:
d_unique <- unique(d_cov$Description)
set.seed(1)
select <- sample(d_unique, NFI_share*length(d_unique))
d_cov <- d_cov[d_cov$Description %in% select, ]

## 3. Calculate biomass and add climate data to d_cov --------------------------

NFI <- NFI_ESS

## The next three lines will become obsolete once biomass is available in Heureka
d_cov$bmSpruce <- d_cov$VolumeSpruce*dens_spruce/10000 ## Mazziotta et al. used kg/m2
d_cov$bmPine <- d_cov$VolumePine*dens_pine/10000
d_cov$bmBirch <- d_cov$VolumeBirch*dens_birch/10000

## Add period 19 & 20 to the data: Discuss this step with Tord !!!!!!!!!!!!!!!!!
## For now I use period 1 for period 1 and period 18 for 19 & 20
## Is period 0 = 2010?
T1 <- NFI[NFI$PERIOD == 18, ]; T1$PERIOD <- 19
T2 <- NFI[NFI$PERIOD == 18, ]; T2$PERIOD <- 20
NFI <- rbind(NFI, T1, T2)

## Merge Heureka with NFI data:
colnames_clim <- paste0(c("psum_", "tsum_"), 
                        ifelse(climate == "RCP0", "CC", climate))
d_cov <- merge(d_cov, 
               NFI[, c("Description", "PERIOD", "SoilMoist", "SoilMoist.cont",
                       "region2", "peat", "join_eps", colnames_clim)],
               all.x = TRUE, 
               by.x = c("Description", "period"),
               by.y = c("Description", "PERIOD"))
colnames(d_cov)[(length(d_cov)-1):length(d_cov)] <- c("psum", "tsum")

## 4. Add ESS to d_cov according to model formulas -------------------------

## Add means and SDs to data for standardisation below:
d_cov <- merge(d_cov, md_orig, by = "region2")

## Define prediction function: ## How does Phi come in here ????? 
## It is a precision parameter and therefor obsolete for prediction ???
## e.g.: https://stats.stackexchange.com/questions/524960/beta-regression-how-to-interpret-the-p-value-of-the-phi-coefficient#:~:text=In%20beta%20regression%20you%20assume%20that%20the%20dependent,-%20just%20like%20the%20mean%20%CE%BC%20as%20well.
pred_ESS <- function(x){
  ESS <- mp["trak-level intercept", ] +
          mp["temperature sum", ]*(x$tsum-x$Tsum.mean)/x$Tsum.sd +
          mp["precipitation sum", ]*(x$psum-x$Psum.mean)/x$Psum.sd +
          mp["stand age", ]*(x$Age-x$age.mean)/x$age.sd +
          mp["soil moisture", ]*(x$SoilMoist-x$Smoist.mean)/x$Smoist.sd + ## Cont or integer?
          mp["peat soil (Y/N)", ]*(x$peat-x$peat.mean)/x$peat.sd + ## Standardising a categorical variable is strange to me?
          mp["spruce biomass", ]*(x$bmSpruce-x$spruce.mean)/x$spruce.sd +
          mp["pine biomass", ]*(x$bmPine-x$pine.mean)/x$pine.sd +
          mp["birch biomass", ]*(x$bmBirch-x$birch.mean)/x$birch.sd +
          mp["stand age^2", ]*(x$Age^2-x$age2.mean)/x$age2.sd +
          mp["soil moisture^2", ]*(x$SoilMoist^2-x$Smoist2.mean)/x$Smoist2.sd + ## Cont or integer?
          mp["spruce biomass * stand age", ]*(x$bmSpruce*x$Age-x$agexspruce.mean)/x$agexspruce.sd +
          mp["pine biomass * stand age", ]*(x$bmPine*x$Age-x$agexpine.mean)/x$agexpine.sd +
          mp["birch biomass * stand age", ]*(x$bmBirch*x$Age-x$agexbirch.mean)/x$agexbirch.sd
  as.list(c(inv.logit(ESS[c(1:2, 4)]), exp(ESS[3]))) ## Different link functions per column!
}

## Predict WDF data:
d_pred <- d_cov[, pred_ESS(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

## Which ESS should be exported?
out_Mazziotta <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                       d_pred[, colnames(d_pred) %in% ESS, with = FALSE])

## -------------------------------END-------------------------------------------

out <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName",
                       "Richness", "BilberryCover", "Wildfood")], ## Only for testing !!!
             d_pred[,-1])


colnames(out)[c(5, 7)] <- c("Richness_old", "Wildfood_old")

sel <- sample(nrow(out), 1000)
ggplot(out[sel, ]) + geom_point(aes(Richness_old, Richness)) + geom_abline(slope = 1)
ggplot(out[sel, ]) + geom_point(aes(Wildfood_old, Wildfood)) + geom_abline(slope = 1)
ggplot(out[sel, ]) + geom_point(aes(BilberryCover, Bilberry.beta))  + geom_abline(slope = 1)

## Wood densities seem ok and the biomass variables are in kg/m2: Now we need actually 
## used wood densities or preferrably data directly from Heureka: 
## "Biomass Above Ground All Species" https://www.heurekaslu.se/wiki/Biomass_Results
