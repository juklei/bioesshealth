## This script adds Ecosystem services to the NFI data with oprediction per
## NFI plot, ControlCategoryName, AlternativeNo, and period
## 
## Models by:
## Mnazziotta et al. (YYYY). 
## Title.
## Journal. doi
##
## First edit: 2021-12-22 
## Last edit: 
##
## Author: Julian Klein

## 1. Clear environment, load libraries, and data ------------------------------

rm(list = ls())
dir.create("clean")

library(data.table)
library(boot)
library(ggplot2)

## Directory to the data coming from Heureka:
dir <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook"

## Define climate:
climate <- "RCP0"
# climate <- "RCP4.5"
# climate <- "RCP8.5"

## Climate data related to the NFI plots:
NFI <- read.csv("data/clim.new.csv") 

## Model parameters for ESS:
model_params <- read.csv("data/ESS.mean.myposteriorsample1_df.beta - Copy.csv")

## Data with mean and SD from the original data:
model_data <- read.csv("data/ESS.R.mean.sd.csv")
  
## -----------------------------------------------------------------------------

if(climate == "RCP0"){file <- "MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"}
if(climate == "RCP4.5"){file <- "MultiForestResults210710_20Periods_InclIntensivve_RCP45.csv"}
if(climate == "RCP8.5"){file <- "MultiForestResults210830_20Periods_InclIntensivve_RCP8_5.csv"}

d_cov <- fread(paste0(dir, "/data/", file),  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumePine", "VolumeSpruce", "VolumeBirch",
                          "Richness", "BilberryCover", "Wildfood"), ## Only for testing !!!
               sep = ";", blank.lines.skip = TRUE)

d_cov <- d_cov[d_cov$period == 0,] ## REDUCED SIZE FOR TRIAL !!!!!!!!!!!!!!!!!!!

## 2. Add climate data to d_cov ------------------------------------------------

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

## 3. Add ESS to d_cov according to model formulas -------------------------

## Make covariat*ESS matrix with estimates as content:
model_params[is.na(model_params)] <- 0 ## unused covariates are 0
mp <- as.matrix(model_params[, 3:6])
dimnames(mp)[[1]] <- as.character(model_params$Covariate)

## Add means and SDs to data for standardisation below:
d_cov <- merge(d_cov, model_data, by = "region2")

## Define prediction function: ## How does Phi come in here ????? 
## It is a precision parameter and therefor eobsolete for prediction ???
## e.g.: https://stats.stackexchange.com/questions/524960/beta-regression-how-to-interpret-the-p-value-of-the-phi-coefficient#:~:text=In%20beta%20regression%20you%20assume%20that%20the%20dependent,-%20just%20like%20the%20mean%20%CE%BC%20as%20well.
pred_ESS <- function(x){
  ESS <- mp["trak-level intercept", ] +
          mp["temperature sum", ]*(x$tsum-x$Tsum.mean)/x$Tsum.sd +
          mp["precipitation sum", ]*(x$psum-x$Psum.mean)/x$Psum.sd +
          mp["stand age", ]*(x$Age-x$age.mean)/x$age.sd +
          mp["soil moisture", ]*(x$SoilMoist-x$Smoist.mean)/x$Smoist.sd + ## Cont or integer?
          mp["peat soil (Y/N)", ]*(x$peat-x$peat.mean)/x$peat.sd + ## Standardising a categorical variable is strange to me?
          mp["spruce biomass", ]*(x$VolumeSpruce-x$spruce.mean)/x$spruce.sd +
          mp["pine biomass", ]*(x$VolumePine-x$pine.mean)/x$pine.sd +
          mp["birch biomass", ]*(x$VolumeBirch-x$birch.mean)/x$birch.sd +
          mp["stand age^2", ]*(x$Age^2-x$age2.mean)/x$age2.sd +
          mp["soil moisture^2", ]*(x$SoilMoist^2-x$Smoist2.mean)/x$Smoist2.sd + ## Cont or integer?
          mp["spruce biomass * stand age", ]*(x$VolumeSpruce*x$Age-x$agexspruce.mean)/x$agexspruce.sd +
          mp["pine biomass * stand age", ]*(x$VolumePine*x$Age-x$agexpine.mean)/x$agexpine.sd +
          mp["birch biomass * stand age", ]*(x$VolumeBirch*x$Age-x$agexbirch.mean)/x$agexbirch.sd
  as.list(c(inv.logit(ESS[c(1:2, 4)]), exp(ESS[3]))) ## Different link functions per column!
}

## Predict WDF data:
d_pred <- d_cov[, pred_ESS(.SD), by = 1:nrow(d_cov)]
  
## 5. Create output data -------------------------------------------------------

out <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName",
                       "Richness", "BilberryCover", "Wildfood")], ## Only for testing !!!
                   d_pred[,-1])

fwrite(out, paste0("clean/MFO_ESS_", climate, ".csv"), row.names = FALSE)
  
## TEMP ----------------- TEMP ----------------------- TEMP --------------------

colnames(out)[c(5, 7)] <- c("Richness_old", "Wildfood_old")

sel <- sample(nrow(out), 1000)
ggplot(out[sel, ]) + geom_point(aes(Richness_old, Richness)) + geom_abline(slope = 1)
ggplot(out[sel, ]) + geom_point(aes(Wildfood_old, Wildfood)) + geom_abline(slope = 1)
ggplot(out[sel, ]) + geom_point(aes(BilberryCover, Bilberry.beta))  + geom_abline(slope = 1)

## Many predictions too high:
nrow(out[out$Richness > max(out$Richness_old),])/nrow(out)

## The means and SDs delivered are very different from when I calculate them from the
## Heureka data:
mean(d_cov$agexspruce.mean)/mean(d_cov$VolumeSpruce*d_cov$Age)
mean(d_cov$agexspruce.sd)/sd(d_cov$VolumeSpruce*d_cov$Age)

mean(d_cov$spruce.mean)/mean(d_cov$VolumeSpruce)
mean(d_cov$spruce.sd)/sd(d_cov$VolumeSpruce)

## The problem seems that biomass in Mazziotta et al. != VolSpruce !!!!!!!!!!!!!
## How do I calculate biomass? By using wood densities? Directly from Heureka?
## Is it measured by ha or by m2?????

## -------------------------------END-------------------------------------------
