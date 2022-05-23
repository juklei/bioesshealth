## This script adds wood-decaying fungi models to the NFI data with one occupancy
## per NFI plot, ControlCategoryName, AlternativeNo, and period
##
## Thee Moor et al. model is independent of climate so we only add it to RCP0
##
## All model data wood-decaying fungi comes from the SI of the article below:
## 
## Models by:
## Moor H, Nordén J, Penttilä R, Siitonen J, Snäll T. 
## Long-term effects of colonization-extinction dynamics of generalist versus 
## specialist wood-decaying fungi. 
## J Ecol. 2021;109:491-503. https://doi.org/10.1111/1365-2745.13526
##
## First edit: 2022-05-19 
## Last edit: 
##
## Author: Julian Klein


### to do:

## Use Fig. S4 to see which models are used for which forest age
## Use table S4 to standardise the variables coming from HEureka:
## How to handle the minimum deadwood diameters? Use mean?

## Formula to calculate occupancy at every timestep:
#p_occ[periodN] <- (1-p_occ[periodN-1])*p_col[periodN]+p_occ[periodN-1]*(1-p_ext[periodN])

## Use table S5 to define occupancy at period = 0
## Use table S6 to define extinction / colonization probability and with the 
## formula above the occupancy at timestep period = 1:20
## Use inv.logit() to calculate p_ext & p_occ and inv.cloglog for p_col


## Question: IN the stakeholder manuscript it was downed dead wood volume spruce, 
## here it is just dead wood volume: Clarify by reading the main text!



## Now same procedure as with the other scripts:

## 1) Create 3 model parameter matrices, one for occupancy t0, one for p_col and one for p_ext
## 2) Standardize the Heureka variables
## 3) make function with 0:1 values for every part of the calculation and multiply

## Check if the retention patch division with 10 is also necessary here






## 1. Clear environment, load libraries, and data ------------------------------

rm(list = ls())

library(data.table)
library(boot)

dir <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook"
dir.create("clean")

## Model parameters for wood-decaying fungi (WDF):
params <- read.csv("data/Moor_WDF_model_parameters.csv")

## The original model data with means and SDs of covariates:
mean_sd <- read.csv("data/Moor_WDF_mean&SD.csv", sep = ",")

## Which species use the model mean and SD for diameter >= 5cm?
dbh_spec <- c("T. abietinum", "G. sepiarium", "P. viticola")

d_cov <- fread(paste0(dir, "/data/MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"),  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumeSpruce", "DeadWoodVolumeSpruce"), 
                  sep = ";", blank.lines.skip = TRUE)

## Select random 0.1% of NFI plots:
d_unique <- unique(d_cov$Description)
select <- sample(d_unique, 0.001*length(d_unique))
d_cov <- d_cov[d_cov$Description %in% select, ]

## 2. Arrange model params for WDF ---------------------------------------------



## Add Inf or -Inf were probabilities are set to 0 or 1, e.g. logit(1)=Inf:
params[params$Model.type == "Deterministic: Pcol = 0, Pext = 1" & params$probability == "colonization",
       c("Intercept", "Dead.wood.volume", "Stand.age.at.T2", "Spruce.volume")] <- -Inf
params[params$Model.type == "Deterministic: Pcol = 0, Pext = 1" & params$probability == "extinction",
       c("Intercept", "Dead.wood.volume", "Stand.age.at.T2", "Spruce.volume")] <- Inf
params[params$Model.type == "Deterministic: Pocc = 0" & params$probability == "start",
       c("Intercept", "Dead.wood.volume", "Stand.age.at.T2", "Spruce.volume")] <- -Inf

## Remove the model category column. After adding the deterministic parts,
## it is not needed anymore, but would result in multiple model parameters per
## species when creating the model table below:
params$Model.type <- NULL

## Restructure the data do have species in long format:
params <- as.data.table(params)
mp <- melt(params, id.vars = c("Species", "Stand.age.group.min", "Stand.age.group.max", "probability"))
mp <- dcast(mp, formula = Stand.age.group.min + Stand.age.group.max + probability + variable ~ Species)

## All NA's are 0, e.g. all parameters that are not in the model are 0:
mp[is.na(mp)] <- 0

## 3. Predict initial state ----------------------------------------------------

## Use only inital state covariates:
dc_init <- d_cov[d_cov$ControlCategoryName == "Initial state", ]

## Reduce mp and mean_SD to start occupancy parameters:
mp_init <- mp[mp$probability == "start", ]
msd_init <- as.data.table(mean_sd[mean_sd$Model.component == "colonization", ])

pred_init <- function(x){
  ## Chose model and mean_SD by forest age of Heureka row x:
  mp_T <- mp_init[mp_init$Stand.age.group.min <= round(x$Age) & mp_init$Stand.age.group.max >= round(x$Age), ]
  msd_T <- msd_init[msd_init$Stand.age.group.min <= round(x$Age) & msd_init$Stand.age.group.max >= round(x$Age), ]
  ## Create matrices with variables as row names to have less code below:
  mp_T <- as.matrix(mp_T[, -c(1:3)], rownames = "variable")
  msd5 <- as.matrix(msd_T[msd_T$Minimum.Diameter == "5", -c(2:5)], rownames = "Variable")
  msd10 <- as.matrix(msd_T[msd_T$Minimum.Diameter == "10", -c(2:5)], rownames = "Variable")
  ## Make predictions for the two different diameter classes:
  WDF5 <- mp_T["Intercept", dbh_spec] + 
          mp_T["Dead.wood.volume", dbh_spec]*
          (log(max(x$DeadWoodVolumeSpruce, 1e-12)) - msd5[1, 1])/msd5[1, 2] +
          mp_T["Stand.age.at.T2", dbh_spec]*
          (log(max(x$Age, 1e-12)) - msd5[2, 1])/msd5[2, 2] +
          mp_T["Spruce.volume", dbh_spec]*
          (log(max(x$VolumeSpruce, 1e-12)) - msd5[3, 1])/msd5[3, 2]
  WDF10 <- mp_T["Intercept", which(!colnames(mp_T) %in% dbh_spec)] +
           mp_T["Dead.wood.volume", which(!colnames(mp_T) %in% dbh_spec)]*
           (log(max(x$DeadWoodVolumeSpruce, 1e-12)) - msd10[1, 1])/msd10[1, 2] +
           mp_T["Stand.age.at.T2", which(!colnames(mp_T) %in% dbh_spec)]*
           (log(max(x$Age, 1e-12)) - msd10[2, 1])/msd10[2, 2] +
           mp_T["Spruce.volume", which(!colnames(mp_T) %in% dbh_spec)]*
           (log(max(x$VolumeSpruce, 1e-12)) - msd10[3, 1])/msd10[3, 2]
  ## Because the Inf/-Inf values coming from the deterministic model parts 
  ## disappear during the predictions abive, we need to re-introduce them here
  ## with values from the original model parameter matrix:
  WDF <- c(WDF5, WDF10)[colnames(mp_T)] ## Make sure order of species is the same
  WDF[is.infinite(mp_T[1,])] <- mp_T[1, is.infinite(mp_T[1,])] ## Re-introduce Inf/-Inf
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before!
  as.list(inv.logit(WDF)) ## Multiply also with RpC once added above!
}

## Predict initial state WDF data:
dci_pred <- dc_init[, pred_init(.SD), by = 1:nrow(dc_init)]

## Combine with original data:
dci_pred <- cbind(dc_init[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  dci_pred[, -1])



## Ask helen how she dealt with the minimum diameters and Heureka data !!!!!!!!!
## In the text: 
## T. abietinum, G. sepiarium and P. viticola 5cm data
## All others 10 cm data
## Correct ?????????????????????????








  
## 5. Create output data -------------------------------------------------------

out <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                   d_pred[,-1])

write.csv(out, paste0("clean/MFO_WDF_", climate, ".csv"), row.names = FALSE)
  
## -------------------------------END-------------------------------------------
