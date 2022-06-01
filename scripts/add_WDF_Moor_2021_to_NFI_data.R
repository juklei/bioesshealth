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
## Last edit: 2022-05-25
##
## Author: Julian Klein

## 1. Clear environment, load libraries, and data ------------------------------

rm(list = ls())

library(data.table)
library(boot)
library(VGAM)

start <- Sys.time()

dir <- "C:/MultiforestOptimisationNotebook/multiforestOptimizationNotebook"
dir.create("clean")

## Model parameters for wood-decaying fungi (WDF):
params <- read.csv("data/Moor_WDF_model_parameters.csv")

## The original model data with means and SDs of covariates:
mean_sd <- read.csv("data/Moor_WDF_mean&SD.csv", sep = ",")

## Which species use the model mean and SD for diameter >= 5cm?
dbh_5_spec <- c("T. abietinum", "G. sepiarium", "P. viticola")
dbh_10_spec <- levels(params$Species)[which(!levels(params$Species) %in% dbh_5_spec)]

d_cov <- fread(paste0(dir, "/data/MultiForestResults210704_20Periods_InclIntensivve_NoCC.csv"),  
               select = c("Description", "period", "AlternativeNo", "ControlCategoryName", 
                          "Age", "VolumeSpruce", "DeadWoodVolumeSpruce"), 
                  sep = ";", blank.lines.skip = TRUE)

## Select random share of NFI plots:
share <- 0.001
d_unique <- unique(d_cov$Description)
select <- sample(d_unique, share*length(d_unique))
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

## 3. Create prediction function generic for all probability states ------------


pred_prob <- function(x, probability){
  ## Chose model and mean_SD by forest age of Heureka row x:
  mp_T <- mp_red[mp_red$Stand.age.group.min <= round(x$Age) & mp_red$Stand.age.group.max >= round(x$Age), ]
  msd_T <- msd_red[msd_red$Stand.age.group.min <= round(x$Age) & msd_red$Stand.age.group.max >= round(x$Age), ]
  ## Create matrices with variables as row names to have less code below:
  mp_T <- as.matrix(mp_T[, -c(1:3)], rownames = "variable")
  msd5 <- as.matrix(msd_T[msd_T$Minimum.Diameter == "5", -c(2:5)], rownames = "Variable")
  msd10 <- as.matrix(msd_T[msd_T$Minimum.Diameter == "10", -c(2:5)], rownames = "Variable")
  ## Make predictions for the two different diameter classes:
  WDF5 <- mp_T["Intercept", dbh_5_spec] + 
    mp_T["Dead.wood.volume", dbh_5_spec]*
    (log(max(x$DeadWoodVolumeSpruce, 1e-12)) - msd5[1, 1])/msd5[1, 2] +
    mp_T["Stand.age.at.T2", dbh_5_spec]*
    (ifelse(probability == "extinction", x$Age, log(max(x$Age, 1e-12))) - msd5[2, 1])/msd5[2, 2] +
    mp_T["Spruce.volume", dbh_5_spec]*
    (ifelse(probability == "extinction", x$VolumeSpruce, log(max(x$VolumeSpruce, 1e-12))) - msd5[3, 1])/msd5[3, 2]
  WDF10 <- mp_T["Intercept", dbh_10_spec] +
    mp_T["Dead.wood.volume", dbh_10_spec]*
    (log(max(x$DeadWoodVolumeSpruce, 1e-12)) - msd10[1, 1])/msd10[1, 2] +
    mp_T["Stand.age.at.T2", dbh_10_spec]*
    (ifelse(probability == "extinction", x$Age, log(max(x$Age, 1e-12))) - msd5[2, 1])/msd5[2, 2] +
    mp_T["Spruce.volume", dbh_10_spec]*
    (ifelse(probability == "extinction", x$VolumeSpruce, log(max(x$VolumeSpruce, 1e-12))) - msd5[3, 1])/msd5[3, 2]
  ## Because the Inf/-Inf values coming from the deterministic model parts 
  ## disappear during the predictions abive, we need to re-introduce them here
  ## with values from the original model parameter matrix:
  WDF <- c(WDF5, WDF10)[colnames(mp_T)] ## Make sure order of species is the same
  WDF[is.infinite(mp_T[1,])] <- mp_T[1, is.infinite(mp_T[1,])] ## Re-introduce Inf/-Inf
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before!
  as.list(if(probability == "colonization") clogloglink(WDF, inverse = TRUE) else inv.logit(WDF)) ## Multiply also with RpC once added above!
}

## 4. Predict occupancy, colonisation, and extinction --------------------------

## Initial state Heureka data, model parameters, and Mean & SD:
dc_init <- d_cov[d_cov$ControlCategoryName == "Initial state", ]
mp_red <- mp[mp$probability == "start", ]
msd_red <- as.data.table(mean_sd[mean_sd$Model.component == "colonization", ])
## Predict initial state WDF data and combine with unique ID data:
dci_pred <- dc_init[, pred_prob(.SD, "start"), by = 1:nrow(dc_init)]
dci_pred <- cbind(dc_init[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  dci_pred[, -1])

## Colonisation Heureka data, model parameters, and Mean & SD:
dc_col <- d_cov[d_cov$ControlCategoryName != "Initial state", ]
mp_red <- mp[mp$probability == "colonization", ]
msd_red <- as.data.table(mean_sd[mean_sd$Model.component == "colonization", ])
## Predict colonization state WDF data and combine with unique ID data:
dcc_pred <- dc_col[, pred_prob(.SD, "colonization"), by = 1:nrow(dc_col)]
dcc_pred <- cbind(dc_col[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  dcc_pred[, -1])

## Colonisation Heureka data, model parameters, and Mean & SD:
dc_ext <- d_cov[d_cov$ControlCategoryName != "Initial state", ]
mp_red <- mp[mp$probability == "extinction", ]
msd_red <- as.data.table(mean_sd[mean_sd$Model.component == "extinction", ])
## Predict extinction state WDF data and combine with unique ID data:
dce_pred <- dc_ext[, pred_prob(.SD, "extinction"), by = 1:nrow(dc_ext)]
dce_pred <- cbind(dc_ext[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  dce_pred[, -1])

## 5. Predict occupancy according to colonisation - extinction dynamics --------

## The dynamic occupancy model function:
pred_dyn <- function(x){
  dyn_M[1, ] <- as.matrix((1 - dci_pred[dci_pred$Description %in% x$NFI, ..spec])*
                          x[x$probability == "col" & x$period == 1, ..spec] +
                          dci_pred[dci_pred$Description %in% x$NFI, ..spec]*
                          (1 - x[x$probability == "ext" & x$period == 1, ..spec]))
  for(i in 2:max(x$period)){
    dyn_M[i, ] <- as.matrix((1 - dyn_M[i-1, ])*
                            x[x$probability == "col" & x$period == i, ..spec] +
                            dyn_M[i-1, ]*
                            (1 - x[x$probability == "ext" & x$period == i, ..spec]))
  }
  out <- cbind(1:i, dyn_M)
  colnames(out) <- c("period", spec)
  return(apply(out, 2, as.list))
}

## Combine colonisation and extinction data to calculate dynamics:
dcc_pred$probability <- "col"
dce_pred$probability <- "ext"
dcce <- rbind(dcc_pred, dce_pred)
dcce$NFI <- dcce$Description ## NFI id needed for identifying correct initial state
  
spec <- c(dbh_5_spec, dbh_10_spec)
dyn_M <- matrix(NA, max(d_cov$period), length(spec))
dcce_pred <- dcce[, pred_dyn(.SD), by = c("Description", "AlternativeNo", "ControlCategoryName")]  
  
## 6. Create output data -------------------------------------------------------

## Combine colonisation-extinction predictions with original initial status:
out <- rbind(dci_pred, dcce_pred)

fwrite(out, "clean/MFO_WDF_Moor.csv", row.names = FALSE)

end <- Sys.time()

end-start ## 35min for 1%

## -------------------------------END-------------------------------------------

## Merge with forest age to check correlations:
trial <- merge(as.data.frame(out), d_cov, by = c("period", "Description", "AlternativeNo", "ControlCategoryName"))
plot(as.numeric(trial$`A. lapponica`) ~ log(trial$Age))
plot(as.numeric(trial$`A. lapponica`) ~ log(trial$VolumeSpruce))
plot(as.numeric(trial$`A. lapponica`) ~ log(trial$DeadWoodVolumeSpruce))

mp[, c("variable", "A. lapponica")] ## Should increase with Age and DW
