## This script Bryoria fuscescens to NFI data with one P of occurrence per
## NFI plot, ControlCategoryName, AlternativeNo, and period
## 
## Models by:
## Horstkotte Tim, Moen Jon, Lämås Tomas, Helle Time (2011). 
## The Legacy of Logging-Estimating Arboreal Lichen Occurrence in a Boreal 
## Multiple-Use Landscape on a Two Century Scale
## PLOSONE https://doi.org/10.1371/journal.pone.0028779
##
## First edit: 2022-06-03
## Last edit: 2022-06-07
##
## Author: Julian Klein

rm(d_cov)

## 1. Define model parameters, minimum age thresholds and mean&SD --------------

## The model parameters (Table 4):
mp <- structure(c(-3.11, 0.04), 
                .Dim = 2:1, 
                .Dimnames = list(c("Constant", "Forest Age"), "B. fuscescens"))

## 2. Load Heureka data --------------------------------------------------------

if(climate == "RCP0") file <- paste0(dir_HK, file_RCP0)
if(climate == "RCP45") file <- paste0(dir_HK, file_RCP45)
if(climate == "RCP85") file <- paste0(dir_HK, file_RCP85)

d_cov <- fread(file,  
               select = c("Description", "period", "Region", "AlternativeNo", 
                          "ControlCategoryName", "Age"),
               sep = ";", blank.lines.skip = TRUE)

## Select random share of NFI plots:
d_unique <- unique(d_cov$Description)
set.seed(1)
select <- sample(d_unique, NFI_share*length(d_unique))
d_cov <- d_cov[d_cov$Description %in% select, ]

## 3. Add B. fuscescens to d_cov according to model formula --------------------

## Define prediction function: 
pred_BF <- function(x){
  ifelse(x$Region %in% c(1, 21, 22), ## Model only relevant in the reindeer herding regions.
         inv.logit(mp["Constant", ] + mp["Forest Age", ]*x$Age),
         0)
}

## Predict WDF data:
d_pred <- d_cov[, pred_BF(.SD), by = 1:nrow(d_cov)]
colnames(d_pred)[2] <- colnames(mp)

## 5. Create output data -------------------------------------------------------

## Which ESS should be exported?
out_Horstkotte <- cbind(d_cov[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                        d_pred[, "B. fuscescens"])

## -------------------------------END-------------------------------------------

## Compare to Fig. 2 in Horstkotte et al. 2011
out_comp <- cbind(d_cov[, "Age"], d_pred[, "B. fuscescens"])
plot(out_comp$Age, out_comp$`B. fuscescens`)
