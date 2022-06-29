## This script adds wood-decaying fungi models to the NFI data with one occupancy
## per NFI plot, ControlCategoryName, AlternativeNo, and period
##
## Thee Moor et al. model is independent of climate.
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
## Last edit: 2022-06-28
##
## Author: Julian Klein

rm(d_cov, mp, md_orig, dbh_5_spec, dbh_10_spec)

## 1.1. Define model parameters, minimum age thresholds and mean&SD ------------

## The model parameters:
mp <- structure(list(Stand.age.group.min = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 21L, 
                                             21L, 21L, 21L, 21L, 21L, 21L, 21L, 
                                             64L, 64L, 64L, 64L, 64L, 64L, 64L, 64L), 
                     Stand.age.group.max = c(20L, 20L, 20L, 20L, 20L, 20L, 20L, 
                                             20L, 63L, 63L, 63L, 63L, 63L, 63L, 
                                             63L, 63L, 1000L, 1000L, 1000L, 1000L, 
                                             1000L, 1000L, 1000L, 1000L), 
                     probability = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 
                                               1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 
                                               1L, 1L, 1L, 2L, 2L, 2L, 2L), 
                                             .Label = c("colonization", "extinction", "start"), 
                                             class = "factor"), 
                     variable = structure(c(1L, 2L, 3L,4L, 1L, 2L, 3L, 4L, 1L, 2L, 
                                            3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 
                                            4L, 1L, 2L, 3L, 4L), 
                                          .Label = c("Intercept", "Dead.wood.volume", 
                                                     "Stand.age.at.T2", "Spruce.volume"), 
                                          class = "factor"), 
                     `A. lapponica` = c(-Inf, -Inf, -Inf, -Inf, Inf, Inf, Inf, 
                                        Inf, -Inf, -Inf, -Inf, -Inf, Inf, Inf, 
                                        Inf, Inf, -5.54, 2.3, 1.97, 0, -1.31, 0, 0, 0), 
                     `A. serialis` = c(-0.55, 0, 0, 0, 0.45, 0, 0, 0, -1.19, 3.52, 
                                       0, 0, -1.91, 0, 0, 0, -1.19, 3.52, 0, 0, 
                                       -1.91, 0, 0, 0), 
                     `F. pinicola` = c(-4.83, 5.91, 0, 0, 0.94, 0, 0, 0, 0.35, 
                                       2.07, 0, 0, -2.19, 0, 0, 0, 0.35, 2.07, 
                                       0, 0, -2.19, 0, 0, 0), 
                     `F. rosea` = c(-Inf, -Inf, -Inf, -Inf, Inf, Inf, Inf, Inf, 
                                    -Inf, -Inf, -Inf, -Inf, Inf, Inf, Inf, Inf, 
                                    -4.17, 1.72, 1.77, 0, -1.27, 0, 0, 0), 
                     `G. sepiarium` = c(5.3, 9.52, 0, 0, -1.48, 0, 0, 0, -1.75, 
                                        0, 0, -1.31, 0.17, 0, 0, 0, -1.75, 0, 0, 
                                        -1.31, 0.17, 0, 0, 0), 
                     `P. centrifuga` = c(-Inf, -Inf, -Inf, -Inf, Inf, Inf, Inf, 
                                         Inf, -Inf, -Inf, -Inf, -Inf, Inf, Inf, 
                                         Inf, Inf, -4.64, 2.99, 0, 0, -0.05, 0, 0, 0), 
                     `P. ferrugineofuscus` = c(-Inf, -Inf, -Inf, -Inf, Inf, Inf, 
                                               Inf, Inf, -Inf, -Inf, -Inf, -Inf, 
                                               Inf, Inf, Inf, Inf, -2.69, 2.49, 
                                               4.05, 0, -1.88, 0, 0, 0), 
                     `P. nigrolimitatus` = c(-Inf, -Inf, -Inf, -Inf, Inf, Inf, 
                                             Inf, Inf, -Inf, -Inf, -Inf, -Inf, 
                                             Inf, Inf, Inf, Inf, -9.02, 8.07, 
                                             4.37, 0, -2.02, -11.11, 0, 0), 
                     `P. viticola` = c(-7.8, 0, 8.07, 0, 0.46, 0, 0, 0, -Inf, -Inf, 
                                       -Inf, -Inf, Inf, Inf, Inf, Inf, -0.91, 
                                       0.67, 0.64, 0, -3.91, -8.62, 0, 0), 
                     `T. abietinum` = c(-0.62, 0, 0, 0, -0.09, 0, 0, 0, 7.13, 
                                        4.63, 0, 0, -2.72, 0, 0, 0, 7.13, 4.63, 
                                        0, 0, -2.72, 0, 0, 0)), 
                row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 13L, 14L, 15L, 16L, 
                              17L, 18L, 19L, 20L, 25L, 26L, 27L, 28L, 29L, 30L, 
                              31L, 32L), 
                class = "data.frame")

## The mean and SDs from the original data:
md_orig <- structure(list(Variable = structure(c(1L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 
                                                 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 
                                                 4L, 4L, 4L, 4L, 2L, 2L, 2L, 2L), 
                                               .Label = c("Dead wood volume (log)", 
                                                          "Spruce volume", 
                                                          "Spruce volume (log)", 
                                                          "Stand age", 
                                                          "Stand age (log)"), 
                                               class = "factor"), 
                          Minimum.Diameter = c(5L, 5L, 10L, 10L, 5L, 5L, 10L, 10L, 
                                               5L, 5L, 10L, 10L, 5L, 5L, 10L, 10L, 
                                               5L, 5L, 10L, 10L, 5L, 5L, 10L, 10L), 
                          Stand.age.group.min = c(21L, 0L, 21L, 0L, 21L, 0L, 21L, 
                                                  0L, 21L, 0L, 21L, 0L, 21L, 0L, 
                                                  21L, 0L, 21L, 0L, 21L, 0L, 21L, 
                                                  0L, 21L, 0L), 
                          Stand.age.group.max = c(1000L, 20L, 1000L, 20L, 1000L, 
                                                  20L, 1000L, 20L, 1000L, 20L, 
                                                  1000L, 20L, 1000L, 20L, 1000L, 
                                                  20L, 1000L, 20L, 1000L, 20L, 
                                                  1000L, 20L, 1000L, 20L), 
                          Model.component = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
                                                        1L, 1L, 1L, 1L, 1L, 1L, 
                                                        2L, 2L, 2L, 2L, 2L, 2L, 
                                                        2L, 2L, 2L, 2L, 2L, 2L), 
                                                      .Label = c("colonization", "extinction"), 
                                                      class = "factor"), 
                          Mean = c(2.22, 0.46, 2.4, 0.18, 4.79, 2.72, 4.79, 2.76, 
                                   5.18, 4.36, 5.2, 4.36, 2.22, 0.46, 2.4, 0.18, 
                                   126.83, 15.69, 126.95, 16.19, 201.68, 102.98, 
                                   205.39, 105.44), 
                          SD = c(1.86, 1.11, 1.72, 1.25, 0.31, 0.27, 0.31, 0.25, 
                                 0.58, 0.77, 0.59, 0.8, 1.86, 1.11, 1.72, 1.25, 
                                 41.7, 3.75, 42.03, 3.5, 84.32, 77.8, 84.33, 82.48)), 
                     class = "data.frame", 
                     row.names = c(NA, -24L))

## Reduce to species found in Moor et al 2021:
WDF <- WDF[WDF != "P. chrysoloma"]
## Which species use the model mean and SD for diameter >= 5cm?
dbh_5_spec <- c("T. abietinum", "G. sepiarium", "P. viticola")
dbh_10_spec <- c("A. lapponica", "A. serialis", "F. pinicola", "F. rosea", 
                 "P. centrifuga", "P. ferrugineofuscus", "P. nigrolimitatus")

## 1.2. Reduce all information to selected WDF --------------------------------- 
mp <- mp[, c("Stand.age.group.min", "Stand.age.group.max", "probability", "variable", WDF)]
dbh_5_spec <- dbh_5_spec[dbh_5_spec %in% WDF]
dbh_10_spec <- dbh_10_spec[dbh_10_spec %in% WDF]

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

## 3. Create prediction function generic for all probability states ------------

pred_prob <- function(x, probability){
  ## Chose model and md_orig by forest age of Heureka row x:
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
  names(WDF5) <- dbh_5_spec
  WDF10 <- mp_T["Intercept", dbh_10_spec] +
    mp_T["Dead.wood.volume", dbh_10_spec]*
    (log(max(x$DeadWoodVolumeSpruce, 1e-12)) - msd10[1, 1])/msd10[1, 2] +
    mp_T["Stand.age.at.T2", dbh_10_spec]*
    (ifelse(probability == "extinction", x$Age, log(max(x$Age, 1e-12))) - msd5[2, 1])/msd5[2, 2] +
    mp_T["Spruce.volume", dbh_10_spec]*
    (ifelse(probability == "extinction", x$VolumeSpruce, log(max(x$VolumeSpruce, 1e-12))) - msd5[3, 1])/msd5[3, 2]
  names(WDF10) <- dbh_10_spec
  ## Because the Inf/-Inf values coming from the deterministic model parts 
  ## disappear during the predictions above, we need to re-introduce them here
  ## with values from the original model parameter matrix:
  WDF <- c(WDF5, WDF10)[colnames(mp_T)] ## Making sure the order of species in WDF is the same as in mp_T for next step to work
  WDF[is.infinite(mp_T[1,])] <- mp_T[1, is.infinite(mp_T[1,])] ## Re-introduce Inf/-Inf
  # RpC <- ifelse(x$ret_patch == 1, 0.1, 1) ## Define ret_patch in d_cov before!
  as.list(if(probability == "colonization") clogloglink(WDF, inverse = TRUE) else inv.logit(WDF)) ## Multiply also with RpC once added above!
}

## 4.1. Predict start occupancy with InitialState data -------------------------
## Start values going into the colonisation extinction dynamics are the carrying 
## capacity with InitialState data in Heureka as predictors. 
## The carrying capacity is K = c/(c+e). 
## From: solve(K = (1-K)*c+K(1-e), K)
mp <- as.data.table(mp)

## Initial state Heureka data:
dc_init <- d_cov[d_cov$ControlCategoryName == "Initial state", ]

## Predict colonization rate with period 0 data:
mp_red <- mp[mp$probability == "colonization", ]
msd_red <- as.data.table(md_orig[md_orig$Model.component == "colonization", ])
dci_col <- dc_init[, pred_prob(.SD, "colonization"), by = 1:nrow(dc_init)]

## Predict extinction rate with period 0 data:
mp_red <- mp[mp$probability == "extinction", ]
msd_red <- as.data.table(md_orig[md_orig$Model.component == "extinction", ])
dci_ext <- dc_init[, pred_prob(.SD, "extinction"), by = 1:nrow(dc_init)]

## Combine species names:
spec <- c(dbh_5_spec, dbh_10_spec)

## Calculate carrying capacity K = c/(c+e) for every NFI:
d_K <- dci_col[, ..spec] / (dci_col[, ..spec] + dci_ext[, ..spec])
d_K <- cbind(dc_init[, c("Description", "AlternativeNo", "ControlCategoryName", "period")], 
             as.data.table(d_K))

## 4.2. Predict colonisation and extinction ------------------------------------

## Colonisation Heureka data, model parameters, and Mean & SD:
dc_col <- d_cov[d_cov$ControlCategoryName != "Initial state", ]
mp_red <- mp[mp$probability == "colonization", ]
msd_red <- as.data.table(md_orig[md_orig$Model.component == "colonization", ])
## Predict colonization state WDF data and combine with unique ID data:
dcc_pred <- dc_col[, pred_prob(.SD, "colonization"), by = 1:nrow(dc_col)]
dcc_pred <- cbind(dc_col[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  dcc_pred[, -1])

## Colonisation Heureka data, model parameters, and Mean & SD:
dc_ext <- d_cov[d_cov$ControlCategoryName != "Initial state", ]
mp_red <- mp[mp$probability == "extinction", ]
msd_red <- as.data.table(md_orig[md_orig$Model.component == "extinction", ])
## Predict extinction state WDF data and combine with unique ID data:
dce_pred <- dc_ext[, pred_prob(.SD, "extinction"), by = 1:nrow(dc_ext)]
dce_pred <- cbind(dc_ext[, c("Description", "period", "AlternativeNo", "ControlCategoryName")],
                  dce_pred[, -1])

## 5. Predict occupancy according to colonisation - extinction dynamics --------

## The dynamic occupancy model function:
pred_dyn <- function(x){
  dyn_M[1, ] <- as.matrix((1 - d_K[d_K$Description %in% x$NFI, ..spec])*
                          x[x$probability == "col" & x$period == 1, ..spec] +
                            d_K[d_K$Description %in% x$NFI, ..spec]*
                          (1 - x[x$probability == "ext" & x$period == 1, ..spec]))
  for(i in 2:max(x$period)){
    dyn_M[i, ] <- as.matrix((1 - dyn_M[i-1, ])*
                            x[x$probability == "col" & x$period == i, ..spec] +
                            dyn_M[i-1, ]*
                            (1 - x[x$probability == "ext" & x$period == i, ..spec]))
  }
  
  out <- cbind(1:i, dyn_M)
  colnames(out) <- c("period", spec)
  return(as.data.table(out))
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
d_pred <- rbind(d_K, dcce_pred)

## Select wood-decaying fungi again to export and rename:
d_pred[, colnames(d_pred) %in% c(WDF, "period", "Description", "AlternativeNo", "ControlCategoryName"), with = FALSE]
colnames(d_pred)[5:length(d_pred)] <- paste(colnames(d_pred)[5:length(d_pred)], "Moor")
out_Moor <- d_pred

## -------------------------------END-------------------------------------------

## Merge with forest age to check correlations:
trial <- merge(as.data.frame(d_pred), d_cov, by = c("period", "Description", "AlternativeNo", "ControlCategoryName"))
plot(as.numeric(trial$`A. lapponica`) ~ log(trial$Age))
plot(as.numeric(trial$`A. lapponica`) ~ log(trial$VolumeSpruce))
plot(as.numeric(trial$`A. lapponica`) ~ log(trial$DeadWoodVolumeSpruce))

mp[, c("variable", "A. lapponica")] ## Should increase with Age and DW
