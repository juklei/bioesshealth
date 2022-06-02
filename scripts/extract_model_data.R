## Extract relevant parameters from data and copy them into model specific R
## files. This script keeps track of this for all models.

library(data.table)

## 1. --------------------------------------------------------------------------
## Mair L, Jönsson M, Räty M, et al.
## Land use changes could modify future negative effects of climate change on 
## old-growth forest indicator species. 
## Divers Distrib. 2018;00:1-10. https://doi.org/10.1111/ddi.12771

rm(list = ls())

## Model parameters for wood-decaying fungi (WDF):
model_params_wdf <- read.csv("data/Table_SummaryModelCompare-GLMnewfitting_03juli2020_TS.csv")
median_extract <- function(x){
  T1 <- strsplit(as.character(x), " ")
  return(as.numeric(sapply(T1, "[", 1)))
}
mp <- t(apply(model_params_wdf[, 2:11], 2, median_extract))
colnames(mp) <- as.character(model_params_wdf$Species)

## Minimum age of a forest to host the species:
min_age <- c(87, 75, 64, 64, 76, 83) ## From table S4.3
names(min_age) <- as.character(model_params_wdf$Species)

## The original model fitting data set to calculate mean and SD from:
model_data <- read.table("data/DataToJulian20211217.txt", sep = "", header = TRUE)  
md <- model_data[, c("gran_max", "ald_max", "tempann", "precsumson", "swe_twi")]
md_orig <- rbind(apply(md, 2, mean), apply(md, 2, sd))
dimnames(md_orig)[[1]] <- c("Mean", "SD") 
  
## Use dput() to extract code to create the model data inside the model scripts:
dput(mp)
dput(min_age)
dput(md_orig)

## 2. --------------------------------------------------------------------------
## Löbel, S., Schröder, B., & Snäll, T. (2021). 
## Projected shifts in deadwood bryophyte communities under national climate and 
## forestry scenarios benefit large competitors and impair small species. 
## Journal of Biogeography, 48, 3170-3184. https://doi.org/10.1111/jbi.14278

rm(list = ls())

## Model parameters for bryophytes:
model_params_bry <- fread("data/Bryo_CoeffLogReg_exclConn_inclFAge_corr.csv")
mp <- dcast(model_params_bry, CovariateID ~ SpeciesID, value.var = "Estimate", fill = 0)
mp <- as.matrix(mp[,-1], rownames = mp$CovariateID) ## Must be a matrix

## Minimum age of a forest to host the species: There are more species in the 
## minimum age data set than in the model parameters data set. 
min_age <- read.table("data/ForestAgeThresholdsBryo.txt", sep = "", header = TRUE)
min_age <- min_age[min_age$Species %in% colnames(mp), c("q2.5", "Species")] ## lower CI according to text
min_age_q2.5 <- as.vector(min_age$q2.5)
names(min_age_q2.5) <- min_age$Species

## The original model fitting data set to calculate mean and SD from:
model_data <- fread("data/StandParametersBryophytes.txt", sep = "\t", header = TRUE)
md_orig <- dcast(melt(model_data), variable ~ Covariate)
row.names(md_orig) <- md_orig$variable
md_orig <- as.matrix(md_orig, rownames = "variable")

## Use dput() to extract code to create the model data inside the model scripts:
dput(mp)
dput(min_age_q2.5)
dput(md_orig)

## 3. --------------------------------------------------------------------------
## Moor H, Nordén J, Penttilä R, Siitonen J, Snäll T. 
## Long-term effects of colonization-extinction dynamics of generalist versus 
## specialist wood-decaying fungi. 
## J Ecol. 2021;109:491-503. https://doi.org/10.1111/1365-2745.13526

rm(list = ls())

## Model parameters for wood-decaying fungi (WDF):
params <- read.csv("data/Moor_WDF_model_parameters.csv")

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

## The original model data with means and SDs of covariates:
md_orig <- read.csv("data/Moor_WDF_mean&SD.csv", sep = ",")

## Use dput() to extract code to create the model data inside the model scripts:
dput(as.data.frame(mp))
dput(md_orig)

## 4. --------------------------------------------------------------------------
## Models by:
## Mnazziotta et al. (YYYY). 
## Title.
## Journal. doi

rm(list = ls())

## Model parameters for ESS:
model_params <- read.csv("data/ESS.mean.myposteriorsample1_df.beta - Copy.csv")
## Make covariat*ESS matrix with estimates as content:
model_params[is.na(model_params)] <- 0 ## unused covariates are 0
mp <- as.matrix(model_params[, 3:6])
dimnames(mp)[[1]] <- as.character(model_params$Covariate)

## Data with mean and SD from the original data:
md_orig <- read.csv("data/ESS.R.mean.sd.csv")

## Use dput() to extract code to create the model data inside the model scripts:
dput(mp)
dput(md_orig)

## -------------------------------END-------------------------------------------





















