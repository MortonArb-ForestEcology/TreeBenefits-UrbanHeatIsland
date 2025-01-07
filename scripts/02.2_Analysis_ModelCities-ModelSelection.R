# This script will just pull the data and run some LST models to decide which one we should move forward with
library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=T

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")

file.cityStatsRegion <- file.path(path.cities, "../city_stats_model-selection.csv")

cityStatsRegion <- read.csv(file.cityStatsRegion)

# Looking at dR2adj --> Pos is alternate does better
cityStatsRegion$dR2adj.Log <- cityStatsRegion$LSTmodelLog.R2adj - cityStatsRegion$LSTmodel.R2adj 
cityStatsRegion$dR2adj.SCover <- cityStatsRegion$LSTmodelSCover.R2adj - cityStatsRegion$LSTmodel.R2adj 
cityStatsRegion$dR2adj.SCoverTree <- cityStatsRegion$LSTmodelSCoverTree.R2adj - cityStatsRegion$LSTmodel.R2adj 

# change in RMSE; negative means alternate does better
cityStatsRegion$dRMSE.Log <-cityStatsRegion$LSTmodelLog.RMSE -  cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSE.SCover <- cityStatsRegion$LSTmodelSCover.RMSE - cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSE.SCoverTree <- cityStatsRegion$LSTmodelSCoverTree.RMSE - cityStatsRegion$LSTmodel.RMSE

# Change in AIC; negative means reduction
cityStatsRegion$dAIC.Log <- cityStatsRegion$LSTmodelLog.AIC - cityStatsRegion$LSTmodel.AIC
cityStatsRegion$dAIC.SCover <-  cityStatsRegion$LSTmodelSCover.AIC - cityStatsRegion$LSTmodel.AIC
cityStatsRegion$dAIC.SCoverTree <-  cityStatsRegion$LSTmodelSCoverTree.AIC - cityStatsRegion$LSTmodel.AIC
cityStatsRegion$dAIC.SCoverTree2 <-  cityStatsRegion$LSTmodelSCoverTree.AIC - cityStatsRegion$LSTmodelSCover.AIC

# Looking at dRMSE as a percent reduction
cityStatsRegion$dRMSEperc.Log <- cityStatsRegion$dRMSE.Log/cityStatsRegion$LSTmodel.RMSE

cityStatsRegion$dRMSEperc.SCover <- cityStatsRegion$dRMSE.SCover/cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSEperc.SCoverTree <- cityStatsRegion$dRMSE.SCoverTree/cityStatsRegion$LSTmodel.RMSE

summary(cityStatsRegion)

