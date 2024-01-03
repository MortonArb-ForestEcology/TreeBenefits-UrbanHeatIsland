# Doing some warming and greening projections on cities
# Vegetation: 1) As is; 2) Green Blob (even at target) 3) Uniform increase 4) bottom-up
# -- Vegetation goal: How much water will a city need to meet its goals?  How does canopy equality affect water needs?
# -- Vegetation Methods: apply scenario to each pixel; for 4) ???
# -- Save along the way: percent of each city in 5% bins so we can graph the changes in distribution
# Climate: 1) current; 2) mid-century: ssp 245, 585; 3) end-century: ssp 245, 585
# -- Climate Goal: How will the water needs of the canopy change with climate change; how does that compare to changes in precip 
# -- Climate Method: look at the delta in temperature; add uniformly to area (acknowledge we're dealing with air vs. lst temps here, but it's a start!); compare to delta in precip; all deltas calculated as a change from weighted-average temp & precip for each GCM x scenario; for precip consider both absolute and % change
# -- Save along the way: climate change projection stats for each city: average warming, drying/wetting per city 
#        

###########################################
# Load Packages, set paths ----
###########################################
library(tidyr)

library(ggplot2); library(RColorBrewer); library(cowplot)
library(ggalt); library(sf)
library(mapproj)

path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")
###########################################


###########################################
# Read in city data
###########################################
# cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
# cityAll.stats <- cityAll.stats[,!grepl("ETmodel", names(cityAll.stats))]
# cityAll.stats <- cityAll.stats[,!grepl("corr.", names(cityAll.stats))]
# cityAll.stats <- cityAll.stats[,!grepl("trend", names(cityAll.stats))]
# summary(cityAll.stats)
# 
# summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])

StatsCombined <- read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
StatsCombined <- StatsCombined[,!(grepl("ETmodel", names(StatsCombined)))]
summary(StatsCombined)

cityAll.ET <- read.csv(file.path(path.google, "city_stats_all_ET-Clim.csv"))
length(cityAll.ET$ISOURBID[which(cityAll.ET$Precip.GLDAS<0.01)])

cityAll.ET <- cityAll.ET[cityAll.ET$ISOURBID %in% StatsCombined$ISOURBID & cityAll.ET$Precip.GLDAS>0.01,]
summary(cityAll.ET)

# Read in CMIP6 metadata 
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_deviations.csv"))
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
summary(cmip6)

###########################################
