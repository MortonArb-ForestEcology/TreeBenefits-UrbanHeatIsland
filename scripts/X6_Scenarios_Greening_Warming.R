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

library(mgcv)
library(ggplot2); library(RColorBrewer); library(cowplot)
library(ggalt); library(sf)
library(mapproj)
library(scales)

path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")
path.et <- file.path(path.google, "ET_models_v2")

biome.pall.all = c("Taiga"= "#2c5c74", 
                   "Tundra"="#6d8e9d",
                   "Temperate Broadleaf Forest" = "#7f310f",
                   "Temperate Conifer Forest" = "#4d1e10",
                   "Temperate Grassland/Savanna" = "#b09c41",
                   "Montane Grassland/Savanna" = "#a0b8c7",
                   "Mediterranean" = "#bf772e",
                   "Desert" = "#c89948",
                   "Flooded Grassland/Savanna" = "#e0dfa1",
                   "Tropical Grassland/Savanna" = "#a6b39e",
                   "Tropical Dry Broadleaf Forest" = "#7a9c64",
                   "Tropical Conifer Forest" = "#488458",
                   "Tropical Moist Broadleaf Forest"= "#266240",
                   "Mangroves" = "#9c8c94")

###########################################


###########################################
# Read in city data & summarizing baseline ----
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


cityAll.ET <- read.csv(file.path(path.google, "city_stats_all_ET-GLDAS.csv"))
cityAll.ET$ETpixels.prop <- cityAll.ET$n.pixels.ET/cityAll.ET$n.pixels
cityAll.ET$ET.cv <- cityAll.ET$ETobs.sd/(cityAll.ET$ETobs.max - cityAll.ET$ETobs.min)
summary(cityAll.ET)

plot(ETmodel.R2adj ~ ET.cv, data=cityAll.ET)

biome.order <- aggregate(Tmean.GLDAS ~ biomeName, data=cityAll.ET, FUN=mean)
biome.order <- biome.order[order(biome.order$Tmean.GLDAS),]

StatsCombined$biomeName <- factor(StatsCombined$biomeName, levels=biome.order$biomeName)
cityAll.ET$biomeName <- factor(cityAll.ET$biomeName, levels=biome.order$biomeName)


###########################################




###########################################
# Bringing in climate change and running some scenarios ----
###########################################

# Read in CMIP6 metadata 
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_deviations.csv"))
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
summary(cmip6)

###########################################


###########################################
# Reading in an testing a model
###########################################
CITY="CAN15001"

# modETCity <- load(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
modETCity <- readRDS("~/Desktop/CAN15001_Model-ET_annual_gam.rds")
?predict.gam
test <- predict(modETCity, type="terms", exclude="as.factor(year)")

test2 <- predict(modETCity, type="terms", exclude=c("s(cover.tree)", "s(cover.veg)", "s(LST_Day)", "s(x,y)"))
summary(test2)
###########################################


###########################################
# Iterating through climate change scenarios
###########################################
###########################################
