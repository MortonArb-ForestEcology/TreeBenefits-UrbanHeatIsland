# Looking at the ET Models and doing some predictions of water use and greening scenarios
library(ggplot2)

# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google)


# Adding info for the previous version so we can double check
# path.google2 <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v2")
# path.cities2 <- file.path(path.google2)
# file.cityAll.stats2 <- file.path(path.cities2, "city_stats_all.csv")


path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)


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

world <- map_data("world")
# ##########################################


# ##########################################
# Read in Data; do some cleanup ----
# ##########################################
# Reading in our old, full dataset
cityAll.stats <- read.csv(file.path(path.cities, "city_stats_all.csv"))
cityAll.stats$biome <- gsub("flodded", "flooded", cityAll.stats$biome) # Whoops, had a typo!  Not going to reprocess now.
summary(as.factor(cityAll.stats$biome))

cityAll.stats$biomeName <- car::recode(cityAll.stats$biome, 
                                       "'boreal forest/taiga'='Taiga';
                                       'tundra'='Tundra';
                                       'montane grassland/savanna'='Montane Grassland/Savanna';
                                       'temperate broadleaf/mixed forest'='Temperate Broadleaf Forest';
                                       'temperate coniferous forest'='Temperate Conifer Forest';
                                       'temperate grassland/savanna'='Temperate Grassland/Savanna';
                                       'mediterranean'='Mediterranean';
                                       'desert/xeric shrublands'='Desert';
                                       'flooded grassland/savanna'='Flooded Grassland/Savanna';
                                       'tropical grassland/savannas'='Tropical Grassland/Savanna';
                                       'tropical dry broadleaf forest'='Tropical Dry Broadleaf Forest';
                                       'tropical coniferous forest'='Tropical Conifer Forest';
                                       'tropical moist broadleaf forest'='Tropical Moist Broadleaf Forest';
                                       'mangroves'='Mangroves'")

cityAll.stats$biomeClim[grepl("tropical", cityAll.stats$biome) | grepl("flooded", cityAll.stats$biome) | grepl("mangroves", cityAll.stats$biome)] <- "Tropical/Subtropical"
cityAll.stats$biomeClim[grepl("temperate", cityAll.stats$biome)] <- "Temperate"
cityAll.stats$biomeClim[grepl("xeric", cityAll.stats$biome) | grepl("mediterranean", cityAll.stats$biome)] <- "Dry"
cityAll.stats$biomeClim[grepl("taiga", cityAll.stats$biome) | grepl("tundra", cityAll.stats$biome) | grepl("montane", cityAll.stats$biome)] <- "Polar/Montane"
summary(as.factor(cityAll.stats$biomeClim))

cityAll.stats$biomeVeg[grepl("forest", cityAll.stats$biome) | grepl("mangrove", cityAll.stats$biome)] <- "Forest"
cityAll.stats$biomeVeg[grepl("grassland", cityAll.stats$biome)] <- "Grassland/Savanna"
cityAll.stats$biomeVeg[grepl("shrub", cityAll.stats$biome) | grepl("tundra", cityAll.stats$biome) | grepl("mediterranean", cityAll.stats$biome)] <- "Shrubland"
summary(as.factor(cityAll.stats$biomeVeg))
# unique(cityAll.stats$ISO3)

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)
# Get rid of anything that involves the ETmodel stuff
cityAll.stats <- cityAll.stats[,!grepl("ETmodel", names(cityAll.stats))]
cityAll.stats <- cityAll.stats[,!grepl("corr.", names(cityAll.stats))]
cityAll.stats <- cityAll.stats[,!grepl("trend", names(cityAll.stats))]
summary(cityAll.stats)


# Reading in our ET dataset
cityAll.ET <- read.csv(file.path(path.cities, "city_stats_all_ET.csv"))
cityAll.ET$ETmodel.R2adj[cityAll.ET$ETmodel.R2adj<0] <- NA
cityAll.ET <- cityAll.ET[!is.na(cityAll.ET$ETmodel.R2adj),]
summary(cityAll.ET)

cityAll.stats <- cityAll.stats[cityAll.stats$ISOURBID %in% cityAll.ET$ISOURBID[!is.na(cityAll.ET$ETmodel.R2adj)],]
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),9:25])
summary(cityAll.stats)

# Reading in the climate dataset (Terraclimate)
# Units:
#  - temperature (tmax, tmin) - C (month mean)
#  - preciptiation (ppt) - mm/month (total)
#  - evapotranspriation (aet, pet) - mm/month (total)
#  - vapor pressure deficit (def) - kPA (month mean)
#  - soil moisture (soil) - mm (total column; end of month)
cityClim <- read.csv(file.path(path.cities, "city_climatology.csv"))
cityClim$TIME <- as.factor(cityClim$TIME)
summary(cityClim)

# Convert monthly totals to daily means
cityClim[cityClim$LATITUDE<0,c("ppt", "aet", "pet")] <- cityClim[cityClim$LATITUDE<0,c("ppt", "aet", "pet")]/sum(lubridate::days_in_month(1:2))
cityClim[cityClim$LATITUDE>0,c("ppt", "aet", "pet")] <- cityClim[cityClim$LATITUDE>0,c("ppt", "aet", "pet")]/sum(lubridate::days_in_month(7:8))
summary(cityClim)

cityClim <- cityClim[cityClim$TIME=="current" & cityClim$ISOURBID %in% cityAll.ET$ISOURBID,]
summary(cityClim)

# Merge some (current) climate data into the ET estimates
cityAll.ET <- merge(cityAll.ET, cityClim[,c("ISOURBID", "tmin", "tmax", "ppt", "aet")], all.x=T)
cityAll.ET$ppt[cityAll.ET$ppt==0] <- NA
cityAll.ET$et.ppt <- cityAll.ET$ETpred.mean/cityAll.ET$ppt #>1 means more water than comes from the sky during that time
cityAll.ET$aet.ppt <- cityAll.ET$aet/cityAll.ET$ppt #>1 means more water than comes from the sky during that time
summary(cityAll.ET)

plot(ETobs.mean ~ ETpred.mean, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(ETobs.mean ~ aet, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(ETpred.mean ~ aet, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(LST.ET.mean ~ tmax, data=cityAll.ET); abline(a=0, b=1, col="red")
