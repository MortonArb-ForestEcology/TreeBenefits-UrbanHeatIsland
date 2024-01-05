# Looking at the ET Models and doing some predictions of water use and greening scenarios
library(ggplot2)

# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
library(raster)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google)
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v3/")


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
cityAll.ET <- cityAll.ET[!is.na(cityAll.ET$ETmodel.R2adj) & cityAll.ET$ETobs.max>1,] # get rid of anything we didn't model or that has a very low range of ET
summary(cityAll.ET)

cityAll.stats <- cityAll.stats[cityAll.stats$ISOURBID %in% cityAll.ET$ISOURBID[!is.na(cityAll.ET$ETmodel.R2adj)],]
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),9:25])
summary(cityAll.stats)

cityAll.ET <- merge(cityAll.ET, cityAll.stats[,c("ISOURBID", "biome", "biomeName")], all.x=T, all.y=F)
summary(cityAll.ET)

# Reading in the climate datasets -- GLDAS
# Units:
#  - temperature (Tair_f_inst_mean) - K  --> save as C to jive with MODIS data; note: is mean daily air temp; not day surface temp
#  - precipitation (Evap_tavg_mean) - kg/m2/s (= mm/s) --> save as mm/day to jive with MODIS data
#  - evapotranspriation (Rainf_f_tavg_mean) - kg/m2/s (= mm/s) --> save as mm/day to jive with MODIS data

# Setting up some dummy columns for GLDAS data; story
cityAll.ET[,c("ET.GLDAS", "Tmean.GLDAS", "Precip.GLDAS")] <- NA

f.gldas <- dir(path.raw, "GLDAS")
head(f.gldas)

pb <- txtProgressBar(0, nrow(cityAll.ET), style=3)
for(i in 1:nrow(cityAll.ET)){
  setTxtProgressBar(pb, i)
  
  CITY <- cityAll.ET$ISOURBID[i]
  fCity <- grep(CITY, f.gldas)
  
  if(length(fCity)==0) next 
  
  cityClim <- stack(file.path(path.raw, f.gldas[fCity]))
  # plot(cityClim)
  
  etNow <- getValues(cityClim[["Evap_tavg_mean"]])*60*60*24
  precipNow <- getValues(cityClim[["Rainf_f_tavg_mean"]])*60*60*24
  tempNow <- getValues(cityClim[["Tair_f_inst_mean"]])-273.15
  
  etNow <- etNow[!is.na(etNow)]
  precipNow <- precipNow[!is.na(precipNow)]
  tempNow <- tempNow[!is.na(tempNow)]
  
  if(length(etNow)==0) next
  
  if(length(etNow)==1){
    cityAll.ET$ET.GLDAS[i] <- etNow
    cityAll.ET$Precip.GLDAS[i] <- precipNow
    cityAll.ET$Tmean.GLDAS[i] <- tempNow
  } else {
    cityAll.ET$ET.GLDAS[i] <- mean(etNow)
    cityAll.ET$Precip.GLDAS[i] <- mean(precipNow)
    cityAll.ET$Tmean.GLDAS[i] <- mean(tempNow)
  }
  
  rm(cityClim)
  
}
summary(cityAll.ET)
head(cityAll.ET[is.na(cityAll.ET$ET.GLDAS),])

# Comparing our predicted and Observed ET vs precip
cityAll.ET$ETpred.Precip <- cityAll.ET$ETpred.mean/cityAll.ET$Precip.GLDAS # less than 1 means more precip than used by veg
cityAll.ET$ETgldas.Precip <- cityAll.ET$ET.GLDAS/cityAll.ET$Precip.GLDAS # less than 1 means more precip than used by veg
summary(cityAll.ET)

write.csv(cityAll.ET, file.path(path.cities, "city_stats_all_ET-GLDAS.csv"), row.names=F)
# ##########################################


# ##########################################
# Do some data exploration ----
# ##########################################
cityAll.ET <- read.csv(file.path(path.cities, "city_stats_all_ET-GLDAS.csv"))

plot(ETobs.mean ~ ETpred.mean, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(ETobs.mean ~ ET.GLDAS, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(ETpred.mean ~ ET.GLDAS, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(LST.ET.mean ~ Tmean.GLDAS, data=cityAll.ET); abline(a=0, b=1, col="red")

ggplot(data=cityAll.ET) +
  # coord_equal() +
  geom_point(aes(x=ETpred.mean, y=ET.GLDAS, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  scale_color_manual(values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="top")
  
  
# How frequently doe
length(which(cityAll.ET$ETpred.Precip<1))/length(which(!is.na(cityAll.ET$ETpred.Precip)))
length(which(cityAll.ET$ETgldas.Precip<1))/length(which(!is.na(cityAll.ET$ETgldas.Precip)))

