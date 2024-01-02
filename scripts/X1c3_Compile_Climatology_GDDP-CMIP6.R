# Extract & Format CMIP6 data into a single table for easy use

library(raster)
library(ggplot2); library(RColorBrewer); library(cowplot)


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google)
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_GDDP-CMIP6")


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

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)

modsCMIP6 = c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CESM2', 'CESM2-WACCM', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'CanESM5', 'EC-Earth3', 'EC-Earth3-Veg-LR', 'FGOALS-g3', 'GFDL-CM4', 'GFDL-ESM4', 'GISS-E2-1-G', 'HadGEM3-GC31-LL','IITM-ESM', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR', 'KACE-1-0-G', 'KIOST-ESM', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'NESM3','NorESM2-MM', 'TaiESM1', 'UKESM1-0-LL', 'HadGEM3-GC31-MM',  'NorESM2-LM')
modsCMIP6 <- gsub("-", ".", modsCMIP6)

climCombos <- data.frame(Var1=c("tas", "pr"), Var2="historical", Var3=2014)
climCombos <- rbind(climCombos, expand.grid(c("tas", "pr"), c("ssp45", "ssp85"), c("2020", "2050", "2100")))
names(climCombos) <- c("Var", "Sceanrio", "Time")
cityAllCMIP6 <- merge(cityAll.stats[,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE")], climCombos, all=T)
cityAllCMIP6[,modsCMIP6] <- NA
dim(cityAllCMIP6)
summary(cityAllCMIP6)
head(cityAllCMIP6)

f.cmip6 <- dir(path.raw, "CMIP6")
head(f.cmip6)

pb <- txtProgressBar(0, nrow(cityAllCMIP6), style=3)
pbInd = 0
for(CITY in unique(cityAllCMIP6$ISOURBID)){
  # CITY <- cityAllCMIP6$ISOURBID[i]
  print(CITY)
  fCity <- f.cmip6[grep(CITY, f.cmip6)]
  
  if(length(fCity)==0) next 
  
  for(j in 1:length(fCity)){
    fNow <- fCity[j]
    fsplit <- strsplit(fNow, "_")[[1]]
    scenNow <- fsplit[3]
    yrNow <- strsplit(fsplit[4], "-")[[1]][2]
    varNow <- strsplit(fsplit[5], "[.]")[[1]][1]
    if(scenNow=="historical") yrNow = 2014
    
    rowInd <- which(cityAllCMIP6$ISOURBID==CITY & cityAllCMIP6$Var==varNow & cityAllCMIP6$Sceanrio==scenNow & cityAllCMIP6$Time==yrNow)
    
    cityClim <- raster::stack(file.path(path.raw, fNow))
    gcmNames <- names(cityClim)
    gcmMeans <- apply(as.array(cityClim), 3, FUN=mean)
    names(gcmMeans) <- gcmNames
    # gcmMeans
    
    if(varNow=="pr") gcmMeans <- gcmMeans*60*60*24;
    if(varNow=="tas") gcmMeans <- gcmMeans-273.15;
    
    cityAllCMIP6[rowInd,gcmNames] <- gcmMeans
    
    rm(cityClim)
    
    pbInd = pbInd+1
    setTxtProgressBar(pb, pbInd)
  }
  # plot(cityClim)
  
  
  
  
}
summary(cityAllCMIP6)
head(cityAllCMIP6)


write.csv(cityAllCMIP6, file.path(path.cities, "city_stats_all_ET-Clim.csv"), row.names=F)