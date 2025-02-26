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

path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
path.cities <- file.path(path.google, "data_processed_final")
path.et <- file.path(path.google, "ET_models")
path.EEout <- file.path("~/Google Drive/My Drive", "UHI_Analysis_Output_Final_v4")

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

overwrite=F
###########################################


###########################################
# Read in city data & summarizing baseline ----
###########################################

cols.modET <- c("modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom50", "modET.TreeCityBottom25",
                "modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")

if(file.exists(file.path(path.google, "city_stats_all_ET_scenarios.csv")) & !overwrite){
  cityAnalyStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
} else {
  
  cityAll.stats <- read.csv(file.path(path.google, "city_stats_model.csv"))
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
  cityAll.stats$biomeCode <- car::recode(cityAll.stats$biome, 
                                         "'boreal forest/taiga'='Tai';
                                       'tundra'='Tun';
                                       'montane grassland/savanna'='MGS';
                                       'temperate broadleaf/mixed forest'='TeBF';
                                       'temperate coniferous forest'='TeCF';
                                       'temperate grassland/savanna'='TeGS';
                                       'mediterranean'='Med';
                                       'desert/xeric shrublands'='Des';
                                       'flooded grassland/savanna'='FGS';
                                       'tropical grassland/savannas'='TrGS';
                                       'tropical dry broadleaf forest'='TrDBF';
                                       'tropical coniferous forest'='TrCF';
                                       'tropical moist broadleaf forest'='TrMBF';
                                       'mangroves'='Man'")
  summary(cityAll.stats)
  
  CityBuffStats <- read.csv(file.path(path.google, "city_stats_core-buffer.csv"))
  CityBuffStats$factor <- factor(CityBuffStats$factor, levels=c("LST", "tree", "other veg"))
  CityBuffStats <- CityBuffStats[!is.na(CityBuffStats$ISOURBID),]
  summary(CityBuffStats)
  
  CityBuffwide <- CityBuffStats[CityBuffStats$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p")]
  names(CityBuffwide) <- gsub("mean", "LST", names(CityBuffwide))
  CityBuffwide[,c("value.tree.core", "value.tree.diff", "value.tree.diff.p")] <- CityBuffStats[CityBuffStats$factor=="tree", c("value.mean.core", "value.mean.diff", "value.mean.diff.p")]
  summary(CityBuffwide)
  
  # cityLST <- cityBuffAnaly[cityBuffAnaly$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
  # names(cityLST) <- gsub("mean", "LST", names(cityLST))
  # names(cityLST)[which(names(cityLST)=="trend.p.core")] <- "trend.LST.p.core"
  
  cityAnalyStats <- merge(cityAll.stats, CityBuffwide, all=T)
  summary(cityAnalyStats)
  
  
  # cityAll.ET <- read.csv(file.path(path.google, "city_stats_all_ET-GLDAS.csv"))
  # cityAll.ET$ETpixels.prop <- cityAll.ET$n.pixels.ET/cityAll.ET$n.pixels
  # cityAll.ET$ET.cv <- cityAll.ET$ETobs.sd/(cityAll.ET$ETobs.max - cityAll.ET$ETobs.min)
  # summary(cityAll.ET)
  # 
  # plot(ETmodel.R2adj ~ ET.cv, data=cityAll.ET)
  
  # biome.order <- aggregate(Tmean.GLDAS ~ biomeName, data=cityAll.ET, FUN=mean)
  # biome.order <- biome.order[order(biome.order$Tmean.GLDAS),]
  # 
  # cityAnalyStats$biomeName <- factor(cityAnalyStats$biomeName, levels=biome.order$biomeName)
  # cityAll.ET$biomeName <- factor(cityAll.ET$biomeName, levels=biome.order$biomeName)
  # summary(cityAnalyStats)
  
  
  # Add Columns for the stats we want to model
  cityAnalyStats$tree.mean.TreeTargetBiome <- NA # This is the mean of our greening distribution
  cityAnalyStats$tree.mean.TreeTargetBottom25 <- NA # This is the mean of our greening distribution
  cityAnalyStats$tree.mean.TreeCityBottom50 <- NA # This is the mean of our greening distribution
  cityAnalyStats$tree.mean.TreeCityBottom25 <- NA # This is the mean of our greening distribution
  cityAnalyStats[,cols.modET] <- NA
}
summary(cityAnalyStats)
###########################################

###########################################
# Setting up some meta-data for greening 
###########################################

# Calculating the Biome targets
cityAnalyStats$TreeCoverUHINeed <- -cityAnalyStats$value.LST.diff/cityAnalyStats$LSTmodel.tree.slope
cityAnalyStats$TreeCoverTargetUHI <- cityAnalyStats$TreeCoverUHINeed + cityAnalyStats$value.tree.core
summary(cityAnalyStats)

# Doing the targets based only on cities with UHIs to be consistent with our past results
citiesUHI <- which(cityAnalyStats$value.LST.diff>0 & cityAnalyStats$value.LST.diff.p<0.01)

biomeTargetStats <- aggregate(ISOURBID~biomeName, data=cityAnalyStats[citiesUHI,], FUN=length)
# names(biomeTargetStats)
biomeTargetStats[,c("UHI", "CurrentTreeCover", "TargetTreeCover")] <- aggregate(cbind(value.LST.diff, value.tree.core, TreeCoverTargetUHI)~biomeName, data=cityAnalyStats[citiesUHI,], FUN=median)[,c("value.LST.diff", "value.tree.core", "TreeCoverTargetUHI")]
biomeTargetStats


treeBreaks <- seq(0,100, by=5)
treeColLabs <- paste("tree", 
                     paste(stringr::str_pad(treeBreaks[1:(length(treeBreaks)-1)], width=2, side="left", pad=0), 
                           stringr::str_pad(treeBreaks[2:(length(treeBreaks))], width=2, side="left", pad=0), sep="-"),
                     sep="_")

# Setting up some data frames to look at the distribution of tree cover in cities
if(file.exists(file.path(path.google, "TreeDistribution_Current.csv")) & !overwrite){
  treeDistCurrent <- read.csv(file.path(path.google, "TreeDistribution_Current.csv"))
  treeDistGreen <- read.csv(file.path(path.google, "TreeDistribution_Greening-Bottom25.csv"))
  treeDistGreenCity50 <- read.csv(file.path(path.google, "TreeDistribution_Greening-CityBottom50.csv"))
  treeDistGreenCity25 <- read.csv(file.path(path.google, "TreeDistribution_Greening-CityBottom25.csv"))
} else {
  treeDistCurrent <- data.frame(ISOURBID=cityAnalyStats$ISOURBID)
  treeDistCurrent[,treeColLabs] <- NA
  
  treeDistGreen <- treeDistCurrent
  treeDistGreenCity50 <- treeDistCurrent
  treeDistGreenCity25 <- treeDistCurrent
}
summary(treeDistCurrent)
summary(treeDistGreen)
###########################################


###########################################
# Bringing in climate change and running some scenarios ----
###########################################

# Read in CMIP6 metadata 
if(file.exists(file.path(path.google, "city_stats_all_CMIP6_ET.csv")) & !overwrite){
  cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_ET.csv"))
  cmip6$Scenario <- as.factor(cmip6$Scenario)
  cmip6$Time <- as.factor(cmip6$Time)
  
} else {
  cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_deviations.csv"))
  cmip6$Scenario <- as.factor(cmip6$Scenario)
  cmip6$Time <- as.factor(cmip6$Time)
  cmip6 <- cmip6[cmip6$Scenario %in% c("ssp245", "ssp585") & cmip6$Time %in% c(2050, 2100) & !is.na(cmip6$tas.diff),]
  cmip6$modET <- NA
}
summary(cmip6)

###########################################


###########################################
# Reading in an testing a model ----
###########################################
# CITY="CAN15001"
# CITY="USA26687" # Chicago
# rowCity <- which(cityAnalyStats$NAME=="Roanoke" & !is.na(cityAnalyStats$NAME))
# CITY=cityAnalyStats$ISOURBID[rowCity]
# cityAnalyStats[cityAnalyStats$ISOURBID==CITY,]
pb <- txtProgressBar(min=0, max=nrow(cityAnalyStats), style=3)

length(dir(path.EEout))
files.temp <- dir(path.EEout, "GLDAS21_annualMeans")
length(files.temp)
nrow(cityAnalyStats)

for(rowCity in 1:nrow(cityAnalyStats)){
  setTxtProgressBar(pb, rowCity)
  
  if(!overwrite & !any(is.na(cityAnalyStats[rowCity,cols.modET]))) next # If we've done this city, skip it
  
  CITY=cityAnalyStats$ISOURBID[rowCity]
  print(CITY)
  
  if(!any(grepl(CITY, files.temp)) | !dir.exists(file.path(path.cities, CITY))) next
  
  fTemp <- files.temp[grep(CITY, files.temp)]
  
  TempCity <- read.csv(file.path(path.EEout, fTemp[length(fTemp)]))
  TempCity$Tair_f_inst_mean <- TempCity$Tair_f_inst_mean-273.15
  TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")] <- TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")]*60*60*24
  
  dfCity <- read.csv(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")))
  dfCity$Tair.mean <- mean(TempCity$Tair_f_inst_mean, na.rm=T)
  dfCity <- dfCity[dfCity$cityBounds & !is.na(dfCity$tree.mean),]
  # summary(dfCity)
  
  
  # dfCityET <- read.csv(file.path(path.et, CITY, paste0(CITY, "_ET_means.csv")))
  # summary(dfCityET)
  
  if(!dir.exists(file.path(path.et, CITY))) next # This city doesn't have any ET model, so just skip it!
  if(!file.exists(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam-summary.rds")))) next # This city doesn't have any ET model, so just skip it!
  modETCitySum <- readRDS(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam-summary.rds")))
  # modETCitySum
  
  intYear <- which(grepl("year", names(modETCitySum$p.coeff)))
  cityIntercept <- mean(modETCitySum$p.coeff[intYear]) # Taking the mean year intercept
  # yrstr <- paste(names(modETCitySum$p.coeff)[1])
  yrUse <- as.numeric(stringr::str_sub(names(modETCitySum$p.coeff)[intYear[1]], start=-4)) # Using a dummy year just to get the model to run
  
  dfCity$Intercept <- cityIntercept
  
  # Read in the full model
  modETCity <- readRDS(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
  # modETCity <- readRDS("~/Desktop/CAN15001_Model-ET_annual_gam.rds")
  
  
  # ------------------
  # Generating our baseline ET Estimate
  # ------------------
  # Store info about the current tree distribution-- these are proportions 
  nCity <- nrow(dfCity)
  for(j in 2:length(treeBreaks)){
  if(j==2){
    treeDistCurrent[rowCity,j] <- length(which(dfCity$tree.mean>=treeBreaks[j-1] & dfCity$tree.mean<=treeBreaks[j]))/nCity
  } else {
    treeDistCurrent[rowCity,j] <- length(which(dfCity$tree.mean>treeBreaks[j-1] & dfCity$tree.mean<=treeBreaks[j]))/nCity
  }
  }
  # treeDistCurrent[rowCity,]
  # sum(treeDistCurrent[rowCity,2:ncol(treeDistCurrent)])
  write.csv(treeDistCurrent, file.path(path.google, "TreeDistribution_Current.csv"), row.names=F)
  
  dfMod <- data.frame(cover.tree=dfCity$tree.mean, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)

  # Run the model excluding a particular year effect so we can add in the mean intercept; because the model has the response with a sqrt., we need to square this prediction to get us back into ET space
  dfCity$modET.Base <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
  summary(dfCity) 
  
  cityAnalyStats$modET.Base[rowCity] <- mean(dfCity$modET.Base)
  
  # ------------------
  
  # ------------------
  # Greening Scenarios ----
  # ------------------
  treeCitymean <- mean(dfCity$tree.mean)
  treeBiomeRef <- biomeTargetStats$TargetTreeCover[biomeTargetStats$biomeName==cityAnalyStats$biomeName[cityAnalyStats$ISOURBID==CITY]]
  
  # # City with homogenous tree cover (current levels)
  # dfMod <- data.frame(cover.tree=mean(dfCity$tree.mean), cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)
  # dfCity$modET.TreeEven <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
  # 
  # cityAnalyStats$modET.TreeEven[rowCity] <- mean(dfCity$modET.TreeEven)
  # 
  # 
  # # City with homogenous tree cover -- Target Levels
  # 
  # if(length(treeBiomeRef)>0){
  #   cityAnalyStats$tree.mean.TreeTargetBiome[rowCity] <- treeBiomeRef
  #   
  #   dfMod <- data.frame(cover.tree=treeBiomeRef, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)
  #   dfCity$modET.TreeTargetEven <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
  #   
  #   cityAnalyStats$modET.TreeTargetEven[rowCity] <- mean(dfCity$modET.TreeTargetEven)
  # }

    # ------------
  # Bottom-Up approach to greening --> anything less than the biome target gets 1/4 way there (diff * 0.25)
  # ------------
  dfCity$cover.tree.TreeTargetBottom25 <- dfCity$tree.mean
  dfCity$cover.tree.TreeTargetBottom25[dfCity$tree.mean<treeBiomeRef] <- dfCity$tree.mean[dfCity$tree.mean<treeBiomeRef] + (treeBiomeRef-dfCity$tree.mean[dfCity$tree.mean<treeBiomeRef])*0.25
  summary(dfCity)
  
  
  for(j in 2:length(treeBreaks)){
    if(j==2){
      treeDistGreen[rowCity,j] <- length(which(dfCity$cover.tree.TreeTargetBottom25>=treeBreaks[j-1] & dfCity$cover.tree.TreeTargetBottom25<=treeBreaks[j]))/nCity
    } else {
      treeDistGreen[rowCity,j] <- length(which(dfCity$cover.tree.TreeTargetBottom25>treeBreaks[j-1] & dfCity$cover.tree.TreeTargetBottom25<=treeBreaks[j]))/nCity
    }
  }
  # treeDistGreen[rowCity,]
  # sum(treeDistGreen[rowCity,2:ncol(treeDistCurrent)])
  
  write.csv(treeDistGreen, file.path(path.google, "TreeDistribution_Greening-Bottom25.csv"), row.names=F)
  
  dfMod <- data.frame(cover.tree=dfCity$cover.tree.TreeTargetBottom25, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)
  
  png(file.path(path.et, CITY, paste0(CITY, "_TreeCover_Modeled_TargetBottom25.png")), height=4, width=6, units="in", res=180)
  par(mfrow=c(2,1))
  hist(dfCity$tree.mean, xlim=c(0,100))
  hist(dfMod$cover.tree, xlim=c(0,100))
  par(mfrow=c(1,1))
  dev.off()
  
  dfCity$modET.TreeTargetBottom25 <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
  summary(dfCity)
  
  
  cityAnalyStats$tree.mean.TreeTargetBottom25[rowCity] <- mean(dfCity$cover.tree.TreeTargetBottom25)
  cityAnalyStats$modET.TreeTargetBottom25[rowCity] <- mean(dfCity$modET.TreeTargetBottom25)
  # cityAnalyStats[rowCity,]
  # ------------
  
  # ------------
  # 50-precent greening rel to city mean
  # ------------
  # treeDistGreenCity50 <- treeDistCurrent
  # treeDistGreenCity25 <- treeDistCurrent
  
  
  dfCity$cover.tree.TreeCityBottom50 <- dfCity$tree.mean
  dfCity$cover.tree.TreeCityBottom50[dfCity$tree.mean<treeCitymean] <- dfCity$tree.mean[dfCity$tree.mean<treeCitymean] + (treeCitymean-dfCity$tree.mean[dfCity$tree.mean<treeCitymean])*0.5
  summary(dfCity)
  
  
  for(j in 2:length(treeBreaks)){
    if(j==2){
      treeDistGreenCity50[rowCity,j] <- length(which(dfCity$cover.tree.TreeCityBottom50>=treeBreaks[j-1] & dfCity$cover.tree.TreeCityBottom50<=treeBreaks[j]))/nCity
    } else {
      treeDistGreenCity50[rowCity,j] <- length(which(dfCity$cover.tree.TreeCityBottom50>treeBreaks[j-1] & dfCity$cover.tree.TreeCityBottom50<=treeBreaks[j]))/nCity
    }
  }
  # treeDistGreen[rowCity,]
  # sum(treeDistGreen[rowCity,2:ncol(treeDistCurrent)])
  
  write.csv(treeDistGreen, file.path(path.google, "TreeDistribution_Greening-CityBottom50.csv"), row.names=F)
  
  dfMod <- data.frame(cover.tree=dfCity$cover.tree.TreeCityBottom50, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)
  
  png(file.path(path.et, CITY, paste0(CITY, "_TreeCover_CityBottom50.png")), height=4, width=6, units="in", res=180)
  par(mfrow=c(2,1))
  hist(dfCity$tree.mean, xlim=c(0,100))
  hist(dfMod$cover.tree, xlim=c(0,100))
  par(mfrow=c(1,1))
  dev.off()
  
  dfCity$modET.TreeCityBottom50 <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
  summary(dfCity)
  
  
  cityAnalyStats$tree.mean.TreeCityBottom50[rowCity] <- mean(dfCity$cover.tree.TreeCityBottom50)
  cityAnalyStats$modET.TreeCityBottom50[rowCity] <- mean(dfCity$modET.TreeCityBottom50)
  # cityAnalyStats[rowCity,]
  # ------------
  
  # ------------
  # 25-precent greening rel to city mean
  # ------------
  # treeDistGreenCity50 <- treeDistCurrent
  # treeDistGreenCity25 <- treeDistCurrent
  
  
  dfCity$cover.tree.TreeCityBottom25 <- dfCity$tree.mean
  dfCity$cover.tree.TreeCityBottom25[dfCity$tree.mean<treeCitymean] <- dfCity$tree.mean[dfCity$tree.mean<treeCitymean] + (treeCitymean-dfCity$tree.mean[dfCity$tree.mean<treeCitymean])*0.25
  summary(dfCity)
  
  
  for(j in 2:length(treeBreaks)){
    if(j==2){
      treeDistGreenCity25[rowCity,j] <- length(which(dfCity$cover.tree.TreeCityBottom25>=treeBreaks[j-1] & dfCity$cover.tree.TreeCityBottom25<=treeBreaks[j]))/nCity
    } else {
      treeDistGreenCity25[rowCity,j] <- length(which(dfCity$cover.tree.TreeCityBottom25>treeBreaks[j-1] & dfCity$cover.tree.TreeCityBottom25<=treeBreaks[j]))/nCity
    }
  }
  # treeDistGreen[rowCity,]
  # sum(treeDistGreen[rowCity,2:ncol(treeDistCurrent)])
  
  write.csv(treeDistGreen, file.path(path.google, "TreeDistribution_Greening-CityBottom25.csv"), row.names=F)
  
  dfMod <- data.frame(cover.tree=dfCity$cover.tree.TreeCityBottom25, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)
  
  png(file.path(path.et, CITY, paste0(CITY, "_TreeCover_CityBottom25.png")), height=4, width=6, units="in", res=180)
  par(mfrow=c(2,1))
  hist(dfCity$tree.mean, xlim=c(0,100))
  hist(dfMod$cover.tree, xlim=c(0,100))
  par(mfrow=c(1,1))
  dev.off()
  
  dfCity$modET.TreeCityBottom25 <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
  summary(dfCity)
  
  
  cityAnalyStats$tree.mean.TreeCityBottom25[rowCity] <- mean(dfCity$cover.tree.TreeCityBottom25)
  cityAnalyStats$modET.TreeCityBottom25[rowCity] <- mean(dfCity$modET.TreeCityBottom25)
  # cityAnalyStats[rowCity,]
  # ------------
  # ------------------
  
  
  
  
  # ------------------
  # Warming Scenarios ----
  # Adding the mean end-century warming from ssp245, ssp585
  # ------------------
  for(SSP in unique(cmip6$Scenario)){
  print(paste0("--", SSP))
  for(TIME in unique(cmip6$Time)){
    print(paste0("   ", TIME))
    datTmp <- data.frame(cover.tree=dfCity$tree.mean, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean, x=dfCity$x, y=dfCity$y, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)
    
    gcmNow <- unique(cmip6$GCM[cmip6$ISOURBID==CITY & cmip6$Scenario==SSP & cmip6$Time==TIME])
    datTmp[,gcmNow] <- NA
    
    for(GCM in unique(cmip6$GCM[cmip6$ISOURBID==CITY & cmip6$Scenario==SSP & cmip6$Time==TIME])){
      cmip6Row <- which(cmip6$ISOURBID==CITY & cmip6$Scenario==SSP & cmip6$Time==TIME & cmip6$GCM==GCM)
      
      
      warmNow <- cmip6$tas.diff[cmip6Row]
      
      dfMod <- data.frame(cover.tree=dfCity$tree.mean, cover.veg=dfCity$veg.mean, Tair_f_inst_mean=dfCity$Tair.mean+warmNow, x=dfCity$x, y=dfCity$y, year=yrUse, cityBounds=dfCity$cityBounds, Intercept=cityIntercept)

      datTmp[,GCM] <- (predict(modETCity, type="link", exclude="as.factor(year)", newdata=dfMod) + dfCity$Intercept)^2
      summary(datTmp)
      
      cmip6$modET[cmip6Row] <- mean(datTmp[,GCM])
    } # End GCM
    
    summary(datTmp)
    write.csv(datTmp, file.path(path.et, CITY, paste0(CITY, "_ET_", SSP, "_", TIME, ".csv")), row.names=F)
    
    dfCity[,paste("modET", SSP, TIME, sep=".")] <- apply(datTmp[,gcmNow], 1, mean, na.rm=T)
    summary(dfCity)
    
    # This should be the same as doing the mean of the pixel means, but I feel better doing the mean of the regional means here
    cityAnalyStats[rowCity,paste("modET", SSP, TIME, sep=".")] <- mean(cmip6$modET[cmip6$ISOURBID==CITY & cmip6$Scenario==SSP & cmip6$Time==TIME], na.rm=T)
    # summary(cityAnalyStats)
    # cityAnalyStats[rowCity,]
  } # End Time
  
  } # End SSP
  
  write.csv(cmip6, file.path(path.google, "city_stats_all_CMIP6_ET.csv"), row.names=F)
  
  # ------------------
  
  
  write.csv(cityAnalyStats, file.path(path.google, "city_stats_all_ET_scenarios.csv"), row.names=F)

}
