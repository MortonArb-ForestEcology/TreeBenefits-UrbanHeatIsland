# Combine datasets & subset to make analyses more streamlined
# Outline
# 01. Trees cool cities & need water to do so: ---- 
#     Trees have a clear, consistent cooling potential on global urban surface temperatures… [treat cooling capacity as a known; we add a tiny bit of nuance; quickly bring in nuance & water]
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
path.cities <- file.path(path.google, "data_processed_final")
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v4/")

path.figs <- file.path(path.google, "figures_manuscript")
dir.create(path.figs, recursive=T, showWarnings=F)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 1. Read in base datasets -----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 1.1 the baseline data with the LST model
cityAll.stats <- read.csv(file.path(path.google, "city_stats_model.csv"))
cityAll.stats <- cityAll.stats[,!grepl("ET", names(cityAll.stats))] # Get rid of anythign ET from this dataframe
summary(cityAll.stats[,])

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


# 1.2. dataset comparing the city core & buffer so we can quantify UHI & tree deficit
CityBuffStats <- read.csv(file.path(path.google, "city_stats_core-buffer.csv"))
CityBuffStats$factor <- factor(CityBuffStats$factor, levels=c("LST", "tree", "other veg"))
CityBuffStats <- merge(CityBuffStats, cityAll.stats[,c("ISOURBID", "NAME", "LONGITUDE", "LATITUDE", "biomeName", "ES00POP")], all.x=T)
# CityBuffStats <- CityBuffStats[!is.na(CityBuffStats$ISOURBID),]
summary(CityBuffStats)

# 1.3. Read in ET model data
cityAnalyET <- read.csv(file.path(path.google, "city_stats_all_ET.csv"))
cityAnalyET$ETmodel.R2adj[cityAnalyET$ETmodel.R2adj<0] <- NA 
summary(cityAnalyET)
nrow(cityAnalyET)

# 1.4. Read in GLDAS data
cityAll.gldas <- cityAll.stats[,c("ISOURBID", "NAME", "LONGITUDE", "LATITUDE", "biomeName", "biomeCode", "ES00POP")]
pb <- txtProgressBar(min=0, max=nrow(cityAll.gldas), style=3)
for(i in 1:nrow(cityAll.gldas)){
  CITY <- cityAll.gldas$ISOURBID[i]
  cityGLDAS <- read.csv(file.path(path.raw, paste0(CITY, "_GLDAS21_annualMeans.csv")))
  cityAll.gldas[i,c("ET.GLDAS", "Precip.GLDAS", "Tmean.GLDAS")] <- colMeans(cityGLDAS[,c("Evap_tavg_mean", "Rainf_f_tavg_mean", "Tair_f_inst_mean")], na.rm=T)
  
  cityAll.gldas$ET.GLDAS[i] <- cityAll.gldas$ET.GLDAS[i]*60*60*24
  cityAll.gldas$Precip.GLDAS[i] <- cityAll.gldas$Precip.GLDAS[i]*60*60*24
  cityAll.gldas$Tmean.GLDAS[i] <- cityAll.gldas$Tmean.GLDAS[i]-273.15
  
  setTxtProgressBar(pb, i)
}

summary(cityAll.gldas)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Step 2: Figure out what our criteria for cities to show is & subset it ----
#  -- enough data to do the temporal trend analysis (removes 96 cities)
#  -- R2 cutoff?
#  -- Max Tree & veg cover for any pixel >10% (allow robust parameterization)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Number of cities showing stat.sig UHI (p<0.01)
citiesUHI <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig]
length(citiesUHI)
nrow(cityAll.stats)


summary(CityBuffStats)

# Getting rid of 4-sigma outliers for difference between metro core & reference region
# -- This could be because of data errors, topography, etc.
lstDiffMean <- mean(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST"], na.rm=T)
lstDiffSD <- sd(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST"], na.rm=T)
lstDiffMean; lstDiffSD

cityDiffCOLD <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<lstDiffMean-4*lstDiffSD & !is.na(CityBuffStats$value.mean.diff)]
cityDiffHOT <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>lstDiffMean+4*lstDiffSD & !is.na(CityBuffStats$value.mean.diff)]

# CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$ISOURBID %in% cityDiffCOLD,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "value.mean.core", "value.mean.buffer", "value.mean.diff", "value.mean.diff.sig")]
# CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$ISOURBID %in% cityDiffHOT,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "value.mean.core", "value.mean.buffer", "value.mean.diff", "value.mean.diff.sig")]

cityAll.stats[cityAll.stats$ISOURBID %in% cityDiffCOLD,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biome.prop", "n.pixels")]
cityAll.stats[cityAll.stats$ISOURBID %in% cityDiffHOT,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biome.prop", "n.pixels")]

# Getting rid of 4-sigma outliers for difference between metro core & reference region --> there are none!
hist(CityBuffStats$value.mean.core[CityBuffStats$factor=="LST"])
summary(CityBuffStats$value.mean.core[CityBuffStats$factor=="LST"])
summary(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="LST"])

lstCoreMean <- mean(CityBuffStats$value.mean.core[CityBuffStats$factor=="LST"], na.rm=T)
lstCoreSD <- sd(CityBuffStats$value.mean.core[CityBuffStats$factor=="LST"], na.rm=T)
lstCoreMean; lstCoreSD

lstBuffMean <- mean(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="LST"], na.rm=T)
lstBuffSD <- sd(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="LST"], na.rm=T)
lstCoreMean; lstCoreSD

cityCoreCOLD  <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.core<lstCoreMean-4*lstCoreSD & !is.na(CityBuffStats$value.mean.core)]
cityCoreHOT <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.core>lstCoreMean+4*lstCoreSD & !is.na(CityBuffStats$value.mean.core)]


CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$ISOURBID %in% cityCoreCOLD,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "value.mean.core", "value.mean.buffer", "value.mean.diff", "value.mean.diff.sig")]
CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$ISOURBID %in% cityCoreHOT,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "value.mean.core", "value.mean.buffer", "value.mean.diff", "value.mean.diff.sig")]

# Getting rid of 4-sigma outliers for LST std dev in the region --> most of these are mountainous or ecotonal cities that don't meet other criteria, but they also capture some of our oddest slopes too
lstSDMean <- mean(cityAll.stats$LST.sd, na.rm=T)
lstSDSD <- sd(cityAll.stats$LST.sd, na.rm=T)
lstSDMean; lstSDSD
citySDLo  <- cityAll.stats$ISOURBID[cityAll.stats$LST.sd<lstSDMean-4*lstSDSD]
citySDHi  <- cityAll.stats$ISOURBID[cityAll.stats$LST.sd>lstSDMean+4*lstSDSD]
cityNoET <- cityAll.stats$ISOURBID[!cityAll.stats$ISOURBID %in% unique(cityAnalyET$ISOURBID[!is.na(cityAnalyET$ETmodel.R2adj)])]
length(cityNoET)
# cityAll.stats[cityAll.stats$ < 1.0,]

summary(cityAll.stats)

citiesUse <- cityAll.stats$n.pixels>=1000 & 
  cityAll.stats$biome.prop>=0.75 &
  # cityAll.stats$tree.max>10 & cityAll.stats$veg.max>10 &
  cityAll.stats$LST.sd >=1 & cityAll.stats$tree.sd >= 1 & cityAll.stats$veg.sd >= 1 & cityAll.stats$elev.sd >= 1 &
  !cityAll.stats$ISOURBID %in% c(cityDiffHOT, cityDiffCOLD, cityCoreHOT, cityCoreCOLD, citySDLo, citySDHi) & cityAll.stats$ISOURBID %in% unique(cityAnalyET$ISOURBID[!is.na(cityAnalyET$ETmodel.R2adj)])


length(which(!cityAll.stats$ISOURBID %in% citiesUHI)) # NOT CRITERIA, but good to know

nrow(cityAll.stats)
# length(which(is.na(cityAll.stats$trend.LST.slope))) # Insufficient data temporally
length(which(cityAll.stats$n.pixels<1000)) # Too few pixels
length(which(cityAll.stats$biome.prop<0.75)) # Too heterogenous a biome
length(cityNoET)
length(which(cityAll.stats$LST.sd<1)) # Too low variation in LST
length(which(cityAll.stats$tree.sd<1)) # Too low variation in tree cover
length(which(cityAll.stats$veg.sd<1)) # Too low variation in Veg
length(which(cityAll.stats$elev.sd<1)) # Too low variation in topography
length(cityDiffHOT)
length(cityDiffCOLD)
length(citySDHi)
length(citySDLo)
# length(cityCoreHOT)
# length(cityCoreCOLD)

length(which(citiesUse)) # Final number 

# UHI-only 
# cityStatsAnaly <- cityAll.stats[which(citiesUse & cityAll.stats$ISOURBID %in% citiesUHI),]
# cityBuffAnaly <- CityBuffStats[CityBuffStats$ISOURBID %in% cityStatsAnaly$ISOURBID,]

cityStatsAnaly <- cityAll.stats[which(citiesUse),]
cityStatsET <- cityAnalyET[which(citiesUse),]
cityBuffAnaly <- CityBuffStats[CityBuffStats$ISOURBID %in% cityStatsAnaly$ISOURBID,]
cityGLDASAnaly <- cityAll.gldas[cityAll.gldas$ISOURBID %in% cityStatsAnaly$ISOURBID,c("ISOURBID", "NAME","Tmean.GLDAS", "Precip.GLDAS", "ET.GLDAS")]
summary(cityStatsAnaly)
summary(cityStatsET)
summary(cityGLDASAnaly)
summary(as.factor(cityStatsAnaly$biomeName))
dim(cityStatsAnaly)

# cityStatsAnaly[cityStatsAnaly$LSTmodel.tree.slope < -1.0,]
cityStatsAnaly[cityStatsAnaly$LSTmodel.R2adj < 0.35,]
cityStatsET[cityStatsET$ETmodel.R2adj < 0.35,]

nrow(cityStatsAnaly)


cityTree <- cityBuffAnaly[cityBuffAnaly$factor=="tree", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.sig")]
names(cityTree) <- gsub("mean", "tree", names(cityTree))
# names(cityTree)[which(names(cityTree)=="trend.p.core")] <- "trend.tree.p.core"

cityOther <- cityBuffAnaly[cityBuffAnaly$factor=="other veg", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.sig")]
names(cityOther) <- gsub("mean", "other", names(cityOther))
# names(cityOther)[which(names(cityOther)=="trend.p.core")] <- "trend.other.p.core"

cityVeg <- merge(cityTree, cityOther, all=T)
summary(cityVeg)

cityLST <- cityBuffAnaly[cityBuffAnaly$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.sig")]
names(cityLST) <- gsub("mean", "LST", names(cityLST))
# names(cityLST)[which(names(cityLST)=="trend.p.core")] <- "trend.LST.p.core"

summary(cityStatsAnaly[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE", "LSTslope.tree", "LSTmodelFinal.tree.p", "LSTslope.veg", "LSTmodelFinal.veg.p")])

StatsCombined <- merge(cityStatsAnaly[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE", "LSTslope.tree", "LSTmodelFinal.tree.p", "LSTslope.veg", "LSTmodelFinal.veg.p",
                                         "LST.mean", "LST.sd", "LST.min", "LST.max",
                                         "tree.mean", "tree.sd", "tree.min", "tree.max",
                                         "veg.mean", "veg.sd", "veg.min", "veg.max")],
                       cityGLDASAnaly, all=T)
StatsCombined <- merge(StatsCombined, cityVeg, all=T)
StatsCombined <- merge(StatsCombined, cityLST, all=T)
StatsCombined <- merge(StatsCombined, cityStatsET[,c("ISOURBID", "ETobs.mean", "ETobs.sd", "ETobs.min", "ETobs.max", "ETmodel.R2adj", "ETmodel.RMSE", "ETpred.mean", "ETpred.sd", "ETpred.min", "ETpred.max")], all=T)
summary(StatsCombined)

# Quick aggregation of cities by GLDAS Mean Summer Temp and Mean Summer Precip to have a consistent order to our cities
biome.order <- aggregate(cbind(Tmean.GLDAS, Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)
biome.order$n.cities <- aggregate(cbind(Tmean.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN=length)[,"Tmean.GLDAS"]
biome.order <- biome.order[order(biome.order$Tmean.GLDAS),]
biome.order

biomeAll <- biome.order
sum(biomeAll$n.cities[biome.order$n.cities<20])

sum(biomeAll$n.cities)

# Wokring only with cities where there are >20 cities in the biome
biome.order <- biome.order[biome.order$n.cities>=20,]
biome.order
sum(biome.order$n.cities)

write.csv(biome.order, file.path(path.google, "UHIs-FinalCityDataForAnalysis_BiomeOrder.csv"), row.names=F)


# **** Saving this combined & formatted dataset for collaborators or other sharing **** ----
length(which(!StatsCombined$biomeName %in% biome.order$biomeName))
StatsCombined <- StatsCombined[StatsCombined$biomeName %in% biome.order$biomeName,]
nrow(StatsCombined)

write.csv(StatsCombined, file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"), row.names=F)

write.csv(StatsCombined, file.path(path.google, "SupplementalData-1_UHIs-FinalCityDataForAnalysis.csv"), row.names=F)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
