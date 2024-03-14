###########################################
# Script Description & Outline ----
###########################################
# Purpose: Test out some new figures for the resubmission -- putting everything into temperature frame for Nature Cities

# Manuscript Outline
library(ggplot2); library(RColorBrewer); library(cowplot)
library(ggalt); library(sf)

###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")

path.figs <- file.path(path.google, "figures_manuscript")
dir.create(path.figs, recursive=T, showWarnings=F)


grad.lst <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
grad.lstHot <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026") # ends with red

grad.treeDiff <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") # ends with teal
grad.tree <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529") # ends with green

grad.otherDiff <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e7f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419") # Green
grad.other <- c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081")

grad.modfit <- c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177", "#49006a")


biome.order <-c('Taiga'='Tai',
                'Tundra'='Tun',
                'Montane Grassland/Savanna'='MGS',
                'Temperate Broadleaf Forest'='TeBF',
                'Temperate Conifer Forest'='TeCF',
                'Temperate Grassland/Savanna'='TeGS',
                'Mediterranean'='Med',
                'Desert'='Des',
                'Flooded Grassland/Savanna'='FGS',
                'Tropical Grassland/Savanna'='TrGS',
                'Tropical Dry Broadleaf Forest'='TrDBF',
                'Tropical Conifer Forest'='TrCF',
                'Tropical Moist Broadleaf Forest'='TrMBF',
                'Mangroves'='Man')
biome.order


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

biomeCode.pall.all = c("Tai"= "#2c5c74", 
                       "Tun"="#6d8e9d",
                       "TeBF" = "#7f310f",
                       "TeCF" = "#4d1e10",
                       "TeGS" = "#b09c41",
                       "MGS" = "#a0b8c7",
                       "Med" = "#bf772e",
                       "Des" = "#c89948",
                       "FGS" = "#e0dfa1",
                       "TrGS" = "#a6b39e",
                       "TrDBF" = "#7a9c64",
                       "TrCF" = "#488458",
                       "TrMBF"= "#266240",
                       "Man" = "#9c8c94")

library(sp); library(sf)
library(rworldmap)

world <- map_data("world")
projLongLat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projRobin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
projWinTri <- "+proj=wintri"

worldR <- getMap()
worldBBox <- Polygon(matrix(c(-180, -90,
                              -180, 83.64513,  
                              180, 83.64513,
                              180, -90), ncol=2, byrow=T))
worldBBox <- st_sf(geometry = st_sfc(
  st_polygon(x = list(cbind(c(-180, rep(180, 100), rep(-180, 100)),
                            c(-90, seq(-90, 83.64513, length = 100), 
                              seq(83.64513, -90, length = 100))))),
  crs = 'WGS84'))

worldBBox <- SpatialPolygons(list(Polygons(list(worldBBox), ID="a")), proj4string = CRS(projLongLat))
worldRobin <- fortify(spTransform(worldR, CRS(projRobin)))
worldWinTri <- fortify(spTransform(worldR, CRS(projWinTri)))
worldBBoxRobin <- st_transform(worldBBox, CRS(projRobin))
worldBBoxWinTri <- st_transform(worldBBox, CRS(projWinTri))
##########################################

# Generated in script 07_Manuscript_Analysis[...]
StatsCombined <- read.csv(file.path(path.cities, "..", "UHIs-FinalCityDataForAnalysis.csv"))
summary(StatsCombined)

# Start by summarizing Patterns of UHI & Vegetation in Cities
biomeSummary <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[,], FUN=length)
names(biomeSummary)[3] <- "Total.N"
biomeSummary$LSTmed <- aggregate(value.LST.core ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
biomeSummary$UHI.med <- aggregate(value.LST.diff ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
biomeSummary$Tree.med <- aggregate(value.tree.core ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
biomeSummary$TreeDiff.med <- aggregate(value.tree.diff ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
biomeSummary$OtherVeg.med <- aggregate(value.other.core ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
biomeSummary$OtherVegDiff.med <- aggregate(value.other.diff ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
biomeSummary

# Characterizing Cities with observed UHI UHI
citiesUHI <- which(StatsCombined$value.LST.diff>0)
length(citiesUHI); nrow(StatsCombined); length(citiesUHI)/nrow(StatsCombined)

biomeUHISummary <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[citiesUHI,], FUN=length)
names(biomeUHISummary)[3] <- "citiesUHI.N"
biomeUHISummary$citiesUHI.LSTmed <- aggregate(value.LST.core ~ biomeName, data=StatsCombined[citiesUHI,], FUN=median, na.rm=T)[,2]
biomeUHISummary$UHI.UHI.med <- aggregate(value.LST.diff ~ biomeName, data=StatsCombined[citiesUHI,], FUN=median, na.rm=T)[,2]
biomeUHISummary$UHI.Tree.med <- aggregate(value.tree.core ~ biomeName, data=StatsCombined[citiesUHI,], FUN=median, na.rm=T)[,2]
biomeUHISummary$UHI.TreeDiff.med <- aggregate(value.tree.diff ~ biomeName, data=StatsCombined[citiesUHI,], FUN=median, na.rm=T)[,2]
biomeUHISummary$UHI.OtherVeg.med <- aggregate(value.other.core ~ biomeName, data=StatsCombined[citiesUHI,], FUN=median, na.rm=T)[,2]
biomeUHISummary$UHI.OtherVegDiff.med <- aggregate(value.other.diff ~ biomeName, data=StatsCombined[citiesUHI,], FUN=median, na.rm=T)[,2]
biomeUHISummary <- merge(biomeSummary[c("biomeName", "biomeCode", "Total.N")], biomeUHISummary, all=T)
biomeUHISummary$citiesUHI.p <- round(biomeUHISummary$citiesUHI.N/biomeUHISummary$Total.N, 2)
biomeUHISummary

# Characterizing Cities with UHI & Warming
citiesWarmUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$trend.LST.core>0)
length(citiesWarmUHI); length(citiesUHI); nrow(StatsCombined); length(citiesWarmUHI)/length(citiesUHI); length(citiesWarmUHI)/nrow(StatsCombined)

biomeWarmUHISummary <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[citiesWarmUHI,], FUN=length)
names(biomeWarmUHISummary)[3] <- "citiesWarmUHI.N"
biomeWarmUHISummary$citiesWarmUHI.LSTmed <- aggregate(value.LST.core ~ biomeName, data=StatsCombined[citiesWarmUHI,], FUN=median, na.rm=T)[,2]
biomeWarmUHISummary$WarmUHI.WarmUHI.med <- aggregate(value.LST.diff ~ biomeName, data=StatsCombined[citiesWarmUHI,], FUN=median, na.rm=T)[,2]
biomeWarmUHISummary$WarmUHI.Tree.med <- aggregate(value.tree.core ~ biomeName, data=StatsCombined[citiesWarmUHI,], FUN=median, na.rm=T)[,2]
biomeWarmUHISummary$WarmUHI.TreeDiff.med <- aggregate(value.tree.diff ~ biomeName, data=StatsCombined[citiesWarmUHI,], FUN=median, na.rm=T)[,2]
biomeWarmUHISummary$WarmUHI.OtherVeg.med <- aggregate(value.other.core ~ biomeName, data=StatsCombined[citiesWarmUHI,], FUN=median, na.rm=T)[,2]
biomeWarmUHISummary$WarmUHI.OtherVegDiff.med <- aggregate(value.other.diff ~ biomeName, data=StatsCombined[citiesWarmUHI,], FUN=median, na.rm=T)[,2]
biomeWarmUHISummary <- merge(biomeSummary[c("biomeName", "biomeCode", "Total.N")], biomeWarmUHISummary, all=T)
biomeWarmUHISummary$citiesWarmUHI.p <- round(biomeWarmUHISummary$citiesWarmUHI.N/biomeWarmUHISummary$Total.N, 2)
biomeWarmUHISummary

# Characterizing Cities with intensifying UHI
citiesIntensifyUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$trend.LST.diff>0)
length(citiesIntensifyUHI); length(citiesUHI); nrow(StatsCombined); length(citiesIntensifyUHI)/length(citiesUHI); length(citiesIntensifyUHI)/nrow(StatsCombined)

biomeIntensifyUHISummary <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[citiesIntensifyUHI,], FUN=length)
names(biomeIntensifyUHISummary)[3] <- "citiesIntensifyUHI.N"
biomeIntensifyUHISummary$citiesIntensifyUHI.LSTmed <- aggregate(value.LST.core ~ biomeName, data=StatsCombined[citiesIntensifyUHI,], FUN=median, na.rm=T)[,2]
biomeIntensifyUHISummary$IntensifyUHI.IntensifyUHI.med <- aggregate(value.LST.diff ~ biomeName, data=StatsCombined[citiesIntensifyUHI,], FUN=median, na.rm=T)[,2]
biomeIntensifyUHISummary$IntensifyUHI.Tree.med <- aggregate(value.tree.core ~ biomeName, data=StatsCombined[citiesIntensifyUHI,], FUN=median, na.rm=T)[,2]
biomeIntensifyUHISummary$IntensifyUHI.TreeDiff.med <- aggregate(value.tree.diff ~ biomeName, data=StatsCombined[citiesIntensifyUHI,], FUN=median, na.rm=T)[,2]
biomeIntensifyUHISummary$IntensifyUHI.OtherVeg.med <- aggregate(value.other.core ~ biomeName, data=StatsCombined[citiesIntensifyUHI,], FUN=median, na.rm=T)[,2]
biomeIntensifyUHISummary$IntensifyUHI.OtherVegDiff.med <- aggregate(value.other.diff ~ biomeName, data=StatsCombined[citiesIntensifyUHI,], FUN=median, na.rm=T)[,2]
biomeIntensifyUHISummary <- merge(biomeSummary[c("biomeName", "biomeCode", "Total.N")], biomeIntensifyUHISummary, all=T)
biomeIntensifyUHISummary$citiesIntensifyUHI.p <- round(biomeIntensifyUHISummary$citiesIntensifyUHI.N/biomeIntensifyUHISummary$Total.N, 2)
biomeIntensifyUHISummary

# Characterizing Cities with any Warming
citiesWarm <- which(StatsCombined$trend.LST.core>0)
length(citiesWarm); length(citiesUHI); nrow(StatsCombined); length(citiesWarm)/nrow(StatsCombined)

biomeWarmSummary <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[citiesWarm,], FUN=length)
names(biomeWarmSummary)[3] <- "citiesWarm.N"
biomeWarmSummary$citiesWarm.LSTmed <- aggregate(value.LST.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
biomeWarmSummary$Warm.Warm.med <- aggregate(value.LST.diff ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
biomeWarmSummary$Warm.Tree.med <- aggregate(value.tree.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
biomeWarmSummary$Warm.TreeDiff.med <- aggregate(value.tree.diff ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
biomeWarmSummary$Warm.OtherVeg.med <- aggregate(value.other.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
biomeWarmSummary$Warm.OtherVegDiff.med <- aggregate(value.other.diff ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
biomeWarmSummary <- merge(biomeSummary[c("biomeName", "biomeCode", "Total.N")], biomeWarmSummary, all=T)
biomeWarmSummary$citiesWarm.p <- round(biomeWarmSummary$citiesWarm.N/biomeWarmSummary$Total.N, 2)
biomeWarmSummary


cbind(biomeWarmSummary[,c("biomeName", "biomeCode", "Total.N", "citiesWarm.N", "citiesWarm.p")],
      biomeUHISummary[,c("citiesUHI.N", "citiesUHI.p")], 
      biomeWarmUHISummary[,c("citiesWarmUHI.N", "citiesWarmUHI.p")],
      biomeIntensifyUHISummary[,c("citiesIntensifyUHI.N", "citiesIntensifyUHI.p")])




cbind(biomeUHISummary[,c("biomeName", "biomeCode", "Total.N", "citiesUHI.N", "citiesUHI.p")], 
      biomeIntensifyUHISummary[,c("citiesIntensifyUHI.N", "citiesIntensifyUHI.p")],
      biomeWarmSummary[,c("citiesWarm.N", "citiesWarm.p")],
      biomeWarmUHISummary[,c("citiesWarmUHI.N", "citiesWarmUHI.p")]
      )











# StatsCombined$LSTTrend.Tree <- StatsCombined$trend.tree.core*StatsCombined$model.tree.slope 
# StatsCombined$LSTTrend.Other <- StatsCombined$trend.other.core*StatsCombined$model.veg.slope 
# StatsCombined$TargetConstantUHI <- -StatsCombined$trend.LST.diff/StatsCombined$model.tree.slope + StatsCombined$trend.tree.core
# StatsCombined$TargetOffsetWarming <- -StatsCombined$trend.LST.core/StatsCombined$model.tree.slope + StatsCombined$trend.tree.core

StatsCombined$EstLST2001 <- StatsCombined$value.LST.core - StatsCombined$trend.LST.core*10
StatsCombined$EstLST2020 <- StatsCombined$value.LST.core + StatsCombined$trend.LST.core*10
# StatsCombined$EstLST2020Trees <- StatsCombined$value.LST.core + 20*StatsCombined$trend.tree.core*StatsCombined$model.tree.slope 
StatsCombined$TargetLST2020NoUHI <- StatsCombined$EstLST2020 - StatsCombined$trend.LST.diff*20
StatsCombined$TargetLST2020NoWarming <- StatsCombined$EstLST2001 - 0*20
summary(StatsCombined)
summary(StatsCombined[StatsCombined$trend.LST.diff>0,])

citiesWarm <- which(StatsCombined$trend.LST.core>0)
intensifyUHI <- which(StatsCombined$trend.LST.diff>0 & StatsCombined$trend.LST.core>0)
nrow(StatsCombined); length(citiesWarm); length(intensifyUHI)


ggplot(data=StatsCombined[citiesWarm,]) +
  ggtitle("Warming Cities") +
  facet_wrap(~biomeCode) +
  geom_segment(aes(x=2001, xend=2020, y=EstLST2001-EstLST2001, yend=EstLST2020-EstLST2001), size=0.1, alpha=0.7, color="gray80") +
  geom_hline(yintercept=0, color="red3", size=0.2) +
  theme_classic()
  
ggplot(data=StatsCombined[citiesIntenseUHI,]) +
  ggtitle("Warming + Intensifying UHI Cities") +
  facet_wrap(~biomeCode) +
  geom_segment(aes(x=2001, xend=2020, y=EstLST2001-EstLST2001, yend=EstLST2020-EstLST2001), size=0.1, alpha=0.7, color="gray80") +
  geom_hline(yintercept=0, color="red3", size=0.2) +
  theme_classic()


# NTot <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[,], FUN=length)
# names(NTot)[3] <- "N.Total"


TrendLSTsObs <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined[,], FUN=length)
names(TrendLSTsObs)[3] <- "N.Analyzed"
TrendLSTsObs$EstLST2001 <- aggregate(EstLST2001 ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
TrendLSTsObs$EstLST2020 <- aggregate(EstLST2020 ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
TrendLSTsObs$TrendObsLSTMed <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[,], FUN=median, na.rm=T)[,2]
TrendLSTsObs$TrendObsLSTlo <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[,], FUN=quantile, 0.25)[,2]
TrendLSTsObs$TrendObsLSThi <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[,], FUN=quantile, 0.75)[,2]
TrendLSTsObs

# TrendLSTsObs <- merge(NTot, TrendLSTsObs, all=T)

# Removed subsetting Intensifying UHI from this # intensifyUHI
# intensifyUHI <- StatsCombined$value.LST.diff>0 & StatsCombined$trend.LST.core>0 & StatsCombined$trend.LST.diff>0 & StatsCombined$trend.LST.diff.p<0.01
# length(which(intensifyUHI))/length(citiesUHI)

TrendLSTsUHI <- aggregate(ISOURBID ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=length)
names(TrendLSTsUHI)[2] <- "N.UHI"
TrendLSTsUHI$TrendUHIdiffMed <- aggregate(trend.LST.diff ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendLSTsUHI$TrendUHIdiffLo <- aggregate(trend.LST.diff ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendLSTsUHI$TrendUHIdiffHi <- aggregate(trend.LST.diff ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendLSTsUHI$TargetUHILSTMed <- aggregate(TargetLST2020NoUHI ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendLSTsUHI$TargetUHILSTLo <- aggregate(TargetLST2020NoUHI ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendLSTsUHI$TargetUHILSTHi <- aggregate(TargetLST2020NoUHI ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendLSTsUHI <- merge(TrendLSTsObs, TrendLSTsUHI, all=T)
TrendLSTsUHI

# TableTrendsUHI <- data.frame(Biome=TrendLSTsUHI$biomeName,
#                              N.Analyzed = TrendLSTsUHI$N.Analyzed,
#                              N.IntensifyUHI = TrendLSTsUHI$N.UHI,
#                              UHITrend = paste0(round(TrendLSTsUHI$TrendUHIMed, 2), " (", round(TrendLSTsUHI$TrendUHIlo, 2), " - ", round(TrendLSTsUHI$TrendUHIhi, 2),")"),
#                              UHILSTTrend = paste0(round(TrendLSTsUHI$TrendUHILSTMed, 2), " (", round(TrendLSTsUHI$TrendUHILSTlo, 2), " - ", round(TrendLSTsUHI$TrendUHILSThi, 2),")"),
#                              UHILSTTarget = paste0(round(TrendLSTsUHI$TargetUHILSTMed, 2), " (", round(TrendLSTsUHI$TargetUHILSTlo, 2), " - ", round(TrendLSTsUHI$TargetUHILSThi, 2),")"))
# TableTrendsUHI <- TableTrendsUHI[order(TableTrendsUHI$Biome),]
# TableTrendsUHI
# 
# write.csv(TableTrendsUHI, file.path(path.figs, "SuppTable4_Biome_LSTTrendTargets-UHI.csv"), row.names=F)


# Adding back in filter for warming cities period
# citiesWarm <- StatsCombined$trend.LST.core>0 & StatsCombined$trend.LST.p.core < 0.01
# length(which(citiesWarm))/nrow(StatsCombined)

TrendLSTsWarm <- aggregate(ISOURBID ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=length)
names(TrendLSTsWarm)[2] <- "N.Warm"
TrendLSTsWarm$TrendWarmMed <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendLSTsWarm$TrendWarmlo <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendLSTsWarm$TrendWarmhi <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
# TrendLSTsWarm$TrendWarmLSTMed <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
# TrendLSTsWarm$TrendWarmLSTlo <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
# TrendLSTsWarm$TrendWarmLSThi <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendLSTsWarm$TargetWarmLSTMed <- aggregate(TargetLST2020NoWarming ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendLSTsWarm$TargetWarmLSTlo <- aggregate(TargetLST2020NoWarming ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendLSTsWarm$TargetWarmLSThi <- aggregate(TargetLST2020NoWarming ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendLSTsWarm <- merge(TrendLSTsObs, TrendLSTsWarm, all=T)
TrendLSTsWarm


# TableTrendsWarm <- data.frame(Biome=TrendLSTsWarm$biomeName,
#                               N.Analyzed = TrendLSTsWarm$N.Analyzed,
#                               N.IntensifyWarm = TrendLSTsWarm$N.Warm,
#                               WarmTrend = paste0(round(TrendLSTsWarm$TrendWarmMed, 2), " (", round(TrendLSTsWarm$TrendWarmlo, 2), " - ", round(TrendLSTsWarm$TrendWarmhi, 2),")"),
#                               WarmLSTTrend = paste0(round(TrendLSTsWarm$TrendWarmLSTMed, 2), " (", round(TrendLSTsWarm$TrendWarmLSTlo, 2), " - ", round(TrendLSTsWarm$TrendWarmLSThi, 2),")"),
#                               WarmLSTTarget = paste0(round(TrendLSTsWarm$TargetWarmLSTMed, 2), " (", round(TrendLSTsWarm$TargetWarmLSTlo, 2), " - ", round(TrendLSTsWarm$TargetWarmLSThi, 2),")"))
# TableTrendsWarm <- TableTrendsWarm[order(TableTrendsWarm$Biome),]
# TableTrendsWarm
# 
# write.csv(TableTrendsWarm, file.path(path.figs, "SuppTable5_Biome_LSTTrendTargets-Warm.csv"), row.names=F)



# # # Making the slope figure (Fig. 3) ----
TrendsLSTAll <- merge(TrendLSTsUHI, TrendLSTsWarm, all=T)
# TrendsLSTAll$YendUHI <- TrendsLSTAll$EstLST2001+20*TrendsLSTAll$TargetUHILSTMed
# TrendsLSTAll$YendWarm <- TrendsLSTAll$EstLST2001+20*TrendsLSTAll$TargetWarmLSTMed
TrendsLSTAll$YendUHI <- TrendsLSTAll$TargetUHILSTMed
TrendsLSTAll$YendWarm <- TrendsLSTAll$TargetWarmLSTMed

# Creating a vector to help annotate
TrendsLSTAll$YlabUHI <-TrendsLSTAll$YendUHI
TrendsLSTAll$YlabWarm <-TrendsLSTAll$YendWarm
TrendsLSTAll


summary(StatsCombined[StatsCombined$biomeName %in% TrendsLSTAll$biomeName[TrendsLSTAll$N.Analyzed>=50] & StatsCombined$trend.LST.diff>0 & StatsCombined$trend.LST.core>0,])
# figTrends <- ggplot(data=TrendsLSTAll[TrendsLSTAll$N.Analyzed>=50,]) +
ggplot(data=TrendsLSTAll[TrendsLSTAll$N.Analyzed>=20,]) +
  facet_wrap(~biomeCode) +
  geom_segment(data=StatsCombined[StatsCombined$biomeName %in% TrendsLSTAll$biomeName[TrendsLSTAll$N.Analyzed>=20] & StatsCombined$trend.LST.diff>0 & StatsCombined$trend.LST.core>0 ,], aes(x=2001, xend=2020, y=EstLST2001-EstLST2001, yend=EstLST2020-EstLST2001), size=0.1, alpha=0.7, color="gray80") +
  geom_segment(aes(x=2001, xend=2020, y=EstLST2001-EstLST2001, yend=EstLST2020-EstLST2001, color="Observed Trend"), size=2) +
  geom_segment(aes(x=2001, xend=2020, y=EstLST2001-EstLST2001, yend=YendUHI-EstLST2001, color="Mitigate Intensifying UHI"), size=2, linetype="dashed") +
  geom_segment(aes(x=2001, xend=2020, y=EstLST2001-EstLST2001, yend=YendWarm-EstLST2001, color="Mitigate Warming Trend"), size=2, linetype="dashed") +
  # geom_text(aes(x=2020, y=EstLST2020-1, label=paste0("+",round(20*TrendObsLSTMed, 1), "%"), color="Observed Trend"), hjust=0, show.legend=F, size=3 ) +
  # geom_text(aes(x=2020, y=YendUHI+1, label=paste0("+", round(20*TargetUHILSTMed, 1), "%"), color="Mitigate Intensifying UHI"), hjust=0, show.legend=F, size=3) +
  # geom_text(aes(x=2020, y=YendWarm+3, label=paste0("+", round(20*TargetWarmLSTMed, 1), "%"), color="Mitigate Warming Trend"), hjust=0, show.legend=F, size=3) +
  # scale_color_manual(name="LST Cover Trends", values=c("Observed Trend" = "#005a32", "Mitigate Intensifying UHI"="#3FA242", "Mitigate Warming Trend"="#A3D16B"))+
  # scale_x_continuous(name="Year", breaks=c(2001, 2020), limits=c(1999, 2025)) +
  # scale_y_continuous(name="LST Cover (%)") +
  # guides(label=NA) +
  theme(legend.position="top",
        legend.title=element_text(face='bold'),
        legend.key = element_rect(fill=NA),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        strip.text = element_text(color="black", face="bold"),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))
# figTrends


summary(StatsCombined[StatsCombined$biomeCode=="TeBF",])
nrow(StatsCombined[StatsCombined$biomeCode=="TeBF",])
nrow(StatsCombined[StatsCombined$biomeCode=="TeBF" & StatsCombined$trend.LST.core>0,])

nrow(StatsCombined[StatsCombined$biomeCode=="TeBF",])
nrow(StatsCombined[StatsCombined$biomeCode=="TeBF" & StatsCombined$trend.LST.diff>0,])
