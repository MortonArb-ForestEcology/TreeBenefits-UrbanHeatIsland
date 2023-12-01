###########################################
# Script Description & Outline ----
###########################################
# Purpose: Stats & figures corresponding to current manuscript outline; cleaned up and organized

# Manuscript Outline
# ---------
# 1. Cooling Contribution of trees & non-tree vegetation
# ---------
# ---------
# 2. Vegetation cover targets to fully mitigate UHIs and warming trends
# ---------
# ---------



###########################################

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

# grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with red
# grad.lst <- c("#2166ac", "#67a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2182b") # ends with red
grad.lst <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
# grad.lstHot <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026") # ends with red
grad.lstHot <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026") # ends with red


# grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
# grad.treeDiff <- c("#8c510a", "#d8b365", "#f6e8c3", "#f5f5f5", "#c7eae5", "#5ab4ac", "#01665e") # ends with teal
grad.treeDiff <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") # ends with teal
# grad.tree <- c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#31a354", "#006837") # ends with green
grad.tree <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529") # ends with green


# grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
# grad.otherDiff <- c("#c51b7d", "#e0a3c0", "#fde0ef", "#f7f7f7", "#e6f5d0", "#a1d76a", "#4d9221") # Green
grad.otherDiff <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e7f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419") # Green

# grad.other <- c("#f0f9e8", "#ccebc5", "#a8ddb5", "#7bccc4", "#43a2ca", "#0868ac") # ends with Blue
grad.other <- c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081")

grad.modfit <- c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177", "#49006a")



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
worldR <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

projLongLat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projRobin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
projWinTri <- "+proj=wintri"

# worldR <- getMap()
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
worldRobin <- fortify(st_transform(worldR, CRS(projRobin)))
worldWinTri <- fortify(st_transform(worldR, CRS(projWinTri)))
worldBBoxRobin <- st_transform(worldBBox, CRS(projRobin))
worldBBoxWinTri <- st_transform(worldBBox, CRS(projWinTri))
##########################################

# ##########################################
# Read in base datasets ----
# ##########################################
# Regional Sumary Stuff ----
cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])

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

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]
biome.order$biomeCode <- car::recode(biome.order$biomeName, 
                                     " 'Taiga'='Tai';
                                       'Tundra'='Tun';
                                       'Montane Grassland/Savanna'='MGS';
                                       'Temperate Broadleaf Forest'='TeBF';
                                       'Temperate Conifer Forest'='TeCF';
                                       'Temperate Grassland/Savanna'='TeGS';
                                       'Mediterranean'='Med';
                                       'Desert'='Des';
                                       'Flooded Grassland/Savanna'='FGS';
                                       'Tropical Grassland/Savanna'='TrGS';
                                       'Tropical Dry Broadleaf Forest'='TrDBF';
                                       'Tropical Conifer Forest'='TrCF';
                                       'Tropical Moist Broadleaf Forest'='TrMBF';
                                       'Mangroves'='Man'")
biome.order

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)
cityAll.stats$biomeCode <- factor(cityAll.stats$biomeCode, levels=biome.order$biomeCode)
summary(cityAll.stats)

CityBuffStats <- read.csv(file.path(path.google, "city_stats_core-buffer.csv"))
CityBuffStats$factor <- factor(CityBuffStats$factor, levels=c("LST", "tree", "other veg"))
CityBuffStats <- merge(CityBuffStats, cityAll.stats[,c("ISOURBID", "NAME", "LONGITUDE", "LATITUDE", "biomeName", "ES00POP")], all.x=T)
summary(CityBuffStats)


CityBuffStats[CityBuffStats$value.mean.core==0,] # This is for tree cover, so VERY very low.
summary(CityBuffStats[CityBuffStats$value.mean.core<1,]) # This is for tree cover, so VERY very low.


# Doing a spatial transformation on our city coords
spCity <- SpatialPoints(cityAll.stats[,c("LONGITUDE", "LATITUDE")], proj4string = CRS(projLongLat))
spCityRobin <- spTransform(spCity, CRS(projRobin))
spCityWinTri <- spTransform(spCity, CRS(projWinTri))
head(spCityRobin)
head(spCityWinTri)
cityAll.stats[,c("xRobin", "yRobin")] <- coordinates(spCityRobin)
cityAll.stats[,c("xWinTri", "yWinTri")] <- coordinates(spCityRobin)
# ##########################################

# ##########################################
# Step 1: Figure out what our criteria for cities to show is ----
#  -- enough data to do the temporal trend analysis (removes 96 cities)
#  -- R2 cutoff?
#  -- Max Tree & veg cover for any pixel >10% (allow robust parameterization)
# ##########################################
citiesUHI <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01]
length(citiesUHI)

nrow(cityAll.stats)


summary(CityBuff.stats)

# Getting rid of 4-sigma outliers for difference between metro core & reference region
lstDiffMean <- mean(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST"], na.rm=T)
lstDiffSD <- sd(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST"], na.rm=T)
lstDiffMean; lstDiffSD

cityDiffCOLD <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<lstDiffMean-4*lstDiffSD]
cityDiffHOT <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>lstDiffMean+4*lstDiffSD]

cityAll.stats[cityAll.stats$ISOURBID %in% cityDiffHOT,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biome", "biome.prop", "n.pixels")]
cityAll.stats[cityAll.stats$ISOURBID %in% cityDiffCOLD,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biome", "biome.prop", "n.pixels")]

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

cityCoreCOLD  <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.core<lstCoreMean-4*lstCoreSD]
cityCoreHOT <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.core>lstCoreMean+4*lstCoreSD]

# Getting rid of 4-sigma outliers for LST std dev in the region --> most of these are mountainous or ecotonal cities that don't meet other criteria, but they also capture some of our oddest slopes too
lstSDMean <- mean(cityAll.stats$LST.sd, na.rm=T)
lstSDSD <- sd(cityAll.stats$LST.sd, na.rm=T)
lstSDMean; lstSDSD
citySDLo  <- cityAll.stats$ISOURBID[cityAll.stats$LST.sd<lstSDMean-4*lstSDSD]
citySDHi  <- cityAll.stats$ISOURBID[cityAll.stats$LST.sd>lstSDMean+4*lstSDSD]


# cityAll.stats[cityAll.stats$ < 1.0,]

summary(cityAll.stats)

citiesUse <- !is.na(cityAll.stats$trend.LST.slope) & 
  cityAll.stats$n.pixels>=1000 & 
  cityAll.stats$biome.prop>=0.75 &
  # cityAll.stats$tree.max>10 & cityAll.stats$veg.max>10 &
  cityAll.stats$LST.sd >=1 & cityAll.stats$tree.sd >= 1 & cityAll.stats$veg.sd >= 1 & cityAll.stats$elev.sd >= 1 &
  !cityAll.stats$ISOURBID %in% c(cityDiffHOT, cityDiffCOLD, cityCoreHOT, cityCoreCOLD, citySDLo, citySDHi)


# length(which(!cityAll.stats$ISOURBID %in% citiesUHI))
length(which(is.na(cityAll.stats$trend.LST.slope)))
length(which(cityAll.stats$n.pixels<1000))
length(which(cityAll.stats$biome.prop<0.75))
length(which(cityAll.stats$LST.sd<1))
length(which(cityAll.stats$tree.sd<1))
length(which(cityAll.stats$veg.sd<1))
length(which(cityAll.stats$elev.sd<1))
length(cityDiffHOT)
length(cityDiffCOLD)
length(cityCoreHOT)
length(cityCoreCOLD)
length(citySDLo)
length(citySDHi)


# UHI-only 
# cityStatsAnaly <- cityAll.stats[which(citiesUse & cityAll.stats$ISOURBID %in% citiesUHI),]
# cityBuffAnaly <- CityBuffStats[CityBuffStats$ISOURBID %in% cityStatsAnaly$ISOURBID,]

cityStatsAnaly <- cityAll.stats[which(citiesUse),]
cityBuffAnaly <- CityBuffStats[CityBuffStats$ISOURBID %in% cityStatsAnaly$ISOURBID,]
summary(cityStatsAnaly)
summary(cityStatsAnaly$biomeName)
dim(cityStatsAnaly)

# cityStatsAnaly[cityStatsAnaly$LSTmodel.tree.slope < -1.0,]
cityStatsAnaly[cityStatsAnaly$LSTmodel.R2adj < 0.35,]

nrow(cityStatsAnaly)


cityTree <- cityBuffAnaly[cityBuffAnaly$factor=="tree", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityTree) <- gsub("mean", "tree", names(cityTree))
names(cityTree)[which(names(cityTree)=="trend.p.core")] <- "trend.tree.p.core"

cityOther <- cityBuffAnaly[cityBuffAnaly$factor=="other veg", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityOther) <- gsub("mean", "other", names(cityOther))
names(cityOther)[which(names(cityOther)=="trend.p.core")] <- "trend.other.p.core"

cityVeg <- merge(cityTree, cityOther, all=T)
summary(cityVeg)

cityLST <- cityBuffAnaly[cityBuffAnaly$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityLST) <- gsub("mean", "LST", names(cityLST))
names(cityLST)[which(names(cityLST)=="trend.p.core")] <- "trend.LST.p.core"

summary(cityStatsAnaly[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "LSTmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.tree.p", "LSTmodel.veg.slope", "LSTmodel.veg.p")])

StatsCombined <- merge(cityStatsAnaly[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "xRobin", "yRobin", "xWinTri", "yWinTri", "biomeName", "biomeCode", "LSTmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.tree.p", "LSTmodel.veg.slope", "LSTmodel.veg.p", "ETmodel.R2adj", "ETmodel.tree.slope", "ETmodel.tree.p", "ETmodel.veg.slope", "ETmodel.veg.p", "trendYear.LST.mean.slope", "trendYear.LST.mean.p", "trendYear.tree.mean.slope", "trendYear.tree.mean.p", "trendYear.veg.mean.slope", "trendYear.veg.mean.p")],
                       cityVeg, all=T)
StatsCombined <- merge(StatsCombined, cityLST, all=T)
summary(StatsCombined)


# Saving this combined & formatted dataset for collaborators or other sharing
write.csv(StatsCombined, file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"), row.names=F)
# SUPPLEMENTAL TABLE ----
# Global + Biome:
# -- Cities Considered
# -- Cities exlcuded for each criteria
# ##########################################

mean(StatsCombined$LSTmodel.R2adj); median(StatsCombined$LSTmodel.R2adj)

# test <- data.frame(worldR)
# head(test)

MapModFits <- ggplot(data=StatsCombined[,]) +
  geom_sf(data=worldBBoxRobin, fill="gray90", size=0.1) +
  # geom_sf(data=worldR, fill="gray50", size=0.1) +
  # geom_map(data=worldR, map=worldRobin, aes(x=long, y=lat, map_id=id), fill="gray50") +
  geom_point(aes(x=xRobin, y=yRobin, color=LSTmodel.R2adj), size=0.25, alpha=0.8) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_stepsn(name="R2-adjusted", colors=grad.modfit) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))


png(file.path(path.figs, "FigureS1_ModelR2adj.png"), height=6, width=8, units="in", res=320)
MapModFits
dev.off()

tiff(file.path(path.figs, "FigureS1_ModelR2adj.tiff"), height=6, width=8, units="in", res=320)
MapModFits
dev.off()

# ##########################################
# 1. Cooling contribution of trees and non-tree vegetation to cities (4-5 paragraphs)
# Key Result/Messages: 
#   - Trees cool at 5.5x the rate of other vegetation
#   - On average, 37% of the urban heat island effect is attributable to lower in tree cover in the metro core than reference region.
#   - Without trees, the UHI would be double what it currently is
#   - Although trees cool more than other veg, the relative dominance of other veg mean that they have similar cooling effects on cities.
#
# Key Figure/Table Components (needs to show):
#   - Fig 1: Map of UHI & Urban Veg Covers; latitude bands for UHI & Tree/Veg slopes
#       - Supplement: Break down by biome
#       - Purpose: Comparison of tree vs non-tree slopes across geography/biomes
#   - Fig 2: Total cooling contribution of veg by biome; + warming because of Veg cover difference with buffer
#       - Purpose: Cooling contribution of current tree/other veg cover & deficit → i know the histograms are hard, but maybe have the deficit (core minus reference) negative and show it counteracting the current contribution?  Or have it above showing what it would be if we met that target?
#       - Comparison of tree/non-tree cooling as a function of percent cover?? ← this has been hard to show previously, but maybe it’ll work if we focus on tree vs. non-tree & show biomes as individual panels?
# ##########################################



breaksUHI <- c(0, round(quantile(StatsCombined$value.LST.diff, seq(0.2, 1, length.out=length(grad.lstHot))), 1))

# Current gradient is 10
LSTquint <- quantile(StatsCombined$value.LST.diff[StatsCombined$value.LST.diff>0], seq(0.2, 1, by=0.2))
breaksLST <- c(-rev(LSTquint), 0, LSTquint)
names(breaksLST) <- round(breaksLST, 1)
# breaksLST <- c(0, round(quantile(StatsCombined$value.LST.diff, seq(0.2, 1, length.out=length(grad.lst))), 1))

breaksVeg <- seq(0, max(c(StatsCombined$value.tree.core, StatsCombined$value.other.core)), length.out=length(grad.tree))

breaksTree <- c(0, round(quantile(StatsCombined$value.tree.core, seq(0.2, 1, length.out=length(grad.tree))), 0))
breaksOther <- c(0, round(quantile(StatsCombined$value.other.core, seq(0.2, 1, length.out=length(grad.other))), 0))

StatsCombined$biomeCodeRev <- factor(StatsCombined$biomeCode, levels=rev(levels(StatsCombined$biomeCode)))


mapLST <- ggplot(data=StatsCombined[,]) +
  geom_sf(data=worldBBoxRobin, fill="gray90", size=0.1) +
  # geom_map(data=worldRobin, map=worldRobin, aes(x=long, y=lat, map_id=id), fill="gray30") +
  geom_point(aes(x=xRobin, y=yRobin, color=value.LST.diff), size=0.25, alpha=0.8) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_stepsn(colors=grad.lst, breaks=breaksLST) +
  labs(color="LST Diff.\n(˚C)") +
  # labs(color=expression(paste("LST Diff\n("*degree*C*"")"))) +
  # labs(color=c(paste("LST Diff\n"),expression(""*degree*"C)"))) +
  # scale_color_stepsn(name="LST Diff.\n(deg. C)", colors=grad.lst, breaks=breaksLST) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

mapTree <- ggplot(data=StatsCombined[,]) +
  geom_sf(data=worldBBoxRobin, fill="gray90", size=0.1) +
  # geom_map(data=worldRobin, map=worldRobin, aes(x=long, y=lat, map_id=id), fill="gray30") +
  geom_point(aes(x=xRobin, y=yRobin, color=value.tree.core), size=0.25) +
  scale_color_stepsn(name="Tree\nCover (%)", colors=grad.tree, breaks=seq(0, 90, by=10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))


mapOther <- ggplot(data=StatsCombined[,]) +
  geom_sf(data=worldBBoxRobin, fill="gray90", size=0.1) +
  # geom_map(data=worldRobin, map=worldRobin, aes(x=long, y=lat, map_id=id), fill="gray30") +
  geom_point(aes(x=xRobin, y=yRobin, color=value.other.core), size=0.25) +
  scale_color_stepsn(name="Non-Tree\nCover (%)", colors=grad.other, breaks=seq(0, 90, by=10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))  
  
UHIBiome <- ggplot(data=StatsCombined[,],) +
  coord_flip() +
  geom_violin(aes(x=biomeCodeRev, y=value.LST.diff, fill=biomeCode), scale="width") +
  geom_point(data=StatsCombined[StatsCombined$biomeCodeRev=="Tun",], aes(x=biomeCodeRev, y=value.LST.diff, color=biomeCode)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.all) +
  scale_color_manual(values=biomeCode.pall.all) +
  labs(y="LST Difference (˚C)", x="Biome") +
  guides(fill="none", color="none") +
  theme(legend.title=element_blank(),
        legend.key = element_rect(fill=NA),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(color="black", size=0.1),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black", size=unit(8, "pt")),
        axis.text.x=element_text(color="black", size=unit(8, "pt")),
        axis.title=element_text(color="black", face="bold", size=unit(10, "pt")),
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))
  


treeSlopeBiome <- ggplot(data=StatsCombined[,],) +
  coord_flip() +
  geom_violin(aes(x=biomeCodeRev, y=LSTmodel.tree.slope, fill=biomeCode), scale="width") +
  geom_point(data=StatsCombined[StatsCombined$biomeCodeRev=="Tun",], aes(x=biomeCodeRev, y=LSTmodel.tree.slope, color=biomeCode)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.all) +
  scale_color_manual(values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(c(StatsCombined$LSTmodel.tree.slope, StatsCombined$LSTmodel.veg.slope))) +
  labs(y="Tree Slope (˚C/% cover)", x="Biome") +
  guides(fill="none", color="none") +
  theme(legend.title=element_blank(),
        legend.key = element_rect(fill=NA),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(color="black", size=0.1),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black", size=unit(8, "pt")),
        axis.text.x=element_text(color="black", size=unit(8, "pt")),
        axis.title=element_text(color="black", face="bold", size=unit(10, "pt")),
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

otherSlopeBiome <- ggplot(data=StatsCombined[,],) +
  coord_flip() +
  geom_violin(aes(x=biomeCodeRev, y=LSTmodel.veg.slope, fill=biomeCode), scale="width") +
  geom_point(data=StatsCombined[StatsCombined$biomeCodeRev=="Tun",], aes(x=biomeCodeRev, y=LSTmodel.veg.slope, color=biomeCode)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.all) +
  scale_color_manual(values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(c(StatsCombined$LSTmodel.tree.slope, StatsCombined$LSTmodel.veg.slope))) +
  labs(y="Non-Tree Slope (˚C/% cover)", x="Biome") +
  guides(fill="none", color="none") +
  theme(legend.title=element_blank(),
        legend.key = element_rect(fill=NA),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(color="black", size=0.1),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black", size=unit(8, "pt")),
        axis.text.x=element_text(color="black", size=unit(8, "pt")),
        axis.title=element_text(color="black", face="bold", size=unit(10, "pt")),
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

# mapLST
# mapTree
# mapOther
# UHILat
# TreeSlopeLat
# OtherSlopeLat

png(file.path(path.figs, "Figure1_UHI_Veg_SlopesBiome.png"), height=8, width=8, units="in", res=320)
plot_grid(mapLST, UHIBiome, mapTree, treeSlopeBiome, mapOther, otherSlopeBiome, nrow=3, rel_widths = c(0.6, 0.6), labels=c("A","B", "C", "D", "E", "F"))
dev.off() 

tiff(file.path(path.figs, "Figure1_UHI_Veg_SlopesBiome.tiff"), height=8, width=8, units="in", res=320)
plot_grid(mapLST, UHIBiome, mapTree, treeSlopeBiome, mapOther, otherSlopeBiome, nrow=3, rel_widths = c(0.6, 0.6), labels=c("A","B", "C", "D", "E", "F"))
dev.off() 


# Trees have a clear, consistent cooling potential on global urban surface temperatures, with a global median effect of XXX˚C per percent tree cover (SD XXX˚C/%), and a significant cooling effect in XX% of those cities (Fig. 1)
# hist(cityStatsAnaly$LSTmodel.tree.slope)
treeCoolMean <- mean(StatsCombined$LSTmodel.tree.slope)
treeCoolSD <- sd(StatsCombined$LSTmodel.tree.slope)
round(treeCoolMean, 2); round(treeCoolSD, 2)

round(length(which(StatsCombined$LSTmodel.tree.p<0.01 & StatsCombined$LSTmodel.tree.slope<0))/length(StatsCombined$LSTmodel.tree.slope),2)

# At a global scale, the cooling effect of trees was more than XXX times that of non-tree vegetation (XXX˚C/%; SD XXXX˚C/%).  
otherCoolMean <- mean(StatsCombined$LSTmodel.veg.slope)
otherCoolSD <- sd(StatsCombined$LSTmodel.veg.slope)
round(otherCoolMean, 2); round(otherCoolSD, 2)
round(treeCoolMean/otherCoolMean, 1)

# Breaking down contributions of tree cover to LST & UHI ----
StatsCombined$EffectLST.tree <- StatsCombined$LSTmodel.tree.slope*StatsCombined$value.tree.core
StatsCombined$EffectLST.other <- StatsCombined$LSTmodel.veg.slope*StatsCombined$value.other.core
StatsCombined$ContribUHI.tree <- StatsCombined$LSTmodel.tree.slope*StatsCombined$value.tree.diff
StatsCombined$ContribUHI.other <- StatsCombined$LSTmodel.veg.slope*StatsCombined$value.other.diff
summary(StatsCombined)
hist(StatsCombined$EffectLST.tree)

citiesUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01)

# Creating a dataframe that will lest us stack
effectsUHI <- stack(StatsCombined[citiesUHI,c("EffectLST.tree", "EffectLST.other", "value.LST.diff")])
effectsUHI$ind <- as.character(effectsUHI$ind)
effectsUHI$ind[grep("tree", effectsUHI$ind)] <- "Tree"
effectsUHI$ind[grep("other", effectsUHI$ind)] <- "Non-Tree Veg"
effectsUHI$ind[grep("LST", effectsUHI$ind)] <- "Remaining UHI"
effectsUHI$ind <- factor(effectsUHI$ind, levels=c("Tree", "Non-Tree Veg", "Remaining UHI"))

effectsUHI[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "value.LST.diff.p")] <- StatsCombined[citiesUHI,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "value.LST.diff.p")]
summary(effectsUHI)

# "#67001f" "#b2182b" "#d6604d" "#f4a582" "#fbbdc7" "#d1e5f0" "#92c5de" "#4393c3" "#2166ac" "#053061"
# "#f03b20"    "#ef3b2c"
plotTempEffects <- ggplot(data=effectsUHI, aes(x=biomeCode, y=values, fill=ind)) + 
  geom_bar(stat="summary", fun="median") +
  geom_hline(yintercept=0, size=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  
  geom_text(x="Des", y=-7, hjust=1, label="Cooling Effect") +
  geom_text(x="Des", y=2.5, hjust=1, label="Warming Effect") +
  coord_cartesian(ylim=c(-7.5, 2.5)) +
  labs(x="Biome", y="Temperature Effect (˚C)") +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face="bold"),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))


# png(file.path(path.figs, "base", "Figure2_TempEffects.png"), height=8, width=6, units="in", res=320)
# plotTempEffects
# dev.off()


# Lower tree canopy in the metropolitan core than the surrounding  region accounts for nearly one-third (31% SD 39%) of the observed warming in the XXXX% of cities showing significant (p<0.01) UHI effects
citiesUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01)
length(citiesUHI)/nrow(StatsCombined)

hist(StatsCombined$ContribUHI.tree/StatsCombined$value.LST.diff)

round(mean(StatsCombined$ContribUHI.tree[citiesUHI]/StatsCombined$value.LST.diff[citiesUHI]),2); round(sd(StatsCombined$ContribUHI.tree[citiesUHI]/StatsCombined$value.LST.diff[citiesUHI]), 2)



# If tree cover in these metropolitan cores matched that of the reference region, the observed UHI effect would be reduced by a [mean/median] 0.43˚C (SD 0.53˚C), with the greatest reductions in UHI occurring in biomes with the greatest tree cover deficits.
round(mean(StatsCombined$ContribUHI.tree[citiesUHI]),2); round(sd(StatsCombined$ContribUHI.tree[citiesUHI]), 2)

# Globally, trees cool cities an average of XXX˚C (SD XXX˚C) with a only mean cover of XXX% (SD XXX%).  In contrast, non-tree vegetation contributes a mean XXXX˚C (SD XXX˚C) cooling with XX% (SD XX%) cover
round(mean(StatsCombined$EffectLST.tree),2); round(sd(StatsCombined$EffectLST.tree), 2)
round(mean(StatsCombined$value.tree.core),0); round(sd(StatsCombined$value.tree.core), 0)

round(mean(StatsCombined$EffectLST.other),2); round(sd(StatsCombined$EffectLST.other), 2)
round(mean(StatsCombined$value.other.core),0); round(sd(StatsCombined$value.other.core), 0)




# BIOME BREAKDOWNS ----
# On a per-percent cover basis, the cooling potential of trees is greatest in arid and semi-arid biomes where the natural landscape is dominated by grasses and shrubby vegetation (SUPPLEMENT FIG/TABLE)
# SUPPELEMENTAL TABLE 2 BY BIOME---
#  - Cities in analysis; Mean Core [LST/Tree/Veg]; Mean Diff [LST/Tree/Veg]; 
ncitiesAll <- aggregate(ISOURBID ~ biomeName, data=cityAll.stats, FUN = length)
names(ncitiesAll) <- c("Biome", "N.Total")

nsigUHI <- aggregate(ISOURBID ~ biomeName, data=StatsCombined[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01, ], FUN = length)
names(nsigUHI)[2] <- c("N.UHI")


ncitiesAnaly <- aggregate(ISOURBID ~ biomeName, data=StatsCombined, FUN = length)
ncitiesAnaly <- merge(ncitiesAnaly, nsigUHI, all=T)
ncitiesAnaly$N.UHI[is.na(ncitiesAnaly$N.UHI)] <- 0

ncitiesAnaly[,c("LST.mean", "Tree.mean", "Other.mean")] <- round(aggregate(cbind(value.LST.core, value.tree.core, value.other.core) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:4)], 2)
ncitiesAnaly[,c("LST.sd", "Tree.sd", "Other.sd")] <- round(aggregate(cbind(value.LST.core, value.tree.core, value.other.core) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:4)], 2)
ncitiesAnaly[,c("LSTDiff.mean", "TreeDiff.mean", "OtherDiff.mean")] <- round(aggregate(cbind(value.LST.diff, value.tree.diff, value.other.diff) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:4)], 2)
ncitiesAnaly[,c("LSTDiff.sd", "TreeDiff.sd", "OtherDiff.sd")] <- round(aggregate(cbind(value.LST.diff, value.tree.diff, value.other.diff) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:4)], 2)
# ncitiesAnaly[,c("LST.sd", "Tree.sd", "Other.sd")] <- round(aggregate(cbind(value.LST.core, value.tree.core, value.other.core) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:4)], 2)

ncitiesAnaly

TableCitySummary <- data.frame(Biome = ncitiesAnaly$biomeName,
                               N.Analyzed = ncitiesAnaly$ISOURBID, 
                               N.UHI = ncitiesAnaly$N.UHI, 
                               LST.mean = paste0(ncitiesAnaly$LST.mean, " (", ncitiesAnaly$LST.sd, ")"),
                               Tree.mean = paste0(round(ncitiesAnaly$Tree.mean, 0), " (", round(ncitiesAnaly$Tree.sd, 0), ")"),
                               OtherVeg.mean = paste0(round(ncitiesAnaly$Other.mean, 0), " (", round(ncitiesAnaly$Other.sd, 0), ")"),
                               LST.diff = paste0(ncitiesAnaly$LSTDiff.mean, " (", ncitiesAnaly$LSTDiff.sd, ")"),
                               Tree.diff = paste0(round(ncitiesAnaly$TreeDiff.mean, 0), " (", round(ncitiesAnaly$TreeDiff.sd, 0), ")"),
                               OtherVeg.diff = paste0(round(ncitiesAnaly$OtherDiff.mean, 0), " (", round(ncitiesAnaly$OtherDiff.sd, 0), ")"))

TableCitySummary <- merge(ncitiesAll, TableCitySummary, all=T)
TableCitySummary <- TableCitySummary[order(TableCitySummary$Biome),]
TableCitySummary


write.csv(TableCitySummary, file.path(path.figs, "SuppTable1_Biome_CitySummaryStats.csv"), row.names=F)

# SUPPELEMENTAL TABLE 3 BY BIOME---
# - Tree/Veg Stats: slope, pSig, Cooling Contrib, UHI due to diff
CoolStats <- aggregate(ISOURBID ~ biomeName, data=StatsCombined, FUN = length)
CoolStats[,c("TreeSlope.mean", "OtherSlope.mean")] <- round(aggregate(cbind(LSTmodel.tree.slope, LSTmodel.veg.slope) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:3)], 2)
CoolStats[,c("TreeSlope.sd", "OtherSlope.sd")] <- round(aggregate(cbind(LSTmodel.tree.slope, LSTmodel.veg.slope) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:3)], 2)

CoolStats[,c("EffectLST.Tree.mean", "EffectLST.Other.mean")] <- round(aggregate(cbind(EffectLST.tree, EffectLST.other) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:3)], 2)
CoolStats[,c("EffectLST.Tree.sd", "EffectLST.Other.sd")] <- round(aggregate(cbind(EffectLST.tree, EffectLST.other) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:3)], 2)
CoolStats[,c("EffectUHI.Tree.mean", "EffectUHI.Other.mean")] <- round(aggregate(cbind(ContribUHI.tree, ContribUHI.other) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:3)], 2)
CoolStats[,c("EffectUHI.Tree.sd", "EffectUHI.Other.sd")] <- round(aggregate(cbind(ContribUHI.tree, ContribUHI.other) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:3)], 2)

CoolStats$TreeCool.sig <- round(aggregate(LSTmodel.tree.slope ~ biomeName, data=StatsCombined[StatsCombined$LSTmodel.tree.slope<0 & StatsCombined$LSTmodel.tree.p<0.01,], FUN = length)[,2], 2)
CoolStats$OtherCool.sig <- round(aggregate(LSTmodel.veg.slope ~ biomeName, data=StatsCombined[StatsCombined$LSTmodel.veg.slope<0 & StatsCombined$LSTmodel.veg.p<0.01,], FUN = length)[,2], 2)
CoolStats$TreeCool.sig.Percent <- round(CoolStats$TreeCool.sig/CoolStats$ISOURBID*100,0)
CoolStats$OtherCool.sig.Percent <- round(CoolStats$OtherCool.sig/CoolStats$ISOURBID*100,0)
CoolStats

CoolStatsSummary <- data.frame(Biome = CoolStats$biomeName,
                               N.Analyzed = CoolStats$ISOURBID, 
                               TreeSlope = paste0(CoolStats$TreeSlope.mean, " (", CoolStats$TreeSlope.sd,")"),
                               PercentTreeCool = paste0(CoolStats$TreeCool.sig.Percent, "%"),
                               OtherSlope = paste0(CoolStats$OtherSlope.mean, " (", CoolStats$OtherSlope.sd,")"),
                               PercentOtherCool = paste0(CoolStats$OtherCool.sig.Percent, "%"),
                               LST.Effect.TreeCover = paste0(CoolStats$EffectLST.Tree.mean, " (", CoolStats$EffectLST.Tree.sd,")"),
                               LST.Effect.OtherCover = paste0(CoolStats$EffectLST.Other.mean, " (", CoolStats$EffectLST.Other.sd,")"),
                               LST.Effect.TreeDiff = paste0(CoolStats$EffectUHI.Tree.mean, " (", CoolStats$EffectUHI.Tree.sd,")"),
                               LST.Effect.OtherDiff = paste0(CoolStats$EffectUHI.Other.mean, " (", CoolStats$EffectUHI.Other.sd,")"))

CoolStatsSummary

write.csv(CoolStatsSummary, file.path(path.figs, "SuppTable2_Biome_CoolingEffects.csv"), row.names=F)

# ##########################################


# ##########################################
# 2. Vegetation cover targets to fully offset UHIs and warming trends ----
#    Key Result/Messages: 
#       - On average, urban tree cover would need to be double current levels to fully offset the UHI with tree cover alone.  (Presumably the number for non-tree vegetation will be ridiculously high)
#       - In order to have offset the increased warming in cities relative to the reference region, tree cover would need to have increased at more than 6 times the observed median (this would have the UHI rate constant instead of growing)
# 
#   Key Figure/Table Components (needs to show):
#       - [See Fig 2] Tree & other veg targets to fully offset UHI (where things stand relative to an achievable high goal)
#       - Figure 3: 
#           - Purpose: Rate of Tree Cover growth needed to hold UHI effect constant, fully combat recent warming (and compared to what it currently was)
#           - Picture a line showing the current rate of tree growth → the slope would be MUCH steeper to hold the UHI constant; the slope would be even steeper to have held actual mean summer day surface temperature constant
# ##########################################
# How much more trees would there need to be to offset UHI?
StatsCombined$TreeCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$LSTmodel.tree.slope
StatsCombined$OtherCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$LSTmodel.veg.slope

# 
StatsCombined$TreeCoverTargetUHI <- StatsCombined$TreeCoverUHINeed + StatsCombined$value.tree.core
StatsCombined$OtherCoverTargetUHI <- StatsCombined$OtherCoverUHINeed + StatsCombined$value.other.core
summary(StatsCombined)

# For trees to be the sole solution for offsetting UHI effects globally, mean tree cover would need to more than double (2.25x), increasing from the current mean of 15.4% (SD 10.9%) to 32% (SD 27%)
round(median(StatsCombined$TreeCoverTargetUHI, na.rm=T)/median(StatsCombined$value.tree.core), 1)

round(median(StatsCombined$value.tree.core, na.rm=T), 0)
round(median(StatsCombined$TreeCoverTargetUHI, na.rm=T), 0)

length(which(StatsCombined$value.tree.core>median(StatsCombined$TreeCoverTargetUHI, na.rm=T)))
nrow(StatsCombined)
length(which(StatsCombined$value.tree.core>median(StatsCombined$TreeCoverTargetUHI, na.rm=T)))/nrow(StatsCombined)

install.pacages("ungeviz")

TreeCoverTarget <- ggplot(data=StatsCombined[citiesUHI,], aes(x=biomeCode, y=TreeCoverTargetUHI, fill="Biome Target", color="Biome Target")) +  
  geom_bar(stat="summary", fun="median") +
  # geom_segment(yend=0, aes(xend=biomeCode), stat="summary", fun="median", size=2) +
  geom_violin(aes(x=biomeCode, y=value.tree.core, fill="Current", color="Current"), scale="width") +
  geom_hline(yintercept=30, linetype="dashed") +
  annotate("text", x=13, y=35, label="30%\nTarget") +
  # geom_point(stat="summary", fun="median", size=5) +
  scale_fill_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4])) +
  scale_color_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4])) +
  labs(x="Biome", y="Tree Cover (%)") +
  scale_y_continuous(limits=c(0,70), expand=c(0,0)) +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face='bold'),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

png(file.path(path.figs, "Figure2_TempEffects_CoverTargets.png"), height=8, width=6, units="in", res=320)
plot_grid(plotTempEffects, TreeCoverTarget, ncol=1, labels=c("A", "B"))
dev.off()

tiff(file.path(path.figs, "Figure2_TempEffects_CoverTargets.tiff"), height=8, width=6, units="in", res=320)
plot_grid(plotTempEffects, TreeCoverTarget, ncol=1, labels=c("A", "B"))
dev.off()

# Coming up with biome-specific targets
biomeTargetStats <- aggregate(ISOURBID~biomeName, data=StatsCombined[citiesUHI,], FUN=length)
# names(biomeTargetStats)
biomeTargetStats[,c("UHI", "CurrentTreeCover", "TargetTreeCover")] <- aggregate(cbind(value.LST.diff, value.tree.core, TreeCoverTargetUHI)~biomeName, data=StatsCombined[citiesUHI,], FUN=median)[,c("value.LST.diff", "value.tree.core", "TreeCoverTargetUHI")]
biomeTargetStats[,c("UHI.lo", "CurrentTreeCover.lo", "TargetTreeCover.lo")] <- aggregate(cbind(value.LST.diff, value.tree.core, TreeCoverTargetUHI)~biomeName, data=StatsCombined[citiesUHI,], FUN=quantile, 0.25)[,c("value.LST.diff", "value.tree.core", "TreeCoverTargetUHI")]
biomeTargetStats[,c("UHI.hi", "CurrentTreeCover.hi", "TargetTreeCover.hi")] <- aggregate(cbind(value.LST.diff, value.tree.core, TreeCoverTargetUHI)~biomeName, data=StatsCombined[citiesUHI,], FUN=quantile, 0.75)[,c("value.LST.diff", "value.tree.core", "TreeCoverTargetUHI")]
biomeTargetStats

summary(StatsCombined$TreeCoverTargetUHI[citiesUHI])

biomeTreeTarget <- data.frame(Biome=biomeTargetStats$biomeName,
                              N.Cities.UHI=biomeTargetStats$ISOURBID,
                              UHI=paste0(round(biomeTargetStats$UHI, 1), " (", round(biomeTargetStats$UHI.lo, 1), " - ", round(biomeTargetStats$UHI.hi, 1), ")"),
                              CurrentTreeCover=paste0(round(biomeTargetStats$CurrentTreeCover, 0), " (", round(biomeTargetStats$CurrentTreeCover.lo, 0), " - ", round(biomeTargetStats$CurrentTreeCover.hi, 0), ")"),
                              TargetTreeCover=paste0(round(biomeTargetStats$TargetTreeCover, 0), " (", round(biomeTargetStats$TargetTreeCover.lo, 0), " - ", round(biomeTargetStats$TargetTreeCover.hi, 0), ")"))
biomeTreeTarget$Biome <- factor(biomeTreeTarget$Biome, levels=levels(StatsCombined$biomeName))
biomeTreeTarget <- biomeTreeTarget[order(biomeTreeTarget$Biome),]
biomeTreeTarget
write.csv(biomeTreeTarget, file.path(path.figs, "SuppTable3_Biome_TreeCoverTargets.csv"), row.names=F)


# Cities in temperate conifer biomes have among the highest urban tree cover (XXX%, Table S3), but a tree-only solution for UHIs would require tree cover to be double that (XXX%) to offset the median observed UHI of 1.6˚C.This would require tree cover in all cities in this biome to be XX% higher than the tree cover seen in XX City, which has the highest tree cover (Table S3)
UHI.TBF <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01 & StatsCombined$biomeCode=="TeBF")

median(StatsCombined$value.tree.core[UHI.TBF])
median(StatsCombined$TreeCoverTargetUHI[UHI.TBF])
median(StatsCombined$value.LST.diff[UHI.TBF])

summary(StatsCombined$value.tree.core[UHI.TBF])

length(which(StatsCombined$value.tree.core[UHI.TBF]>median(StatsCombined$TreeCoverTargetUHI[UHI.TBF])))/length(UHI.TBF)


StatsCombined[StatsCombined$value.tree.core==max(StatsCombined$value.tree.core[UHI.TBF]),]

# To achieve the same level of cooling using non-tree vegetation, median cover in global UHI cities would need to be XXX%. 
round(median(StatsCombined$OtherCoverTargetUHI[UHI.TBF]), 0)

# In temperate conifer biomes, non-tree vegetation would need to increase from XX% to XX% to offset that region’s UHI. <-- cut this sentence because TeCF is a place where non-tree vegetation 
round(median(StatsCombined$value.other.core[UHI.TBF]), 0)
round(median(StatsCombined$OtherCoverTargetUHI[UHI.TBF]), 0)



# Now looking at the trend stuff ----
# The UHI effect significantly (p<0.01) intensified between 2001 and 2020 in 49% of the 1568 UHI cities, and 69% of all 2047 cities showed a significant warming trend
intensifyUHI <- StatsCombined$value.LST.diff>0 & StatsCombined$value.tree.diff.p < 0.01 & StatsCombined$trend.LST.core>0 & StatsCombined$trend.LST.diff>0 & StatsCombined$trend.LST.diff.p<0.01
length(which(intensifyUHI))/length(citiesUHI)

citiesWarm <- StatsCombined$trend.LST.core>0 & StatsCombined$trend.LST.p.core < 0.01
length(which(citiesWarm))/nrow(StatsCombined)

hist(StatsCombined$trend.LST.core)
mean(StatsCombined$trend.LST.core); sd(StatsCombined$trend.LST.core)
median(StatsCombined$trend.LST.core)

summary(StatsCombined$trend.LST.core>0)
summary(StatsCombined$trend.LST.diff.p<0.01)

# Tree cover significantly increased (p<0.01) in two-thirds of cities with a median rate of 0.03%/yr and a global median rate of 0.04%/yr.  
round(median(StatsCombined$trend.tree.core), 2)
round(median(StatsCombined$trend.tree.core)*20, 0)

treesGrow <- which(StatsCombined$trend.tree.core>0 & StatsCombined$trend.tree.p.core<0.01)
treesLoss <- which(StatsCombined$trend.tree.core<0 & StatsCombined$trend.tree.p.core<0.01)
otherGrow <- which(StatsCombined$trend.other.core>0 & StatsCombined$trend.other.p.core<0.01)
length(treesGrow)
length(treesGrow)/nrow(StatsCombined)
length(treesLoss)/nrow(StatsCombined)


median(StatsCombined$trend.LST.core[treesGrow])
median(StatsCombined$trend.LST.core)

median(StatsCombined$trend.other.core)
round(median(StatsCombined$trend.other.core)*20, 0)

# Our analysis of 2047 cities synthesizes 20-year trends from over XXXX million pixels, representing a vastly larger scope than previous studies.  
dim(cityStatsAnaly); dim(StatsCombined)
sum(cityStatsAnaly$n.pixels)


# Calculating the temperature contributions of changes in vegetation cover
StatsCombined$LSTTrend.Tree <- StatsCombined$trend.tree.core*StatsCombined$LSTmodel.tree.slope 
StatsCombined$LSTTrend.Other <- StatsCombined$trend.other.core*StatsCombined$LSTmodel.veg.slope 
StatsCombined$TargetConstantUHI <- -StatsCombined$trend.LST.diff/StatsCombined$LSTmodel.tree.slope + StatsCombined$trend.tree.core
StatsCombined$TargetOffsetWarming <- -StatsCombined$trend.LST.core/StatsCombined$LSTmodel.tree.slope + StatsCombined$trend.tree.core

round(median(StatsCombined$LSTTrend.Tree[treesGrow])*20, 1)
round(median(StatsCombined$LSTTrend.Tree[treesGrow]/(StatsCombined$LSTTrend.Tree[treesGrow] + StatsCombined$trend.LST.core[treesGrow])), 2)

length(otherGrow)
round(median(StatsCombined$LSTTrend.Other[otherGrow])*20, 2)

summary(StatsCombined)

# For the cities showing an intensifying UHI, tree cover would need to grow at a median rate of XX%/yr. – X times the observed rate – to hold the UHI constant. 
round(median(StatsCombined$TargetConstantUHI[intensifyUHI]), 1)
round(median(StatsCombined$trend.tree.core[intensifyUHI]), 1)

# To fully offset warming observed in our study, tree cover would need to increase even faster at a median rate of X%/yr.
round(median(StatsCombined$TargetOffsetWarming[citiesWarm]), 1)
median(StatsCombined$value.tree.core[citiesWarm])

round(median(StatsCombined$TargetOffsetWarming[citiesWarm])*20, 1)
round(median((StatsCombined$TargetOffsetWarming[citiesWarm]*20 + StatsCombined$value.tree.core[citiesWarm])/StatsCombined$value.tree.core[citiesWarm]), 1)

round(median(StatsCombined$trend.tree.core[citiesWarm]), 1)


# # # Making the table to go with the slopes (Fig. 3) ----
# Want to show:  n cities; ;
#                median tree cover trend (all cities); 
#                n cities w/ intensifying UHI; 
#                median tree cover trend (UHI Cities); 
#                tree cover trend to hold UHI constant; 
#                n cities w/ warming 
#                median tree cover trend (warming Cities); 
#                tree cover trend to offset warming
StatsCombined$EstTree2001 <- StatsCombined$value.tree.core - StatsCombined$trend.tree.core*10
StatsCombined$EstTree2020 <- StatsCombined$value.tree.core + StatsCombined$trend.tree.core*10
StatsCombined$TargetTree2020UHI <- StatsCombined$EstTree2001 + StatsCombined$TargetConstantUHI*20
StatsCombined$TargetTree2020Warming <- StatsCombined$EstTree2001 + StatsCombined$TargetOffsetWarming*20
summary(StatsCombined)


TrendTreesObs <- aggregate(ISOURBID ~ biomeName + biomeCode, data=StatsCombined, FUN=length)
names(TrendTreesObs)[3] <- "N.Analyzed"
TrendTreesObs$EstTree2001 <- aggregate(EstTree2001 ~ biomeName, data=StatsCombined, FUN=median, na.rm=T)[,2]
TrendTreesObs$EstTree2020 <- aggregate(EstTree2020 ~ biomeName, data=StatsCombined, FUN=median, na.rm=T)[,2]
TrendTreesObs$TrendObsTreeMed <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined, FUN=median, na.rm=T)[,2]
TrendTreesObs$TrendObsTreelo <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined, FUN=quantile, 0.25)[,2]
TrendTreesObs$TrendObsTreehi <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined, FUN=quantile, 0.75)[,2]
TrendTreesObs

TrendTreesUHI <- aggregate(ISOURBID ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=length)
names(TrendTreesUHI)[2] <- "N.UHI"
TrendTreesUHI$TrendUHIMed <- aggregate(trend.LST.diff ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendTreesUHI$TrendUHIlo <- aggregate(trend.LST.diff ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendTreesUHI$TrendUHIhi <- aggregate(trend.LST.diff ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendTreesUHI$TrendUHITreeMed <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendTreesUHI$TrendUHITreelo <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendTreesUHI$TrendUHITreehi <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendTreesUHI$TargetUHITreeMed <- aggregate(TargetConstantUHI ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=median, na.rm=T)[,2]
TrendTreesUHI$TargetUHITreelo <- aggregate(TargetConstantUHI ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.25)[,2]
TrendTreesUHI$TargetUHITreehi <- aggregate(TargetConstantUHI ~ biomeName, data=StatsCombined[intensifyUHI,], FUN=quantile, 0.75)[,2]
TrendTreesUHI <- merge(TrendTreesObs, TrendTreesUHI, all=T)
TrendTreesUHI

TableTrendsUHI <- data.frame(Biome=TrendTreesUHI$biomeName,
                          N.Analyzed = TrendTreesUHI$N.Analyzed,
                          N.IntensifyUHI = TrendTreesUHI$N.UHI,
                          UHITrend = paste0(round(TrendTreesUHI$TrendUHIMed, 2), " (", round(TrendTreesUHI$TrendUHIlo, 2), " - ", round(TrendTreesUHI$TrendUHIhi, 2),")"),
                          UHITreeTrend = paste0(round(TrendTreesUHI$TrendUHITreeMed, 2), " (", round(TrendTreesUHI$TrendUHITreelo, 2), " - ", round(TrendTreesUHI$TrendUHITreehi, 2),")"),
                          UHITreeTarget = paste0(round(TrendTreesUHI$TargetUHITreeMed, 2), " (", round(TrendTreesUHI$TargetUHITreelo, 2), " - ", round(TrendTreesUHI$TargetUHITreehi, 2),")"))
TableTrendsUHI <- TableTrendsUHI[order(TableTrendsUHI$Biome),]
TableTrendsUHI

write.csv(TableTrendsUHI, file.path(path.figs, "SuppTable4_Biome_TreeTrendTargets-UHI.csv"), row.names=F)



TrendTreesWarm <- aggregate(ISOURBID ~ biomeName, data=StatsCombined[citiesWarm,], FUN=length)
names(TrendTreesWarm)[2] <- "N.Warm"
TrendTreesWarm$TrendWarmMed <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
TrendTreesWarm$TrendWarmlo <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=quantile, 0.25)[,2]
TrendTreesWarm$TrendWarmhi <- aggregate(trend.LST.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=quantile, 0.75)[,2]
TrendTreesWarm$TrendWarmTreeMed <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
TrendTreesWarm$TrendWarmTreelo <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=quantile, 0.25)[,2]
TrendTreesWarm$TrendWarmTreehi <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined[citiesWarm,], FUN=quantile, 0.75)[,2]
TrendTreesWarm$TargetWarmTreeMed <- aggregate(TargetOffsetWarming ~ biomeName, data=StatsCombined[citiesWarm,], FUN=median, na.rm=T)[,2]
TrendTreesWarm$TargetWarmTreelo <- aggregate(TargetOffsetWarming ~ biomeName, data=StatsCombined[citiesWarm,], FUN=quantile, 0.25)[,2]
TrendTreesWarm$TargetWarmTreehi <- aggregate(TargetOffsetWarming ~ biomeName, data=StatsCombined[citiesWarm,], FUN=quantile, 0.75)[,2]
TrendTreesWarm <- merge(TrendTreesObs, TrendTreesWarm, all=T)
TrendTreesWarm


TableTrendsWarm <- data.frame(Biome=TrendTreesWarm$biomeName,
                             N.Analyzed = TrendTreesWarm$N.Analyzed,
                             N.IntensifyWarm = TrendTreesWarm$N.Warm,
                             WarmTrend = paste0(round(TrendTreesWarm$TrendWarmMed, 2), " (", round(TrendTreesWarm$TrendWarmlo, 2), " - ", round(TrendTreesWarm$TrendWarmhi, 2),")"),
                             WarmTreeTrend = paste0(round(TrendTreesWarm$TrendWarmTreeMed, 2), " (", round(TrendTreesWarm$TrendWarmTreelo, 2), " - ", round(TrendTreesWarm$TrendWarmTreehi, 2),")"),
                             WarmTreeTarget = paste0(round(TrendTreesWarm$TargetWarmTreeMed, 2), " (", round(TrendTreesWarm$TargetWarmTreelo, 2), " - ", round(TrendTreesWarm$TargetWarmTreehi, 2),")"))
TableTrendsWarm <- TableTrendsWarm[order(TableTrendsWarm$Biome),]
TableTrendsWarm

write.csv(TableTrendsWarm, file.path(path.figs, "SuppTable5_Biome_TreeTrendTargets-Warm.csv"), row.names=F)



# # # Making the slope figure (Fig. 3) ----
TrendsTreeAll <- merge(TrendTreesUHI, TrendTreesWarm, all=T)
TrendsTreeAll$YendUHI <- TrendsTreeAll$EstTree2001+20*TrendsTreeAll$TargetUHITreeMed
TrendsTreeAll$YendWarm <- TrendsTreeAll$EstTree2001+20*TrendsTreeAll$TargetWarmTreeMed

# Creating a vector to help annotate
TrendsTreeAll$YlabUHI <-TrendsTreeAll$YendUHI
TrendsTreeAll$YlabWarm <-TrendsTreeAll$YendWarm
TrendsTreeAll
summary(StatsCombined)

figTrends <- ggplot(data=TrendsTreeAll[TrendsTreeAll$N.Analyzed>=50,]) +
  facet_wrap(~biomeCode) +
  geom_segment(data=StatsCombined[StatsCombined$biomeName %in% TrendsTreeAll$biomeName[TrendsTreeAll$N.Analyzed>=50],], aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020), size=0.1, alpha=0.7, color="gray80") +
  geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020, color="Observed Trend"), size=2) +
  geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=YendUHI, color="Mitigate Intensifying UHI"), size=2, linetype="dashed") +
  geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=YendWarm, color="Mitigate Warming Trend"), size=2, linetype="dashed") +
  geom_text(aes(x=2020, y=EstTree2020-1, label=paste0("+",round(20*TrendObsTreeMed, 1), "%"), color="Observed Trend"), hjust=0, show.legend=F, size=3 ) +
  geom_text(aes(x=2020, y=YendUHI+1, label=paste0("+", round(20*TargetUHITreeMed, 1), "%"), color="Mitigate Intensifying UHI"), hjust=0, show.legend=F, size=3) +
  geom_text(aes(x=2020, y=YendWarm+3, label=paste0("+", round(20*TargetWarmTreeMed, 1), "%"), color="Mitigate Warming Trend"), hjust=0, show.legend=F, size=3) +
  scale_color_manual(name="Tree Cover Trends", values=c("Observed Trend" = "#005a32", "Mitigate Intensifying UHI"="#3FA242", "Mitigate Warming Trend"="#A3D16B"))+
  scale_x_continuous(name="Year", breaks=c(2001, 2020), limits=c(1999, 2025)) +
  scale_y_continuous(name="Tree Cover (%)") +
  guides(label=NA) +
  theme(legend.position=c(0.67, 0.15),
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
figTrends


png(file.path(path.figs, "Figure3_TreeCover_ObservedTargets.png"), height=6, width=6, units="in", res=320)
figTrends
dev.off()

tiff(file.path(path.figs, "Figure3_TreeCover_ObservedTargets.tiff"), height=6, width=6, units="in", res=320)
figTrends
dev.off()

