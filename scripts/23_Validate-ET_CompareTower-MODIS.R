# Doing the full quantitative comparisons of Flux Tower data and our 1 km interpolated MODIS product

library(ggplot2)
library(mgcv)
library(nlme)

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")
path.tower <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/ET Validation")
path.analysis <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")

# path.figsMS <- file.path(path.google, "figures_manuscript")

datTower <- read.csv(file.path(path.tower, "FluxTower_ETcomparison_AllTowers.csv"))
datTower <- datTower[!is.na(datTower$ET.pixel),]
length(unique(datTower$SITE_ID))
length(unique(datTower$ISOURBID))
dim(datTower)

datTower$Error.pixel <- datTower$ET.pixel - datTower$ET
datTower$Error.modis <- datTower$ET.modis - datTower$ET
datTower$Error.gldas <- datTower$ET.gldas - datTower$ET
datTower$Error.pixel2 <- datTower$Error.pixel^2
datTower$Error.modis2 <- datTower$Error.modis^2
datTower$Error.gldas2 <- datTower$Error.gldas^2
summary(datTower)

# Pull in our model data to add some of those stats
cities.et <- read.csv(file.path(path.cities, "../UHIs-FinalCityDataForAnalysis.csv"))
summary(cities.et)


# Aggregating to to Tower level to get some summary stats
aggTower <- aggregate(cbind(ET, TA, ET.pixel, ET.modis, ET.gldas, Error.pixel, Error.modis, Error.gldas, Error.pixel2, Error.modis2, Error.gldas2) ~ ISOURBID + ISO3 + NAME + SITE_ID + IGBP + TOWER_LAT + TOWER_LONG, data=datTower, FUN=mean, na.rm=T)
aggTower[,c("RMSE.pixel", "RMSE.modis", "RMSE.gldas")] <- sqrt(aggTower[,c("Error.pixel2", "Error.modis2", "Error.gldas2")])
aggTower$YEARS <- NA
aggTower$n.YRS <- NA

aggTowerSD <- aggregate(cbind(ET, TA, ET.pixel, ET.modis, ET.gldas, Error.pixel, Error.modis, Error.gldas, Error.pixel2, Error.modis2, Error.gldas2) ~ ISOURBID + ISO3 + NAME + SITE_ID + IGBP + TOWER_LAT + TOWER_LONG, data=datTower, FUN=sd, na.rm=T)


for(i in 1:nrow(aggTower)){
  SITE <- aggTower$SITE_ID[i]
  datSite <- datTower[datTower$SITE_ID==SITE,]
  CITY <- aggTower$ISOURBID[i]
  
  rowDatAll <- which(cities.et$ISOURBID==CITY)
  aggTower[i, "ETmodel.R2adj"] <- cities.et$ETmodel.R2adj[rowDatAll]
  aggTower[i, "ETmodel.RMSE"] <- cities.et$ETmodel.RMSE[rowDatAll]
  
  YRS <- unique(datSite$YEAR)
  aggTower[i,"YEARS"] <- paste(YRS, collapse=" ")
  aggTower[i,"YR.min"] <- min(YRS)
  aggTower[i,"YR.max"] <- max(YRS)
  aggTower[i, "n.YRS"] <- length(YRS)
  
  if(length(YRS)>2){
    lmSitePix <- lm(ET ~ ET.pixel, data=datSite)
    lmSiteMod <- lm(ET ~ ET.modis, data=datSite)
    lmSiteGld <- lm(ET ~ ET.gldas, data=datSite)
    summary(lmSitePix)
    summary(lmSiteMod)
    summary(lmSiteGld)
    
    aggTower[i,"R2.pixel"] <- summary(lmSitePix)$r.squared
    aggTower[i,"R2.modis"] <- summary(lmSiteMod)$r.squared
    aggTower[i,"R2.gldas"] <- summary(lmSiteGld)$r.squared
  }
}
summary(aggTower)
mean(aggTower$RMSE.pixel); sd(aggTower$RMSE.pixel)
mean(aggTower$RMSE.modis); sd(aggTower$RMSE.modis)
mean(aggTower$RMSE.gldas); sd(aggTower$RMSE.gldas)

mean(aggTower$R2.pixel, na.rm=T); sd(aggTower$R2.pixel, na.rm=T)
mean(aggTower$R2.modis, na.rm=T); sd(aggTower$R2.modis, na.rm=T)
mean(aggTower$R2.gldas, na.rm=T); sd(aggTower$R2.gldas, na.rm=T)
# aggTower <- aggTower[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG", "YEARS", "n.YRS", "ET", "TA", "ETmodel.R2adj", "ETmodel.RMSE", "ET.pixel", "ET.modis", "ET.gldas", "Error.pixel", "Error.modis", "Error.gldas", "RMSE.pixel", "RMSE.modis", "RMSE.gldas", "R2.pixel", "R2.modis", "R2.gldas")]

write.csv(aggTower, file.path(path.tower, "FluxTower_ETcomparison_AllTowers-Aggregated.csv"), row.names=F)

# create a function to paste mean & sd
# SigFig = significant figures
pasteXSD <- function(x, stdDev, SigFig){ paste0(round(x, SigFig), " (", round(stdDev, SigFig), ")")}

# Create a supplement-worth table summarizing stats for each tower ----
aggTowerTable <- aggTower[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG", "n.YRS", "YR.min", "YR.max")]
aggTowerTable[,c("ETmodel.R2adj", "ETmodel.RMSE")] <- round(aggTower[,c("ETmodel.R2adj", "ETmodel.RMSE")], 2)
aggTowerTable$ET.tower <- pasteXSD(x=aggTower$ET, stdDev=aggTowerSD$ET, SigFig=2)
aggTowerTable$ET.model <- pasteXSD(x=aggTower$ET.pixel, stdDev=aggTowerSD$ET.pixel, SigFig=2)
aggTowerTable$ET.modis <- pasteXSD(x=aggTower$ET.modis, stdDev=aggTowerSD$ET.modis, SigFig=2)
aggTowerTable$ET.gldas <- pasteXSD(x=aggTower$ET.gldas, stdDev=aggTowerSD$ET.gldas, SigFig=2)
aggTowerTable$Error.model <- pasteXSD(x=aggTower$Error.pixel, stdDev=aggTowerSD$Error.pixel, SigFig=2)
aggTowerTable$Error.modis <- pasteXSD(x=aggTower$Error.modis, stdDev=aggTowerSD$Error.modis, SigFig=2)
aggTowerTable$Error.gldas <- pasteXSD(x=aggTower$Error.gldas, stdDev=aggTowerSD$Error.gldas, SigFig=2)
aggTowerTable[,c("RMSE.model", "RMSE.modis", "RMSE.gldas", "R2.model", "R2.modis", "R2.gldas")] <- round(aggTower[,c("RMSE.pixel", "RMSE.modis", "RMSE.gldas", "R2.pixel", "R2.modis", "R2.gldas")], 2)

head(aggTowerTable)
write.csv(aggTowerTable, file.path(path.tower, "SUPPLEMENT_FluxTower_ETcomparison_AllTowers-Aggregated-Clean.csv"), row.names=F)

write.csv(aggTowerTable, file.path(path.analysis, "SUPPLEMENT_FluxTower_ETcomparison_AllTowers-Aggregated-Clean.csv"), row.names=F)

# Create a supplement-worth table summarizing stats for each LC type ----
aggLCmean <- aggregate(cbind(n.YRS, ET, TA, ETmodel.R2adj, ETmodel.RMSE, ET.pixel, ET.modis, ET.gldas, Error.pixel, Error.modis, Error.gldas, RMSE.pixel, RMSE.modis, RMSE.gldas, R2.pixel, R2.modis, R2.gldas) ~ IGBP , data=aggTower, FUN=mean, na.rm=T)
aggLCsd <- aggregate(cbind(n.YRS, ET, TA, ETmodel.R2adj, ETmodel.RMSE, ET.pixel, ET.modis, ET.gldas, Error.pixel, Error.modis, Error.gldas, RMSE.pixel, RMSE.modis, RMSE.gldas, R2.pixel, R2.modis, R2.gldas) ~ IGBP , data=aggTower, FUN=sd, na.rm=T)

for(i in 1:nrow(aggLCmean)){
  IGBP <- aggLCmean$IGBP[i]
  aggLCmean[i,"nCities"] <- length(unique(aggTower$ISOURBID[aggTower$IGBP==IGBP]))
  aggLCmean[i, "nTowers"] <- length(unique(aggTower$SITE_ID[aggTower$IGBP==IGBP]))
}
aggLCmean

aggLCTable <- aggLCmean[,c("IGBP", "nCities", "nTowers")]
aggLCTable$n.YRS <- pasteXSD(x=aggLCmean$n.YRS, stdDev=aggLCsd$n.YRS, SigFig=0)
aggLCTable$ETmodel.R2adj <- pasteXSD(x=aggLCmean$ETmodel.R2adj, stdDev=aggLCsd$ETmodel.R2adj, SigFig=2)
aggLCTable$ETmodel.RMSE <- pasteXSD(x=aggLCmean$ETmodel.RMSE, stdDev=aggLCsd$ETmodel.RMSE, SigFig=2)
aggLCTable$ET.tower <- pasteXSD(x=aggLCmean$ET, stdDev=aggLCsd$ET, SigFig=2)
aggLCTable$ET.model <- pasteXSD(x=aggLCmean$ET.pixel, stdDev=aggLCsd$ET.pixel, SigFig=2)
aggLCTable$ET.modis <- pasteXSD(x=aggLCmean$ET.modis, stdDev=aggLCsd$ET.modis, SigFig=2)
aggLCTable$ET.gldas <- pasteXSD(x=aggLCmean$ET.gldas, stdDev=aggLCsd$ET.gldas, SigFig=2)
aggLCTable$Error.model <- pasteXSD(x=aggLCmean$Error.pixel, stdDev=aggLCsd$Error.pixel, SigFig=2)
aggLCTable$Error.modis <- pasteXSD(x=aggLCmean$Error.modis, stdDev=aggLCsd$Error.modis, SigFig=2)
aggLCTable$Error.gldas <- pasteXSD(x=aggLCmean$Error.gldas, stdDev=aggLCsd$Error.gldas, SigFig=2)
aggLCTable$RMSE.model <- pasteXSD(x=aggLCmean$RMSE.pixel, stdDev=aggLCsd$RMSE.pixel, SigFig=2)
aggLCTable$RMSE.modis <- pasteXSD(x=aggLCmean$RMSE.modis, stdDev=aggLCsd$RMSE.modis, SigFig=2)
aggLCTable$RMSE.gldas <- pasteXSD(x=aggLCmean$RMSE.gldas, stdDev=aggLCsd$RMSE.gldas, SigFig=2)
aggLCTable$R2.model <- pasteXSD(x=aggLCmean$R2.pixel, stdDev=aggLCsd$R2.pixel, SigFig=2)
aggLCTable$R2.modis <- pasteXSD(x=aggLCmean$R2.modis, stdDev=aggLCsd$R2.modis, SigFig=2)
aggLCTable$R2.gldas <- pasteXSD(x=aggLCmean$R2.gldas, stdDev=aggLCsd$R2.gldas, SigFig=2)


aggLCTable
write.csv(aggLCTable, file.path(path.tower, "SupplementalData-2_FluxTower_Summary_Landcover-Clean.csv"), row.names=F)


aggTowerStack <- stack(aggTower[,c("RMSE.pixel", "RMSE.modis", "RMSE.gldas")])
names(aggTowerStack) <- c("RMSE", "dataset")
aggTowerStack$IGBP <- aggTower$IGBP
aggTowerStack$ISOURBID <- aggTower$ISOURBID
aggTowerStack$R2 <- stack(aggTower[,c("R2.pixel", "R2.modis", "R2.gldas")])[,"values"]
aggTowerStack$dataset <- car::recode(aggTowerStack$dataset, "'RMSE.pixel'='Model'; 'RMSE.modis'='MODIS'; 'RMSE.gldas'='GLDAS'")
aggTowerStack$dataset <- factor(aggTowerStack$dataset, levels=c("Model", "MODIS", "GLDAS"))
summary(aggTowerStack)

nlcdPaletteGG = c("Urban-Open"= '#dec5c5', "Urban-Low"='#d99282', "Urban-Medium"='#eb0000', "Urban-High"= '#ab0000', "Forest" = '#68ab5f', "Grassland"='#dfdfc2', "Crop"='#ab6c28');

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

colorsIGBP <- c("WET" = "#6c9fb8", "EBF" = "#1c5f2c", "DBF" = "#68ab5f", "MF" = "#b5c58f", "OSH" = "#ccb879", "GRA" = "#dfdfc2", "CRO" = "#ab6c28", "URB" = "#eb0000")
colorIGBPdf <- data.frame(IGBP.Code=names(colorsIGBP), color=colorsIGBP, description=c("Wetland", "Crop", "Grassland", "Urban", "Forest, Dcd. Brd.", "Forest, Evg. Brd.", "Shrubland", "Forest, Mixed"))

order.IGBP <- c("WET", "EBF", "DBF", "MF", "OSH", "GRA", "CRO", "URB")
aggTowerStack$IGBP <- factor(aggTowerStack$IGBP, levels=order.IGBP)
colorIGBPdf$IGBP.Code <- factor(colorIGBPdf$IGBP.Code, levels=order.IGBP)
colorIGBPdf <- colorIGBPdf[sort(colorIGBPdf$IGBP.Code),]
  

aggTowerStack2 <- stack(aggTowerStack[,c("RMSE", "R2")])
aggTowerStack2[,c("dataset", "IGBP", "ISOURBID")] <- aggTowerStack[,c("dataset", "IGBP", "ISOURBID")]
summary(aggTowerStack2)

valMeans <- aggregate(values ~ dataset + ind, data=aggTowerStack2, FUN=mean, na.rm=T)

png(file.path(path.tower, "FluxTower_Validation_SummaryStats.png"), height=8, width=8, units="in", res=220)
ggplot(data=aggTowerStack2) +
  facet_grid(dataset~ind, scales="free_x") +
  geom_histogram(aes(x=values, fill=IGBP)) +
  geom_vline(data=valMeans, aes(xintercept=values), linetype="dashed", linewidth=1.5) +
  scale_fill_manual(values=c("WET" = "#6c9fb8", "EBF" = "#1c5f2c", "DBF" = "#68ab5f", "MF" = "#b5c58f", "OSH" = "#ccb879", "GRA" = "#dfdfc2", "CRO" = "#ab6c28", "URB" = "#eb0000"),
                    labels=c("Wetland", "Forest, Evg. Broad.", "Forest, Dec. Broad", "Forest, Mixed", "Shrubland", "Grassland", "Cropland", "Urban")) +
  theme_bw()
dev.off()

mean(aggTower$RMSE.pixel); sd(aggTower$RMSE.pixel)
mean(aggTower$RMSE.modis); sd(aggTower$RMSE.modis)


# Number of towers where our model does better than MODIS
nrow(aggTower)
length(which(aggTower$RMSE.pixel<aggTower$RMSE.modis))
length(which(aggTower$RMSE.pixel<aggTower$RMSE.gldas))

length(which(!is.na(aggTower$R2.pixel)))
length(which(aggTower$R2.pixel>aggTower$R2.modis))
length(which(aggTower$R2.pixel>aggTower$R2.gldas))




summary(datTower[abs(datTower$Error.pixel)>abs(datTower$Error.modis) & !is.na(datTower$Error.modis),])
summary(datTower[abs(datTower$Error.pixel)>1,])

ggplot(data=datTower) +
  geom_histogram(aes(x=Error.pixel, fill=IGBP)) +
  geom_vline(xintercept=0, linetype="dashed") +
  theme_bw()

# ggplot(data=datTower) +
#   geom_histogram(aes(x=Error.pixelSD, fill=IGBP)) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   # coord_cartesian(xlim=c(-5,10)) +
#   theme_bw()

ggplot(data=datTower) +
  geom_point(aes(x=ET.pixel, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")

ggplot(data=datTower) +
  geom_point(aes(x=ET.modis, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")

ggplot(data=datTower) +
  geom_point(aes(x=TA.gldas, y=TA)) +
  geom_abline(intercept=0, slope=1, color="black")

lmTA <- lm(TA ~ TA.gldas, data=datTower)
summary(lmTA)

lmETgldas <- lm(ET ~ ET.gldas, data=datTower)
summary(lmETgldas)

lmETmodis <- lm(ET ~ ET.modis, data=datTower[,])
summary(lmETmodis)

# When comparing the fit with MODIS, important to make sure we're doing apples-to-apples
lmETpixel <- lm(ET ~ ET.pixel, data=datTower[!is.na(datTower$ET.modis),])
summary(lmETpixel)

lmETpg <- lm(ET.gldas ~ ET.pixel, data=datTower)
summary(lmETpg)


lmETtower <- lm(ET ~ ET.modTower, data=datTower[!is.na(datTower$ET.modis),])
summary(lmETtower)

lmETtowerAll <- lm(ET ~ ET.modTower, data=datTower)
summary(lmETtowerAll)



lmETavg <- lm(ET ~ ET.modTower, data=aggTower, na.action=na.omit)
summary(lmETavg)

lmeETavg <- lme(ET ~ ET.modTower, random=list(SITE_ID=~1), data=aggTower, na.action=na.omit)
summary(lmeETavg)
MuMIn::r.squaredGLMM(lmeETavg)

ggplot(data=aggTower) +
  geom_point(aes(x=ET.pixel, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")


lmeETpix <- lme(ET ~ ET.pixel, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmeETpix)
MuMIn::r.squaredGLMM(lmeETpix)

lmeETpix2 <- lme(ET ~ ET.pixel, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower[!is.na(datTower$ET.modis),], na.action=na.omit)
# summary(lmeETpix2)
MuMIn::r.squaredGLMM(lmeETpix2)

lmeETmodis <- lme(ET ~ ET.modis, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmeETmodis)
MuMIn::r.squaredGLMM(lmETmodis)


lmETgldas <- lme(ET ~ ET.gldas, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmETgldas)
MuMIn::r.squaredGLMM(lmETgldas)

lmETgldas2 <- lme(ET.gldas ~ ET.modTower, random=list(SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmETgldas2)
MuMIn::r.squaredGLMM(lmETgldas2)


png(file.path(path.tower, "FluxTower_ETcomparison_AllTowers_scatter.png"), height=8, width=8, units="in", res=220)
ggplot(data=datTower) +
  geom_point(aes(x=ET.pixel, y=ET, color=Dataset)) +
  geom_abline(intercept=0, slope=1, color="black")
dev.off()

png(file.path(path.tower, "FluxTower_ETcomparison_AllTowers-IGBP_scatter.png"), height=8, width=8, units="in", res=220)
ggplot(data=datTower) +
  geom_point(aes(x=ET.pixel, y=ET, color=IGBP)) +
  geom_abline(intercept=0, slope=1, color="black")
dev.off()

png(file.path(path.tower, "FluxTower_ETcomparison_Urban.png"), height=8, width=8, units="in", res=220)
ggplot(data=datTower[datTower$IGBP=="URB",]) +
  geom_point(aes(x=ET.pixel, y=ET, color=IGBP)) +
  geom_abline(intercept=0, slope=1, color="black")
dev.off()


lmeETpixUrbMod <- lme(ET ~ ET.modis, random=list(SITE_ID=~1), data=datTower[datTower$IGBP=="URB",], na.action=na.omit)
summary(lmeETpixUrbMod)
MuMIn::r.squaredGLMM(lmeETpixUrbMod) 

lmeETpixUrb <- lme(ET ~ ET.pixel, random=list(SITE_ID=~1), data=datTower[datTower$IGBP=="URB",], na.action=na.omit)
summary(lmeETpixUrb)
MuMIn::r.squaredGLMM(lmeETpixUrb) 

lmeETpixUrb2 <- lme(ET ~ ET.pixel, random=list(SITE_ID=~1), data=datTower[datTower$IGBP=="URB" & !is.na(datTower$ET.modis),], na.action=na.omit)
summary(lmeETpixUrb2)
MuMIn::r.squaredGLMM(lmeETpixUrb2) 

lmETpixUrb <- lm(ET ~ ET.pixel, data=datTower[datTower$IGBP=="URB",], na.action=na.omit)
summary(lmETpixUrb)
# MuMIn::r.squaredGLMM(lmeETpixUrb)
