# Validate ET models using available Flux Tower data
# Step 2: extract the mean summer ET estimate for the pixel of the flux tower
# Step 3: Compare the tower estimate to the pixel; where we have enough data, do a regression for goodness of fit


library(raster); library(sp); library(terra); library(sf)
library(ggplot2)
library(mgcv)
library(nlme)

ETColors <- c('#ffffff', '#fcd163', '#99b718', '#66a000', '#3e8601', '#207401', '#056201',
              '#004c00', '#011301')

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4/data_processed_final")
path.tower <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/ET Validation")


ameriflux <- read.csv(file.path(path.tower, "AmerifluxSites_ET_summerMeans.csv"))
ameriflux$Dataset <- "Ameriflux"

euroflux <- read.csv(file.path(path.tower, "EurofluxSites_ET_summerMeans.csv"))
euroflux$Dataset <- "Euroflux"

fluxnet <- read.csv(file.path(path.tower, "FluxnetSites_ET_summerMeans.csv"))
fluxnet$Dataset <- "Fluxnet2015"

datTower <- rbind(ameriflux, euroflux[!euroflux$SITE_ID %in% ameriflux$SITE_ID,], fluxnet[!fluxnet$SITE_ID %in% euroflux$SITE_ID,])
datTower <- datTower[!is.na(datTower$ET),]
datTower$ISOURBID <- as.factor(datTower$ISOURBID)
datTower$ISO3 <- as.factor(datTower$ISO3)
datTower$SITE_ID <- as.factor(datTower$SITE_ID)
datTower$IGBP <- as.factor(datTower$IGBP)
datTower$Dataset <- as.factor(datTower$Dataset)
summary(datTower)
length(unique(datTower$SITE_ID))
length(unique(datTower$ISOURBID))
hist(datTower$ET)

# We need to get the tower coords in MODIS projection, so we need to make it a spatial file
projMODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
sdei.urb <- st_transform(sdei.urb, crs(projMODIS))
summary(sdei.urb)
# summary(sdi.urb[sdei.urb$])

# ggplot(data=sdei.urb) +
#   # coord_cartesian(xlim=c(-60,-80), ylim=c(40,60)) +
#   geom_sf() +
#   # scale_x_continuous(limits=c(-90, -70)) +
#   # scale_y_continuous(limits=c(30,45))
#   scale_x_continuous(limits=c(12032742, 12749003)) +
#   scale_y_continuous(limits=c(3642276,4086117))
  

towerSP <- st_as_sf(datTower, coords=c("TOWER_LONG", "TOWER_LAT"))
st_crs(towerSP) <- st_crs(sdei.urb)
towerSP <- st_transform(towerSP, crs(projMODIS))
summary(towerSP)
datTower[,c("x", "y")] <- st_coordinates(towerSP)
summary(datTower)
# plot(towerSP)

# Loading data we'll need to do the model ET calculation
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")


files.elev <- dir(path.EEout, "elevation")
files.temp <- dir(path.EEout, "GLDAS21_annualMeans")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.et <- dir(path.EEout, "ETmean")


for(CITY in unique(datTower$ISOURBID)){
  print(CITY)
  towerCity <- unique(datTower$SITE_ID[datTower$ISOURBID==CITY])
  
  fELEV <- files.elev[grep(CITY, files.elev)]
  fTemp <- files.temp[grep(CITY, files.temp)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  fVEG <- files.veg[grep(CITY, files.veg)]
  fET <- files.et[grep(CITY, files.et)]
  
  TempCity <- read.csv(file.path(path.EEout, fTemp[length(fTemp)]))
  TempCity$Tair_f_inst_mean <- TempCity$Tair_f_inst_mean-273.15
  TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")] <- TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")]*60*60*24
  
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))/8
  layers.use <- names(treeCity)[names(treeCity) %in% names(etCity)]
  
  layersET <- names(etCity)[names(etCity) %in% names(treeCity)]
  
  coordsET <- data.frame(coordinates(etCity))
  coordsET$location <- paste0("x", coordsET$x, "y", coordsET$y)
  
  coordsCity <- data.frame(coordinates(elevCity)) 
  coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
  coordsVeg <- data.frame(coordinates(treeCity))
  coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)
  
  valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
  names(valsCityVeg) <- c("cover.tree", "year")
  valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
  valsCityVeg$x <- coordsVeg$x
  valsCityVeg$y <- coordsVeg$y
  valsCityVeg$location <- coordsVeg$location
  
  valsCity <- valsCityVeg
  # summary(valsCity)
  
  valsET <- stack(data.frame(getValues(etCity)))
  names(valsET) <- c("ET", "year")
  valsET$x <- coordsET$x
  valsET$y <- coordsET$y
  valsET$location <- coordsET$location
  summary(valsET)
  
  # nrow(coordsCity); nrow(coordsTemp)
  if(all(coordsET$location == coordsCity$location)){
    valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
    # valsCity <- merge(valsCity, valsTemp, all.x=T, all.y=T)
  } else if( nrow(coordsET) == nrow (coordsCity)) {  
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsCity$x, y1=coordsCity$y, x2 = valsET$x, y2 = valsET$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
      
    }  else {
      stop("Something's really off")
    }
  }
  
  # Re-labeling the year column to make life easier
  # valsCity <- valsCity[!is.na(valsCity$cover.tree),] # need to actually NOT delete the NAs!
  valsCity$year <- as.numeric(substr(valsCity$year,3,6))
  valsCity <- merge(valsCity, TempCity, all.x=T)
  summary(valsCity)
  
  modCity <- readRDS(file.path(path.cities, "../ET_models", CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
  # cityDF <- modCity$model
  # summary(modCity)
  
  datTower[datTower$ISOURBID==CITY, "ETmodel.R2"] <- summary(modCity)$r.sq
  
  for(i in seq_along(towerCity)){
    TOWER <- towerCity[i]
    # print(TOWER)
    datNow <- datTower[datTower$SITE_ID==TOWER,]
    towerX <- unique(datNow$x)
    towerY <- unique(datNow$y)
    
    # If our tower isn't actually in the range of which we have values for, skip it! 
    
    # Calculate the euclidean distance to the points
    pixelDist <- sqrt((valsCity$x-towerX)^2 + (valsCity$y-towerY)^2)
    minPixel <- which(pixelDist==min(pixelDist))
    datPixel <- valsCity[minPixel,]
    datPixel$ET.pixel <- predict(modCity, newdata=datPixel)^2
    datPixel$x <- towerX
    datPixel$y <- towerY
    datPixel$ET.modTower <- predict(modCity, newdata=datPixel)^2
    summary(datPixel)
    
    
    png(file.path(path.tower, "TowerLocationMaps", paste0(CITY, "_", TOWER, ".png")),
        height=8, width=8, units="in", res=220)
    print(
      ggplot(data=sdei.urb[sdei.urb$ISOURBID==CITY,]) +
        ggtitle(paste0(CITY, " (", datNow$NAME[1], ")", " - ", TOWER)) +
        # coord_equal() +
        geom_point(data=valsCity[valsCity$year==min(valsCity$year),], aes(x=x, y=y, color=ET), alpha=0.5) +
        geom_sf(fill=NA, color="blue3", linewidth=2) +
        geom_point(data=datNow[1,], aes(x=x, y=y), size=5, color="blue") +
        scale_color_stepsn(name="ET modis", colors=ETColors, n.breaks=13) 
    )
    dev.off()
    
    
    for(YR in unique(datNow$YEAR)){
      rowNow <- which(datTower$SITE_ID==TOWER & datTower$YEAR==YR)
      if(!any(datPixel$year==YR)) next
      
      datTower[rowNow,c("TA.gldas", "cover.tree", "cover.veg", "ET.modis", "ET.gldas", "ET.pixel", "ET.modTower")] <- datPixel[datPixel$year==YR,c("Tair_f_inst_mean", "cover.tree", "cover.veg", "ET", "Evap_tavg_mean", "ET.pixel", "ET.modTower")]
    }
 
  }
  # summary(datTower)
}
summary(datTower)
write.csv(datTower, file.path(path.tower, "FluxTower_ETcomparison_AllTowers.csv"))

datTower$Error.Pixel <- datTower$ET.pixel - datTower$ET
datTower$Error.Modis <- datTower$ET.modis - datTower$ET
summary(datTower)


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

lmETmodis <- lm(ET ~ ET.modis, data=datTower)
summary(lmETmodis)

lmETpixel <- lm(ET ~ ET.pixel, data=datTower)
summary(lmETpixel)

lmETpg <- lm(ET.gldas ~ ET.pixel, data=datTower)
summary(lmETpg)


lmETtower <- lm(ET ~ ET.modTower, data=datTower)
summary(lmETtower)


aggTower <- aggregate(cbind(ET, TA, ET.pixel, ET.modTower, ET.gldas, Error.Pixel)~ISOURBID + ISO3 + NAME + SITE_ID + IGBP + TOWER_LAT + TOWER_LONG, data=datTower, FUN=mean)

lmETavg <- lm(ET ~ ET.modTower, data=aggTower, na.action=na.omit)
summary(lmETavg)

lmeETavg <- lme(ET ~ ET.modTower, random=list(SITE_ID=~1), data=aggTower, na.action=na.omit)
summary(lmeETavg)
MuMIn::r.squaredGLMM(lmeETavg)

ggplot(data=aggTower) +
  geom_point(aes(x=ET.pixel, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")


lmET <- lme(ET ~ ET.modTower, random=list(SITE_ID=~1), data=datTower, na.action=na.omit)
summary(lmET)
MuMIn::r.squaredGLMM(lmET)

lmETgldas <- lme(ET ~ ET.gldas, random=list(SITE_ID=~1), data=datTower, na.action=na.omit)
summary(lmETgldas)
MuMIn::r.squaredGLMM(lmETgldas)

lmETgldas2 <- lme(ET.gldas ~ ET.modTower, random=list(SITE_ID=~1), data=datTower, na.action=na.omit)
summary(lmETgldas2)


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
