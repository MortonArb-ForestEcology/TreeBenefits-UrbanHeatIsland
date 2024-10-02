# Validate ET models using available Flux Tower data
# Step 2: extract the mean summer ET estimate for the pixel of the flux tower
# Step 3: Compare the tower estimate to the pixel; where we have enough data, do a regression for goodness of fit


# library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

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
sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)

towerSP <- st_as_sf(datTower, coords=c("TOWER_LONG", "TOWER_LAT"))
st_crs(towerSP) <- st_crs(sdei.urb)
projMODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
towerSP <- st_transform(towerSP, crs(projMODIS))
summary(towerSP)
datTower[,c("x", "y")] <- st_coordinates(towerSP)
summary(datTower)
# plot(towerSP)

# Loading data we'll need to do the model ET calculation
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")


files.elev <- dir(path.EEout, "elevation")
files.temp <- dir(path.EEout, "GLDAS21_annualMeans")
# files.et <- dir(path.EEout, "ETmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")


for(CITY in unique(datTower$ISOURBID)){
  print(CITY)
  towerCity <- unique(datTower$SITE_ID[datTower$ISOURBID==CITY])
  
  fELEV <- files.elev[grep(CITY, files.elev)]
  fTemp <- files.temp[grep(CITY, files.temp)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  fVEG <- files.veg[grep(CITY, files.veg)]
  TempCity <- read.csv(file.path(path.EEout, fTemp[length(fTemp)]))
  TempCity$Tair_f_inst_mean <- TempCity$Tair_f_inst_mean-273.15
  TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")] <- TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")]*60*60*24
  
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))
  
  coordsCity <- data.frame(coordinates(elevCity)) 
  coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
  coordsVeg <- data.frame(coordinates(treeCity))
  coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)
  
  valsCityVeg <- stack(data.frame(getValues(treeCity)))
  names(valsCityVeg) <- c("cover.tree", "year")
  valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity)))[,1]
  valsCityVeg$x <- coordsVeg$x
  valsCityVeg$y <- coordsVeg$y
  valsCityVeg$location <- coordsVeg$location
  valsCityVeg <- valsCityVeg[!is.na(valsCityVeg$cover.tree),]
  valsCityVeg$year <- as.numeric(substr(valsCityVeg$year,3,6))
  
  valsCity <- merge(valsCityVeg, TempCity, all.x=T)
  summary(valsCity)
  
  etCity <- readRDS(file.path(path.cities, "../ET_models", CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
  # cityDF <- etCity$model
  summary(etCity)
  for(i in seq_along(towerCity)){
    TOWER <- towerCity[i]
    # print(TOWER)
    datNow <- datTower[datTower$SITE_ID==TOWER,]
    towerX <- unique(datNow$x)
    towerY <- unique(datNow$y)
    
    # Calculate the euclidean distance to the points
    pixelDist <- sqrt((valsCity$x-towerX)^2 + (valsCity$y-towerY)^2)
    minPixel <- which(pixelDist==min(pixelDist))
    datPixel <- valsCity[minPixel,]
    datPixel$ET.pixel <- predict(etCity, newdata=datPixel)^2
    datPixel$x <- towerX
    datPixel$y <- towerY
    datPixel$ET.modTower <- predict(etCity, newdata=datPixel)^2
    summary(datPixel)
    
    for(YR in unique(datNow$YEAR)){
      rowNow <- which(datTower$SITE_ID==TOWER & datTower$YEAR==YR)
      if(!any(datPixel$year==YR)) next
      
      datTower[rowNow,c("TA.gldas", "cover.tree", "cover.veg", "ET.gldas", "ET.pixel", "ET.modTower")] <- datPixel[datPixel$year==YR,c("Tair_f_inst_mean", "cover.tree", "cover.veg", "Evap_tavg_mean", "ET.pixel", "ET.modTower")]
    }
 
  }
  # summary(datTower)
}
summary(datTower)
write.csv(datTower, file.path(path.tower, "FluxTower_ETcomparison_AllTowers.csv"))

library(nlme)
lmET <- lme(ET ~ ET.modTower, random=list(SITE_ID=~1), data=datTower, na.action=na.omit)
summary(lmET)
MuMIn::r.squaredGLMM(lmET)

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
