# Validate ET models using available Flux Tower data
# Step 2: extract the mean summer ET estimate for the pixel of the flux tower
# Step 3: Compare the tower estimate to the pixel; where we have enough data, do a regression for goodness of fit


library(raster); library(sp); library(terra); library(sf)
library(ggplot2)
library(mgcv)
library(nlme)

library(rgee); 
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_TowerValidation"
assetHome <- ee_get_assethome()

# Loading data we'll need to do the model ET calculation
path.EEoutSpat <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")


ETColors <- c('#ffffff', '#fcd163', '#99b718', '#66a000', '#3e8601', '#207401', '#056201',
              '#004c00', '#011301')

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")
path.tower <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/ET Validation")


files.elev <- dir(path.EEout, "elevation")
files.temp <- dir(path.EEout, "GLDAS21_annualMeans")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.et <- dir(path.EEout, "ETmean")

# Getting our SDEI data layer
sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]


# Now getting the flux tower data
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

# Making the tower list a shapefile so we can more easily line it up
towerSP <- st_as_sf(datTower, coords=c("TOWER_LONG", "TOWER_LAT"), crs=st_crs(sdei.urb))

# Converting things to earth engine so we can get the reproject/transforms to look right
ee_sdei <- sf_as_ee(sdei.urb[sdei.urb$ISOURBID %in% datTower$ISOURBID,]) # Making
ee_tower <- sf_as_ee(towerSP)

# Making sure things imported correctly
Map$addLayer(ee_tower, visParams = list(
  pointRadius = 10,
  color = "FF0000"
), 'Flux Towers')


# Reading in one of our raster layers so we can use its projection info
vegMask <- ee$Image("users/crollinson/MOD44b_1km_Reproj_VegMask")
# Map$addLayer(vegMask)

# Pulling all the components even though we just need the whole projection
projMask = vegMask$projection()
projCRS = projMask$crs()
projTransform <- unlist(projMask$getInfo()$transform)

# Transform the tower poitns
ee_towerTrans <- ee_tower$map(function(x){ 
  x$transform(projMask)
  })
# ee_print(ee_tower2)

# Transform the city shapefiles to make it easier to check
ee_sdeiTrans <- ee_sdei$map(function(x){ 
  x$transform(projMask, maxError=42) # 42 because why not... it's the answer to life, the universe and everything
})
# ee_print(ee_sdei2)

# Save the 
towerMODIS <- ee_as_sf(ee_towerTrans, via="drive", dsn="FluxTowers_MODISproj.shp", crs=projCRS, timePrefix = F, overwrite=T)
sdeiMODIS <- ee_as_sf(ee_sdeiTrans, via="drive", dsn="SDEI-Towers_MODISproj.shp", crs=projCRS, timePrefix = F, overwrite=T)
# 
# ee_as_sf(ee_tower2, via="drive", dsn="Tower-test2.csv", crs=projCRS)

# Move our shapefiles elsewhere
if(!dir.exists("../data_shapefiles")) dir.create("../data_shapefiles", recursive=T)
fmove <- dir(".", "MODISproj")
for(i in seq_along(fmove)){
  file.copy(from=fmove[i], to=file.path("../data_shapefiles", fmove[i]), overwrite=T, copy.mode=T)
  file.remove(fmove[i])
}

# # We need to get the tower coords in MODIS projection, so we need to make it a spatial file
# # Opening an example raster to pull what we need --> this is urbana-champaign
testElev <- raster(file.path(path.EEout, "USA31965_elevation.tif"))
plot(testElev)
testElev
elevPTs <- data.frame(elev=getValues(testElev))
elevPTs[,c("x", "y")] <- coordinates(testElev)
summary(elevPTs)
 
cityOrig <- st_transform(sdei.urb[sdei.urb$ISOURBID=="USA31965",], projection(testElev))
cityNew <- sdeiMODIS[sdeiMODIS$ISOURBID=="USA31965",]
testCityOrig <- st_coordinates(cityOrig)
testCityNew <- st_coordinates(cityNew)

datTower[,c("x","y")] <- st_coordinates(towerMODIS)

# 
ggplot() +
  # coord_cartesian(xlim=c(-60,-80), ylim=c(40,60)) +
  coord_equal() +
  geom_tile(data=elevPTs, aes(x=x,y=y, fill=elev)) +
  geom_polygon(data=testCityOrig, aes(x=X, y=Y), fill=NA, color="black") +
  geom_polygon(data=testCityNew, aes(x=X, y=Y), fill=NA, color="red2") +
  geom_point(data=datTower[datTower$ISOURBID=="USA31965",], aes(x=x, y=y), color="orange2")


for(CITY in unique(datTower$ISOURBID)){
  print(CITY)
  towerCity <- unique(datTower$SITE_ID[datTower$ISOURBID==CITY])
  citySP <- sdeiMODIS[sdeiMODIS$ISOURBID==CITY,]
  cityPoly <- st_coordinates(citySP)
  
  
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
  valsCity <- valsCity[!is.na(valsCity$cover.tree),] # need to actually NOT delete the NAs!
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
      ggplot() +
        ggtitle(paste0(CITY, " (", datNow$NAME[1], ")", " - ", TOWER)) +
        coord_equal() +
        geom_raster(data=valsCity[valsCity$year==min(valsCity$year),], aes(x=x, y=y, fill=ET)) +
        # geom_polygon(data=cityPoly, aes(x=X, y=Y), fill=NA, color="blue4", linewidth=1) +
        geom_point(data=datNow[1,], aes(x=x, y=y), size=5, color="blue") +
        scale_fill_stepsn(name="ET modis", colors=ETColors, n.breaks=13) 
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
write.csv(datTower, file.path(path.tower, "FluxTower_ETcomparison_AllTowers.csv"), row.names=F)

