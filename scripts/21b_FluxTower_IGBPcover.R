# Validate ET models using available Flux Tower data
# Step 2: extract the mean summer ET estimate for the pixel of the flux tower
# Step 3: Compare the tower estimate to the pixel; where we have enough data, do a regression for goodness of fit


library(raster); library(sp); library(terra); library(sf)
library(ggplot2)

library(rgee); 
# ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_TowerValidation"
assetHome <- ee_get_assethome()

# Loading data we'll need to do the model ET calculation


ETColors <- c('#ffffff', '#fcd163', '#99b718', '#66a000', '#3e8601', '#207401', '#056201',
              '#004c00', '#011301')

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v5/ET_models")
path.tower <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/ET Validation")
# path.EEoutSpat <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v5")

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
coordsTower <- aggregate(cbind(TOWER_LONG, TOWER_LAT) ~ ISOURBID + NAME + SITE_ID + IGBP, data=datTower, FUN=mean)
towerSP <- st_as_sf(coordsTower, coords=c("TOWER_LONG", "TOWER_LAT"), crs=st_crs(sdei.urb))
ee_tower <- sf_as_ee(towerSP)

# Making sure things imported correctly
Map$addLayer(ee_tower, visParams = list(
  pointRadius = 10,
  color = "FF0000"
), 'Flux Towers')

# Load in the MCD12C1.061 landcover product that has %cover of each IGBP
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12C1#bands
# var igbpLandCover = dataset.select('Majority_Land_Cover_Type_1');
igbpCodes <- data.frame(value=0:16, name=c("WAT", "ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA", "SAV", "GRA", "WET", "CRO", "URB", "CVM", "SNO", "BSV"))

igbpLandCoverVis = list(
  min = 0,
  max = 16.0,
  palette = c(
    '1c0dff', '05450a', '086a10', '54a708', '78d203', '009900', 'c6b044', 'dcd159',
    'dade48', 'fbff13', 'b6ff05', '27ff87', 'c24f44', 'a5a5a5', 'ff6d4c',
    '69fff8', 'f9ffa4'
  )
)
setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

mcd12 <- ee$ImageCollection("MODIS/061/MCD12C1")$map(setYear)
ee_print(mcd12)
Map$addLayer(mcd12$select("Majority_Land_Cover_Type_1")$first(), igbpLandCoverVis, 'IGBP Land Cover');

projMCD = mcd12$first()$projection()
projCRS = projMCD$crs()
projTransform <- unlist(projMCD$getInfo()$transform)


# We want to get the fractional landcover of IGBP landcover classes, which is "Type 1" in the product
# There are 16 IGBP classes;
IGBPbands <- paste0("Land_Cover_Type_1_Percent_Class_", 0:16)

# We want to do two types of extractions: 
# 1. For the flux tower pixel
# 2. Average over the whole area we trained the ET model on for flux tower cities
fluxCities <- unique(towerSP$ISOURBID)

# Go ahead and load the city data like we have in the past for everything
sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})
ee_print(citiesUse)

# Lets loop through the cities first
pb <- txtProgressBar(min=0, max=length(fluxCities), style=3)
for(i in 1:length(fluxCities)){
  setTxtProgressBar(pb, i)
  cityID <- fluxCities[i]
  # cityNow <- CitySP$filter('NAME=="Chicago"')$first()
  cityNow <- citiesUse$filter(ee$Filter$eq('ISOURBID', cityID))
  # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
  # Map$addLayer(cityNow)
  #-------
  
  
  #-------
  # Extracting Data for the whole region
  #-------
  # Start Tree Cover Layer
  igbpCity <- mcd12$map(function(img){
    return(img$clip(cityNow))
  })
  # ee_print(igbpCity)
  # ee_print(igbpCity$first())
  # Map$addLayer(igbpCity$select("Majority_Land_Cover_Type_1")$first(), igbpLandCoverVis, 'IGBP Land Cover');
  
  # Can't export an image collection easily; so rather than mess with it, we'll just skip it
  # export.IGBP <- ee_image_to_drive(image=igbpCity, description=paste0(cityID, "MCD12C1_IGBP_Spat"), fileNamePrefix=paste0(cityID, "_IGBP"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
  # # timePrefix=F, region=cityNow$geometry(), maxPixels=5e7, crs=projCRS, crsTransform=projTransform
  # export.IGBP$start()
  # 
  
  # # Code from NDVI work tor educe to a single value --> will need to be inside a map function()
  tableYr <- ee$FeatureCollection(igbpCity$map(function(img){
    RedMn =img$reduceRegion(reducer= ee$Reducer$mean(), geometry=cityNow$geometry(),
                            scale=5600, # hard-coded, but it's what has to happen to work
                            maxPixels=1e6)
    # RedMn$getInfo()  
    return(ee$Feature(NULL, RedMn)$set('year', img$get('year'))$set('system:index', img$get('system:index')))
    
  }))
  
  fileNamePrefix = paste0(cityID, "_MCD12C1_IGBP")
  
  igbpStatsSave <- ee_table_to_drive(collection=tableYr,
                                      description=paste0("Save_", fileNamePrefix),
                                      folder=GoogleFolderSave,
                                      fileNamePrefix=fileNamePrefix,
                                      timePrefix=F,
                                      fileFormat="CSV",
                                      selectors=c("year", IGBPbands))
  igbpStatsSave$start()
}  

# ftest <- dir(file.path("~/Google Drive/My Drive/",GoogleFolderSave), "IGBP")
# test  <- read.csv(file.path("~/Google Drive/My Drive/",GoogleFolderSave, ftest[2]))
# apply(test[,2:ncol(test)], 1, sum)

# Now to do the same thing with the location of each flux tower
igbpVals <- ee_extract(x=mcd12$select(IGBPbands), y=ee_tower, scale=5600, fun=ee$Reducer$mean(), via="getInfo")
dim(igbpVals)
# summary(igbpVals)
igbpVals[1:5, 1:10]
# head(igbpVals)

datStack <- stack(igbpVals[,grep(IGBPbands[1], names(igbpVals))])
names(datStack) <- c(igbpCodes$name[1], "year")
datStack$year <- as.numeric(substr(datStack$year, 2, 5))
datStack$SITE_ID <- igbpVals$SITE_ID
head(datStack)

# Now I need to reshahpe this
colsID <- c("IGBP", "ISOURBID", "NAME", "SITE_ID")
yrsIGBP <-  unique(datStack$year)
dfClean <- data.frame(IGBP=rep(igbpVals$IGBP, length(yrsIGBP)),
                      ISOURBID=rep(igbpVals$ISOURBID, length(yrsIGBP)),
                      NAME=rep(igbpVals$NAME, length(yrsIGBP)),
                      SITE_ID=rep(igbpVals$SITE_ID, length(yrsIGBP)),
                      year = rep(yrsIGBP, each=nrow(igbpVals))
                      )
dim(dfClean)

for(i in 1:nrow(igbpCodes)){
  colsNow <- paste0("X", yrsIGBP, "_01_01_", IGBPbands[i])
  datStack <- stack(igbpVals[,colsNow])
  names(datStack) <- c(igbpCodes$name[i], "year")
  datStack$SITE_ID <- igbpVals$SITE_ID
  datStack$year <- as.numeric(substr(datStack$year, 2, 5))
  head(datStack)
  dim(datStack)
  
  # if(!"year" %in% names(dfClean)) dfClean[,"year"] <- datStack$year
  dfClean[,igbpCodes$name[i]] <- datStack[,igbpCodes$name[i]]
}
summary(dfClean)

write.csv(dfClean, file.path(path.tower, "FluxTowers_IGBP_pixel.csv"), row.names=F)
head(dfClean)
