# Extract GLDAS 2.1 climatology from earth engine

## 
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
## 
# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v3"
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)

assetHome <- ee_get_assethome()

##################### 
# 0. Set up some choices for data quality thresholds
##################### 
thresh.sigma <- 6 # Use 6-sigma outliers for the data filtering\
thresh.pts <- 50
thresh.prop <- 0.5 # The proportion of data needed for a time point to be "good"; currenlty 0.5
overwrite=F
##################### 


##################### 
# 0. Set up helper functions
##################### 
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}
##################### 


##################### 
# 1. Load and select cities
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]
cityIdAll <- sdei.df$ISOURBID

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei.first())

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
# ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})

##################### 

##################### 
# 2. Load in data layers 
####################

tempColors <- c(
  '040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
  '0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
  '3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
  'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
  'ff0000', 'de0101', 'c21301', 'a71001', '911003'
)
vizTemp <- list(
  min=10.0,
  max=47.0,
  palette=tempColors
);

vizTempK <- list( 
  min=10.0+273.15,
  max=47.0+273.15,
  palette=tempColors
);

vizPrecip <- list( 
  min=0.,
  max=0.0001,
  palette=rev(tempColors)
);
##################### 

##################### 
# Load in base GLDAS ----
##################### 
# -----------
# GLDAS2.1: https://developers.google.com/earth-engine/datasets/catalog/NASA_GLDAS_V021_NOAH_G025_T3H
# -----------
# bBoxS = ee$Geometry$BBox(-180, -60, 180, 5);
# bBoxN = ee$Geometry$BBox(-180, -5, 180, 75);
# maskBBox <- ee$Geometry$BBox(-180, -60, 180, 75)
# 
# # .1 - Northern Hemisphere: July/August
# # JulAugList <- ee_manage_assetlist(path_asset = "users/crollinson/LST_JulAug_Clean/")
# GLDASJulAug <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
# GLDASJulAug <- GLDASJulAug$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
# ee_print(GLDASJulAug)
# 
# # GLDASJulAug$first()$propertyNames()$getInfo()
# # ee_print(GLDASJulAug$first())
# # Map$addLayer(GLDASJulAug$first()$select('Tair_f_inst'), vizTempK, "Jul/Aug Temperature")
# # Map$addLayer(GLDASJulAug$first()$select('Evap_tavg'), vizPrecip, "Jul/Aug ET")
# # Map$addLayer(GLDASJulAug$first()$select('Rainf_f_tavg'), vizPrecip, "Jul/Aug PR")
# 
# GLDASJulAugAvg <- GLDASJulAug$reduce(ee$Reducer$mean())
# 
# projGLDAS = GLDASJulAugAvg$select('Rainf_f_tavg_mean')$projection()
# projCRS = projGLDAS$crs()
# projTransform <- unlist(projGLDAS$getInfo()$transform)
# 
# saveGLDASNH <- ee_image_to_asset(GLDASJulAugAvg, description="Save_GLDAS_NH", assetId=file.path(assetHome, "GLDAS21_Climatology_2001_2020_JulAug"), maxPixels = 10e9, scale=27829.87, region = bBoxN, crs=projCRS, crsTransform=projTransform, overwrite=T)
# saveGLDASNH$start()
# 
# 
# GLDASJanFeb <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(1,60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
# GLDASJanFeb <- GLDASJanFeb$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
# # ee_print(GLDASJanFeb)
# # Map$addLayer(GLDASJanFeb$first()$select('Evap_tavg'), vizPrecip, "Jan/Feb ET")
# 
# GLDASJanFebAvg <- GLDASJanFeb$reduce(ee$Reducer$mean())
# saveGLDASSH <- ee_image_to_asset(GLDASJanFebAvg, description="Save_GLDAS_SH", assetId=file.path(assetHome, "GLDAS21_Climatology_2001_2020_JanFeb"), maxPixels = 10e9, scale=27829.87, region = bBoxS, crs=projCRS, crsTransform=projTransform, overwrite=T)
# saveGLDASSH$start()

GLDASJulAug <- ee$Image('users/crollinson/GLDAS21_Climatology_2001_2020_JulAug')
ee_print(GLDASJulAug)
# Map$addLayer(GLDASJulAug$select('Tair_f_inst_mean'), vizTempK, "Jul/Aug Temperature")
# Map$addLayer(GLDASJulAug$select('Evap_tavg_mean'), vizPrecip, "Jul/Aug ET")
# Map$addLayer(GLDASJulAug$select('Rainf_f_tavg_mean'), vizPrecip, "Jul/Aug PR")


GLDASJanFeb <- ee$Image('users/crollinson/GLDAS21_Climatology_2001_2020_JanFeb')
ee_print(GLDASJanFeb)
##################### 

##################### 
# Set up the function
##################### 
extractGLDAS <- function(CitySP, CityNames, GLDAS, GoogleFolderSave, overwrite=F, ...){
  # CITIES needs to be a list
  # Vegetation should be the reprojected MODIS44b product with year added in
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    # cityNow <- CitySP$filter('NAME=="Chicago"')$first()
    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNow)
    #-------
    
    
    #-------
    # Extracting Data for the whole region
    #-------
    # Start Tree Cover Layer
    gldasCity <- GLDAS$clip(cityNow)
    # ee_print(gldasCity)
    # Map$addLayer(gldasCity$select("Tair_f_inst_mean"), vizTempK, 'Temperature')
    # Map$addLayer(gldasCity$select('Evap_tavg_mean'), vizPrecip, "Jul/Aug ET")
    # Map$addLayer(gldasCity$select('Rainf_f_tavg_mean'), vizPrecip, "Jul/Aug PR")
    
    exportGLDAS <- ee_image_to_drive(image=gldasCity, description=paste0(cityID, "_GLDAS21"), fileNamePrefix=paste0(cityID, "_GLDAS21"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e7, crs=projCRS, crsTransform=projTransform)
    exportGLDAS$start()
  }  
}
##################### 

##################### 
# 3 . Start extracting data for each city
# NOTE: This will need to become a loop, but lets get it working first
# https://r-spatial.github.io/rgee/articles/rgee03.html
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())

cityIdS <-sdei.df$ISOURBID[sdei.df$LATITUDE<0]
cityIdNW <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE<=0]
cityIdNE1 <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>0 & sdei.df$LONGITUDE<=75]
cityIdNE2 <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>75]
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)

# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "GLDAS")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)


# Running a test case
# CITY = "SWE3477"
# extractVeg(CitySP=citiesUse, CityNames = CITY, TREE=modTree, VEG = modVeg, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# testTree <- raster(file.path(path.google, GoogleFolderSave, paste0(CITY, "_Vegetation_PercentTree.tif")))
# plot(testTree[[1]])


if(length(cityIdS)>0){
  extractGLDAS(CitySP=citiesUse, CityNames = cityIdS, GLDAS=GLDASJanFeb, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}


if(length(cityIdNW)>0){
  extractGLDAS(CitySP=citiesUse, CityNames = cityIdNW, GLDAS=GLDASJulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE1)>0){
  extractGLDAS(CitySP=citiesUse, CityNames = cityIdNE1, GLDAS=GLDASJulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE2)>0){
  extractGLDAS(CitySP=citiesUse, CityNames = cityIdNE2, GLDAS=GLDASJulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

##################### 

# Checking a test case
test <- stack(file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v3/", "GBR4139_GLDAS21.tif"))
plot(test)
test
