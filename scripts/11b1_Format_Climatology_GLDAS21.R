# Extract GLDAS 2.1 climatology from earth engine

##
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
##
# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v4"
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)

assetHome <- ee_get_assethome()
overwrite=F


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
# Load in base GLDAS & format it ----
#####################
# -----------
# GLDAS2.1: https://developers.google.com/earth-engine/datasets/catalog/NASA_GLDAS_V021_NOAH_G025_T3H
# -----------
bBoxS = ee$Geometry$BBox(-180, -60, 180, 5);
bBoxN = ee$Geometry$BBox(-180, -5, 180, 75);
maskBBox <- ee$Geometry$BBox(-180, -60, 180, 75)

# .1 - Northern Hemisphere: July/August
# JulAugList <- ee_manage_assetlist(path_asset = "users/crollinson/LST_JulAug_Clean/")
GLDASJulAug <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
GLDASJulAug <- GLDASJulAug$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
ee_print(GLDASJulAug)

# GLDASJulAug$first()$propertyNames()$getInfo()
# ee_print(GLDASJulAug$first())
# Map$addLayer(GLDASJulAug$first()$select('Tair_f_inst'), vizTempK, "Jul/Aug Temperature")
# Map$addLayer(GLDASJulAug$first()$select('Evap_tavg'), vizPrecip, "Jul/Aug ET")
# Map$addLayer(GLDASJulAug$first()$select('Rainf_f_tavg'), vizPrecip, "Jul/Aug PR")

GLDASJulAugAvg <- GLDASJulAug$reduce(ee$Reducer$mean())

projGLDAS = GLDASJulAugAvg$select('Rainf_f_tavg_mean')$projection()
projCRS = projGLDAS$crs()
projTransform <- unlist(projGLDAS$getInfo()$transform)

saveGLDASNH <- ee_image_to_asset(GLDASJulAugAvg, description="Save_GLDAS_NH", assetId=file.path(assetHome, "GLDAS21_Climatology_2001_2020_JulAug"), maxPixels = 10e9, scale=27829.87, region = bBoxN, crs=projCRS, crsTransform=projTransform, overwrite=T)
saveGLDASNH$start()


GLDASJanFeb <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(1,60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
GLDASJanFeb <- GLDASJanFeb$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
# ee_print(GLDASJanFeb)
# Map$addLayer(GLDASJanFeb$first()$select('Evap_tavg'), vizPrecip, "Jan/Feb ET")

GLDASJanFebAvg <- GLDASJanFeb$reduce(ee$Reducer$mean())
saveGLDASSH <- ee_image_to_asset(GLDASJanFebAvg, description="Save_GLDAS_SH", assetId=file.path(assetHome, "GLDAS21_Climatology_2001_2020_JanFeb"), maxPixels = 10e9, scale=27829.87, region = bBoxS, crs=projCRS, crsTransform=projTransform, overwrite=T)
saveGLDASSH$start()
