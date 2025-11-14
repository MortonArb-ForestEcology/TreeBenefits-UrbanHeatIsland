# Extract annual summer temperature from GLDAS 2.1 -- save as a table this time

##
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
##
# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
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
# 1. Load in data layers
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
# GLDASJulAug <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
# GLDASJulAug <- GLDASJulAug$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
# ee_print(GLDASJulAug)
# Map$addLayer(GLDASJulAug$select('Tair_f_inst_mean'), vizTempK, "Jul/Aug Temperature")
# Map$addLayer(GLDASJulAug$select('Evap_tavg_mean'), vizPrecip, "Jul/Aug ET")
# Map$addLayer(GLDASJulAug$select('Rainf_f_tavg_mean'), vizPrecip, "Jul/Aug PR")
ee_manage_create(file.path(assetHome, "GLDAS_Annual_JulAug"), asset_type="ImageCollection")
ee_manage_create(file.path(assetHome, "GLDAS_Annual_JanFeb"), asset_type="ImageCollection")

# Doing northern hemisphere
for(YR in 2001:2020){
  GLDASJulAug <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date(paste0(YR,"-01-01"), paste0(YR, "-12-31")))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
  GLDASJulAug <- GLDASJulAug$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
  
  projGLDAS = GLDASJulAug$first()$projection()
  projCRS = projGLDAS$crs()
  projTransform <- unlist(projGLDAS$getInfo()$transform)
  
  gldasMean <- GLDASJulAug$reduce(ee$Reducer$mean())$set(ee$Dictionary(list(year=YR)))
  # ee_print(gldasMean)
  # Map$addLayer(gldasMean$select('Tair_f_inst_mean'), vizTempK, "Jul/Aug Temperature")
  
  saveGLDASAnn <- ee_image_to_asset(gldasMean, description=paste("Save", "GLDAS_Annual_JulAug", YR, sep="_"), assetId=file.path(assetHome, "GLDAS_Annual_JulAug", paste("GLDAS_Annual_JulAug", YR, sep="_")), maxPixels = 10e9, scale=27829.87, region = bBoxN, crs=projCRS, crsTransform=projTransform, overwrite=T)
  saveGLDASAnn$start()
}

# Now doing southern hemisphere
for(YR in 2001:2020){
  GLDASJanFeb <- ee$ImageCollection('NASA/GLDAS/V021/NOAH/G025/T3H')$filter(ee$Filter$dayOfYear(1,60))$filter(ee$Filter$date(paste0(YR,"-01-01"), paste0(YR, "-12-31")))$map(addTime)$select(c("Evap_tavg", "Rainf_f_tavg", "Tair_f_inst"));
  GLDASJanFeb <- GLDASJanFeb$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
  
  projGLDAS = GLDASJanFeb$first()$projection()
  projCRS = projGLDAS$crs()
  projTransform <- unlist(projGLDAS$getInfo()$transform)
  
  
  gldasMean <- GLDASJanFeb$reduce(ee$Reducer$mean())$set(ee$Dictionary(list(year=YR)))
  # ee_print(gldasMean)
  # Map$addLayer(gldasMean$select('Tair_f_inst_mean'), vizTempK, "Jul/Aug Temperature")
  
  saveGLDASAnn <- ee_image_to_asset(gldasMean, description=paste("Save", "GLDAS_Annual_FanFeb", YR, sep="_"), assetId=file.path(assetHome, "GLDAS_Annual_JanFeb", paste("GLDAS_Annual_JanFeb", YR, sep="_")), maxPixels = 10e9, scale=27829.87, region = bBoxS, crs=projCRS, crsTransform=projTransform, overwrite=T)
  saveGLDASAnn$start()
  
}
