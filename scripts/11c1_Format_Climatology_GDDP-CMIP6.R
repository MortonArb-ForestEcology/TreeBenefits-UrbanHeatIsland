# Extract NASA NEX-GDDP CMIP6 historical & future climatology from earth engine
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GDDP-CMIP6

## 
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
## 

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- file.path("~/Google Drive/My Drive")

assetHome <- ee_get_assethome()

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
# Load in base NASA NEX GDDP ----
##################### 
bBoxS = ee$Geometry$BBox(-180, -60, 180, 5);
bBoxN = ee$Geometry$BBox(-180, -5, 180, 75);
maskBBox <- ee$Geometry$BBox(-180, -60, 180, 75)


# -----------
# NASA NEX-GGDP-CMIP6: https://developers.google.com/earth-engine/datasets/catalog/NASA_GDDP-CMIP6
# -----------
modsCMIP6 = c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CESM2', 'CESM2-WACCM', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'CanESM5', 'EC-Earth3', 'EC-Earth3-Veg-LR', 'FGOALS-g3', 'GFDL-CM4', 'GFDL-ESM4', 'GISS-E2-1-G', 'HadGEM3-GC31-LL','IITM-ESM', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR', 'KACE-1-0-G', 'KIOST-ESM', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'NESM3','NorESM2-MM', 'TaiESM1', 'UKESM1-0-LL')
# Excluded models:  'HadGEM3-GC31-MM',  'NorESM2-LM', 
scenarios = c("historical", "ssp245", "ssp585")
timeframes = data.frame(start=c(2001, 2050-19, 2100-19),
                        end = c(2020, 2050, 2100),
                        timeLabel = c("current", "midcentury", "endcentury"))

ee_manage_create(file.path(assetHome, "GDDP-CMIP6_JulAug"), asset_type="ImageCollection")
ee_manage_create(file.path(assetHome, "GDDP-CMIP6_JanFeb"), asset_type="ImageCollection")

for(MOD in modsCMIP6){
  # MOD=modsCMIP6[1]
  print(MOD)
  for(SCEN in scenarios){
    print(paste0(" -- ", SCEN))
    # SCEN = scenarios[1]
    for(i in 1:nrow(timeframes)){
      START = timeframes$start[i]
      END = timeframes$end[i]
      TIMELABEL = timeframes$timeLabel[i]
      
      if(SCEN=="historical" & TIMELABEL!="current") next
      print(paste0("     -- ", TIMELABEL))
      # .1 - Northern Hemisphere: July/August
      # JulAugList <- ee_manage_assetlist(path_asset = "users/crollinson/LST_JulAug_Clean/")
      GDDPJulAug <- ee$ImageCollection('NASA/GDDP-CMIP6')$filter(ee$Filter$eq('model', MOD))$filter(ee$Filter$eq('scenario', SCEN))$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date(paste0(START, "-01-01"), paste0(END, "-12-31")))$map(addTime)$select(c("tas", "pr"))
      GDDPJulAug <- GDDPJulAug$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
      # ee_print(GDDPJulAug)
      # ee_print(GDDPJulAug$first())
      
      GDDPJulAugAvg <- GDDPJulAug$reduce(ee$Reducer$mean())$set("model", MOD)$set("scenario", SCEN)$set("timeframe",  TIMELABEL)
      GDDPJulAugAvg <- GDDPJulAugAvg$set('system:id', paste(MOD, SCEN, paste(START, END, sep="-"), sep="_"))
      # GDDPJulAugAvg$propertyNames()$getInfo()
      # ee_print(GDDPJulAugAvg)
      
      
      projGDDP = GDDPJulAugAvg$select('tas_mean')$projection()
      projCRS = projGDDP$crs()
      projTransform <- unlist(projGDDP$getInfo()$transform)
      
      saveGDDPNH <- ee_image_to_asset(GDDPJulAugAvg, description=paste("Save_GDDP_NH", MOD, SCEN, paste(START, END, sep="-"), sep="_"), assetId=file.path(assetHome, "GDDP-CMIP6_JulAug", paste("GDDP-CMIP6_Climatology_JulAug", MOD, SCEN, paste(START, END, sep="-"), sep="_")), maxPixels = 10e9, scale=27829.87, region = bBoxN, crs=projCRS, crsTransform=projTransform, overwrite=T)
      saveGDDPNH$start()
      
      
      # .1 - Southern Hemisphere: Jan/Feb
      # JanFebList <- ee_manage_assetlist(path_asset = "users/crollinson/LST_JanFeb_Clean/")
      GDDPJanFeb <- ee$ImageCollection('NASA/GDDP-CMIP6')$filter(ee$Filter$eq('model', MOD))$filter(ee$Filter$eq('scenario', SCEN))$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date(paste0(START, "-01-01"), paste0(END, "-12-31")))$map(addTime)$select(c("tas", "pr"))
      GDDPJanFeb <- GDDPJanFeb$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
      # ee_print(GDDPJanFeb)
      # ee_print(GDDPJanFeb$first())
      
      GDDPJanFebAvg <- GDDPJanFeb$reduce(ee$Reducer$mean())$set("model", MOD)$set("scenario", SCEN)$set("timeframe", TIMELABEL)
      GDDPJanFebAvg <- GDDPJanFebAvg$set('system:id', paste(MOD, SCEN, paste0(START, END, sep="-"), sep="_"))
      # GDDPJanFebAvg$propertyNames()$getInfo()
      # ee_print(GDDPJanFebAvg)
      
      
      # projGDDP = GDDPJanFebAvg$select('tas_mean')$projection()
      # projCRS = projGDDP$crs()
      # projTransform <- unlist(projGDDP$getInfo()$transform)
       
      saveGDDPSH <- ee_image_to_asset(GDDPJanFebAvg, description=paste("Save_GDDP_SH", MOD, SCEN, paste(START, END, sep="-"), sep="_"), assetId=file.path(assetHome, "GDDP-CMIP6_JanFeb", paste("GDDP-CMIP6_Climatology_JanFeb", MOD, SCEN, paste(START, END, sep="-"), sep="_")), maxPixels = 10e9, scale=27829.87, region = bBoxS, crs=projCRS, crsTransform=projTransform, overwrite=T)
      saveGDDPSH$start()
    } # End timeframe loop
  } # End scenario loop
} # End Model Loop
# test <- ee$Image(file.path(assetHome, paste("GDDP-CMIP6_Climatology_2001_2020_JanFeb", MOD, SCEN, sep="_")))
# ee_print(test)
# 
# Map$addLayer(test$select('tas_mean'), vizTempK, "Jul/Aug Temperature")
# Map$addLayer(test$select('pr_mean'), vizPrecip, "Precip")
# 
# GLDAS <- ee$Image("users/crollinson/GLDAS21_Climatology_2001_2020_JanFeb")
# Map$addLayer(GLDAS$select('Rainf_f_tavg_mean'), vizPrecip, "Jul/Aug PR")
