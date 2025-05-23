# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine
# Note: This script creates a second buffer layer where we've masked out other urban areas (whether in our sample or not) from the buffer.  

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v4"
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)

##################### 
# 0. Set up some choices for data quality thresholds
##################### 
# yr.analy <- 2001:2020
# thresh.sigma <- 6 # Use 6-sigma outliers for the data filtering\
# thresh.pts <- 50
# thresh.prop <- 0.5 # The proportion of data needed for a time point to be "good"; currenlty 0.5
overwrite=F
##################### 

##################### 
# 1. Load and select cities
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]
cityIDsAll <- sdei.df$ISOURBID

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei.first())
ee_print(sdei)

sdeiSimple <- sdei$map(function(x){
  x$setGeometry(x$geometry()$simplify(100))
})
ee_print(sdeiSimple)

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
ee_print(citiesUse)

# Making the buffer file a separate thing!
citiesBuff <- citiesUse$map(function(f){f$buffer(10e3)})

# Excluding other cities (that meet our analysis criteria or not) 
# citiesBuffExcl <- citiesBuff$map(function(x){
#   geom <- x$geometry()
#   diff_geom <- geom$difference(sdeiSimple$geometry(), ee$ErrorMargin(1))
#   x$setGeometry(diff_geom)
# })
# ee_print(citiesBuffExcl)
# Map$addLayer(citiesBuffExcl)
#####################


##################### 
# 2. Load in data layers  -- formatting in script 1!
####################
vegMask <- ee$Image("users/crollinson/MOD44b_1km_Reproj_VegMask")
# Map$addLayer(vegMask)

# vegMaskN <- ee$Image("users/crollinson/MOD44b_1km_Reproj_VegMask_NH")
# vegMaskS <- ee$Image("users/crollinson/MOD44b_1km_Reproj_VegMask_SH")
# Map$addLayer(vegMaskN)
# Map$addLayer(vegMaskS)

projMask = vegMask$projection()
projCRS = projMask$crs()
projTransform <- unlist(projMask$getInfo()$transform)

####################




#####################

# -----------
extractCityMask <- function(cityBuff, cityRaw, CityNames, BASE, GoogleFolderSave, overwrite=F, ...){
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    # cityID="USA26687" # Chicago
    cityID <- CityNames[i]
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNowBuff <- cityBuff$filter(ee$Filter$eq('ISOURBID', cityID)) # Note: this is only getting used for the geometry arguement.  We'll see how it works
    # Map$centerObject(cityNowBuff) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNowBuff)
    cityRawClip <- cityRaw$map(function(x){
      x$intersection(cityNowBuff$geometry(), ee$ErrorMargin(1))
    })
    # Map$centerObject(cityNowBuff) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityRawClip)
    
    # Now removing the clip area from the buffer
    cityBuffExcl <- cityNowBuff$map(function(x){
      geom <- x$geometry()
      diff_geom <- geom$difference(cityRawClip$geometry(), ee$ErrorMargin(1))
      x$setGeometry(diff_geom)
    })
    # Map$addLayer(cityBuffExcl)
    
    #-------
    # extracting elevation -- 
    #  NOTE: Doing outlier removal because there are some known issues with a couple points: https://developers.google.com/earth-engine/datasets/catalog/JAXA_ALOS_AW3D30_V3_2
    #-------
    # baseCity <- BASE$clip(cityNowBuff)
    # Map$addLayer(baseCity)
    baseCity <- BASE$clip(cityBuffExcl)
    # Map$addLayer(baseCity)
    
    # Save elevation only if it's worth our while -- Note: Still doing the extraction & computation first since we use it as our base
    export.mask <- ee_image_to_drive(image=baseCity, description=paste0(cityID, "_Buffer-NoUrb"), fileNamePrefix=paste0(cityID, "_Buffer-NoUrb"), folder=GoogleFolderSave, timePrefix=F, region=cityNowBuff$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.mask$start()
    # ee_monitoring(export.elev)
    #-------
  } # End i loop
} # End function


# -----------

##################### 
# 3 . Start extracting data for each city -- only ones that were done before!
##################### 

cityIdS <-sdei.df$ISOURBID[sdei.df$LATITUDE<0]
cityIdN <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0]
# length(cityIdS); length(cityIdNW)

# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  mask.done <- dir(file.path(path.google, GoogleFolderSave), "_Buffer-NoUrb.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(mask.done, "_"), function(x){x[1]}))
  
  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdN <- cityIdN[!cityIdN %in% cityRemove]
  
} # End remove cities loop
length(cityIdS); length(cityIdN)


# citiesSouth <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdS)))
# citiesNorth <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdN)))

buffSouth <- citiesBuff$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdS)))
buffNorth <- citiesBuff$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdN)))

# TEST CITY
# CITY = "SWE3477"
# extractCityMask(cityBuff=buffNorth, cityRaw=sdei, CityNames=CITY, BASE=vegMask, GoogleFolderSave, overwrite=T)
# 
# testBuff <- raster(file.path(path.google, GoogleFolderSave, paste0(CITY, "_CityMask.tif")))
# plot(testBuff)
# testBuff2 <- raster(file.path(path.google, GoogleFolderSave, paste0(CITY, "_Buffer-NoUrb.tif")))
# plot(testBuff2)
# testdf <- data.frame(coordinates(testBuff2))
# testdf$valsOrig <- getValues(testBuff)
# testdf$valsBuff <- getValues(testBuff2)
# summary(testdf)

if(length(cityIdS)>0){
  extractCityMask(cityBuff=buffSouth, cityRaw=sdei, CityNames=cityIdS, BASE=vegMask, GoogleFolderSave, overwrite=T)
}

if(length(cityIdN)>0){
  extractCityMask(cityBuff=buffNorth, cityRaw=sdei, CityNames=cityIdN, BASE=vegMask, GoogleFolderSave, overwrite=T)
}


