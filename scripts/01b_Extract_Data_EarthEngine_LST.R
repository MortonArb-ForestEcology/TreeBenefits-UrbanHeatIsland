## 
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
## 
# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Library/CloudStorage", user.google, "My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v2"

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

## Just testing to make sure it works
# popLarge <- citiesBuff$filter(ee$Filter$gte('ES00POP', 1e6))$filter(ee$Filter$gte('SQKM_FINAL', 1e2))
# ee_print(popLarge) # 389 cities
# Map$addLayer(popLarge)
# citiesBuff <- popLarge

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
# -----------
# 2.a - Land Surface Temperature
# -----------
# 2.a.1 - Northern Hemisphere: July/August
# JulAugList <- ee_manage_assetlist(path_asset = "users/crollinson/LST_JulAug_Clean/")
tempJulAug <- ee$ImageCollection("users/crollinson/LST_JulAug_Clean")
tempJulAug <- tempJulAug$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!
# ee_print(tempJulAugX)
# tempJulAug$first()$propertyNames()$getInfo()
# tempJulAug$first()$get("system:id")$getInfo()
# ee_print(tempJulAug$first())
# Map$addLayer(tempJulAugX$first(), vizTempK, "Jul/Aug Temperature")

# 2.a.2 - Southern Hemisphere: Jan/Feb
# JanFebList <- ee_manage_assetlist(path_asset = "users/crollinson/LST_JanFeb_Clean/")
tempJanFeb <- ee$ImageCollection("users/crollinson/LST_JanFeb_Clean");
tempJanFeb <- tempJanFeb$map(setYear) # Note: This is needed here otherwise the format is weird and code doesn't work!

# ee_print(tempJanFeb)
# Map$addLayer(tempJanFeb$first(), vizTempK, "Jan/Feb Temperature")

projLST = tempJulAug$first()$projection()
projCRS = projLST$crs()
projTransform <- unlist(projLST$getInfo()$transform)

# -----------

# -----------

##################### 

## Making the workflow a function that we can then feed N/S data to
# Cities needs to be an EarthEngine Feature List
extractTempEE <- function(CitySP, CityNames, TEMPERATURE, GoogleFolderSave, overwrite=F, ...){
  # cityseq <- seq_len(CITIES$length()$getInfo())
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    # cityNow <- CitySP$filter('ISOURBID'=="NZL96")
    # Map$centerObject(cityNow) 
    # Map$addLayer(cityNow)

    #-------
    # Now doing Land Surface Temperature
    #-------
    # Map$addLayer(tempHemi$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
    
    # JUST GET AN MASK THE RAW DATA FIRST
    tempCityAll <- TEMPERATURE$map(function(img){
      tempNow <- img$clip(cityNow)
      # dat <- tempNow$gt(0)
      # tempNow <- tempNow$updateMask(dat)
      return(tempNow)
    })
    # ee_print(tempCityAll)
    # tempCityAll$first()$get("year")$getInfo()
    # Map$addLayer(tempCityAll$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
    
 
    # ---------------
    # Need to run this first so that layers without data are removed up front
    # ---------------
    setNPts <- function(img){
      npts.now <- img$select("LST_Day_1km")$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
      return(img$set("n_Pts", npts.now$get("LST_Day_1km")))#$set("p_Pts", p.now$get
    }
    
    tempCityAll <- tempCityAll$map(setNPts)
    # ee_print(tempCityAll)
    
    # Making sure we have at least 50% of the points present
    # tempCityAll$select("2016_02_18")
    # tempCityAll$first()$get("n_Pts")$getInfo()
    # tempCityAll$first()$get("year")$getInfo()
    ptsString <- tempCityAll$aggregate_array("n_Pts")$sort()
    ptsMax <- ptsString$get(-1)
    ptsThresh <- ee$Number(ptsMax)$multiply(ee$Number(thresh.prop))
    
    tempCityAll <- tempCityAll$filter(ee$Filter$gte("n_Pts", ptsThresh)) # have at least 5    
    # ee_print(tempCityAll)
    # ---------------

    ## ----------------
    # Now remove outliers
    # Once it DOES work, we can re-run the the setNPts
    ## ----------------
    lstOutliers <- function(img){
      # Calculate the means & sds for the region
      tempStats <- img$select("LST_Day_1km")$reduceRegion(reducer=ee$Reducer$mean()$combine(
        reducer2=ee$Reducer$stdDev(), sharedInputs=T),
        geometry=cityNow$geometry(), scale=1e3)
      
      # Cacluate the key numbers for our sanity
      tmean <- ee$Number(tempStats$get("LST_Day_1km_mean"))
      tsd <- ee$Number(tempStats$get("LST_Day_1km_stdDev"))
      thresh <- tsd$multiply(thresh.sigma)
      
      # Do the filtering
      dat.low <- img$gte(tmean$subtract(thresh))
      dat.hi <- img$lte(tmean$add(thresh))
      img <- img$updateMask(dat.low)
      img <- img$updateMask(dat.hi)
      
      # Map$addLayer(tempNow$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
      return(img)
    }
    
    tempCityAll <- tempCityAll$map(lstOutliers)
    # ee_print(tempCityAll)
    # Map$addLayer(tempCityAll$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
    
    # ---------------
    # Remove poor data layers
    # ---------------
    setNPts <- function(img){
      npts.now <- img$select("LST_Day_1km")$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
      # p.now <- list(p_Pts=ee$Number(npts.now$get("LST_Day_1km"))$divide(npts.elev))
      # p.now <- ee$Dictionary(p.now)
      
      # test <- img$set("n_Pts", npts.now$get("LST_Day_1km"))$set("p_Pts", p.now$get("p_Pts"))
      # test$get("p_Pts")$getInfo()
      return(img$set("n_Pts", npts.now$get("LST_Day_1km")))#$set("p_Pts", p.now$get("p_Pts")))
    }
    
    tempCityAll <- tempCityAll$map(setNPts)
    # ee_print(tempCityAll)

    tempCityAll <- tempCityAll$filter(ee$Filter$gte("n_Pts", ptsThresh)) # have at least 50% of the data points (see top of script)
    ## ----------------
    
    ## ----------------
    # Now calculate Temperature Deviations
    ## ----------------
    # tempCityAll <- tempCityAll$map(function(img){
    #   tempMed <- img$select('LST_Day_1km')$reduceRegion(reducer=ee$Reducer$median(), geometry=cityNow$geometry(), scale=1e3)
    #   tempDev <- img$select('LST_Day_1km')$subtract(ee$Number(tempMed$get('LST_Day_1km')))$rename('LST_Day_Dev')$toFloat()
    #   return(img$addBands(tempDev))
    # })
    # # print("Temperature Deviation (Raw)", tempCityAll);
    # # Map$addLayer(tempCityAll$first()$select('LST_Day_Dev'), vizTempAnom, 'Surface Temperature - Anomaly');
    # # devList = tempCityAll.toList(tempCityAll.size())
    # # Map.addLayer(ee.Image(devList.get(12)).select('LST_Day_Dev'), vizTempAnom, 'Surface Temperature - Anomaly');
    ## ----------------
    
    
    ## ----------------
    # Now lets do our annual means
    ## ----------------
    # Only iterate through years with some data! 
    # tempCityAll$aggregate_array("year")$getInfo()
    yrList <- ee$List(tempCityAll$aggregate_array("year"))$distinct()
    yrString <- yrList$map(ee_utils_pyfunc(function(j){
      return(ee$String("YR")$cat(ee$String(ee$Number(j)$format())))
    }))

    tempYrMean <- yrList$map(ee_utils_pyfunc(function(j){
      YR <- ee$Number(j);
      START <- ee$Date$fromYMD(YR,1,1);
      END <- ee$Date$fromYMD(YR,12,31);
      lstYR <- tempCityAll$filter(ee$Filter$date(START, END))
      # // var lstDev =  // make each layer an anomaly map
      tempMean <- lstYR$select('LST_Day_1km')$reduce(ee$Reducer$mean())
      # tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
      tempAgg <- ee$Image(tempMean)
      
      ## ADD YEAR AS A PROPERTY!!
      tempAgg <- tempAgg$set(ee$Dictionary(list(year=YR)))
      tempAgg <- tempAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
      # ee_print(tempAgg)
      # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
      # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
      
      return (tempAgg); # update to standardized once read
    }))
    tempYrMean <- ee$ImageCollection$fromImages(tempYrMean) # go ahead and overwrite it since we're just changing form
    tempYrMean <- ee$ImageCollection$toBands(tempYrMean)$rename(yrString)
    # tempYrMean <- tempYrMean$setDefaultProjection(projLST)
    # ee_print(tempYrMean)
    # Map$addLayer(tempYrMean$select('YR2020'), vizTempK, 'Mean Surface Temperature (K)');
    
    export.TempMean <- ee_image_to_drive(image=tempYrMean, description=paste0(cityID, "_LST_Day_Tmean"), fileNamePrefix=paste0(cityID, "_LST_Day_Tmean"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e7, crs=projCRS, crsTransform=projTransform)
    
    export.TempMean$start()
    # ee_monitoring(export.TempMean)
    
    
    # tempYrDev <- yrList$map(ee_utils_pyfunc(function(j){
    #   YR <- ee$Number(j);
    #   START <- ee$Date$fromYMD(YR,1,1);
    #   END <- ee$Date$fromYMD(YR,12,31);
    #   lstYR <- tempCityAll$filter(ee$Filter$date(START, END))
    #   tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
    #   tempAgg <- ee$Image(tempDev)
    #   
    #   ## ADD YEAR AS A PROPERTY!!
    #   tempAgg <- tempAgg$set(ee$Dictionary(list(year=YR)))
    #   tempAgg <- tempAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
    #   # ee_print(tempAgg)
    #   # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
    #   # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
    #   
    #   return (tempAgg); # update to standardized once read
    #   
    # }))
    # tempYrDev <- ee$ImageCollection$fromImages(tempYrDev) # go ahead and overwrite it since we're just changing form
    # tempYrDev <- ee$ImageCollection$toBands(tempYrDev)$rename(yrString2)
    # tempYrDev <- tempYrDev$setDefaultProjection(projLST)
    # 
    # export.TempDev <- ee_image_to_drive(image=tempYrDev, description=paste0(cityID, "_LST_Day_Tdev"), fileNamePrefix=paste0(cityID, "_LST_Day_Tdev"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6)
    # export.TempDev$start()
    # ee_monitoring(export.TempDev)
    ## ----------------
  } # End Loop
  
}


##################### 
# 3 . Start extracting data for each city
# NOTE: This will need to become a loop, but lets get it working first
# https://r-spatial.github.io/rgee/articles/rgee03.html
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())

cityIdS <-sdei.df$ISOURBID[sdei.df$LATITUDE<0]
cityIdN <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0]
length(cityIdS); length(cityIdN)

# If we're not trying to overwrite our files, remove files that were already done
# cityRemove <- vector()
# cityRemove <- c("IND58965", "BRA58970", "IDN58965")

if(!overwrite){
  ### Filter out sites that have been done!
  tmean.done <- dir(file.path(path.google, GoogleFolderSave), "LST_Day_Tmean")

  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(tmean.done, "_"), function(x){x[1]}))

  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdN <- cityIdN[!cityIdN %in% cityRemove]
  
} # End remove cities loop

# citiesSouth <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdS)))
# citiesNorth <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdN)))

# citiesSouth$size()$getInfo()
length(cityIdS)

# citiesNorth$size()$getInfo()
length(cityIdN)


# 
if(length(cityIdS)>0){
  extractTempEE(CitySP=citiesUse, CityNames = cityIdS, TEMPERATURE=tempJanFeb$select("LST_Day_1km"), GoogleFolderSave = GoogleFolderSave)
}

# 
if(length(cityIdN)>0){
  extractTempEE(CitySP=citiesUse, CityNames = cityIdN, TEMPERATURE=tempJulAug$select("LST_Day_1km"), GoogleFolderSave = GoogleFolderSave)
}

# # All except 3 were run successfully
# if(ncitiesNorthW>0){
#   citiesNorthWList <- citiesNorthW$toList(ncitiesNorthW) 
#   extractTempEE(CITIES=citiesNorthWList, TEMPERATURE=lstNHFinal, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

### FOR LOOP ENDS HERE
##################### 

