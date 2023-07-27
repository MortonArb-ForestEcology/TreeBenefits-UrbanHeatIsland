## 
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
## 
# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "~/Google Drive/My Drive/"
GoogleFolderSave <- "UHI_Analysis_v3Test_CHICAGO"
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

addYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

bitwiseExtract <- function(input, fromBit, toBit) {
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  return(input$rightShift(fromBit)$bitwiseAnd(mask))
}


applyLandsatBitMask = function(img){
  qaPix <- img$select('QA_PIXEL');
  qaRad <- img$select('QA_RADSAT');
  terrMask <- qaRad$bitwiseAnd(11)$eq(0); ## get rid of any terrain occlusion
  # satMask <- qaRad$bitwiseAnd(3 << 4)$eq(0); ## get rid of any saturated bands we use to calculate NDVI
  satMask <- bitwiseExtract(qaRad, 3, 4)$eq(0) ## get rid of any saturated bands we use to calculate NDVI 
  # clearMask <- qaPix$bitwiseAnd(1<<7)$eq(0)
  clearMask <- bitwiseExtract(qaPix, 1, 5)$eq(0)
  waterMask <- bitwiseExtract(qaPix, 7, 7)$eq(0)
  cloudConf = bitwiseExtract(qaPix, 8, 9)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  shadowConf <- bitwiseExtract(qaPix, 10, 11)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  snowConf <- bitwiseExtract(qaPix, 12, 13)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  
  
  img <- img$updateMask(clearMask$And(waterMask)$And(cloudConf)$And(shadowConf)$And(snowConf)$And(terrMask)$And(satMask));
  
  return(img)
  
}

# Function for combining images with the same date
# 2nd response from here: https:#gis.stackexchange.com/questions/280156/mosaicking-image-collection-by-date-day-in-google-earth-engine 
mosaicByDate <- function(imcol, dayWindow){
  # imcol: An image collection
  # returns: An image collection
  imlist = imcol$toList(imcol$size())
  
  # Note: needed to specify the ee_utils_pyfunc since it's not an image collection
  unique_dates <- imlist$map(ee_utils_pyfunc(function(img){
    return(ee$Image(img)$date()$format("YYYY-MM-dd"))
  }))$distinct()
  
  # Same as above: what we're mappign through is a List, so need to call python
  mosaic_imlist = unique_dates$map(ee_utils_pyfunc(function(d){
    d = ee$Date(d)
    dy= d$get('day');    
    m= d$get('month');
    y= d$get('year');
    
    im = imcol$filterDate(d$advance(-dayWindow, "day"), d$advance(dayWindow, "day"))$reduce(ee$Reducer$median()) # shoudl influence the window for image aggregation
    
    return(im$set("system:time_start", d$millis(), 
                  "system:id", d$format("YYYY-MM-dd"),
                  'date', d, 'day', dy, 'month', m, 'year', y))
  }))
  
  # testOUT <- ee$ImageCollection(mosaic_imlist)
  # ee_print(testOUT)
  return (ee$ImageCollection(mosaic_imlist))
}

addNDVI <- function(img){
  return( img$addBands(img$normalizedDifference(c('nir','red'))$rename('NDVI')));
}

# This funciton will extract the year-averaged LST & NDVI for landsat image collection
processLandsatData <- function(imcol, LandsatLabel, ...){
  yrList <- ee$List(imcol$aggregate_array("year"))$distinct()$sort()
  yrString <- yrList$map(ee_utils_pyfunc(function(j){
    return(ee$String("YR")$cat(ee$String(ee$Number(j)$format())))
  }))
  # yrString$getInfo()
  
  
  TempYrAvg <- yrList$map(ee_utils_pyfunc(function(j){
    YR <- ee$Number(j);
    START <- ee$Date$fromYMD(YR,1,1);
    END <- ee$Date$fromYMD(YR,12,31);
    lstYR <- imcol$filter(ee$Filter$date(START, END))
    
    # tempMedian <- lstYR$select('LST_K')$reduce(ee$Reducer$median())
    tempMedian <- lstYR$select('LST_K')$reduce(ee$Reducer$mean())
    
    tempAgg <- ee$Image(tempMedian)
    
    ## ADD YEAR AS A PROPERTY!!
    tempAgg <- tempAgg$set(ee$Dictionary(list(year=YR)))
    tempAgg <- tempAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
    # ee_print(tempAgg)
    # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
    # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Median Surface Temperature - Anomaly');
    
    return (tempAgg); # update to standardized once read
  }))
  
  TempYrAvg <- ee$ImageCollection$fromImages(TempYrAvg) # go ahead and overwrite it since we're just changing form
  # ee_print(TempYrAvg)
  # Map$addLayer(TempYrAvg$first()$select('LST_K_mean'), vizTempK, "Mean Surface Temperature")
  
  tempYrAvg <- ee$ImageCollection$toBands(TempYrAvg)$rename(yrString)
  
  
  export.TempMean <- ee_image_to_drive(image=tempYrAvg, description=paste(cityID, LandsatLabel, "LST", sep="_"), fileNamePrefix=paste(cityID, LandsatLabel, "LST", sep="_"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=10e12, crs=projCRS, crsTransform=projTransform)
  export.TempMean$start()
  
  
  # Extract & Save NDVI
  NDVIYrAvg <- yrList$map(ee_utils_pyfunc(function(j){
    YR <- ee$Number(j);
    START <- ee$Date$fromYMD(YR,1,1);
    END <- ee$Date$fromYMD(YR,12,31);
    ndviYR <- imcol$filter(ee$Filter$date(START, END))
    
    # NDVIMedian <- lstYR$select('LST_K')$reduce(ee$Reducer$median())
    NDVIMedian <- ndviYR$select('NDVI')$reduce(ee$Reducer$mean())
    
    NDVIAgg <- ee$Image(NDVIMedian)
    
    ## ADD YEAR AS A PROPERTY!!
    NDVIAgg <- NDVIAgg$set(ee$Dictionary(list(year=YR)))
    NDVIAgg <- NDVIAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
    # ee_print(NDVIAgg)
    
    return (NDVIAgg); # update to standardized once read
  }))
  
  NDVIYrAvg <- ee$ImageCollection$fromImages(NDVIYrAvg) # go ahead and overwrite it since we're just changing form
  # ee_print(NDVIYrAvg)
  # Map$addLayer(NDVIYrAvg$first()$select('NDVI_mean'), ndviVis, "NDVI")
  
  NDVIYrAvg <- ee$ImageCollection$toBands(NDVIYrAvg)$rename(yrString)
  
  
  export.NDVIMean <- ee_image_to_drive(image=NDVIYrAvg, description=paste(cityID, LandsatLabel, "NDVI", sep="_"), fileNamePrefix=paste(cityID, LandsatLabel, "NDVI", sep="_"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=10e12, crs=projCRS, crsTransform=projTransform)
  export.NDVIMean$start()
}



##################### 


##################### 
# 1. Load and select cities
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$NAME=="Chicago" & !is.na(sdei.df$NAME),]
# sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]
cityIDsAll <- sdei.df$ISOURBID

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei.first())

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2))$filter(ee$Filter$eq('NAME', 'Chicago')) 
# ee_print(citiesUse) # This function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})
# ee_print(citiesUse)
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

ndviVis = list(
  min= 0.0,
  max= 1,
  palette= c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
  )
)


# -----------
# Vegetation mask for projection info
# -----------
vegMask <- ee$Image('users/crollinson/MOD44b_250m_native_Percent_Tree_Cover')
# vegMask <- ee$Image('users/crollinson/MERIT-DEM-v1_250m_Reproj')
ee_print(vegMask)

maskGeom <- vegMask$geometry()$getInfo()
maskBBox <- ee$Geometry$BBox(-180, -90, 180, 90) # The world

# getting the projection details for MODIS
projveg = vegMask$projection() # Gettign the projection for the Landsat LST layer
projCRS = projveg$crs()
projTransform <- unlist(projveg$getInfo()$transform)

# ee_print(projveg)
projTransform #should produce NUMBERS!

# -----------

# -----------

##################### 

## Making the workflow a function that we can then feed N/S data to
# Cities needs to be an EarthEngine Feature List
extractTempEE <- function(CitySP, CityNames,  GoogleFolderSave, overwrite=F, ...){
  # cityseq <- seq_len(CITIES$length()$getInfo())
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    # cityID = "Chicago"
    # cityNow <- citiesUse$filter('NAME=="Chicago"')
    
    # Figure out if we need N or S hemisphere; 
    # NOTE: if this is slow, then just have it as a specified thing from the function since we have fed N/S separately anyways
    cityCent <- cityNow$geometry()$centroid()
    lat <- cityCent$coordinates()$get(1)
    if( lat$getInfo() >= 0){
      dayStart = 181; dayEnd=240
    } else {
      dayStart = 1; dayEnd=60
    }
    # cityNow <- CitySP$filter('ISOURBID'=="NZL96")
    # Map$centerObject(cityNow) 
    # Map$addLayer(cityNow)

    #-------------------
    # Now doing Land Surface Temperature
    #-------------------
    # Read in the appropriate landsat data using the filter bounds function & appropriate time for N/S hemisphere
    # # # NOTE: Working with just 1 year to try and get this working!!
    
    # ----------
    # Landsat 8 -----
    # ----------
    landsat8 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$filterBounds(cityNow)$filter(ee$Filter$dayOfYear(dayStart, dayEnd))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$map(function(img){
      # Add date info
      d= ee$Date(img$get('system:time_start'));
      dy= d$get('day');    
      m= d$get('month');
      y= d$get('year');
      
      # # Add masks 
      img <- applyLandsatBitMask(img)
      
      # #scale correction; doing here & separating form NDVI so it gets saved on the image
      lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
      lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);
      
      # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
      return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
    })$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI);
    # ee_print(landsat8)
    # Map$addLayer(landsat8$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    l8Reproj = landsat8$map(function(img){
      return(img$reproject(projveg)$reduceResolution(reducer=ee$Reducer$mean()))
             })$map(addYear) # flipping teh order here to specify the reproject first seems to have helped.
    # ee_print(l8Reproj)
    # Map$addLayer(l8Reproj$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")

    # Now that we've re-projected, lets clip to the region
    l8Here <- l8Reproj$map(function(img){ return(img$clip(cityNow))})
    # Map$addLayer(l8Here$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    processLandsatData(imcol=l8Here, LandsatLabel = "Landsat8", GoogleFolderSave = GoogleFolderSave )
    # ----------
    
    
    # ----------
    # Landsat 7 -----
    # ----------
    landsat7 <- ee$ImageCollection('LANDSAT/LE07/C02/T1_L2')$filterBounds(cityNow)$filter(ee$Filter$dayOfYear(dayStart, dayEnd))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$map(function(img){
      # Add date info
      d= ee$Date(img$get('system:time_start'));
      dy= d$get('day');    
      m= d$get('month');
      y= d$get('year');
      
      # # Add masks 
      img <- applyLandsatBitMask(img)
      
      # #scale correction; doing here & separating form NDVI so it gets saved on the image
      lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
      lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
      
      # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
      return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
    })$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI);
    # ee_print(landsat7)
    # Map$addLayer(landsat7$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    l7Reproj = landsat7$map(function(img){
      return(img$reproject(projveg)$reduceResolution(reducer=ee$Reducer$mean()))
    })$map(addYear) # flipping teh order here to specify the reproject first seems to have helped.
    # ee_print(l7Reproj)
    # Map$addLayer(l7Reproj$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    # Now that we've re-projected, lets clip to the region
    l7Here <- l7Reproj$map(function(img){ return(img$clip(cityNow))})
    # Map$addLayer(l7Here$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    processLandsatData(imcol=l7Here, LandsatLabel = "Landsat7")
    # ----------
    
    # ----------
    # Landsat 5 -----
    # ----------
    landsat5 <- ee$ImageCollection('LANDSAT/LT05/C02/T1_L2')$filterBounds(cityNow)$filter(ee$Filter$dayOfYear(dayStart, dayEnd))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$map(function(img){
      # Add date info
      d= ee$Date(img$get('system:time_start'));
      dy= d$get('day');    
      m= d$get('month');
      y= d$get('year');
      
      # # Add masks 
      img <- applyLandsatBitMask(img)
      
      # #scale correction; doing here & separating form NDVI so it gets saved on the image
      lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
      lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
      
      # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
      return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
    })$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI);
    # ee_print(landsat5)
    # Map$addLayer(landsat5$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    l5Reproj = landsat5$map(function(img){
      return(img$reproject(projveg)$reduceResolution(reducer=ee$Reducer$mean()))
    })$map(addYear) # flipping teh order here to specify the reproject first seems to have helped.
    # ee_print(l5Reproj)
    # Map$addLayer(l5Reproj$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    # Now that we've re-projected, lets clip to the region
    l5Here <- l5Reproj$map(function(img){ return(img$clip(cityNow))})
    # Map$addLayer(l5Here$first()$select('LST_K'), vizTempK, "Raw Surface Temperature")
    
    processLandsatData(imcol=l5Here, LandsatLabel = "Landsat5", GoogleFolderSave = GoogleFolderSave )
    # ----------

    
    # ----------
    # Combined Landsat data: l8 (2013-2020), l7 (2000-2020), l5 (2000-2012)
    # ----------
    landsatAll <- l8Here$merge(l7Here)$merge(l5Here)
    # ee_print(landsatAll)
    
    processLandsatData(imcol=landsatAll, LandsatLabel = "Landsat-AllCombined", GoogleFolderSave = GoogleFolderSave )
    # ----------
    
  }
}


##################### 
# 3 . Start extracting data for each city
# NOTE: This will need to become a loop, but lets get it working first
# https://r-spatial.github.io/rgee/articles/rgee03.html
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())

extractTempEE(CitySP=citiesUse, CityNames = cityIDsAll, GoogleFolderSave = GoogleFolderSave)


# 
# cityIdS <-sdei.df$ISOURBID[sdei.df$LATITUDE<0]
# cityIdNW <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE<=0]
# cityIdNE1 <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>0 & sdei.df$LONGITUDE<=75]
# cityIdNE2 <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>75]
# length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)
# 
# # If we're not trying to overwrite our files, remove files that were already done
# cityRemove <- vector()
# if(!overwrite){
#   ### Filter out sites that have been done!
#   tmean.done <- dir(file.path(path.google, GoogleFolderSave), "LST")
#   
#   # Check to make sure a city has all three layers; if it doesn't do it again
#   cityRemove <- unlist(lapply(strsplit(tmean.done, "_"), function(x){x[1]}))
# 
#   cityIdS <- cityIdS[!cityIdS %in% cityRemove]
#   cityIdNW <- cityIdNW[!cityIdNW %in% cityRemove]
#   cityIdNE1 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
#   cityIdNE2 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
#   
# } # End remove cities loop
# length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)
# # 
# if(length(cityIdS)>0){
#   extractTempEE(CitySP=citiesUse, CityNames = cityIdS, GoogleFolderSave = GoogleFolderSave)
# }
# 
# # 
# if(length(cityIdNW)>0){
#   extractTempEE(CitySP=citiesUse, CityNames = cityIdNW, GoogleFolderSave = GoogleFolderSave)
# }
# 
# if(length(cityIdNE1)>0){
#   extractTempEE(CitySP=citiesUse, CityNames = cityIdNE1, GoogleFolderSave = GoogleFolderSave)
# }
# 
# if(length(cityIdNE2)>0){
#   extractTempEE(CitySP=citiesUse, CityNames = cityIdNE2, GoogleFolderSave = GoogleFolderSave)
# }
# 

##################### 

