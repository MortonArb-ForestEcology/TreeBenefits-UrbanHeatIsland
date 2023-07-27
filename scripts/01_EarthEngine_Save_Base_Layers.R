# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "~/Google Drive/My Drive/"
GoogleFolderSave <- "UHI_Analysis_Output_v3"
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

addYear = function(img) {
  d= ee$Date(ee$Number(img$get('system:time_start')));
  y= ee$Number(d$get('year'));
  return(img$set('year', y));
}

bitwiseExtract <- function(input, fromBit, toBit) {
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  return(input$rightShift(fromBit)$bitwiseAnd(mask))
}

addNDVI <- function(img){
  return( img$addBands(img$normalizedDifference(c('nir','red'))$rename('NDVI')));
}


applyLandsatBitMask = function(img){
  qaPix <- img$select('QA_PIXEL');
  qaRad <- img$select('QA_RADSAT');
  terrMask <- qaRad$bitwiseAnd(11)$eq(0); ## get rid of any terrain occlusion
  # satMask <- qaRad$bitwiseAnd(3 << 4)$eq(0); ## get rid of any saturated bands we use to calculate NDVI
  satMask <- bitwiseExtract(qaRad, 3, 4)$eq(0) ## get rid of any saturated bands we use to calculate NDVI 
  # clearMask <- qaPix$bitwiseAnd(1<<7)$eq(0)
  clearMask <- bitwiseExtract(qaPix, 1, 7)$eq(0) 
  cloudConf = bitwiseExtract(qaPix, 8, 9)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  shadowConf <- bitwiseExtract(qaPix, 10, 11)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  snowConf <- bitwiseExtract(qaPix, 12, 13)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
  
  
  img <- img$updateMask(clearMask$And(cloudConf)$And(shadowConf)$And(snowConf)$And(terrMask)$And(satMask));
  
  return(img)
  
}


##################### 
##################### 
# Color Palette etc. ----
##################### 
# Setting the center point for the Arb because I think we'll have more variation
# Map$setCenter(-88.04526, 41.81513, 11);

ndviVis = list(
  min= 0.0,
  max= 1,
  palette= c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
  )
)
##################### 
# 2. Load in data layers 
####################
# -----------
# 2.A MODIS Tree Data----
# -----------
vizTree <- list(
  # bands: ['Percent_Tree_Cover'],
  min=0.0,
  max=100.0,
  palette=c('bbe029', '0a9501', '074b03')
);

vizBit <- list(
  # bands: ['Percent_Tree_Cover'],
  min=0.0,
  max=1,
  palette=c('bbe029', '074b03')
);

tempColors <- c(
  '040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
  '0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
  '3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
  'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
  'ff0000', 'de0101', 'c21301', 'a71001', '911003'
);

vizBit2 <- list(
  min=0,
  max=8,
  palette=tempColors
);

mod44b <- ee$ImageCollection('MODIS/006/MOD44B')$filter(ee$Filter$date("2001-04-01", "2020-12-31")) #MODIS doesn't have 2022 data yet. Subsetting to a shared time period
mod44b <- mod44b$map(setYear)$map(addTime)
# ee_print(mod44b)
# Map$addLayer(mod44b$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')


# Create a noVeg Mask -- do this without having taken out the QAQC first because that will end up doing really weird things!
# mod44bReprojOrig = mod44b$map(function(img){
#   return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
# })$map(addTime); # add year here!
# Map$addLayer(mod44bReprojOrig$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')

vegMask <- mod44b$first()$select("Percent_Tree_Cover", "Percent_NonTree_Vegetation", "Percent_NonVegetated")$reduce('sum')$gt(50)$mask()
# ee_print(vegMask)
# Map$addLayer(vegMask, vizBit)

maskGeom <- vegMask$geometry()$getInfo()
maskBBox <- ee$Geometry$BBox(-180, -90, 180, 90) # The world

# getting the projection details for MODIS
projveg = vegMask$projection() # Gettign the projection for the Landsat LST layer
projCRS = projveg$crs()
projTransform <- unlist(projveg$getInfo()$transform)

# ee_print(projveg)
projTransform #should produce NUMBERS!


# proj4string: "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
saveVegMask <- ee_image_to_asset(vegMask, description="Save_VegetationMask", assetId=file.path(assetHome, "MOD44b_250m_Reproj_VegMask"), maxPixels = 10e12, scale=231.6564, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
saveVegMask$start()



# // Cleaning Up and getting just Good MODIS VCF data
# // Code adapted from https://gis.stackexchange.com/a/349401/5160
# // Let's extract all pixels from the input image where
# Bit Input Layers    State
# 0   DOY 065 – 097   0 Clear; 1 Cloudy
# 1   DOY 113 – 145   0 Clear; 1 Cloudy
# 2   DOY 161 – 193   0 Clear; 1 Cloudy
# 3   DOY 209 – 241   0 Clear; 1 Cloudy
# 4   DOY 257 – 289   0 Clear; 1 Cloudy
# 5   DOY 305 – 337   0 Clear; 1 Cloudy
# 6   DOY 353 – 017   0 Clear; 1 Cloudy
# 7   DOY 033 – 045   0 Clear; 1 Cloudy
# img <- mod44b$first()
vegBitMask <- function(img){
  qcVeg <- img$select('Quality')
  # test <- ee$Image
  bit0 <- bitwiseExtract(qcVeg, 0, 0);
  bit1 <- bitwiseExtract(qcVeg, 1, 1);
  bit2 <- bitwiseExtract(qcVeg, 2, 2);
  bit3 <- bitwiseExtract(qcVeg, 3, 3);  
  bit4 <- bitwiseExtract(qcVeg, 4, 4);
  bit5 <- bitwiseExtract(qcVeg, 5, 5);
  bit6 <- bitwiseExtract(qcVeg, 6, 6);
  bit7 <- bitwiseExtract(qcVeg, 7, 7);
  # Map$addLayer(bit5, vizBit)
  # ee_print(bit0)
  
  imgBitColl <- ee$ImageCollection$fromImages(c(bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7))
  bitSumVeg <- imgBitColl$reduce("sum")
  
  # From the MODIS VCF 6.1 guide: "If the data are “bad” for 2 or more of the 8 time periods the user should be cautious with the vegetation cover value as it may be erroneous due to the poor inputs"  (https://modis-land.gsfc.nasa.gov/pdf/VCF_C61_UserGuide_September2019.pdf)
  bitmaskVeg <- bitSumVeg$lte(6) # from 
  # ee_print(bitmaskVeg)
  # Map$addLayer(bitSumVeg, vizBit2)
  # Map$addLayer(bitmaskVeg, vizBit)
  
  # Could add the gte50 part here
  VegBitMasked <- img$updateMask(bitmaskVeg)
  # ee_print(VegMasked)
  # Map$addLayer(img$select('Percent_Tree_Cover'), vizTree, 'Percent Tree Cover')
  # Map$addLayer(VegMasked$select('Percent_Tree_Cover'), vizTree, 'Percent Tree Cover')
  return(VegBitMasked)
}

mod44bGood <- mod44b$map(vegBitMask)
# Map$addLayer(mod44bGood$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')

# This seems to work, but seems to be very slow
# mod44bReproj = mod44bGood$map(function(img){
#   return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
# })$map(addTime); # add year here!
# # Map$addLayer(mod44bReproj$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')

# wanting to mask out bad values from the MODIS veg layer
mod44bGood <- mod44b$map(function(IMG){IMG$updateMask(vegMask)})
# Map$addLayer(mod44bReproj$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')
# ee_print(mod44bReproj)


# we can't save image collections, so will save one image with multiple bands. These multiple bands are individual years.
yrMod <- ee$List(mod44bGood$aggregate_array("year"))$distinct()
yrString <- ee$List(paste0("YR", yrMod$getInfo()))

modTree <- ee$ImageCollection$toBands(mod44bGood$select("Percent_Tree_Cover"))$rename(yrString)
modVeg <- ee$ImageCollection$toBands(mod44bGood$select("Percent_NonTree_Vegetation"))$rename(yrString)
modBare <- ee$ImageCollection$toBands(mod44bGood$select("Percent_NonVegetated"))$rename(yrString)

# ee_print(modTree)
Map$addLayer(modTree$select("YR2020"), vizTree, "TreeCover")

saveTree <- ee_image_to_asset(modTree, description="Save_Mod44b_TreeCover", assetId=file.path(assetHome, "MOD44b_250m_native_Percent_Tree_Cover"), maxPixels = 10e12, scale=231.6564, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
saveTree$start()

saveVeg <- ee_image_to_asset(modVeg, description="Save_Mod44b_OtherVegCover", assetId=file.path(assetHome, "MOD44b_250m_native_Percent_NonTree_Vegetation"), maxPixels = 10e12, scale=231.6564, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
saveVeg$start()

saveBare <- ee_image_to_asset(modBare, description="Save_Mod44b_NonVeg", assetId=file.path(assetHome, "MOD44b_250m_native_Percent_NonVegetated"), maxPixels = 10e12, scale=231.6564, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
saveBare$start()
# ----------


# # -----------
# # 2.b - Landsat Land Surface Temperature -- Not saving because of the complexities involved----
# # -----------
# 
# # The code below was LST from MODIS--MRA
# # lstConvert <- function(img){
# #   lstDay <- img$select('LST_Day_1km')$multiply(0.02)
# #   lstNight <- img$select('LST_Night_1km')$multiply(0.02)
# #   lstK <- ee$Image(c(lstDay, lstNight));
# #   img <- img$addBands(srcImg=lstK, overwrite=TRUE);
# #   return(img)
# # }
# 
# # We need to get LST from Landsat, becasue it is at a finer resolution (1km MODIS vs. 30m Landsat native; will aggregate to 250m to match MODIS veg cover)
# 
# # function translating reflectance value of LST band (ST_10) into land surface temperature (K)
# Landsat.lstConvert <- function(img) {
#   lstDay <- img$select('ST_B10')$multiply(0.00341802)$add(149)
#   lstK <- ee$Image(c(lstDay));
#   img <- img$addBands(srcImg=lstK, overwrite=TRUE);
# }
# 
# # building the bit mask to separate good vs. bad values based on QA layer that is written in binary
# lstMask <- function(img){
#   qaPix <- img$select('QA_PIXEL');
#   qaRad <- img$select('QA_RADSAT');
#   terrMask <- qaRad$bitwiseAnd(11)$eq(0); ## get rid of any terrain occlusion
#   # satMask <- qaRad$bitwiseAnd(3 << 4)$eq(0); ## get rid of any saturated bands we use to calculate NDVI
#   satMask <- bitwiseExtract(qaRad, 3, 4)$eq(0) ## get rid of any saturated bands we use to calculate NDVI 
#   # clearMask <- qaPix$bitwiseAnd(1<<7)$eq(0)
#   clearMask <- bitwiseExtract(qaPix, 1, 5)$eq(0)
#   waterMask <- bitwiseExtract(qaPix, 7, 7)$eq(0)
#   cloudConf = bitwiseExtract(qaPix, 8, 9)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
#   shadowConf <- bitwiseExtract(qaPix, 10, 11)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
#   snowConf <- bitwiseExtract(qaPix, 12, 13)$lte(1) ## we can only go with low confidence; doing finer leads to NOTHING making the cut
#   
#   
#   img <- img$updateMask(clearMask$And(waterMask)$And(cloudConf)$And(shadowConf)$And(snowConf)$And(terrMask)$And(satMask));
#   
#   return(img)
# }
# 
# tempColors <- c(
#   '040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
#   '0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
#   '3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
#   'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
#   'ff0000', 'de0101', 'c21301', 'a71001', '911003'
# )
# vizTemp <- list(
#   min=10.0,
#   max=47.0,
#   palette=tempColors
# );
# 
# vizTempK <- list( 
#   min=10.0+273.15,
#   max=47.0+273.15,
#   palette=tempColors
# );
# 
# # 2.a.1 - Northern Hemisphere: July/August
# ##################### 
# # Read in & Format Landsat 8 ----
# ##################### 
# # filtering to just days from July and August
# tempJulAug <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$filter(ee$Filter$dayOfYear(182, 243))$filter(ee$Filter$date("2013-04-01", "2020-12-31"))$map(addTime);
# tempJulAug <- tempJulAug$map(Landsat.lstConvert) 
# tempJulAug <- tempJulAug$map(setYear)
# 
# lstDayGoodNH <- tempJulAug$map(lstMask)
# # ee_print(lstDayGoodNH)
# 
# yrList <- ee$List(lstDayGoodNH$aggregate_array("year"))$distinct()$sort()
# yrString <- yrList$map(ee_utils_pyfunc(function(j){
#   return(ee$String("YR")$cat(ee$String(ee$Number(j)$format())))
# }))
# 
# landsatReproj = lstDayGoodNH$map(function(img){
#   return(img$reproject(projveg)$reduceResolution(reducer=ee$Reducer$mean()))
#          })$map(addYear) # flipping teh order here to specify the reproject first seems to have helped.
# ee_print(landsatReproj)
# 
# tempYrMedian <- yrList$map(ee_utils_pyfunc(function(j){
#   YR <- ee$Number(j);
#   START <- ee$Date$fromYMD(YR,1,1);
#   END <- ee$Date$fromYMD(YR,12,31);
#   lstYR <- landsatReproj$filter(ee$Filter$date(START, END))
#  
#   tempMedian <- lstYR$select('ST_B10')$reduce(ee$Reducer$median())
#   
#   tempAgg <- ee$Image(tempMedian)
#   
#   ## ADD YEAR AS A PROPERTY!!
#   tempAgg <- tempAgg$set(ee$Dictionary(list(year=YR)))
#   tempAgg <- tempAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
#   # ee_print(tempAgg)
#   # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
#   # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Median Surface Temperature - Anomaly');
#   
#   return (tempAgg); # update to standardized once read
# }))
# 
# tempYrMedian <- ee$ImageCollection$fromImages(tempYrMedian) # go ahead and overwrite it since we're just changing form
# tempYrMedian <- ee$ImageCollection$toBands(tempYrMedian)$rename(yrString)
# 
# 
# 
# # landsatReproj < landsatReproj$map(function(img){
# #   return(img$reduceResolution(reducer=ee$Reducer$mean()))
# # })  
# 
# Map$addLayer(tempYrMedian$select("YR2020"), vizTempK)
# 
# 
# 
# savetempYrMedian <- ee_image_to_asset(tempYrMedian, description="save_250km_median_LST", assetId=file.path(assetHome, "landsat8_medianLST_250km"), maxPixels = 10e12, scale=231.656, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
# savetempYrMedian$start()
# #---------------------
# # Trying to mosaic tiles from a single date into one layer
# 
# # test <- ee$Algorithms$Landsat$simpleComposite(tempJulAug)
# # Map$addLayer(test, {bands: ['B4', 'B3', 'B2', max:128]}, 'TOA composite')
# 
# # ee_print(tempJulAug, limit(15))
# # ee_print(tempJulAug$first())
# 
# # Map$addLayer(tempJulAug$select("ST_B10")$first(), vizTempK, "LST (K)")
# # Map$addLayer(tempJulAug$select("SR_B4")$first())
# 
# 
# # Don't need the southern hemisphere at the moment; will only deal with US cities.
# # tempJanFeb <- ee$ImageCollection('MODIS/006/MOD11A2')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
# # tempJanFeb <- tempJanFeb$map(lstConvert)
# # tempJanFeb <- tempJanFeb$map(setYear)
# 
# # ee_print(tempJulAug)
# # tempJulAug$first()$propertyNames()$getInfo()
# # ee_print(tempJulAug$first())
# # Map$addLayer(tempJulAug$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")
# 
# Map$addLayer(tempJulAug$first()$select('ST_B10'), vizTempK, "Jul/Aug Temperature")
# 
# # This code would give us the projections for the Landsat code, but we don't need that right now
# # projLST = tempJulAug$select("ST_B10")$first()$projection() # Gettign the projection for the Landsat LST layer
# # projCRS = projLST$crs()
# # projTransform <- unlist(projLST$getInfo()$transform)
# 
# # ee_print(projLST)
# #projTransform #should produce NUMBERS!
# 
# # # Reset the projection so it's all the same as the first for sanity
# # This might be slow, but I think it's going to be better to do now rather than later for consistency with how I've treated other products
# # tempJulAug = tempJulAug$map(function(img){
# #   return(img$reproject(projLST))
# # })
# 
# 
# # tempJanFeb = tempJanFeb$map(function(img){ # don't have Southern Hem temps for the NorthStar Runs.
# #   return(img$reproject(projLST))
# # })
# 
# # Filtering good LST Data --> note: we'll still do some outlier remover from each city
# lstDayGoodNH <- tempJulAug$map(lstMask) # need to do the bitmasking **BEFORE** we do any aggregation to the MODIS data
# 
# # Map$addLayer(lstDayGoodNH$select("ST_B10")$first(), vizTemp)
# 
# # lstDayGoodSH <- tempJanFeb$map(lstMask)
# # Map$addLayer(lstDayGoodNH$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")
# # -----------
# 
# # aggregating the Landsat LST data to the same scale as the MODIS tree data
# 
# 
# landsatReproj = lstDayGoodNH$map(function(img){
#   return(img$reproject(projveg)$reduceResolution(reducer=ee$Reducer$mean())) # flipping teh order here to specify the reproject first seems to have helped.
# })$map(addTime);
# ee_print(landsatReproj)
# 
# # landsatReproj < landsatReproj$map(function(img){
# #   return(img$reduceResolution(reducer=ee$Reducer$mean()))
# # })  
# 
# Map$addLayer(landsatReproj$select("ST_B10")$first(), vizTempK)
# 
# # 
# landsatReproj <- landsatReproj$map(function(IMG){IMG$updateMask(vegMask)}) # need to ask christy if this is the correct mask
# ee_print(landsatReproj)
# 
# # Will need to resample the Landsat data to be at the same scale as the MODIS vegetation product data (250m)
# # Placeholder for later
# # mod44bReprojOrig = mod44b$map(function(img){
# #   return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
# # })$map(addTime); # add year here!
# 
# 
# # -----------
# # Revisit Temperatures to save
# # -----------
# # lstNHmask <- landsatReproj$select("ST_B10")$map(function(IMG){IMG$updateMask(vegMask)}) # is the vegMask correct here
# # lstSHmask <- lstDayGoodSH$select("LST_Day_1km")$map(function(IMG){IMG$updateMask(vegMask)})
# # ee_print(lstNHmask)
# # Map$addLayer(lstNHmask$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")
# 
# # reducing the size of the object we are workign wtih
# # was running into a user memory limit error
# # Still running into this error. May need to clip to just the US to get things to save.
# 
# NHlstonly <- landsatReproj$select("ST_B10")
# ee_print(NHlstonly)
# 
# # Trying to export each collection as a Collection 
# # Source: https://gis.stackexchange.com/questions/407146/export-imagecollection-to-asset
# sizeNH <- NHlstonly$size()$getInfo()
# lstNHList <- NHlstonly$toList(sizeNH)
# 
# ### --------------------------
# ### TESTING ONLY
# # sizeNH=10
# # lstNHList <- NHlstonly$toList(sizeNH) # making the list with just 10 images
# # 
# # # ee_print(ee$Image(lstNHList$get(1)))
# # TEST <- ee$Image(lstNHList$get(1))
# # ee_print(TEST)
# # testID <- TEST$id()$getInfo()
# # testID
# 
# 
# # # # What Christy thinks is happening:----
# 
# # 07/17::Still having memory issues
# # created script 01b_cityExtract_Extract_Data_EarthEngine_LST.R to extract city temperaturesfor every city individually.
# 
# # We have a TON of images because each image is a single point in time for a single tile -- not the whole world like MODIS
# # What we need to do is to either: 
# # 1. combine/*FLATTEN* tiles into single images for each time point (will hopefully work now that we've reduced the resolution, but TBD) and then save each time point
# #   -- Brendon/Christy *may* have a functino that does this --> CR saw Brendond do some sort of flatten function and it seemed unncessary, but I don't remember the context
# # 2. add "filter bounds" to where we first access Landsat 8 data to only pull tiles for the US and then either 
# #     2.1. repeat the process for 1 (should definitely work now becuase it should be totally tractable) 
# #     2.2. cross our fingers and hope that the "filter bounds" bit will work with the saved images and there's not lost metadata we need
# # 3. rejigger the LST workflow to pull directly from Landsat for each city.  This *should* work, but would be a significant rewrite of the code that will probably need Christy's help
# ### --------------------------
# 
# # Map$addLayer(ee$Image(lstNHList$get(5)), vizTempK)
# # sizeSH <- lstSHmask$size()$getInfo()
# # lstSHList <- lstSHmask$toList(sizeSH)
# 
# # Doing a loop for the Northern Hemisphere first
# for(i in 1:sizeNH-1){
#   img <- ee$Image(lstNHList$get(i))
#   imgID <- img$id()$getInfo()
#   print(imgID)
#   # ee_print(img)
#   # Map$addLayer(img, vizTempK, "Jul/Aug Temperature")
#   saveLSTNH <- ee_image_to_asset(img, description=paste0("Save_Repoj_LST_JulAug_", imgID), assetId=file.path(assetHome, "Reporj_LST_JulAug_Clean", imgID), maxPixels = 10e12, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
#   saveLSTNH$start()
# }
# 
# # for(i in 1:sizeSH-1){
# #   img <- ee$Image(lstSHList$get(i))
# #   imgID <- img$id()$getInfo()
# #   # ee_print(img)
# #   # Map$addLayer(img, vizTempK, "JanFeb Temperature")
# #   saveLSTSH <- ee_image_to_asset(img, description=paste0("Save_LST_JanFeb_", imgID), assetId=file.path(assetHome, "LST_JanFeb_Clean", imgID), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
# #   saveLSTSH$start()
# # }
# 
# # for (var i = 0; i < size; i++) {
# #   var img = ee.Image(listOfImage.get(i));
# #   var id = img.id().getInfo() || 'image_'+i.toString();
# #   var region = p.buffer(1000)
# #   var assetId = 'TEST'
# #   
# #   Export.image.toAsset({
# #     image: img,
# #     description: id,
# #     assetId: assetId,
# #     region: region,
# #     scale: 10,
# #     maxPixels: 1e13
# #   })
# # }
# 
# # -----------


# -----------
# Elevation
## Now using MERIT, which has combined several other products and removed bias, including from trees
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017GL072874
# -----------
elev <- ee$Image('MERIT/DEM/v1_0_3')#$select('elevation')
ee_print(elev)

elevReproj <- elev$reproject(projveg)
elevReproj <- elevReproj$updateMask(vegMask)
ee_print(elevReproj)

elevVis = list(
  min= 0,
  max= 5000,
  palette=c ('0000ff', '00ffff', 'ffff00', 'ff0000', 'ffffff')
);
Map$addLayer(elevReproj, elevVis, "Elevation - Masked, reproj")


saveElev <- ee_image_to_asset(elevReproj, description="Save_MERIT_Elevation_Reproj", assetId=file.path(assetHome, "MERIT-DEM-v1_250m_Reproj"), maxPixels = 10e12, scale=231.6564, region = maskBBox, crs="SR-ORG:6974", crsTransform=projTransform, overwrite=T)
saveElev$start()


