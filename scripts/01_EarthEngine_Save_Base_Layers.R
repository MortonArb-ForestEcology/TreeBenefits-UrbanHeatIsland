# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine
# NOTE: Some of this may need to get run in batches because of Earth Engine Storage Limits
library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output_v5"
assetHome <- ee_get_assethome()
assetRoot <- "projects/urbanecodrought/assets"

##################### 
# 0. Set up helper functions
##################### 
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

bitwiseExtract <- function(input, fromBit, toBit) {
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  return(input$rightShift(fromBit)$bitwiseAnd(mask))
}

bBoxS = ee$Geometry$BBox(-180, -60, 180, 5);
bBoxN = ee$Geometry$BBox(-180, -5, 180, 75);
maskBBox <- ee$Geometry$BBox(-180, -60, 180, 75)
##################### 


##################### 
# 2. Load in data layers 
####################
# -----------
# 2.a - Land Surface Temperature -- Not saving because of the complexities involved
# -----------
lstConvert <- function(img){
  lstDay <- img$select('LST_Day_1km')$multiply(0.02)
  lstNight <- img$select('LST_Night_1km')$multiply(0.02)
  lstK <- ee$Image(c(lstDay, lstNight));
  img <- img$addBands(srcImg=lstK, overwrite=TRUE);
  return(img)
}

# // Cleaning Up and getting just Good LST data
# // Code adapted from https://gis.stackexchange.com/a/349401/5160
# // Let's extract all pixels from the input image where
# // Bits 0-1 <= 1 (LST produced of both good and other quality)
# // Bits 2-3 = 0 (Good data quality)
# // Bits 4-5 Ignore, any value is ok
# // Bits 6-7 = 2 (Average LST error ≤ 2K)
# // var lstMask = function(qcDay, lstDay){
# //   var qaMask = bitwiseExtract(qcDay, 0, 1).lte(1)
# //   var dataQualityMask = bitwiseExtract(qcDay, 2, 3).eq(0)
# //   var lstErrorMask = bitwiseExtract(qcDay, 6, 7).eq(0)
# //   var mask = qaMask.and(dataQualityMask).and(lstErrorMask)
# //   var lstDayMasked = lstDay.updateMask(mask)  
# // }
lstMask <- function(img){
  qcDay <- img$select('QC_Day')
  qaMask <- bitwiseExtract(qcDay, 0, 1)$lte(1);
  dataQualityMask <- bitwiseExtract(qcDay, 2, 3)$eq(0)
  lstErrorMask <- bitwiseExtract(qcDay, 6, 7)$lte(2) # setting up error <=2 ˚K
  datVal <- img$select('LST_Day_1km')$gt(0)
  maskT <- qaMask$And(dataQualityMask)$And(lstErrorMask)$And(datVal)
  lstDayMasked <- img$updateMask(maskT)
  return(lstDayMasked)
}

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

# 2.a.1 - Northern Hemisphere: July/August
tempJulAug <- ee$ImageCollection('MODIS/061/MOD11A2')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
tempJulAug <- tempJulAug$map(lstConvert)
tempJulAug <- tempJulAug$map(setYear)

tempJanFeb <- ee$ImageCollection('MODIS/061/MOD11A2')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
tempJanFeb <- tempJanFeb$map(lstConvert)
tempJanFeb <- tempJanFeb$map(setYear)

# ee_print(tempJulAug)
# tempJulAug$first()$propertyNames()$getInfo()
# ee_print(tempJulAug$first())
# Map$addLayer(tempJulAug$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")
# Map$addLayer(tempJanFeb$first()$select('LST_Day_1km'), vizTempK, "Jan/Feb Temperature")

projLST = tempJulAug$select("LST_Day_1km")$first()$projection()
projCRS = projLST$crs()
projTransform <- unlist(projLST$getInfo()$transform)

# ee_print(projLST)


# # Reset the projection so it's all the same as the first for sanity
# This might be slow, but I think it's going to be better to do now rather than later for consistency with how I've treated other products
tempJulAug = tempJulAug$map(function(img){
  return(img$reproject(projLST))
})

tempJanFeb = tempJanFeb$map(function(img){
  return(img$reproject(projLST))
})

# Filtering good LST Data --> note: we'll still do some outlier remover from each city
lstDayGoodNH <- tempJulAug$map(lstMask)
lstDayGoodSH <- tempJanFeb$map(lstMask)
# Map$addLayer(lstDayGoodNH$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")
# -----------

# -----------
# 2.b MODIS Tree Data
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
vizBit2 <- list(
  min=0,
  max=8,
  palette=tempColors
);



mod44b <- ee$ImageCollection('MODIS/006/MOD44B')$filter(ee$Filter$date("2001-01-01", "2020-12-31"))
mod44b <- mod44b$map(setYear)
# ee_print(mod44b)
# Map$addLayer(mod44b$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')


# Create a noVeg Mask -- do this without having taken out the QAQC first because that will end up doing really weird things!
mod44bReprojOrig = mod44b$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!
# Map$addLayer(mod44bReprojOrig$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')

vegMask <- mod44bReprojOrig$first()$select("Percent_Tree_Cover", "Percent_NonTree_Vegetation", "Percent_NonVegetated")$reduce('sum')$gt(50)$mask()
# ee_print(vegMask)
# Map$addLayer(vegMask, vizBit)

# maskGeom <- vegMask$geometry()$getInfo()
# maskBBox <- ee$Geometry$BBox(-180, -90, 180, 90)

# proj4string: "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
saveVegMask <- ee_image_to_asset(vegMask, description="Save_VegetationMask", assetId=file.path(assetHome, "MOD44b_1km_Reproj_VegMask"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
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
mod44bReproj = mod44bGood$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!
# Map$addLayer(mod44bReproj$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')

mod44bReproj <- mod44bReproj$map(function(IMG){IMG$updateMask(vegMask)})
# Map$addLayer(mod44bReproj$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')
# ee_print(mod44bReproj)


yrMod <- ee$List(mod44bReproj$aggregate_array("year"))$distinct()
yrString <- ee$List(paste0("YR", yrMod$getInfo()))

modTree <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_Tree_Cover"))$rename(yrString)
modVeg <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_NonTree_Vegetation"))$rename(yrString)
modBare <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_NonVegetated"))$rename(yrString)

# ee_print(modTree)
Map$addLayer(modTree$select("YR2020"), vizTree, "TreeCover")

saveTree <- ee_image_to_asset(modTree, description="Save_Mod44bReproj_TreeCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveTree$start()

saveVeg <- ee_image_to_asset(modVeg, description="Save_Mod44bReproj_OtherVegCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonTree_Vegetation"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveVeg$start()

saveBare <- ee_image_to_asset(modBare, description="Save_Mod44bReproj_NonVeg", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonVegetated"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveBare$start()
# ----------

# -----------
# Revisit Temperatures to save
# -----------
lstNHmask <- lstDayGoodNH$select("LST_Day_1km")$map(function(IMG){IMG$updateMask(vegMask)})
lstSHmask <- lstDayGoodSH$select("LST_Day_1km")$map(function(IMG){IMG$updateMask(vegMask)})
ee_print(lstNHmask)
# Map$addLayer(lstNHmask$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")

# Trying to export each collection as a Collection 
# Source: https://gis.stackexchange.com/questions/407146/export-imagecollection-to-asset
sizeNH <- lstNHmask$size()$getInfo()
lstNHList <- lstNHmask$toList(sizeNH)

# Doing a loop for the Northern Hemisphere first
ee_manage_create(file.path(assetHome, "LST_JulAug_Clean"), asset_type="ImageCollection")
for(i in 1:sizeNH-1){
  img <- ee$Image(lstNHList$get(i))
  imgID <- img$id()$getInfo()
  # ee_print(img)
  # Map$addLayer(img, vizTempK, "Jul/Aug Temperature")
  saveLSTNH <- ee_image_to_asset(img, description=paste0("Save_LST_JulAug_", imgID), assetId=file.path(assetHome, "LST_JulAug_Clean", imgID), maxPixels = 10e9, scale=926.6, region = bBoxN, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
  saveLSTNH$start()
}


sizeSH <- lstSHmask$size()$getInfo()
lstSHList <- lstSHmask$toList(sizeSH)

ee_manage_create(file.path(assetHome, "LST_JanFeb_Clean"), asset_type="ImageCollection")
for(i in 1:sizeSH-1){
  img <- ee$Image(lstSHList$get(i))$clip(bBoxS)
  imgID <- img$id()$getInfo()
  # ee_print(img)
  # Map$addLayer(img, vizTempK, "JanFeb Temperature")
  saveLSTSH <- ee_image_to_asset(img, description=paste0("Save_LST_JanFeb_", imgID), assetId=file.path(assetHome, "LST_JanFeb_Clean", imgID), maxPixels = 10e9, scale=926.6, region = bBoxS, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
  saveLSTSH$start()
}
# -----------

# -----------
# Evapotranspiration ---- 
# Adding in evapotranspiraiton, which should be similar to LST, but finer in spatial (& temparol??) resolution
# Two Options:
# XX1. https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD16A2#bands says 2001-2023, but description notes major gaps; gapfilled project: https://lpdaac.usgs.gov/products/mod16a2gfv061/
# ---> The data on Earth Engine are no good.  Only >2001 & little urban core coverage  
# 2. https://developers.google.com/earth-engine/datasets/catalog/MODIS_NTSG_MOD16A2_105 # only runs 2000-2014
# --> also missing urban core, but maybe good enough to try
# -----------
# NOTE: This product will only run through 2016
ETConvert <- function(img){
  ET <- img$select('ET')$multiply(0.1)
  # PET <- img$select('PET')$multiply(0.1)
  # evapoT <- ee$Image(c(ET, PET));
  img <- img$addBands(srcImg=ET, overwrite=TRUE);
  return(img)
}

ETColors <- c('#ffffff', '#fcd163', '#99b718', '#66a000', '#3e8601', '#207401', '#056201',
              '#004c00', '#011301')
vizET <- list(
  min=0,
  max=30,
  palette=c('ffffff', 'fcd163', '99b718', '66a000', '3e8601', '207401', '056201',
            '004c00', '011301')
);


ETJulAug <- ee$ImageCollection('MODIS/061/MOD16A2GF')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$select("ET")$map(addTime);
ETJulAug <- ETJulAug$map(ETConvert)
# ETJulAug <- ETJulAug$map(setYear)

ETJanFeb <- ee$ImageCollection('MODIS/061/MOD16A2GF')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$select("ET")$map(addTime);
ETJanFeb <- ETJanFeb$map(ETConvert)
# ETJanFeb <- ETJanFeb$map(setYear)
# 
# ee_print(ETJulAug)
# ETJulAug$first()$propertyNames()$getInfo()
# ee_print(ETJulAug$first())
# Map$addLayer(ETJulAug$first()$select('ET'), vizET, "Jul/Aug Evapotranspiration")
# Map$addLayer(ETJulAug$first()$select('PET'), vizET, "Jul/Aug Evapotranspiration")
# Map$addLayer(ETJanFeb$first()$select('ET'), vizET, "Jan/Feb Evapotranspiration")


# 
ETJulAugClean = ETJulAug$select('ET')$map(function(img){
  return(img$reproject(projLST))
})

ETJanFeb = ETJanFeb$select('ET')$map(function(img){
  return(img$reproject(projLST))
})

# ETJulAugMask <- ETJulAug$select("ET")$map(function(IMG){IMG$updateMask(vegMask)})
# PETJulAugMask <- ETJulAug$select("ET")$map(function(IMG){IMG$updateMask(vegMask)})
# ETJanFebMask <- ETJanFeb$select("ET")$map(function(IMG){IMG$updateMask(vegMask)})
# PETJanFebMask <- ETJanFeb$select("ET")$map(function(IMG){IMG$updateMask(vegMask)})

# ee_print(ETJulAugClean)
# ETJulAug$first()$propertyNames()$getInfo()
# ee_print(ETJulAug$first())
# Map$addLayer(ETJulAug$first()$select('ET'), vizET, "Jul/Aug Evapotranspiration")

# Trying to export each collection as a Collection 
# Doing a loop for the Northern Hemisphere first
# Source: https://gis.stackexchange.com/questions/407146/export-imagecollection-to-asset
sizeETJA <- ETJulAug$size()$getInfo()
ETJAList <- ETJulAug$toList(sizeETJA)

ee_manage_create(file.path(assetHome, "ET_JulAug"), asset_type="ImageCollection")
for(i in 1:sizeETJA-1){
  img <- ee$Image(ETJAList$get(i))
  imgID <- img$id()$getInfo()
  # ee_print(img)
  # Map$addLayer(img$select("ET"), vizET, "Jul/Aug ET")
  saveETNH <- ee_image_to_asset(img$select("ET"), description=paste0("Save_ET_JulAug_", imgID), assetId=file.path(assetHome, "ET_JulAug", imgID), maxPixels = 10e9, scale=926.6, region = bBoxN, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
  saveETNH$start()
}


# Now Doing a loop for the Southern Hemisphere 
sizeETJF <- ETJanFeb$size()$getInfo()
ETJFList <- ETJanFeb$toList(sizeETJF)

ee_manage_create(file.path(assetHome, "ET_JanFeb"), asset_type="ImageCollection")
for(i in 1:sizeETJF-1){
  img <- ee$Image(ETJFList$get(i))
  imgID <- img$id()$getInfo()
  # ee_print(img)
  # Map$addLayer(img$select("ET"), vizET, "Jul/Aug ET")
  saveETSH <- ee_image_to_asset(img$select("ET"), description=paste0("Save_ET_JanFeb_", imgID), assetId=file.path(assetHome, "ET_JanFeb", imgID), maxPixels = 10e9, scale=926.6, region = bBoxS, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
  saveETSH$start()
}
# -----------


# -----------
# Emissivity ---- 
# Pulling from our LST product: https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD11A2
# -----------
emisConvert <- function(img){
  emiss <- img$select('Emis_31')$multiply(0.002)$add(0.49)
  EM <- ee$Image(emiss);
  img <- img$addBands(srcImg=EM, overwrite=TRUE);
  return(img)
}

emisMask <- function(img){
  qcDay <- img$select('QC_Day')
  qaMask <- bitwiseExtract(qcDay, 0, 1)$lte(1);
  dataQualityMask <- bitwiseExtract(qcDay, 2, 3)$eq(0)
  emisErrorMask <- bitwiseExtract(qcDay, 4, 5)$lte(1) # lowest error didn't work for us
  # datVal <- img$select('LST_Day_1km')$gt(0)
  maskEmis <- qaMask$And(dataQualityMask)$And(emisErrorMask)
  emisDayMasked <- img$updateMask(maskEmis)
  return(emisDayMasked)
}

emissColors2 <- c('#0602ff', '#235cb1', '#307ef3', '#269db1', '#30c8e2', '#32d3ef', '#3ae237','#b5e22e', '#d6e21f', '#fff705', '#ffd611', '#ffb613', '#ff8b13', '#ff6e08','#ff500d', '#ff0000', '#de0101', '#c21301')
emissColors <- c('0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef', '3ae237','b5e22e', 'd6e21f', 'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08','ff500d', 'ff0000', 'de0101', 'c21301')
vizEmiss <- list(min=0.90, max=1, palette=emissColors)

# ee$ImageCollection('MODIS/061/MOD11A2')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)
emisJulAug <- ee$ImageCollection('MODIS/061/MOD11A2')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
emisJulAug <- emisJulAug$map(emisConvert)
emisJulAug <- emisJulAug$map(setYear)
# 
emisJanFeb <- ee$ImageCollection('MODIS/061/MOD11A2')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
emisJanFeb <- emisJanFeb$map(emisConvert)
emisJanFeb <- emisJanFeb$map(setYear)
# # 
# ee_print(emisJulAug)
# emisJulAug$first()$propertyNames()$getInfo()
# ee_print(emisJulAug$first())
Map$addLayer(emisJulAug$first()$select('Emis_31'), vizEmiss, "Jul/Aug Emissivity")
Map$addLayer(emisJanFeb$first()$select('Emis_31'), vizEmiss, "Jan/Feb Emissivity")
# 
# 
emisGoodNH <- emisJulAug$map(emisMask)
emisGoodSH <- emisJanFeb$map(emisMask)
# Map$addLayer(emisGoodNH$first()$select('Emis_31'), vizEmiss, "Jul/Aug Emissivity")


emisGoodNH = emisGoodNH$map(function(img){
  return(img$reproject(projLST))
})

emisGoodSH = emisGoodSH$map(function(img){
  return(img$reproject(projLST))
})
ee_print(emisGoodSH)

emisNHmask <- emisGoodNH$select("Emis_31")$map(function(IMG){IMG$updateMask(vegMask)})
emisSHmask <- emisGoodSH$select("Emis_31")$map(function(IMG){IMG$updateMask(vegMask)})
# Map$addLayer(emisNHmask$first()$select('Emis_31'), vizEmiss, "Jul/Aug Emissivity")
# Map$addLayer(emisGoodSH$first()$select('Emis_31'), vizEmiss, "Jan/Feb Emissivity")


# # Try to just go ahead and aggregate to year now
yrList <- ee$List(emisNHmask$aggregate_array("year"))$distinct()
yrString <- yrList$map(ee_utils_pyfunc(function(j){
  return(ee$String("YR")$cat(ee$String(ee$Number(j)$format())))
}))
# yrString$getInfo()

emissNHYrMean <- yrList$map(ee_utils_pyfunc(function(j){
  YR <- ee$Number(j);
  START <- ee$Date$fromYMD(YR,1,1);
  END <- ee$Date$fromYMD(YR,12,31);
  emissYR <- emisNHmask$filter(ee$Filter$date(START, END))
  # // var lstDev =  // make each layer an anomaly map
  emissMean <- emissYR$select('Emis_31')$reduce(ee$Reducer$median())
  # tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
  emissAgg <- ee$Image(emissMean)

  ## ADD YEAR AS A PROPERTY!!
  emissAgg <- emissAgg$set(ee$Dictionary(list(year=YR)))
  emissAgg <- emissAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
  # ee_print(tempAgg)
  # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
  # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');

  return (emissAgg); # update to standardized once read
}))
emissNHYrMean <- ee$ImageCollection$fromImages(emissNHYrMean) # go ahead and overwrite it since we're just changing form
emissNHYrMean <- ee$ImageCollection$toBands(emissNHYrMean)$rename(yrString)
emissNHYrMean <- emissNHYrMean$setDefaultProjection(projLST)
# ee_print(emissNHYrMean)
# Map$addLayer(emissNHYrMean$select('YR2020'), vizemiss, 'Median emiss');

saveEmissNH <- ee_image_to_asset(emissNHYrMean, description="Save_Emissivity_NH", assetId=file.path(assetHome, "Emissivity_MODIS_1km_JulAug"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveEmissNH$start()

# 
emissSHYrMean <- yrList$map(ee_utils_pyfunc(function(j){
  YR <- ee$Number(j);
  START <- ee$Date$fromYMD(YR,1,1);
  END <- ee$Date$fromYMD(YR,12,31);
  emissYR <- emisSHmask$filter(ee$Filter$date(START, END))
  # // var lstDev =  // make each layer an anomaly map
  emissMean <- emissYR$select('Emis_31')$reduce(ee$Reducer$median())
  # tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
  emissAgg <- ee$Image(emissMean)
  
  ## ADD YEAR AS A PROPERTY!!
  emissAgg <- emissAgg$set(ee$Dictionary(list(year=YR)))
  emissAgg <- emissAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
  # ee_print(tempAgg)
  # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
  # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
  
  return (emissAgg); # update to standardized once read
}))
emissSHYrMean <- ee$ImageCollection$fromImages(emissSHYrMean) # go ahead and overwrite it since we're just changing form
emissSHYrMean <- ee$ImageCollection$toBands(emissSHYrMean)$rename(yrString)
emissSHYrMean <- emissSHYrMean$setDefaultProjection(projLST)
# ee_print(emissSHYrMean)
# Map$addLayer(emissSHYrMean$select('YR2020'), vizemiss, 'Median emiss');

saveEmissSH <- ee_image_to_asset(emissSHYrMean, description="Save_Emissivity_SH", assetId=file.path(assetHome, "Emissivity_MODIS_1km_JanFeb"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveEmissSH$start()





# Trying to export each collection as a Collection 
# Source: https://gis.stackexchange.com/questions/407146/export-imagecollection-to-asset
emisSizeNH <- emisNHmask$size()$getInfo()
emisNHList <- emisNHmask$toList(emisSizeNH)

# Doing a loop for the Northern Hemisphere first
# Create the Image Collection folder manually
# ee_manage_create(file.path(assetHome, "Emis_JulAug_Clean"), asset_type="ImageCollection")
for(i in 1:sizeNH-1){
  img <- ee$Image(emisNHList$get(i))
  imgID <- img$id()$getInfo()
  # ee_print(img)
  # Map$addLayer(img, vizTempK, "Jul/Aug Temperature")
  saveemisNH <- ee_image_to_asset(img, description=paste0("Save_Emis_JulAug_", imgID), assetId=file.path(assetRoot, "Emis_JulAug_Clean", imgID), maxPixels = 10e9, scale=926.6, region = bBoxN, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
  saveemisNH$start()
}


sizeSH <- emisSHmask$size()$getInfo()
emisSHList <- emisSHmask$toList(sizeSH)

# Create this manually!
# ee_manage_create(file.path(assetRoot, "Emis_JanFeb_Clean"), asset_type="ImageCollection")

for(i in 1:sizeSH-1){
  img <- ee$Image(emisSHList$get(i))$clip(bBoxS)
  imgID <- img$id()$getInfo()
  # ee_print(img)
  # Map$addLayer(img, vizTempK, "JanFeb Temperature")
  saveemisSH <- ee_image_to_asset(img, description=paste0("Save_Emis_JanFeb_", imgID), assetId=file.path(assetRoot, "Emis_JanFeb_Clean", imgID), maxPixels = 10e9, scale=926.6, region = bBoxS, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
  saveemisSH$start()
}
# -----------


# -----------
# Albedo ---- 
# Pulling from MODIS: https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD43A3
# -----------
albedoConvert <- function(img){
  alb <- img$select('Albedo_WSA_shortwave')$multiply(0.001)
  ALB <- ee$Image(alb);
  img <- img$addBands(srcImg=ALB, overwrite=TRUE);
  return(img)
}

albedoMask <- function(img){
  qcDay <- img$select('BRDF_Albedo_Band_Mandatory_Quality_shortwave')
  qaMask <- qcDay$eq(0);
  # dataQualityMask <- bitwiseExtract(qcDay, 2, 3)$eq(0)
  # albedoErrorMask <- bitwiseExtract(qcDay, 4, 5)$lte(1) # lowest error didn't work for us
  # # datVal <- img$select('LST_Day_1km')$gt(0)
  # maskalbedo <- qaMask$And(dataQualityMask)$And(albedoErrorMask)
  albedoDayMasked <- img$updateMask(qaMask)
  return(albedoDayMasked)
}

albedoColors2 <- c('#0602ff', '#235cb1', '#307ef3', '#269db1', '#30c8e2', '#32d3ef', '#3ae237','#b5e22e', '#d6e21f', '#fff705', '#ffd611', '#ffb613', '#ff8b13', '#ff6e08','#ff500d', '#ff0000', '#de0101', '#c21301')
albedoColors <- c('0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef', '3ae237','b5e22e', 'd6e21f', 'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08','ff500d', 'ff0000', 'de0101', 'c21301')
vizAlbedo <- list(min=0.05, max=0.40, palette=albedoColors)

# ee$ImageCollection('MODIS/061/MOD11A2')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)
albedoJulAug <- ee$ImageCollection('MODIS/061/MCD43A3')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
albedoJulAug <- albedoJulAug$map(albedoConvert)
albedoJulAug <- albedoJulAug$map(setYear)
# 
albedoJanFeb <- ee$ImageCollection('MODIS/061/MCD43A3')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
albedoJanFeb <- albedoJanFeb$map(albedoConvert)
albedoJanFeb <- albedoJanFeb$map(setYear)
# # 
# ee_print(albedoJulAug)
# albedoJulAug$first()$propertyNames()$getInfo()
# ee_print(albedoJulAug$first())
Map$addLayer(albedoJulAug$first()$select('Albedo_WSA_shortwave'), vizAlbedo, "Jul/Aug albedo")
Map$addLayer(albedoJanFeb$first()$select('Albedo_WSA_shortwave'), vizAlbedo, "Jan/Feb albedo")
# 
# 
# albedoGoodNH <- albedoJulAug$map(albedoMask)
# Map$addLayer(albedoGoodNH$first()$select('Albedo_WSA_shortwave'), vizAlbedo, "Jul/Aug Alebdo")

# albedoGoodSH <- albedoJanFeb$map(albedoMask)
# Map$addLayer(albedoGoodSH$first()$select('Albedo_WSA_shortwave'), vizAlbedo, "Jan/Feb Alebdo")


# Making sure to do the reprojection
albedoNHReproj = albedoJulAug$select("Albedo_WSA_shortwave")$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!
# Map$addLayer(albedoNHReproj$first()$select('Albedo_WSA_shortwave'), vizAlbedo, "Jul/Aug Alebdo")
# ee_print(albedoNHReproj)

albedoNHReproj <- albedoNHReproj$map(function(IMG){IMG$updateMask(vegMask)})
# ee_print(albedoNHReproj)

# lstSHmask <- lstDayGoodSH$select("LST_Day_1km")$map(function(IMG){IMG$updateMask(vegMask)})

albedoSHReproj = albedoJanFeb$select("Albedo_WSA_shortwave")$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!
# Map$addLayer(albedoNHReproj$first()$select('Albedo_WSA_shortwave'), vizAlbedo, "Jul/Aug Alebdo")
# ee_print(albedoNHReproj)

albedoSHReproj <- albedoSHReproj$map(function(IMG){IMG$updateMask(vegMask)})


# Try to just go ahead and aggregate to year now
yrList <- ee$List(albedoNHReproj$aggregate_array("year"))$distinct()
yrString <- yrList$map(ee_utils_pyfunc(function(j){
  return(ee$String("YR")$cat(ee$String(ee$Number(j)$format())))
}))

albedoNHYrMean <- yrList$map(ee_utils_pyfunc(function(j){
  YR <- ee$Number(j);
  START <- ee$Date$fromYMD(YR,1,1);
  END <- ee$Date$fromYMD(YR,12,31);
  albedoYR <- albedoNHReproj$filter(ee$Filter$date(START, END))
  # // var lstDev =  // make each layer an anomaly map
  albedoMean <- albedoYR$select('Albedo_WSA_shortwave')$reduce(ee$Reducer$median())
  # tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
  albedoAgg <- ee$Image(albedoMean)
  
  ## ADD YEAR AS A PROPERTY!!
  albedoAgg <- albedoAgg$set(ee$Dictionary(list(year=YR)))
  albedoAgg <- albedoAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
  # ee_print(tempAgg)
  # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
  # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
  
  return (albedoAgg); # update to standardized once read
}))
albedoNHYrMean <- ee$ImageCollection$fromImages(albedoNHYrMean) # go ahead and overwrite it since we're just changing form
albedoNHYrMean <- ee$ImageCollection$toBands(albedoNHYrMean)$rename(yrString)
# albedoNHYrMean <- albedoNHYrMean$setDefaultProjection(projLST)
# ee_print(albedoNHYrMean)
# Map$addLayer(albedoNHYrMean$select('YR2020'), vizAlbedo, 'Median Albedo');

saveAlbedoNH <- ee_image_to_asset(albedoNHYrMean, description="Save_Albedo_NH", assetId=file.path(assetHome, "Albedo_MCD43A3_1km_NorthHemisphere"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveAlbedoNH$start()


albedoSHYrMean <- yrList$map(ee_utils_pyfunc(function(j){
  YR <- ee$Number(j);
  START <- ee$Date$fromYMD(YR,1,1);
  END <- ee$Date$fromYMD(YR,12,31);
  albedoYR <- albedoSHReproj$filter(ee$Filter$date(START, END))
  # // var lstDev =  // make each layer an anomaly map
  albedoMean <- albedoYR$select('Albedo_WSA_shortwave')$reduce(ee$Reducer$median())
  # tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
  albedoAgg <- ee$Image(albedoMean)
  
  ## ADD YEAR AS A PROPERTY!!
  albedoAgg <- albedoAgg$set(ee$Dictionary(list(year=YR)))
  albedoAgg <- albedoAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
  # ee_print(tempAgg)
  # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
  # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
  
  return (albedoAgg); # update to standardized once read
}))
albedoSHYrMean <- ee$ImageCollection$fromImages(albedoSHYrMean) # go ahead and overwrite it since we're just changing form
albedoSHYrMean <- ee$ImageCollection$toBands(albedoSHYrMean)$rename(yrString)
albedoSHYrMean <- albedoSHYrMean$setDefaultProjection(projLST)
# ee_print(albedoSHYrMean)
# Map$addLayer(albedoSHYrMean$select('YR2020'), vizAlbedo, 'Median Albedo');

saveAlbedoSH <- ee_image_to_asset(albedoSHYrMean, description="Save_Albedo_SH", assetId=file.path(assetHome, "Albedo_MCD43A3_1km_SouthHemisphere"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveAlbedoSH$start()

# -----------


# -----------
# Elevation
## Now using MERIT, which has combined several other products and removed bias, including from trees
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017GL072874
# -----------
elev <- ee$Image('MERIT/DEM/v1_0_3')#$select('elevation')
ee_print(elev)

elevReproj <- elev$reproject(projLST)
elevReproj <- elevReproj$updateMask(vegMask)
ee_print(elevReproj)

elevVis = list(
  min= 0,
  max= 5000,
  palette=c ('0000ff', '00ffff', 'ffff00', 'ff0000', 'ffffff')
);
Map$addLayer(elevReproj, elevVis, "Elevation - Masked, reproj")


saveElev <- ee_image_to_asset(elevReproj, description="Save_MERIT_Elevation", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveElev$start()


