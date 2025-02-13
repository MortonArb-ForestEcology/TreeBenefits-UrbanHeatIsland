# Quick script to convert the SDEI and WWF shapefiles to match the projection of the MODIS data
library(raster); library(sp); library(terra); library(sf)
library(ggplot2)

library(rgee); 
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- "~/Google Drive/My Drive/"
GoogleFolderSave <- "UHI_Analysis_Output_Final_Shapefiles"
assetHome <- ee_get_assethome()
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)


# Loading data we'll need to do the model ET calculation
path.EEoutSpat <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")


# Load a standardized raster image
# projects/crollinson/assets
# vegMask <- ee$Image("users/crollinson/MOD44b_1km_Reproj_VegMask")

vegMask <- ee$Image("projects/earthengine-legacy/assets/users/crollinson/MOD44b_1km_Reproj_VegMask")
# Map$addLayer(vegMask)

# Pulling all the components even though we just need the whole projection
projMask = vegMask$projection()
projCRS = projMask$crs()
projTransform <- unlist(projMask$getInfo()$transform)



# Getting our SDEI data layer
sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]

sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]

# Chunking our sdei file into batches of cities
# -- this is super ugly and would probably be smoother IN earth engine, but I'm feeling lazy
citiesList <- list()
citiesFile = 200
citiesStart <- seq(1,nrow(sdei.df), by=citiesFile)
for(i in 1:length(citiesStart)){
  citiesList[[i]] <- sdei.df$ISOURBID[citiesStart[i]:(citiesStart[i]+citiesFile-1)]
}
length(citiesList)
# citiesList[["SDEI-S"]] <- sdei.df$ISOURBID[sdei.df$LATITUDE<0]
# citiesList[["SDEI-NW1"]] <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE<=-75]
# citiesList[["SDEI-NW1"]] <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE<=0]
# citiesList[["SDEI-NE1"]] <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>0 & sdei.df$LONGITUDE<=75]
# citiesList[["SDEI-NE2"]] <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>75]
# length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)
# summary(citiesList)
# 

# Put things in earth engine
# Put things in earth engine
sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei)

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
ee_print(citiesUse)

ee_sdeiTrans <- citiesUse$map(function(x){ 
  x$transform(projMask, maxError=42) # 42 because why not... it's the answer to life, the universe and everything
})
ee_print(ee_sdeiTrans)

# saveSDEI <- ee_table_to_drive(ee_sdeiTrans, description = "SDEI_MODISproj", fileFormat="SHP", folder=GoogleFolderSave, timePrefix = F)
# saveSDEI$start()


for(i in 1:length(citiesList)){
  ee_sdeiSub <- sf_as_ee(sdei.urb[sdei.urb$ISOURBID %in% citiesList[[i]],])
  # ee_print(ee_sdeiSub)
  
  ee_sdeiTransSub <- ee_sdeiSub$map(function(x){ 
    x$transform(projMask, maxError=42) # 42 because why not... it's the answer to life, the universe and everything
  })
  # ee_print(ee_sdeiTransSub)
  # print(paste0(names(citiesList)[i], "_MODISproj"))
  saveSDEI <- ee_table_to_drive(ee_sdeiTransSub, description = paste0("SDEI-", i, "_MODISproj"), fileFormat="SHP", folder=GoogleFolderSave, timePrefix = F)
  saveSDEI$start()
}






# Now doing the same process for ecoregions... it may actually make sense to download an ecoregion shapefile for each city... I'll have to think about that!
ecoregions <- ee$FeatureCollection('projects/crollinson/assets/WWF_Ecoregions');
ee_print(ecoregions)

ecoTrans <- ecoregions$map(function(x){ 
  x$transform(projMask, maxError=42) # 42 because why not... it's the answer to life, the universe and everything
})
# ee_print(ecoTrans)


EcoRegionSave <- "UHI_Analysis_Output_Final_Shapefiles-Ecoregions"
assetHome <- ee_get_assethome()

# "~/Google Drive/My Drive/"
if(!file.exists(file.path(path.google, "My Drive", EcoRegionSave))) dir.create(file.path(path.google, EcoRegionSave), recursive = T)


citiesUseBuff <- ee_sdeiTrans$map(function(f){f$buffer(10e3)})
ee_print(citiesUseBuff)

pb <- txtProgressBar(min=0, max=length(sdei.df$ISOURBID), style=3)
for(i in 1:nrow(sdei.df)){
  setTxtProgressBar(pb, i)
  cityID <- sdei.df$ISOURBID[i]
  # 648 = Chicago
  # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
  cityNow <- citiesUseBuff$filter(ee$Filter$eq('ISOURBID', cityID))
  # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
  # Map$addLayer(cityNow)
  # Map$addLayer(ecoTrans)
  
  cityNow <- cityNow$map(function(x){x$transform(projMask, maxError=42)})
  
  ee_print(cityNow)
  
  
  # biomeNow <- ecoTrans$filterBounds(cityNow)
  biomeNow <- ecoTrans$map(function(x){
    x$intersection(cityNow$first(), maxError=1)
  })
  # Map$addLayer(biomeNow)
  # ee_print(biomeNow)
  
  # Trying to figure out how we can get rid of empty layers
  print(biomeNow$size()$getInfo())
  
  biomeNow$map(function(x) {
    ee$Feature(NULL, list(
      area=x$geometry()$area(),
      valid=x$geometry()$isValid(),
      simple=x$geometry()$isSimple())
    )
  })$getInfo() %>% print()
  
  
  # Filter out empty geompetries
  # biomeNow <- biomeNow$map(function(x){
  #   ee$Feature(ee$Algorithms$If(ee$Number(x$geometry()$area())$gt(0), x, NULL))
  # }, TRUE)
  # ee_print(biomeNow)
  # 
  biomeNowTrans <- biomeNow$map(function(x){ 
    x$transform(projMask, maxError=1) # 42 because why not... it's the answer to life, the universe and everything
  })
  ee_print(biomeNowTrans)
  # print(biomeNowTrans$size()$getInfo())
  
  
  # valList <- biomeNowTrans$reduceRegion(
  #   reducer=ee$Reducer$toList()
  #   geometry = cityNow
  #   
  #   
  # )
  # 
  # biomeTransFilter <- biomeNowTrans$map(function(x){
  #   # ee$Feature(ee$Algorithms$If(x$geometry()$isEmpty(), NULL, x))
  #   # ee$Feature(ee$Algorithms$If(x$geometry()$isEmpty(), NULL, x))
  #   # ee$Feature(ee$Algorithms$If(ee$Geometry$isEmpty(x$geometry()), NULL, x))
  #   ee$Feature(ee$Algorithms$If(ee$Algorithms$IsEqual(x$geometry()$area(), 0), NULL, x))
  # })$filter(ee$Filter$notNull(list("geometry")))
  # ee_print(biomeTransFilter)
  # 
  
  saveEco <- ee_table_to_drive(biomeNowTrans, description = paste0(cityID, "_Ecoregions_MODISproj"), fileFormat="SHP", folder=GoogleFolderSave, timePrefix = F)
  saveEco$start()
  
}
