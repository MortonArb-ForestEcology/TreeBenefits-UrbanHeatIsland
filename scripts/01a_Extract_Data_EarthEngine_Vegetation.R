# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "~/Google Drive/My Drive/"
GoogleFolderSave <- "UHI_Analysis_Output_Final_v3"
assetHome <- ee_get_assethome()


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

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
# ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})
# ee_print(citiesUse)
###################

##################### 
# 2. Load in data layers  -- we did all the reprojeciton etc. in step 1, so this should be faster now, 
####################
vizTree <- list(
  # bands: ['Percent_Tree_Cover'],
  min=0.0,
  max=100.0,
  palette=c('bbe029', '0a9501', '074b03')
);

modTree <- ee$Image(file.path(assetHome,'MOD44b_250m_native_Percent_Tree_Cover'))
modVeg <- ee$Image(file.path(assetHome,'MOD44b_250m_native_Percent_NonTree_Vegetation'))
modBare <- ee$Image(file.path(assetHome,'MOD44b_250m_native_Percent_NonVegetated'))

ee_print(modTree)
# Map$addLayer(modTree$select("YR2020"), vizTree, "Tree Cover: 1km, Reproj")

projTree = modTree$projection()
projCRS = projTree$crs()
projTransform <- unlist(projTree$getInfo()$transform)

##################### 

extractVeg <- function(CitySP, CityNames, TREE, VEG, BARE, GoogleFolderSave, overwrite=F, ...){
  # CITIES needs to be a list
  # Vegetation should be the reprojected MODIS44b product with year added in
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNow)
    #-------
    
    
    #-------
    # Extracting vegetation cover -- we've already masked places where veg/non-veg cover doesn't add up
    #-------
    # Start Tree Cover Layer
    treeCity <- TREE$clip(cityNow)
    # ee_print(treeCity)
    # Map$addLayer(treeCity$select('2020_Percent_Tree_Cover'), vizTree, 'Percent Tree Cover')
    
    exportTree <- ee_image_to_drive(image=treeCity, description=paste0(cityID, "_Vegetation_PercentTree"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentTree"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=10e9, crs=projCRS, crsTransform=projTransform)
    exportTree$start()

    # Start Other Veg Cover Layer
    vegCity <- VEG$clip(cityNow)
    # ee_print(vegCity)
    exportVeg <- ee_image_to_drive(image=vegCity, description=paste0(cityID, "_Vegetation_PercentOtherVeg"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentOtherVeg"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=10e9, crs=projCRS, crsTransform=projTransform)
    exportVeg$start()

    # Start No Veg Cover layer
    bareCity <- BARE$clip(cityNow)
    
    export.bare <- ee_image_to_drive(image=bareCity, description=paste0(cityID, "_Vegetation_PercentNoVeg"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentNoVeg"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=10e9, crs=projCRS, crsTransform=projTransform)
    export.bare$start()
    #-------
    
  }  
}

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
  tree.done <- dir(file.path(path.google, GoogleFolderSave), "PercentTree.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(tree.done, "_"), function(x){x[1]}))
  
  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)


if(length(cityIdS)>0){
  extractVeg(CitySP=citiesUse, CityNames = cityIdS, TREE=modTree, VEG = modVeg, BARE=modBare, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}


if(length(cityIdNW)>0){
  extractVeg(CitySP=citiesUse, CityNames = cityIdNW, TREE=modTree, VEG = modVeg, BARE=modBare, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE1)>0){
  extractVeg(CitySP=citiesUse, CityNames = cityIdNE1, TREE=modTree, VEG = modVeg, BARE=modBare, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE2)>0){
  extractVeg(CitySP=citiesUse, CityNames = cityIdNE2, TREE=modTree, VEG = modVeg, BARE=modBare, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

##################### 

