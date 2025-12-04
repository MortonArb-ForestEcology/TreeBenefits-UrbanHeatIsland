# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)

assetHome <- ee_get_assethome()
assetRoot <- "projects/urbanecodrought/assets"

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
##################### 


##################### 
# 2. Load in data layers  -- we did all the reprojeciton etc. in step 1, so this should be faster now, 
####################
emissColors2 <- c('#0602ff', '#235cb1', '#307ef3', '#269db1', '#30c8e2', '#32d3ef', '#3ae237','#b5e22e', '#d6e21f', '#fff705', '#ffd611', '#ffb613', '#ff8b13', '#ff6e08','#ff500d', '#ff0000', '#de0101', '#c21301')
emissColors <- c('0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef', '3ae237','b5e22e', 'd6e21f', 'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08','ff500d', 'ff0000', 'de0101', 'c21301')
vizEmiss <- list(min=0.90, max=1, palette=emissColors)


emissNH <- ee$Image('projects/urbanecodrought/assets/UHI-analysis/Emissivity_MODIS_1km_JulAug')
emissSH <- ee$Image('projects/urbanecodrought/assets/UHI-analysis/Emissivity_MODIS_1km_JanFeb')
ee_print(emissNH)
# Map$addLayer(emissNH$select("YR2020"), vizEmiss, "Emissivity: 1km, Reproj")
# Map$addLayer(emissSH$select("YR2020"), vizEmiss, "Emissivity: 1km, Reproj")

projTree = emissNH$projection()
projCRS = projTree$crs()
projTransform <- unlist(projTree$getInfo()$transform)
##################### 


extractEmissivity <- function(CitySP, CityNames, ALBEDO, GoogleFolderSave, overwrite=F, ...){
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
    # Extracting Emissivity
    #-------
    emissCity <- ALBEDO$clip(cityNow)
    # ee_print(emissCity)
    # Map$addLayer(emissCity$select('YR2020'), vizTree, 'Percent Tree Cover')
    
    exportEmissivity <- ee_image_to_drive(image=emissCity, description=paste0(cityID, "_Emissivity"), fileNamePrefix=paste0(cityID, "_Emissivity"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e7, crs=projCRS, crsTransform=projTransform)
    exportEmissivity$start()
    # #-------
    
  }  
}



##################### 
# 3 . Start extracting data for each city
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
  tree.done <- dir(file.path(path.google, GoogleFolderSave), "Emissivity.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(tree.done, "_"), function(x){x[1]}))
  
  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)


# Running a test case
# CITY = "SWE3477"
# extractEmissivity(CitySP=citiesUse, CityNames = CITY, ALBEDO=emissNH, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# testEmissivity <- stack(file.path(path.google, GoogleFolderSave, paste0(CITY, "_Emissivity.tif")))
# plot(testEmissivity[[1]])
# plot(testEmissivity)

if(length(cityIdS)>0){
  extractEmissivity(CitySP=citiesUse, CityNames = cityIdS, ALBEDO=emissSH, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}


if(length(cityIdNW)>0){
  extractEmissivity(CitySP=citiesUse, CityNames = cityIdNW, ALBEDO=emissNH, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE1)>0){
  extractEmissivity(CitySP=citiesUse, CityNames = cityIdNE1, ALBEDO=emissNH, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE2)>0){
  extractEmissivity(CitySP=citiesUse, CityNames = cityIdNE2, ALBEDO=emissNH, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}
