# Extract NASA NEX-GDDP CMIP6 historical & future climatology from earth engine
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GDDP-CMIP6

## 
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
## 

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_GDDP-CMIP6"
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

##################### 


# modsCMIP6 = c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CESM2', 'CESM2-WACCM', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'CanESM5', 'EC-Earth3', 'EC-Earth3-Veg-LR', 'FGOALS-g3', 'GFDL-CM4', 'GFDL-ESM4', 'GISS-E2-1-G', 'HadGEM3-GC31-LL','IITM-ESM', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR', 'KACE-1-0-G', 'KIOST-ESM', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'NESM3','NorESM2-MM', 'TaiESM1', 'UKESM1-0-LL')
# # Excluded models:  'HadGEM3-GC31-MM',  'NorESM2-LM', 
# scenarios = c("historical", "ssp245", "ssp585")
# timeframes = data.frame(start=c(2001, 2050-19, 2100-19),
#                         end = c(2020, 2050, 2100),
#                         timeLabel = c("current", "midcentury", "endcentury"))
# collName = c('GDDP-CMIP6_JulAug', 'GDDP-CMIP6_JanFeb')
# SCEN="ssp245"
# TIME = "midcentury"

# This function will need to be run separately for each scenario & timeframe; 
extractCMIP6 <- function(CitySP, CityNames, collName, scenario, timeframe, GoogleFolderSave, overwrite=F, ...){

  GDDP <- ee$ImageCollection(file.path(assetHome, collName))$filter(ee$Filter$eq('scenario', scenario))$filter(ee$Filter$eq('timeframe', timeframe))
  # ee_print(GDDP)
  
  modNow <- ee$List(GDDP$aggregate_array("model"))
  
  # Ugly hack, but I feel better having the years as labels rather than my timeframes
  if(!timeframe %in% c("current", "midcentury", "endcentury")) stop("timeframe label not right!")
  YRLAB <- ifelse(timeframe=="current", "2001-2020", ifelse(timeframe=="midcentury", "2031-2050", "2081-2100"))
  
  # CITIES needs to be a list
  # Vegetation should be the reprojected MODIS44b product with year added in
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    # cityNow <- CitySP$filter('NAME=="Chicago"')$first()
    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNow)
    #-------
    
    
    #-------
    # Extracting Data for the whole region
    #-------
    # Start Tree Cover Layer
    gddpCity <- GDDP$map(function(img){
        return(img$clip(cityNow))
      })
    # ee_print(gddpCity)
    
    # Extract tas & pr to separate images with multiple bands to make the saving process cleaner
    GDDPtas = ee$ImageCollection$toBands(gddpCity$select("tas_mean"))$rename(modNow)
    GDDPpr = ee$ImageCollection$toBands(gddpCity$select("pr_mean"))$rename(modNow)

    exportTAS <- ee_image_to_drive(image=GDDPtas, description=paste(cityID, "GDDP-CMIP6", scenario, YRLAB,  "tas", sep="_"), fileNamePrefix=paste(cityID, "GDDP-CMIP6", scenario, YRLAB,  "tas", sep="_"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e7, crs="EPSG:4326", crsTransform=c(1, 0, 0, 0, 1, 0))
    exportTAS$start()
    
    exportPR <- ee_image_to_drive(image=GDDPpr, description=paste(cityID, "GDDP-CMIP6", scenario, YRLAB,  "pr", sep="_"), fileNamePrefix=paste(cityID, "GDDP-CMIP6", scenario, YRLAB,  "pr", sep="_"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e7, crs="EPSG:4326", crsTransform=c(1, 0, 0, 0, 1, 0))
    exportPR$start()
    
  }  
}

##################### 
# 3 . Start extracting data for each city
# NOTE: Each function will do 2 files for each city; we'll want 2x3 + 1 extractions for each city --> this is 14 files for each city!
# scenarios = c("historical", "ssp245", "ssp585")
# timeframes = data.frame(start=c(2001, 2050-19, 2100-19),
#                         end = c(2020, 2050, 2100),
#                         timeLabel = c("current", "midcentury", "endcentury"))
# collName = c('GDDP-CMIP6_JulAug', 'GDDP-CMIP6_JanFeb')
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())

cityIdS <-sdei.df$ISOURBID[sdei.df$LATITUDE<0]
cityIdNW <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE<=0]
cityIdNE1 <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>0 & sdei.df$LONGITUDE<=75]
cityIdNE2 <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0 & sdei.df$LONGITUDE>75]
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)


#######
# historical; current ----
#######
# # If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_historical_2001")

  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))

  cityIdS.Hist <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.Hist <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.Hist <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.Hist <- cityIdNE2[!cityIdNE2 %in% cityRemove]

} # End remove cities loop
length(cityIdS.Hist); length(cityIdNW.Hist); length(cityIdNE1.Hist); length(cityIdNE2.Hist)
# grep("ISL1285", cityRemove)
# grep("ISL1285", cityIdS.Hist)
# 
# # Running a test case
# # CITY = "SWE3477"
# # extractCMIP6(CitySP=citiesUse, CityNames = CITY, collName="GDDP-CMIP6_JulAug", scenario="historical", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# # test <- stack(file.path(path.google, GoogleFolderSave, paste(CITY, "GDDP-CMIP6", scenario, YRLAB,  "tas.tif", sep="_")))
# # plot(test[[30:32]])
# 
# 
# if(length(cityIdS.Hist)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.Hist, collName="GDDP-CMIP6_JanFeb", scenario="historical", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# 
# if(length(cityIdNW.Hist)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.Hist, collName="GDDP-CMIP6_JulAug", scenario="historical", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# if(length(cityIdNE1)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.Hist, collName="GDDP-CMIP6_JulAug", scenario="historical", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# if(length(cityIdNE2.Hist)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.Hist, collName="GDDP-CMIP6_JulAug", scenario="historical", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
#######

#######
# SSP 245; current ----
#######
# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_ssp245_2001")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS.45.2020 <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.45.2020 <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.45.2020 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.45.2020 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS.45.2020); length(cityIdNW.45.2020); length(cityIdNE1.45.2020); length(cityIdNE2.45.2020)


# if(length(cityIdS.45.2020)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.45.2020, collName="GDDP-CMIP6_JanFeb", scenario="ssp245", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# 
# if(length(cityIdNW.45.2020)>0){ # partially run
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.45.2020, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# if(length(cityIdNE1.45.2020)>0){ # Partially run
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.45.2020, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE2.45.2020)>0){ # Partially run
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.45.2020, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
#######


#######
# SSP 245; midcentury ----
#######
# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_ssp245_2031")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS.45.2050 <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.45.2050 <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.45.2050 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.45.2050 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS.45.2050); length(cityIdNW.45.2050); length(cityIdNE1.45.2050); length(cityIdNE2.45.2050)


# if(length(cityIdS.45.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.45.2050, collName="GDDP-CMIP6_JanFeb", scenario="ssp245", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }


# if(length(cityIdNW.45.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.45.2050, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE1.45.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.45.2050, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE2.45.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.45.2050, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
#######


#######
# SSP 245; endcentury ----
#######
# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_ssp245_2081")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS.45.2100 <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.45.2100 <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.45.2100 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.45.2100 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS.45.2100); length(cityIdNW.45.2100); length(cityIdNE1.45.2100); length(cityIdNE2.45.2100)


# if(length(cityIdS.45.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.45.2100, collName="GDDP-CMIP6_JanFeb", scenario="ssp245", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }


# if(length(cityIdNW.45.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.45.2100, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE1.45.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.45.2100, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# if(length(cityIdNE2.45.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.45.2100, collName="GDDP-CMIP6_JulAug", scenario="ssp245", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
#######


#######
# SSP 585; current ----
#######
# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_ssp585_2001")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS.85.2020 <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.85.2020 <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.85.2020 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.85.2020 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS.85.2020); length(cityIdNW.85.2020); length(cityIdNE1.85.2020); length(cityIdNE2.85.2020)



# if(length(cityIdS.85.2020)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.85.2020, collName="GDDP-CMIP6_JanFeb", scenario="ssp585", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }


# if(length(cityIdNW.85.2020)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.85.2020, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# if(length(cityIdNE1.85.2020)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.85.2020, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
# 
# if(length(cityIdNE2.85.2020)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.85.2020, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="current", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
#######


#######
# SSP 585; midcentury ----
#######
# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_ssp585_2031")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS.85.2050 <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.85.2050 <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.85.2050 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.85.2050 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS.85.2050); length(cityIdNW.85.2050); length(cityIdNE1.85.2050); length(cityIdNE2.85.2050)



# if(length(cityIdS.85.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.85.2050, collName="GDDP-CMIP6_JanFeb", scenario="ssp585", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }


# if(length(cityIdNW.85.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.85.2050, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE1.85.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.85.2050, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE2.85.2050)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.85.2050, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="midcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }
#######


#######
# SSP 585; endcentury ----
#######
# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  gldas.done <- dir(file.path(path.google, GoogleFolderSave), "CMIP6_ssp585_2081")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(gldas.done, "_"), function(x){x[1]}))
  
  cityIdS.85.2100 <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW.85.2100 <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1.85.2100 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2.85.2100 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS.85.2100); length(cityIdNW.85.2100); length(cityIdNE1.85.2100); length(cityIdNE2.85.2100)


# if(length(cityIdS.85.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdS.85.2100, collName="GDDP-CMIP6_JanFeb", scenario="ssp585", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }


# if(length(cityIdNW.85.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNW.85.2100, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

# if(length(cityIdNE1.85.2100)>0){
#   extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE1.85.2100, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

if(length(cityIdNE2.85.2100)>0){
  extractCMIP6(CitySP=citiesUse, CityNames = cityIdNE2.85.2100, collName="GDDP-CMIP6_JulAug", scenario="ssp585", timeframe="endcentury", GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}
#######
##################### 
