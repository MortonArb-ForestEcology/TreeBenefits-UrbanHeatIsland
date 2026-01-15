#  Extracting ERA5-Land reanalysis based on continued reviewer comments.
# this might get used in one of a couple ways
# 1. Use to parameterize a semi-mechanistic model of ET rather than rely on MODIS; doing a ET0 x Kc equation (FAO-style)+ evaluate precip



library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)

assetHome <- ee_get_assethome()
overwrite=F


##################### 
# 1. Load and select cities ----
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]
cityIdAll <- sdei.df$ISOURBID

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# ee_print(sdei)

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
# ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})

##################### 


##################### 
# 0. Set up helper functions ----
##################### 
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}
##################### 
tempColors <- c(
  '040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
  '0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
  '3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
  'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
  'ff0000', 'de0101', 'c21301', 'a71001', '911003'
)
vizTempC <- list(
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
  max=50,
  palette=rev(tempColors)
);

##################### 
# 1. Load in data layers  ----
####################

# Step 1: Get ERA formatted like our existing MODIS data
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR 
# Variables we care about -- CITY SCALE
varsERA <- c("temperature_2m",  "total_precipitation_sum", "surface_thermal_radiation_downwards_sum", "surface_solar_radiation_downwards_sum", "u_component_of_wind_10m", "v_component_of_wind_10m", "dewpoint_temperature_2m", "surface_pressure", "temperature_2m_min", "temperature_2m_max", "evaporation_from_bare_soil_sum") # Note bare sooil = transpriation b/c of weird ECMWF hicckup
unitsERA <- c("K", "m/day", "J/m2", "J/m2", "m/s", "m/s", "K", "Pa", "K", "K", "m/day")

# Note: one less var here because we'll convert u&v wind to wind
varNames <- c("tmean_C", "precip_mm", "rlong_MJm2", "rshort_MJm2", "wind_ms", "tdew_C", "press_kPA", "tmin_C", "tmax_C", "et_mm")
unitsUse <- c("C", "mm/day", "MJ/m2/day", "MJ/m2/Day", "m/s", "C", "kPA", "C", "C", "mm/day")

# Conversion list
# Kelvin to Celsius: C=K-273.15
# m to mm: mm = m*1e3
# J to MJ: MJ = J*1e-6
# Pa to kPa: kPa = Pa*1e-3

convert_era5 <- function(img) {
  # Temperature: Kelvin to Celsius
  # Total Precipitation: Meters to Millimeters (Total accumulated over the hour/day)
  
  tmean_c <- img$select("temperature_2m")$subtract(273.15)$rename("tmean_C")
  precip_mm <- img$select("total_precipitation_sum")$multiply(1000)$rename("precip_mm")
  rlong_MJm2 <- img$select("surface_thermal_radiation_downwards_sum")$divide(1000000)$rename("rlong_MJm2")
  rshort_MJm2 <- img$select("surface_thermal_radiation_downwards_sum")$divide(1000000)$rename("rshort_MJm2")
  tdew_C <- img$select("dewpoint_temperature_2m")$subtract(273.15)$rename("tdew_C")
  press_kPA <- img$select("surface_pressure")$divide(1000)$rename("press_kPA")
  tmin_C <- img$select("temperature_2m_min")$subtract(273.15)$rename("tmin_C") 
  tmax_C <- img$select("temperature_2m_max")$subtract(273.15)$rename("tmax_C") 
  et_mm <- img$select("evaporation_from_bare_soil_sum")$multiply(-1000)$rename("et_mm") # Converting so not negative
  
  # Calculate wind speed: sqrt(u^2 + v^2)
  # .hypot() is the most computationally efficient way to do this
  u_wind <- img$select("u_component_of_wind_10m")
  v_wind <- img$select("v_component_of_wind_10m")
  wind_ms <- u_wind$hypot(v_wind)$rename("wind_ms")
    
  # Return the image with the new bands (and keep original metadata)
  return(img$addBands(tmean_c)$addBands(precip_mm)$addBands(rlong_MJm2)$addBands(rshort_MJm2)$addBands(tdew_C)$addBands(press_kPA)$addBands(tmin_C)$addBands(tmax_C)$addBands(et_mm)$addBands(wind_ms))
}


era5JulAug <- ee$ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(varsERA) # Note: Doing the name change here
era5JulAug <- era5JulAug$map(setYear)$map(convert_era5) # Note: This is needed here otherwise the format is weird and code doesn't work!
# ee_print(era5JulAug)
# Map$addLayer(era5JulAug$first()$select("tmean_C"), vizTempC, "Jul/Aug Temperature")
# Map$addLayer(era5JulAug$first()$select("precip_mm"), vizPrecip, "Jul/Aug Precip")


era5JanFeb <- ee$ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')$filter(ee$Filter$dayOfYear(1,60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime)$select(varsERA)# Note: Doing the name change here
era5JanFeb <- era5JanFeb$map(setYear)$map(convert_era5) # Note: This is needed here otherwise the format is weird and code doesn't work!
# ee_print(era5JanFeb)
# Map$addLayer(era5JanFeb$first()$select("tmean_C"), vizTempC, "Jan/Feb Temperature")
# Map$addLayer(era5JanFeb$first()$select("precip_mm"), vizPrecip, "Jan/Feb Precip")



##################### 
# Set up the function
##################### 
extractERA5 <- function(CitySP, CityNames, ERA5, GoogleFolderSave, overwrite=F, ...){
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
    eraCity <- ERA5$filterBounds(cityNow)
    # eraCity <- map(function(img){
    #   return(img$clip(cityNow))
    # })
    # ee_print(eraCity)
    # ee_print(eraCity$first())
    # Map$addLayer(eraCity$first()$select(varsERA[1]), vizTempK, 'Temperature')
    # Map$addLayer(eraCity$first()$select(varsERA[2]), vizPrecip, "Jul/Aug Precip")
    # Map$addLayer(eraCity$first()$select(varsERA[length(varsERA)]), vizPrecip, "Jul/Aug ET")
    
    
    # # Code from NDVI work tor educe to a single value --> will need to be inside a map function()
    tableDay <- ee$FeatureCollection(eraCity$map(function(img){
      
      date_string <- img$date()$format("YYYY-MM-dd")
      
      RedMn =img$reduceRegion(reducer= ee$Reducer$mean(), geometry=cityNow$geometry(),
                              scale=11132, # hard-coded, but it's what has to happen to work
                              maxPixels=1e4)
      # RedMn$getInfo()  
      return(ee$Feature(NULL, RedMn)$set('year', img$get('year'))$set('date', date_string)$set('system:index', img$get('system:index')))
      
    }))
    
    fileNamePrefix = paste0(cityID, "_ERA5_daily")
    
    eraMeansSave <- ee_table_to_drive(collection=tableDay,
                                        description=paste0("Save_", fileNamePrefix),
                                        folder=GoogleFolderSave,
                                        fileNamePrefix=fileNamePrefix,
                                        timePrefix=F,
                                        fileFormat="CSV",
                                        selectors=c("year", "date", varNames))
    eraMeansSave$start()
  }  
}
##################### 


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

# # If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  era.done <- dir(file.path(path.google, GoogleFolderSave), "_ERA5_daily")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(era.done, "_"), function(x){x[1]}))
  
  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdNW <- cityIdNW[!cityIdNW %in% cityRemove]
  cityIdNE1 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
  
} # End remove cities loop
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)


# # Running a test case
# CITY = "SWE3477"
# extractERA5(CitySP=citiesUse, CityNames = CITY, ERA5=era5JulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# testERA <- read.csv(file.path(path.google, GoogleFolderSave, paste0(CITY, "_ERA5_daily.csv")))
# # plot(testTree[[1]])


if(length(cityIdS)>0){
  extractERA5(CitySP=citiesUse, CityNames = cityIdS, ERA5=era5JanFeb, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}


if(length(cityIdNW)>0){
  extractERA5(CitySP=citiesUse, CityNames = cityIdNW, ERA5=era5JulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE1)>0){
  extractERA5(CitySP=citiesUse, CityNames = cityIdNE1, ERA5=era5JulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(length(cityIdNE2)>0){
  extractERA5(CitySP=citiesUse, CityNames = cityIdNE2, ERA5=era5JulAug, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}
