# Export ERA5-Land total_evaporation as per-city GeoTIFFs for spatial Kc calibration.
# Output: one GeoTIFF per city, 20 bands = summer-mean et_total_mm for years 2001-2020.
# Resolution: ~9km (ERA5-Land native, scale=11132m).
# Clipped to city + 10km buffer extent.
#
# Downstream use: In R, aggregate MODIS MOD44B veg fractions (250m) to match ERA5 pixels,
# then regress ET/ET0 against veg fractions by biome to calibrate Kc values.
# ET0 (city-average) comes from the city-mean ERA5 CSVs extracted by 01f_ERA_Formatting.R.



library(rgee); library(raster); library(terra)
ee_check()
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, project="urbanecodrought")
path.google <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
if(!file.exists(file.path(path.google, GoogleFolderSave))) dir.create(file.path(path.google, GoogleFolderSave), recursive = T)

assetHome <- ee_get_assethome()
overwrite <- FALSE


#####################
# 1. Load and select cities ----
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP >= 100e3 & sdei.df$SQKM_FINAL >= 1e2, ]

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013')
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2))
citiesUse <- citiesUse$map(function(f){ f$buffer(10e3) })
#####################


#####################
# 2. ERA5 setup — same band selection and conversions as 01f_ERA_Formatting.R ----
#####################
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

varsERA <- c("temperature_2m", "total_precipitation_sum",
             "surface_thermal_radiation_downwards_sum", "surface_solar_radiation_downwards_sum",
             "u_component_of_wind_10m", "v_component_of_wind_10m",
             "dewpoint_temperature_2m", "surface_pressure",
             "temperature_2m_min", "temperature_2m_max",
             "evaporation_from_bare_soil_sum", "total_evaporation_sum")

convert_era5 <- function(img) {
  tmean_c    <- img$select("temperature_2m")$subtract(273.15)$rename("tmean_C")
  precip_mm  <- img$select("total_precipitation_sum")$multiply(1000)$rename("precip_mm")
  rlong_MJm2 <- img$select("surface_thermal_radiation_downwards_sum")$divide(1e6)$rename("rlong_MJm2")
  rshort_MJm2 <- img$select("surface_solar_radiation_downwards_sum")$divide(1e6)$rename("rshort_MJm2")
  tdew_C     <- img$select("dewpoint_temperature_2m")$subtract(273.15)$rename("tdew_C")
  press_kPA  <- img$select("surface_pressure")$divide(1000)$rename("press_kPA")
  tmin_C     <- img$select("temperature_2m_min")$subtract(273.15)$rename("tmin_C")
  tmax_C     <- img$select("temperature_2m_max")$subtract(273.15)$rename("tmax_C")
  et_mm      <- img$select("evaporation_from_bare_soil_sum")$multiply(-1000)$rename("et_mm")
  et_total_mm <- img$select("total_evaporation_sum")$multiply(-1000)$rename("et_total_mm")

  u_wind <- img$select("u_component_of_wind_10m")
  v_wind <- img$select("v_component_of_wind_10m")
  wind_ms <- u_wind$hypot(v_wind)$rename("wind_ms")

  return(img$addBands(tmean_c)$addBands(precip_mm)$addBands(rlong_MJm2)$addBands(rshort_MJm2)$
           addBands(tdew_C)$addBands(press_kPA)$addBands(tmin_C)$addBands(tmax_C)$
           addBands(et_mm)$addBands(et_total_mm)$addBands(wind_ms))
}

# Summer ImageCollections — same date windows as 01f
era5JulAug <- ee$ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')$
  filter(ee$Filter$dayOfYear(181, 240))$
  filter(ee$Filter$date("2001-01-01", "2020-12-31"))$
  map(addTime)$select(varsERA)$map(setYear)$map(convert_era5)

era5JanFeb <- ee$ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')$
  filter(ee$Filter$dayOfYear(1, 60))$
  filter(ee$Filter$date("2001-01-01", "2020-12-31"))$
  map(addTime)$select(varsERA)$map(setYear)$map(convert_era5)
#####################


#####################
# 3. Export function ----
#####################
# Exports a multi-band GeoTIFF per city:
#   - 20 bands, one per year (2001-2020)
#   - Each band = mean summer et_total_mm across all days in the season window
#   - Band names: et_total_2001, et_total_2002, ..., et_total_2020
#   - CRS: EPSG:4326, scale: ~9km (11132m = ERA5-Land native)
#   - Clipped to city + 10km buffer extent

extractERA5_etRaster <- function(CitySP, CityNames, ERA5, GoogleFolderSave, overwrite=F){
  years <- 2001:2020
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)

  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    fileNamePrefix <- paste0(cityID, "_ERA5_et_raster")

    if(!overwrite){
      outFile <- file.path(path.google, GoogleFolderSave, paste0(fileNamePrefix, ".tif"))
      if(file.exists(outFile)) next
    }

    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    eraCity <- ERA5$filterBounds(cityNow)

    # Compute per-year summer mean of et_total_mm; stack as one multi-band image
    yearBands <- lapply(years, function(y){
      eraCity$filter(ee$Filter$eq('year', as.integer(y)))$
        select("et_total_mm")$mean()$rename(paste0("et_total_", y))
    })
    etStack <- Reduce(function(a, b){ a$addBands(b) }, yearBands)

    task <- ee_image_to_drive(
      image       = etStack$clip(cityNow$geometry()),
      description = paste0("SaveETRaster_", cityID),
      folder      = GoogleFolderSave,
      fileNamePrefix = fileNamePrefix,
      scale       = 11132,
      region      = cityNow$geometry(),
      fileFormat  = "GeoTIFF",
      crs         = "EPSG:4326"
    )
    task$start()
  }
  close(pb)
}
#####################


#####################
# 4. Hemisphere splits and run ----
#####################
cityIdS   <- sdei.df$ISOURBID[sdei.df$LATITUDE < 0]
cityIdNW  <- sdei.df$ISOURBID[sdei.df$LATITUDE >= 0 & sdei.df$LONGITUDE <= 0]
cityIdNE1 <- sdei.df$ISOURBID[sdei.df$LATITUDE >= 0 & sdei.df$LONGITUDE > 0 & sdei.df$LONGITUDE <= 75]
cityIdNE2 <- sdei.df$ISOURBID[sdei.df$LATITUDE >= 0 & sdei.df$LONGITUDE > 75]
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)

if(!overwrite){
  rasters.done <- dir(file.path(path.google, GoogleFolderSave), pattern="_ERA5_et_raster")
  cityRemove <- sub("_ERA5_et_raster.*", "", rasters.done)
  cityIdS   <- cityIdS[!cityIdS   %in% cityRemove]
  cityIdNW  <- cityIdNW[!cityIdNW  %in% cityRemove]
  cityIdNE1 <- cityIdNE1[!cityIdNE1 %in% cityRemove]
  cityIdNE2 <- cityIdNE2[!cityIdNE2 %in% cityRemove]
}
length(cityIdS); length(cityIdNW); length(cityIdNE1); length(cityIdNE2)

# Test on one city before running the full batch:
# extractERA5_etRaster(citiesUse, "SWE3477", era5JulAug, GoogleFolderSave, overwrite=TRUE)
# testRas <- terra::rast(file.path(path.google, GoogleFolderSave, "SWE3477_ERA5_et_raster.tif"))
# plot(testRas[[1]])  # First band = 2001 summer mean

if(length(cityIdS)   > 0) extractERA5_etRaster(citiesUse, cityIdS,   era5JanFeb, GoogleFolderSave, overwrite)
if(length(cityIdNW)  > 0) extractERA5_etRaster(citiesUse, cityIdNW,  era5JulAug, GoogleFolderSave, overwrite)
if(length(cityIdNE1) > 0) extractERA5_etRaster(citiesUse, cityIdNE1, era5JulAug, GoogleFolderSave, overwrite)
if(length(cityIdNE2) > 0) extractERA5_etRaster(citiesUse, cityIdNE2, era5JulAug, GoogleFolderSave, overwrite)
