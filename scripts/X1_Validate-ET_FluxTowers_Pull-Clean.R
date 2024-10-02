# Validate ET models using available Flux Tower data
# Step 1: figure out what Ameriflux of Fluxnet towers are in the footprint of our analyzed cities


library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
# library(mgcv)

overwrite=F

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4/data_processed_final")
path.tower <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/ET Validation")


# Lets add the ecoregion for each city; accessed 27 Oct 2022 9:30 a.m.
# SDEI shapefile: https://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013/data-download# # NOTE: REQUIRES LOGIN
# ecoregion file: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
sf_use_s2(FALSE)

sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)
# plot(sdei.urb[1,])

# Read in the cities we actually performed our analyses on
cities.et <- read.csv(file.path(path.cities, "../UHIs-FinalCityDataForAnalysis.csv"))
summary(cities.et)

sdei.urb <- sdei.urb[sdei.urb$ISOURBID %in% cities.et$ISOURBID,]

# 1b. Checking Ameriflux for additional locations
library(amerifluxr)
amerifluxAll <- amf_site_info()
summary(amerifluxAll)
amerifluxCC <- amerifluxAll[amerifluxAll$DATA_POLICY=="CCBY4.0",]
summary(amerifluxCC)


ameriflux.sp <- st_as_sf(amerifluxCC, coords=c("LOCATION_LONG", "LOCATION_LAT"))
st_crs(ameriflux.sp) <- st_crs(sdei.urb)

ameriflux.urb <- st_filter(ameriflux.sp, sdei.urb)
summary(ameriflux.urb) # 29 cities with our temporal window in our city footprint and CC-BY-4.0 license
data.frame(ameriflux.urb[,c("SITE_ID", "SITE_NAME", "COUNTRY", "STATE", "IGBP")])

ameriflux.urb2 <- st_join(ameriflux.urb, sdei.urb[,c("ISOURBID", "ISO3", "NAME")], largest=F)
ameriflux.urb2 <- merge(data.frame(ameriflux.urb2), amerifluxCC, all.y=F)
summary(ameriflux.urb2)
length(unique(ameriflux.urb2$ISOURBID)) # 19 unique cities

write.csv(data.frame(ameriflux.urb2), file.path(path.tower, "AmerifluxSites_Use.csv"), row.names=F)


# Extract the actual ameriflux data
if(!dir.exists(file.path(path.tower, "Ameriflux"))) dir.create(file.path(path.tower, "Ameriflux"))
fzip <- dir(file.path(path.tower, "Ameriflux"), ".zip")
# sites.search <- vector()
# for(SITE in ameriflux.urb2$SITE_ID){
#   if(!grepl(SITE, fzip)) sites.search <- c(sites.search, SITE)   # If we don't have a file for the site, append it to our list
# }

amf_download_base(user_id = "crollinson", user_email="crollinson@mortonarb.org", site_id = ameriflux.urb2$SITE_ID, data_product = "BASE-BADM", data_policy="CCBY4.0", agree_policy = TRUE, intended_use="remote_sensing", intended_use_text = "validation of interpolated MODIS ET products in urban areas", out_dir = file.path(path.tower, "Ameriflux"))

# fzip <- dir(file.path(path.tower, "Ameriflux"), ".zip")
fameriflux <- list.dirs(file.path(path.tower, "Ameriflux"), full.names=F)
FP_ls <- amf_variables()
summary(FP_ls)
FP_ls$Name
FP_ls[FP_ls$Name %in% c("TIMESTAMP", "LE", "SLE", "T_BOLE", "TA", "TS"),]

# Doing a test file
amerifluxUse <- data.frame(ameriflux.urb2[sapply(ameriflux.urb2$SITE_ID, FUN=function(x) {any(grepl(x, fameriflux))}) & ameriflux.urb2$DATA_START<2020,])
summary(amerifluxUse)

towerETlist <- list()

pb <- txtProgressBar(min=0, max=nrow(amerifluxUse), style=3)
for(i in 1:nrow(amerifluxUse)){
  setTxtProgressBar(pb, i)
  towerNOW <- amerifluxUse$SITE_ID[i]
  # tableROW <- which(amerifluxUse$SITE_ID==towerNOW)
  summerMO <- ifelse(amerifluxUse$LOCATION_LAT[i]<0, 1, 7)
  
  fnow <- fameriflux[grep(towerNOW, fameriflux)] # Find the directory name
  ftest <- dir(file.path(path.tower, "Ameriflux", fnow), ".csv") # Find the file name
  test <- amf_read_base(file=file.path(path.tower, "Ameriflux", fnow, ftest), unzip=F, parse_timestamp = T)
  summary(test)
  
  test <- test[test$YEAR>=2000 & test$YEAR<=2020 & test$MONTH %in% summerMO:(summerMO+1),]
  summary(test)
  names(test)
  summary(test$LE)
  
  if(!"LE" %in% names(test)) next
  if(length(which(!is.na(test$LE)))==0) next
  
  # If no variable TA, need to extract or calculate the value for the highest sensor
  # Multiple TAs are _H_V_R, with V=1 beig the highest veritcal position
  if(!"TA" %in% names(test)){
    colsTA <- names(test)[grep("TA_1_1_*", names(test))]
    if(length(colsTA)==1) { 
      test$TA <- test[,colsTA]
    } else if(length(colsTA)>1){
      test$TA <- apply(test[,colsTA], 1, mean, na.rm=T)
    } else next
  }
  
  # summary(test[!is.na(test$LE),])
  # length(which(test$LE<0))
  
  # Convert LE to ET: https://ameriflux.lbl.gov/sites/siteinfo/US-INb
  # LE is in W/m2; MODIS ET is in kg/m2/8day
  # According to stack exchange: https://earthscience.stackexchange.com/questions/20733/fluxnet15-how-to-convert-latent-heat-flux-to-actual-evapotranspiration
  # ET = LE/lambda; lambda = 2.501 - (2.361e-3)*Ta; 
  # Ta = air temp in deg. c; lambda in MJ/kg; x 10^6 to convert MJ to J
  # LE = latent heat flux in W/m2 = J/s/m2
  # lambda = latent heat of evaporation = ~2257 J/g (W = J/s)
  test$ET <- test$LE/((2.501 - 2.361e-3*test$TA)*10^6)*60*60*24 # ET In kg/m2/s convert to daily
  summary(test)

  # library(bigleaf)
  # bigET <- LE.to.ET(test$LE, test$TA)*60*60*24
  # summary(bigET)
  
  testDay <- aggregate(cbind(ET, TA) ~ YEAR + MONTH + DAY + DOY, data=test, FUN=mean)
  summary(testDay)
  
  testYr <- aggregate(cbind(ET, TA) ~ YEAR, data=testDay, FUN=mean)
  testYr
  
  testYr[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG")] <- amerifluxUse[i, c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "LOCATION_LAT", "LOCATION_LONG")]
  testYr <- testYr[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG", "YEAR", "TA", "ET")]
  testYr
  
  # Aggregating 
  towerETlist[[towerNOW]] <- testYr
}

towerET <- dplyr::bind_rows(towerETlist)
towerET$ISOURBID <- as.factor(towerET$ISOURBID)
towerET$ISO3 <- as.factor(towerET$ISO3)
towerET$SITE_ID <- as.factor(towerET$SITE_ID)
summary(towerET)

write.csv(towerET, file.path(path.tower, "AmerifluxSites_ET_summerMeans.csv"), row.names=F)

# Pulling International Data from places used in Ukkola et al 2021: https://essd.copernicus.org/articles/14/449/2022/
# OzFlux, La Thuile, Fluxnet 2015
# Pulling European Flux Databases: http://gaia.agraria.unitus.it/home


# # 1b. Reading in locations of Euroflux data <- This has been a NIGHTMARE to work with!
# # https://www.europe-fluxdata.eu/home/sites-list
eurofluxAll <- read.csv(file.path(path.tower, "EuroFlux - SitesList.csv"))
eurofluxAll <- eurofluxAll[!is.na(eurofluxAll$Site.Longitude) & eurofluxAll$Site.Longitude>-180,]
summary(eurofluxAll)
head(eurofluxAll)
# 
# # Look for just sites that have LE
eurofluxLE <- eurofluxAll[grepl("LE", eurofluxAll$Fluxes),]
summary(eurofluxLE)

euroflux.sp <- st_as_sf(eurofluxLE, coords=c("Site.Longitude", "Site.Latitude"))
st_crs(euroflux.sp) <- st_crs(sdei.urb)

euroflux.urb <- st_filter(euroflux.sp, sdei.urb)
summary(euroflux.urb) # 48 cities in our city footprints and CC-BY-4.0 license
data.frame(euroflux.urb[,c("Site.Code", "Site.Name", "Fluxes", "IGBP.Code")])

euroflux.urb2 <- st_join(euroflux.urb, sdei.urb[,c("ISOURBID", "ISO3", "NAME")], largest=F)
euroflux.urb2 <- merge(data.frame(euroflux.urb2), eurofluxAll, all.y=F)
summary(euroflux.urb2)
data.frame(euroflux.urb2)
length(unique(euroflux.urb2$ISOURBID)) # 32 unique cities

write.csv(data.frame(euroflux.urb2), file.path(path.tower, "EurofluxSites_Use.csv"), row.names=F)


feuroflux <- dir(file.path(path.tower, "Euroflux_Sites"), "EFDC_L2_Flx")
# Doing a test file
eurofluxUse <- data.frame(euroflux.urb2[sapply(euroflux.urb2$Site.Code, FUN=function(x) {any(grepl(gsub("-", "", x), feuroflux))}),])
summary(eurofluxUse) # 20 sites!
eurofluxUse$Site.Code

eurofluxETlist <- list()

pb <- txtProgressBar(min=0, max=nrow(eurofluxUse), style=3)
for(i in 1:nrow(eurofluxUse)){
  setTxtProgressBar(pb, i)
  towerNOW <- eurofluxUse$Site.Code[i]
  towerLookUp <- gsub("-", "", towerNOW)
  # tableROW <- which(eurofluxUse$SITE_ID==towerNOW)
  summerMO <- ifelse(eurofluxUse$Site.Latitude[i]<0, 1, 7)
  
  fnow <- feuroflux[grep(towerLookUp, feuroflux)] # Find the directory name
  testYr <- data.frame(YEAR=rep(NA, length(fnow)), ET=rep(NA, length(fnow)), TA=rep(NA, length(fnow)))
  for(j in seq_along(fnow)){
    test <- read.csv(file=file.path(path.tower, "Euroflux_Sites", fnow[j]))
    test[test==-9999] <- NA
    test$YEAR <- as.numeric(substr(test$TIMESTAMP_START, 1, 4))
    test$MONTH <- as.numeric(substr(test$TIMESTAMP_START, 5, 6))
    test$DAY <- as.numeric(substr(test$TIMESTAMP_START, 7, 8))
    test$DATE <- as.Date(paste(test$YEAR, test$MONTH, test$DAY, sep="-"))
    test$DOY <- lubridate::yday(test$DATE)
    
    if(!"LE" %in% names(test) | all(is.na(test$LE)) |
       !"TA" %in% names(test) | all(is.na(test$TA))){
      testYr$YEAR[j] <- test$YEAR[1]
      next
    }

    # test$YEAR <- 
    summary(test)
    
    test <- test[test$YEAR>=2000 & test$YEAR<=2020 & test$MONTH %in% summerMO:(summerMO+1),]
    if(nrow(test)==0 | all(is.na(test$LE)) | all(is.na(test$TA))){
      testYr$YEAR[j] <- test$YEAR[1]
      next
    }
    
    
    summary(test)
    names(test)
    

    # Convert LE to ET: https://ameriflux.lbl.gov/sites/siteinfo/US-INb
    # LE is in W/m2; MODIS ET is in kg/m2/8day
    # According to stack exchange: https://earthscience.stackexchange.com/questions/20733/euroflux15-how-to-convert-latent-heat-flux-to-actual-evapotranspiration
    # ET = LE/lambda; lambda = 2.501 - (2.361e-3)*Ta; Ta = air temp in deg. c; 
    # lambda in MJ/kg; x 10^6 to convert MJ to J
    # LE = latent heat flux in W/m2 = J/s/m2
    # lambda = latent heat of evaporation = ~2257 J/g (W = J/s)
    # library(bigleaf)
    # bigET <- LE.to.ET(test$LE, test$TA)*60*60*24
    # summary(bigET)
    test$ET <- test$LE/((2.501 - 2.361e-3*test$TA)*10^6)*60*60*24 # ET In kg/m2/s convert to daily
    summary(test)
    
    testDay <- aggregate(cbind(ET, TA) ~ YEAR + MONTH + DAY + DOY, data=test, FUN=mean)
    summary(testDay)
    
    testYr[j,] <- aggregate(cbind(ET, TA) ~ YEAR, data=testDay, FUN=mean)
    
  }
  testYr

  
  testYr[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG")] <- eurofluxUse[i, c("ISOURBID", "ISO3", "NAME", "Site.Code", "IGBP.Code", "Site.Latitude", "Site.Longitude")]
  testYr <- testYr[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG", "YEAR", "TA", "ET")]
  testYr
  
  # Aggregating 
  eurofluxETlist[[towerNOW]] <- testYr
}

eurofluxET <- dplyr::bind_rows(eurofluxETlist)
eurofluxET$ISOURBID <- as.factor(eurofluxET$ISOURBID)
eurofluxET$ISO3 <- as.factor(eurofluxET$ISO3)
eurofluxET$SITE_ID <- as.factor(eurofluxET$SITE_ID)
summary(eurofluxET)

write.csv(eurofluxET, file.path(path.tower, "EurofluxSites_ET_summerMeans.csv"), row.names=F)






# 1c. Reading in locations of FLUXNET 2015 data
fluxnet <- googlesheets4::read_sheet(ss="1urdK0oxAWOnEI5pdAmsaQRdTaqbaYxnLL0Ouh4vYxiU")
fluxnet <- fluxnet[!is.na(fluxnet$FLUXNET2015) & !fluxnet$SITE_ID %in% ameriflux.urb2$SITE_ID,]
summary(fluxnet)
fluxnet.sp <- st_as_sf(fluxnet, coords=c("LOCATION_LONG", "LOCATION_LAT"))
st_crs(fluxnet.sp) <- st_crs(sdei.urb)

fluxnet.urb <- st_filter(fluxnet.sp, sdei.urb)
summary(fluxnet.urb)
data.frame(fluxnet.urb)
# summary(fluxnet.urb[!is.na(fluxnet.urb$FLUXNET2015),]) # 12 sites in addition to the Ameriflux cities

fluxnet.yrs <- read.csv(file.path(path.tower, "FLUXENT_Site-Years_2014.csv"))
fluxnet.yrs <- fluxnet.yrs[fluxnet.yrs$Year.Site.ID %in% fluxnet.urb$SITE_ID,]
fluxnet.yrs # Looks like all 20 have at least some data for valdiation

fluxnet.urb2 <- st_join(fluxnet.urb, sdei.urb[,c("ISOURBID", "ISO3", "NAME")], largest=F)
fluxnet.urb2 <- merge(data.frame(fluxnet.urb2), fluxnet, all.y=F)
summary(fluxnet.urb2)
data.frame(fluxnet.urb2)
length(unique(fluxnet.urb2$ISOURBID)) # 11 unique cities

write.csv(data.frame(fluxnet.urb2), file.path(path.tower, "FluxnetSites_Use.csv"), row.names=F)


fFluxnet <- list.dirs(file.path(path.tower, "Fluxnet2015 - Urban Sites"), full.names=F)
# Doing a test file
fluxnetUse <- data.frame(fluxnet.urb2[sapply(fluxnet.urb2$SITE_ID, FUN=function(x) {any(grepl(x, fFluxnet))}),])
summary(fluxnetUse)

fluxnetETlist <- list()

pb <- txtProgressBar(min=0, max=nrow(fluxnet.urb2), style=3)
for(i in 1:nrow(fluxnetUse)){
  setTxtProgressBar(pb, i)
  towerNOW <- fluxnetUse$SITE_ID[i]
  # tableROW <- which(fluxnetUse$SITE_ID==towerNOW)
  summerMO <- ifelse(fluxnetUse$LOCATION_LAT[i]<0, 1, 7)
  
  fnow <- fFluxnet[grep(towerNOW, fFluxnet)] # Find the directory name
  ftest <- dir(file.path(path.tower, "Fluxnet2015 - Urban Sites", fnow), "DD") 
  test <- read.csv(file=file.path(path.tower, "Fluxnet2015 - Urban Sites", fnow, ftest))
  test$YEAR <- as.numeric(substr(test$TIMESTAMP, 1, 4))
  test$MONTH <- as.numeric(substr(test$TIMESTAMP, 5, 6))
  test$DAY <- as.numeric(substr(test$TIMESTAMP, 7, 8))
  test$DATE <- as.Date(paste(test$YEAR, test$MONTH, test$DAY, sep="-"))
  test$DOY <- lubridate::yday(test$DATE)
  # test$YEAR <- 
  summary(test)
  
  test <- test[test$YEAR>=2000 & test$YEAR<=2020 & test$MONTH %in% summerMO:(summerMO+1),]
  summary(test)
  names(test)
  
  # Renaming things for my sanity
  # https://fluxnet.org/data/fluxnet2015-dataset/subset-data-product/
  test$TA <- test$TA_F
  test$LE <- test$LE_F_MDS # gapfilled LE


  # Convert LE to ET: https://ameriflux.lbl.gov/sites/siteinfo/US-INb
  # LE is in W/m2; MODIS ET is in kg/m2/8day
  # According to stack exchange: https://earthscience.stackexchange.com/questions/20733/fluxnet15-how-to-convert-latent-heat-flux-to-actual-evapotranspiration
  # ET = LE/lambda; lambda = 2.501 - (2.361e-3)*Ta; Ta = air temp in deg. c; 
  # lambda in MJ/kg; x 10^6 to convert MJ to J
  # LE = latent heat flux in W/m2 = J/s/m2
  # lambda = latent heat of evaporation = ~2257 J/g (W = J/s)
  # library(bigleaf)
  # bigET <- LE.to.ET(test$LE, test$TA)*60*60*24
  # summary(bigET)
  test$ET <- test$LE/((2.501 - 2.361e-3*test$TA)*10^6)*60*60*24 # ET In kg/m2/s convert to daily
  summary(test)
  
  testDay <- aggregate(cbind(ET, TA) ~ YEAR + MONTH + DAY + DOY, data=test, FUN=mean)
  summary(testDay)
  
  testYr <- aggregate(cbind(ET, TA) ~ YEAR, data=testDay, FUN=mean)
  testYr
  
  testYr[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG")] <- fluxnetUse[i, c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "LOCATION_LAT", "LOCATION_LONG")]
  testYr <- testYr[,c("ISOURBID", "ISO3", "NAME", "SITE_ID", "IGBP", "TOWER_LAT", "TOWER_LONG", "YEAR", "TA", "ET")]
  testYr
  
  # Aggregating 
  fluxnetETlist[[towerNOW]] <- testYr
}

fluxnetET <- dplyr::bind_rows(fluxnetETlist)
fluxnetET$ISOURBID <- as.factor(fluxnetET$ISOURBID)
fluxnetET$ISO3 <- as.factor(fluxnetET$ISO3)
fluxnetET$SITE_ID <- as.factor(fluxnetET$SITE_ID)
summary(fluxnetET)

write.csv(fluxnetET, file.path(path.tower, "FluxnetSites_ET_summerMeans.csv"), row.names=F)
