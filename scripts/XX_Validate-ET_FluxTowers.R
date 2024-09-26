# Validate ET models using available Flux Tower data
# -- figure out what Ameriflux of Fluxnet towers are in the footprint of our analyzed cities
# -- extract the mean summer ET estimate for the pixel of the flux tower


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
ameriflux.sp <- st_as_sf(amerifluxCC, coords=c("LOCATION_LONG", "LOCATION_LAT"))
st_crs(ameriflux.sp) <- st_crs(sdei.urb)

ameriflux.urb <- st_filter(ameriflux.sp, sdei.urb)
summary(ameriflux.urb) # 28 cities with our temporal window in our city footprint and CC-BY-4.0 license
data.frame(ameriflux.urb[,c("SITE_ID", "SITE_NAME", "COUNTRY", "STATE", "IGBP")])

# data.frame(ameriflux.urb)
# summary(ameriflux.urb[!is.na(ameriflux.urb$ameriflux2015),]) # 20 sites in our 2k cities

# Pulling International Data from places used in Ukkola et al 2021: https://essd.copernicus.org/articles/14/449/2022/
# OzFlux, La Thuile, Fluxnet 2015
# Pulling European Flux Databases: http://gaia.agraria.unitus.it/home


# 1a. Reading in locations of FLUXNET 2015 data
fluxnet <- googlesheets4::read_sheet(ss="1urdK0oxAWOnEI5pdAmsaQRdTaqbaYxnLL0Ouh4vYxiU")
summary(fluxnet)
fluxnet.sp <- st_as_sf(fluxnet, coords=c("LOCATION_LONG", "LOCATION_LAT"))
st_crs(fluxnet.sp) <- st_crs(sdei.urb)

fluxnet.urb <- st_filter(fluxnet.sp, sdei.urb)
summary(fluxnet.urb)
data.frame(fluxnet.urb)
summary(fluxnet.urb[!is.na(fluxnet.urb$FLUXNET2015),]) # 15 sites in our 2k cities
summary(fluxnet.urb[!is.na(fluxnet.urb$FLUXNET2015) & !grepl("US-", fluxnet.urb$SITE_ID),]) # 12 international cities
data.frame(fluxnet.urb[!is.na(fluxnet.urb$FLUXNET2015) & !grepl("US-", fluxnet.urb$SITE_ID),])

fluxnet.yrs <- read.csv(file.path(path.tower, "FLUXENT_Site-Years_2014.csv"))
fluxnet.yrs <- fluxnet.yrs[fluxnet.yrs$Year.Site.ID %in% fluxnet.urb$SITE_ID,]
fluxnet.yrs # Looks like all 20 have at least some data for valdiation

