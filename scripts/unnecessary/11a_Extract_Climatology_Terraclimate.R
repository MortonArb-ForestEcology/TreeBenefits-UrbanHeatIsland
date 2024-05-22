# Exploring pulling some climatology data for each city using Terraclimate
# https://www.climatologylab.org/terraclimate.html
# https://www.nature.com/articles/sdata2017191

# TheyWe'll be most interested in the climatologies for 1991-2012 and +2, +4 future scenarios
# http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html
# https://www.climatologylab.org/terraclimate-variables.html
# Vars we'll be interested in: max temp (tmax), min temp (tmin), precip accum (ppt)
# Climate Water Deficit (def); Ref/pot. ET. (pet), act ET (aet), soil moisture (soil)
# NOTE: We only want to pull from Jul/Aug for Northern Hemisphere; Jan/Feb for Southern Hemisphere

library(raster); library(sp); library(terra); library(sf) 
library(ncdf4)
library(ggplot2)
# library(mgcv)

overwrite=F

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4/data_processed_final")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityClim <- file.path(path.cities, "../city_climatology_terraclimate.csv")


sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)


if(!file.exists(file.cityClim) | overwrite){
  # cityClim <- read.csv("../sdei-global-uhi-2013.csv")
  cityClim <- data.frame(ISOURBID = rep(sdei.urb$ISOURBID, each=3),
                         LATITUDE = rep(sdei.urb$LATITUDE, each=3),
                         LONGITUDE = rep(sdei.urb$LONGITUDE, each=3),
                         TIME = c("current", "+2C", "+4C"),
                         tmin = NA, tmax = NA, ppt = NA,
                         aet = NA, pet = NA, def=NA, soil=NA
                         )

  
  summary(cityClim)
  dim(cityClim)
  
  write.csv(cityClim, file.cityClim, row.names=F)  

}

# Read in Summary file -----
cityClim <- read.csv(file.cityClim)
summary(cityClim); dim(cityClim)


# Because of how netcdf extractiosn work, we only want to open a file once, even if we extract a lot of cities from it
varsAll <- c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")


for(VAR in varsAll){
  print(VAR)
  urlNow <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/summaries/TerraClimate19912020_", VAR, ".nc")
  urlFut2 <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/summaries/TerraClimate2C_", VAR, ".nc")
  urlFut4 <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/summaries/TerraClimate4C_", VAR, ".nc")
  ncNow <- nc_open(urlNow)
  ncFut2 <- nc_open(urlFut2)
  ncFut4 <- nc_open(urlFut4)
  
  lon <- ncvar_get(ncNow, "lon")
  lat <- ncvar_get(ncNow, "lat")
  # ncTmax$var$tmax
  
  pb <- txtProgressBar(min=0, max=nrow(sdei.urb), style=3)
  for(i in 1:nrow(sdei.urb)){
    setTxtProgressBar(pb, i)
    
    # i=1
    # plot(sdei.urb[1,])
    URBID <- sdei.urb$ISOURBID[i]
    moInd <- ifelse(sdei.urb$LATITUDE[i]<0, 1, 7) # The month things start on
    
    bbCity <- st_bbox(sdei.urb[i,])
  
    lat.index <- which(lat>=bbCity$ymin & lat<=bbCity$ymax)    #! index values within specified range
    lon.index <- which(lon>=bbCity$xmin & lon<=bbCity$xmax)    
    lat.n <- length(lat.index)                                #!value for count
    lon.n <- length(lon.index)
    start <- c(lon.index[1], lat.index[1], moInd)
    count <- c(lon.n, lat.n, 2)                            #! parameter change: 'NA' instead of '-1' to signify entire dimension
    
    rowNow <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="current")
    rowFut2 <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="+2C")
    rowFut4 <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="+4C")
    cityClim[rowNow, VAR] <- mean(ncvar_get(ncNow, varid = VAR, start = start, count), na.rm=T)
    cityClim[rowFut2, VAR] <- mean(ncvar_get(ncFut2, varid = VAR, start = start, count), na.rm=T)
    cityClim[rowFut4, VAR] <- mean(ncvar_get(ncFut4, varid = VAR, start = start, count), na.rm=T)
  } # end city loop
  
  
  nc_close(ncNow); nc_close(ncFut2); nc_close(ncFut4)
} # End Var loop


write.csv(cityClim, file.cityClim, row.names=F)


# Just doing some quick exploratory graphs.  We'll 
cityClim <- read.csv(file.cityClim)
cityClim$TIME <- as.factor(cityClim$TIME)
summary(cityClim)

summary(cityClim[cityClim$ppt==0,])
summary(cityClim[cityClim$soil==0,])
cityClim[cityClim$soil==0,]
summary(cityClim[cityClim$aet==0,])

ggplot(data=cityClim) +
  facet_grid(TIME~.) +
  geom_histogram(aes(x=def))


diffClim <- data.frame(ISOURBID=unique(cityClim$ISOURBID),
                       tmax.Fut2 = NA,
                       tmax.Fut4 = NA,
                       ppt.Fut2 = NA,
                       ppt.Fut4 = NA,
                       pet.Fut2 = NA,
                       pet.Fut4 = NA,
                       def.Fut2 = NA,
                       def.Fut4 = NA)
for(URBID in unique(cityClim$ISOURBID)){
  rowRef <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="current")
  rowFut2 <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="+2C")
  rowFut4 <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="+4C")
  
  rowDiff <- which(diffClim$ISOURBID==URBID)
  diffClim$tmax.Fut2[rowDiff] <-  cityClim$tmax[rowFut2] - cityClim$tmax[rowRef]
  diffClim$tmax.Fut4[rowDiff] <- cityClim$tmax[rowFut4] - cityClim$tmax[rowRef]
  diffClim$ppt.Fut2[rowDiff] <- cityClim$ppt[rowFut2] - cityClim$ppt[rowRef]
  diffClim$ppt.Fut4[rowDiff] <- cityClim$ppt[rowFut4] - cityClim$ppt[rowRef]
  diffClim$pet.Fut2[rowDiff] <- cityClim$pet[rowFut2] - cityClim$pet[rowRef]
  diffClim$pet.Fut4[rowDiff] <- cityClim$pet[rowFut4] - cityClim$pet[rowRef]
  diffClim$def.Fut2[rowDiff] <- cityClim$def[rowFut2] - cityClim$def[rowRef]
  diffClim$def.Fut4[rowDiff] <- cityClim$def[rowFut4] - cityClim$def[rowRef]
}

summary(diffClim)

ggplot(data=diffClim) +
  # facet_grid(TIME~.) +
  geom_histogram(aes(x=ppt.Fut4))

ggplot(data=diffClim) +
  # facet_grid(TIME~.) +
  geom_histogram(aes(x=tmax.Fut2))

