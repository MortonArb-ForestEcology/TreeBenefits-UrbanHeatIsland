# After having done our preliminary ET model exploration, lets go through the full data for all cities.  I'll probably save the top 1-2

library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=T

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/ET_models")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsET <- file.path(path.cities, "../city_stats_all_ET.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")

# Some color palettes for later
grad.temp <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
grad.elev <- c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4")
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange

grad.et <- rev(c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101")) # Ends with orange


# Lets add the ecoregion for each city; accessed 27 Oct 2022 9:30 a.m.
# SDEI shapefile: https://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013/data-download# # NOTE: REQUIRES LOGIN
# ecoregion file: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)
# plot(sdei.urb[1,])



# If we don't have our summary file yet, create it and create all the column names we're going to want
if(!file.exists(file.cityStatsET) | overwrite){
  # cityStatsET <- read.csv("../sdei-global-uhi-2013.csv")
  cols.keep <-  c("ISOURBID", "ISO3", "URBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP")
  cityStatsET <- data.frame(sdei.urb[, cols.keep])[,1:length(cols.keep)]
  # head(cityStatsET)
  
  # ------------------
  # Some summary stats about the inputs at the region scale 
  # ------------------
  cityStatsET[,"SpatialMistmatch"] <- NA
  # - number of pixels, mean Temp, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
  cityStatsET[,c("n.pixels", "n.pixels.ET", "Temp.ET.mean", "Temp.ET.sd", "Temp.ET.min", "Temp.ET.max", "tree.ET.mean", "tree.ET.sd", "tree.ET.min", "tree.ET.max", "veg.ET.mean", "veg.ET.sd", "veg.ET.min", "veg.ET.max", "ETobs.mean", "ETobs.sd", "ETobs.min", "ETobs.max")] <- NA
  
  # Save the key info from the full model
  cityStatsET[,c("ETmodel.R2adj", "ETmodel.RMSE")] <- NA
  cityStatsET[,c("ETpred.mean", "ETpred.sd", "ETpred.min", "ETpred.max")] <- NA
  

  summary(cityStatsET)
  dim(cityStatsET)
  
  write.csv(cityStatsET, file.cityStatsET, row.names=F)  
  # ------------------
}

# Read in Summary file -----
cityStatsET <- read.csv(file.cityStatsET)
summary(cityStatsET); dim(cityStatsET)

# Get a list of the files that are done
# # Note: Some cities (2-3) seems to have >1 file, which is weird.  Can do a spot check or just roll with the last file like I think I have coded in
files.elev <- dir(path.EEout, "elevation")
files.temp <- dir(path.EEout, "GLDAS21_annualMeans")
files.et <- dir(path.EEout, "ETmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.mask <- dir(path.EEout, "CityMask")
length(files.temp); length(files.et); length(files.tree); length(files.veg); length(files.elev); length(files.mask)
# Note: We're missing 3 cities for ET data

# Figure out which cities have all the layers needed to be analyzed
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.temp <- unlist(lapply(files.temp, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.et <- unlist(lapply(files.et, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.mask <- unlist(lapply(files.mask, FUN=function(x){strsplit(x, "_")[[1]][1]}))


# citiesDone <- unique(cities.temp)

citiesDone <- unique(cities.temp[cities.temp %in% cities.et & cities.temp %in% cities.elev & cities.temp %in% cities.tree & cities.temp %in% cities.veg & cities.temp %in% cities.mask])
length(citiesDone)

# Now compare the done list to what needs to be analyzed
citiesAnalyze <- citiesDone[citiesDone %in% cityStatsET$ISOURBID[is.na(cityStatsET$ETmodel.R2adj)]]
length(citiesAnalyze)

# # Start City Loop -----
for(CITY in citiesAnalyze){
  # # Good Test Cities: Sydney (AUS66430)
  # CITY="AUS66430"; CITY="USA26687"
  # # Cities with poor R2: USA34146; IND55394; IND44003
  # CITY="USA34146"
  # CITY="EGY44702" # Cairo

    
  row.city <- which(cityStatsET$ISOURBID==CITY)
  print(CITY)
  # citySP <- sdei.urb[sdei.urb$ISOURBID==CITY, ]
  # cityBuff <- st_buffer(citySP, dist=10e3)
  
  # length(files.elev); length(files.temp); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fMASK <- files.mask[grep(CITY, files.mask)]
  fELEV <- files.elev[grep(CITY, files.elev)]
  fTemp <- files.temp[grep(CITY, files.temp)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  fVEG <- files.veg[grep(CITY, files.veg)]
  fET <- files.et[grep(CITY, files.et)]
  
  TempCity <- read.csv(file.path(path.EEout, fTemp[length(fTemp)]))
  TempCity$Tair_f_inst_mean <- TempCity$Tair_f_inst_mean-273.15
  TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")] <- TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")]*60*60*24
  
  if(all(is.na(TempCity$Tair_f_inst_mean))){
    print(warning("no actual GLDAS data... skip this location"))
    next
  }
  
  # TempCity
  # plot(Evap_tavg_mean ~ Tair_f_inst_mean, data=TempCity)
  # abline(lm(Evap_tavg_mean ~ Tair_f_inst_mean, data=TempCity), col="red")
  
  # The length statements will grab the newest file if there's more than one
  maskCity <- raster(file.path(path.EEout, fMASK[length(fMASK)]))
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))/8
  
  # Calculating some additional
  
  
  # par(mfrow=c(1,2))
  # plot(elevCity); plot(maskCity)
  # par(mfrow=c(1,1))
  # plot(treeCity)
  # plot(TempCity)
  # plot(etCity)
  
  # Temp.mean <- mean(TempCity)
  # tree.mean <- mean(treeCity)
  # veg.mean <- mean(vegCity)
  # par(mfrow=c(2,2))
  # plot(elevCity); plot(Temp.mean); plot(tree.mean); plot(veg.mean)
  
  # Elevation should be our most reliable data layer, so lets use that as our base
  coordsCity <- data.frame(coordinates(elevCity)) 
  coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
  coordsCity$elevation <- getValues(elevCity)
  coordsCity$cityBounds <- getValues(maskCity)
  coordsCity$cityBounds <- !is.na(coordsCity$cityBounds) # NA = buffer = FALSE citybounds
  
  # Double Checking the mask 
  coordsMask <- data.frame(coordinates(maskCity))
  coordsMask$location <- paste0("x", coordsMask$x, "y", coordsMask$y)
  
  # In case we're missing some years of Temp (likely in the tropics); only pull certain layers
  layers.use <- names(treeCity)[names(treeCity) %in% names(etCity)]
  
  coordsVeg <- data.frame(coordinates(treeCity))
  coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)
  
  
  
  # Adding ET --> note: This will just be a subset of years!
  layersET <- names(etCity)[names(etCity) %in% names(treeCity)]
  
  coordsET <- data.frame(coordinates(etCity))
  coordsET$location <- paste0("x", coordsET$x, "y", coordsET$y)
  
  valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
  names(valsCityVeg) <- c("cover.tree", "year")
  valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
  valsCityVeg$x <- coordsVeg$x
  valsCityVeg$y <- coordsVeg$y
  valsCityVeg$location <- coordsVeg$location
  
  if(all(coordsVeg$location == coordsCity$location)){
    cityStatsET$SpatialMistmatch[row.city] <- F # No problem, we're good
    
    valsCity <- valsCityVeg[,]
    valsCity$elevation <- coordsCity$elevation
    valsCity$cityBounds <- coordsCity$cityBounds
    # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
    
  } else if(nrow(coordsVeg)==nrow(coordsCity)) {
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsVeg$x, y1=coordsVeg$y, x2 = coordsCity$x, y2 = coordsCity$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      
      valsCity <- valsCityVeg[,]
      valsCity$elevation <- coordsCity$elevation
      valsCity$cityBounds <- coordsCity$cityBounds
    }  else {
      stop("Something's really off")
    }
    
  } else if( any(coordsVeg$location %in% coordsCity$location)) {  
    print(warning("Veg and Elev Layer Coords don't match, but at least some do"))
    valsCity <- valsCityVeg[,]
    
    valsCity <- merge(valsCity, coordsCity, all.x=T, all.y=F)
  } else {
    print(warning("Veg and Elev Layer doesn't match. :-( skipping for now to see how prevalent that is"))
    next
  }
  

  valsET <- stack(data.frame(getValues(etCity[[layersET]])))
  names(valsET) <- c("ET", "year")
  valsET$x <- coordsET$x
  valsET$y <- coordsET$y
  valsET$location <- coordsET$location
  summary(valsET)
  
  # nrow(coordsCity); nrow(coordsTemp)
  if(all(coordsET$location == coordsCity$location)){
    valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
    # valsCity <- merge(valsCity, valsTemp, all.x=T, all.y=T)
  } else if( nrow(coordsET) == nrow (coordsCity)) {  
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsCity$x, y1=coordsCity$y, x2 = valsET$x, y2 = valsET$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
      
    }  else {
      stop("Something's really off")
    }
    
    # print(warning("Using a merge to get things together.  Slow, but it should work"))
  } else if( any(coordsET$location %in% valsCity$location)) {
    print(warning("Using a merge to get things together.  Slow, but it should work"))
    valsCity <- merge(valsCity, valsET, all.x=T, all.y=F)
  } else {
    print(warning("Temp coords do not match elev.  Need to re-implment nearest neighbor"))
  }
  
  summary(valsCity)

  
  # Doing some conversion etc
  valsCity$year <- as.numeric(substr(valsCity$year, 3, 6))
  
  yrsCityET <- unique(valsCity$year[!is.na(valsCity$ET)])
  if(length(yrsCityET)<5){
    # # (Note all cities before NGA56716 alphabetically need to be checked for this criteria
    print(warning("ET data available for fewer than 5 years, Need to skip it.") )
    cityStatsET$ETmodel.R2adj[row.city] <- -9999
     
    next 
  }
  
  # yrsCityET <- unique(valsCity$year[!is.na(valsCity$ET)])
  if(length(unique(TempCity$year[!is.na(TempCity$Tair_f_inst_mean) & TempCity$year %in% yrsCityET]))<5){
    # # (Note all cities before NGA56716 alphabetically need to be checked for this criteria
    print(warning("Insufficient overlap between GLDAS and ET data. Need to skip it.") )
    cityStatsET$ETmodel.R2adj[row.city] <- -9999
    
    next 
  }
  
  
  valsCity <- valsCity[!is.na(valsCity$elevation) & !is.na(valsCity$cover.tree) & valsCity$year %in% yrsCityET,] # NOTE: getting rid of years >2014
  summary(valsCity)
  
  if(length(unique(valsCity$location[!is.na(valsCity$ET)]))<50){
    print(warning("Not enough ET pixels to robustly model; skip city"))
    print("") # Just give a clean return before moving on
    cityStatsET$ETmodel.R2adj[row.city] <- -9999
    next
  }
  
  if(max(valsCity$ET, na.rm=T)<0.1){
    print(warning("ET too low to model; skip city"))
    print("") # Just give a clean return before moving on
    cityStatsET$ETmodel.R2adj[row.city] <- -9999
    next
  }
  
  
  # If we made it this far, now merge in the regional stats from GLDAS
  valsCity <- merge(valsCity, TempCity, all.x=T, all.y=F)
  # summary(valsCity)
  
  # valsCityAnn <- aggregate(cbind(cover.tree, cover.veg, ET, Evap_tavg_mean, Tair_f_inst_mean) ~ year, data=valsCity, FUN=mean)
  
  # plot(Tair_f_inst_mean ~ year, data=valsCityAnn)
  # abline(lm(Tair_f_inst_mean ~ year, data=valsCityAnn), col="red")
  # plot(cover.tree ~ year, data=valsCityAnn)
  # abline(lm(cover.tree ~ year, data=valsCityAnn), col="red")

  # plot(Evap_tavg_mean ~ Tair_f_inst_mean, data=valsCityAnn)
  # abline(lm(Evap_tavg_mean ~ Tair_f_inst_mean, data=valsCityAnn), col="red")

  # plot(ET ~ Tair_f_inst_mean, data=valsCityAnn)
  # abline(lm(ET ~ Tair_f_inst_mean, data=valsCityAnn), col="red")
  
  
  # Don't bother creating a folder for a city until we'll have at least something to save!
  dir.create(file.path(path.cities, CITY), recursive = T, showWarnings = F)
  
  # # Save the full output in case we want to share it for review or other uses
  # write.csv(valsCity, file.path(path.cities, CITY, paste0(CITY, "_CityData_All.csv")), row.names=F)
  
  # Saving some summary stats of our inputs -- I know there's a more elegant way to do this, but hey, this works
  # cityStatsET[row.city,]
  # NOTE: In contrast to the big analy script, these are the ranges used to model ET
  cityStatsET$n.pixels[row.city] <- length(unique(valsCity$location))
  cityStatsET$n.pixels.ET[row.city] <- length(unique(valsCity$location[!is.na(valsCity$ET)]))
  cityStatsET$Temp.ET.mean[row.city] <- mean(valsCity$Tair_f_inst_mean[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$Temp.ET.sd[row.city] <- sd(valsCity$Tair_f_inst_mean[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$Temp.ET.min[row.city] <- min(valsCity$Tair_f_inst_mean[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$Temp.ET.max[row.city] <- max(valsCity$Tair_f_inst_mean[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.mean[row.city] <- mean(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.sd[row.city] <- sd(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.min[row.city] <- min(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.max[row.city] <- max(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.mean[row.city] <- mean(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.sd[row.city] <- sd(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.min[row.city] <- min(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.max[row.city] <- max(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)

  # Saving ET params values, but bear in mind they're only a subset of the area!
  cityStatsET$ETobs.mean[row.city] <- mean(valsCity$ET, na.rm=T)
  cityStatsET$ETobs.sd[row.city] <- sd(valsCity$ET, na.rm=T)
  cityStatsET$ETobs.min[row.city] <- min(valsCity$ET, na.rm=T)
  cityStatsET$ETobs.max[row.city] <- max(valsCity$ET, na.rm=T)
  # cityStatsET[row.city,]
  
  
  # having the year factor means we can't predict ET without knowing a regional mean --> 
  # modETCity <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + s(Tair_f_inst_mean, k=3) + s(x,y) + as.factor(year)-1, data=valsCity)
  # modETCity1 <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + s(Tair_f_inst_mean, k=3) + s(x,y), data=valsCity)
  # modETCity2 <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + Tair_f_inst_mean + s(x,y), data=valsCity)
  # modETCity3 <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + Tair_f_inst_mean + s(x,y) + as.factor(year)-1, data=valsCity)
  # # sum.modETCity$p.coeff
  # # sum.modETCity
  # AIC(modETCity, modETCity1, modETCity2, modETCity3)
  # summary(modETCity)
  # summary(modETCity1)
  # summary(modETCity2)
  # summary(modETCity3)
  # 
  # par(mfrow=c(2,2))
  # plot(modETCity)
  # par(mfrow=c(1,1))
  # 
  # par(mfrow=c(2,2))
  # plot(modETCity1)
  # par(mfrow=c(1,1))
  # 
  # par(mfrow=c(2,2))
  # plot(modETCity2)
  # par(mfrow=c(1,1))
  # 
  # par(mfrow=c(2,2))
  # plot(modETCity3)
  # par(mfrow=c(1,1))
  
  # modETCity0 <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + s(Tair_f_inst_mean) + s(x,y), data=valsCity)
  # sum.modETCity0 <- summary(modETCity0)
  # sum.modETCity0
  # par(mfrow=c(2,2))
  # plot(modETCity0)
  # par(mfrow=c(1,1))
  
  
  modETCity <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + Tair_f_inst_mean + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modETCity <- summary(modETCity)
  # sum.modETCity
  # par(mfrow=c(2,2))
  # plot(modETCity)
  # par(mfrow=c(1,1))
  
  
  # Also testing things on the aggregated values; lets save this just in case
  aggCity <- aggregate(cbind(ET, cover.tree, cover.veg, Tair_f_inst_mean) ~ x + y, data=valsCity, FUN=mean, na.rm=T)
  
  # No variation in tair, so need to remove it from the model
  modETCityAgg <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg)  + s(x,y), data=aggCity)
  sum.modETCityAgg <- summary(modETCityAgg)
  # sum.modETCityAgg
  # par(mfrow=c(2,2))
  # plot(modETCityAgg)
  # par(mfrow=c(1,1))
  
  
  
  valsCity$ET.pred <- predict(modETCity, newdata=valsCity)^2 # Shifting to the newdata version to predict for where we have missing data
  valsCity$ET.resid <- valsCity$ET - valsCity$ET.pred # Hand-calculating te residuals... gives the same thing, but hopefully a bit faster
  
  # Saving predicted ET params values, this shoudl generally be lower than our "observed"
  # cityStatsET[,c("ETpred.mean", "ETpred.sd", "ETpred.min", "ETpred.max")] <- NA
  cityStatsET$ETpred.mean[row.city] <- mean(valsCity$ET.pred, na.rm=T)
  cityStatsET$ETpred.sd[row.city] <- sd(valsCity$ET.pred, na.rm=T)
  cityStatsET$ETpred.min[row.city] <- min(valsCity$ET.pred, na.rm=T)
  cityStatsET$ETpred.max[row.city] <- max(valsCity$ET.pred, na.rm=T)
  
  
  # Save the key stats from the big Temp model
  cityStatsET$ETmodel.R2adj[row.city] <- sum.modETCity$r.sq
  cityStatsET$ETmodel.RMSE[row.city] <- sqrt(mean(valsCity$ET.resid^2, na.rm=T))
  # cityStatsET[row.city,]
  
  saveRDS(modETCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
  saveRDS(sum.modETCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model-ET_annual_gam-summary.rds")))
  
  # Save the aggreagte model as well just in case
  write.csv(aggCity, file=file.path(path.cities, CITY, paste0(CITY, "_ET_means.csv")), row.names=F)
  saveRDS(modETCityAgg, file=file.path(path.cities, CITY, paste0(CITY, "_Model-ET_means_gam.rds")))
  saveRDS(sum.modETCityAgg, file=file.path(path.cities, CITY, paste0(CITY, "_Model-ET_means_gam-summary.rds")))
  
  # par(mfrow=c(1,1)); plot(modETCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "ET_GAM_qaqc.png")), height=6, width=9, units="in", res=120)
  par(mfrow=c(2,4))
  plot(modETCity)
  hist(valsCity$ET.resid)
  plot(ET.resid ~ ET.pred, data=valsCity); abline(h=0, col="red")
  plot(ET ~ ET.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  # plot(ET.resid ~ Tair_f_inst_mean, data=valsCity); abline(h=0, col="red")
  # plot(ET.resid ~ cover.tree, data=valsCity); abline(h=0, col="red")
  # plot(ET.resid ~ cover.veg, data=valsCity); abline(h=0, col="red")
  # 
  # plot(ET.pred ~ Tair_f_inst_mean, data=valsCity)
  # plot(ET.pred ~ cover.tree, data=valsCity)
  # plot(ET.pred ~ cover.veg, data=valsCity)
  # plot(ET ~ Tair_f_inst_mean, data=valsCity)
  # plot(ET ~ cover.tree, data=valsCity)
  # plot(ET ~ cover.veg, data=valsCity)
  
  # cityStatsET[row.city,]
  
  
  write.csv(cityStatsET, file.cityStatsET, row.names=F)  # Write our city stats file each time in case it bonks
  
  print("") # Just give a clean return before moving on
  
  # Remove a bunch of stuff for our own sanity
  # rm(elevCity, treeCity, vegCity, TempCity, modETCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.temp.Tree, plot.corr.temp.Veg, plot.corr.Tree.Veg, plot.temp.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.temp, plot.tree, plot.veg, veg.temp, veg.tree, tree.temp, veg.out, tree.out, sum.corrTreeTemp, sum.corrVegTemp, sum.corrVegTree, sum.modETCity)
  
}	

