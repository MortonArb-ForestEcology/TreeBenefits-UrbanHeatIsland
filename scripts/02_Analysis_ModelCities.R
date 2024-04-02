library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=F

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3/data_processed_final")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsRegion <- file.path(path.cities, "../city_stats_all v301.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v3")

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

# ecoregions <- read_sf("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
ecoregions <- read_sf("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
ecoregions$biome.name <- car::recode(ecoregions$BIOME, "'1'='tropical moist broadleaf forest'; 
                                                             '2'='tropical dry broadleaf forest'; 
                                                             '3'='tropical coniferous forest';
                                                             '4'='temperate broadleaf/mixed forest'; 
                                                             '5'='temperate coniferous forest'; 
                                                             '6'='boreal forest/taiga'; 
                                                             '7'='tropical grassland/savannas'; 
                                                             '8'='temperate grassland/savanna'; 
                                                             '9'='flooded grassland/savanna'; 
                                                            '10'='montane grassland/savanna'; 
                                                            '11'='tundra'; 
                                                            '12'='mediterranean'; 
                                                            '13'='desert/xeric shrublands'; 
                                                            '14'='mangroves'")
summary(ecoregions)

# Transform the cities and ecoregion files to what we've worked with for the MODIS data
# NOTE: This *shouldn't* be necessary anymore, but there's a weird duplicate vertex issue that causes problems; doing the spTransform seems to help
projMODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
sdei.urb <- st_transform(sdei.urb, crs(projMODIS))
summary(sdei.urb)
ecoregions <- st_transform(ecoregions, crs(projMODIS))
summary(ecoregions)

# If we don't have our summary file yet, create it and create all the column names we're going to want
if(!file.exists(file.cityStatsRegion) | overwrite){
  # cityStatsRegion <- read.csv("../sdei-global-uhi-2013.csv")
  cols.keep <-  c("ISOURBID", "ISO3", "URBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP")
  cityStatsRegion <- data.frame(sdei.urb[, cols.keep])[,1:length(cols.keep)]
  # head(cityStatsRegion)
  
  # ------------------
  # Some summary stats about the inputs at the region scale 
  # ------------------
  cityStatsRegion[,"SpatialMistmatch"] <- NA
  # - number of pixels, mean LST, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
  cityStatsRegion[,c("biome", "biome.prop", "n.pixels", "LST.mean", "LST.sd", "LST.min", "LST.max", "tree.mean", "tree.sd", "tree.min", "tree.max", "veg.mean", "veg.sd", "veg.min", "veg.max", "elev.mean", "elev.sd", "elev.min", "elev.max", "ET.mean", "ET.sd", "ET.min", "ET.max")] <- NA
  
  # Save the key info from the full model
  cityStatsRegion[,c("LST.NoVeg.model.R2adj", "LST.NoVeg.model.AIC")] <- NA
  cityStatsRegion[,c("LSTmodel.R2adj", "LSTmodel.AIC", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "LSTmodel.elev.slope", "LSTmodel.tree.p", "LSTmodel.veg.p", "LSTmodel.elev.p")] <- NA
  cityStatsRegion[,c("ETmodel.R2adj", "ETmodel.AIC", "ETmodel.tree.slope", "ETmodel.veg.slope", "ETmodel.elev.slope", "ETmodel.tree.p", "ETmodel.veg.p", "ETmodel.elev.p")] <- NA
  
  # I had run this, but removed it because we don't have ET everywhere we need/want it
  # cityStatsRegion[,c("LST.ET.model.R2adj", "LST.ET.model.df", "LST.ET.model.AIC")] <- NA
  
  # Adding some year-based trend that isn't generated by the pixel stats --> this is super necessary for SD as a metric of equity
  cityStatsRegion[,c("trendYear.LST.mean.slope", "trendYear.LST.mean.p")] <- NA
  cityStatsRegion[,c("trendYear.LST.sd.slope", "trendYear.LST.sd.p")] <- NA
  cityStatsRegion[,c("trendYear.tree.mean.slope", "trendYear.tree.mean.p")] <- NA
  cityStatsRegion[,c("trendYear.tree.sd.slope", "trendYear.tree.sd.p")] <- NA
  cityStatsRegion[,c("trendYear.veg.mean.slope", "trendYear.veg.mean.p")] <- NA
  cityStatsRegion[,c("trendYear.veg.sd.slope", "trendYear.veg.sd.p")] <- NA

    
  # For each variable calculate the overall trends --> this will need to be separate from the pixel-by-pixel analysis, although we'll do and save that as well
  cityStatsRegion[,c("trend.LST.slope", "trend.LST.slope.sd", "trend.LST.p")] <- NA
  cityStatsRegion[,c("trend.tree.slope", "trend.tree.slope.sd", "trend.tree.p")] <- NA
  cityStatsRegion[,c("trend.veg.slope", "trend.veg.slope.sd", "trend.veg.p")] <- NA
  cityStatsRegion[,c("trend.ET.slope", "trend.ET.slope.sd", "trend.ET.p")] <- NA
  
  # Also look at the correlation between warming and change in tree & veg cover
  cityStatsRegion[,c("corr.LST.tree.slope", "corr.LST.tree.p", "corr.LST.tree.Rsq")] <- NA
  cityStatsRegion[,c("corr.LST.veg.slope", "corr.LST.veg.p", "corr.LST.veg.Rsq")] <- NA
  cityStatsRegion[,c("corr.tree.veg.slope", "corr.tree.veg.p", "corr.tree.veg.Rsq")] <- NA
  
  summary(cityStatsRegion)
  dim(cityStatsRegion)
  
  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  
  # ------------------
 
  

}

# Read in Summary file -----
cityStatsRegion <- read.csv(file.cityStatsRegion)
summary(cityStatsRegion); dim(cityStatsRegion)

# Get a list of the files that are done
# # Note: Some cities (2-3) seems to have >1 file, which is weird.  Can do a spot check or just roll with the last file like I think I have coded in
files.elev <- dir(path.EEout, "elevation")
files.lst <- dir(path.EEout, "LST_Day_Tmean")
files.et <- dir(path.EEout, "ETmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.mask <- dir(path.EEout, "CityMask")
length(files.elev); length(files.lst); length(files.et); length(files.tree); length(files.veg); length(files.mask)
# Note: We're missing 3 cities for ET data

# Figure out which cities have all the layers needed to be analyzed
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.et <- unlist(lapply(files.et, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.mask <- unlist(lapply(files.mask, FUN=function(x){strsplit(x, "_")[[1]][1]}))


# citiesDone <- unique(cities.lst)

citiesDone <- unique(cities.lst[cities.lst %in% cities.et & cities.lst %in% cities.elev & cities.lst %in% cities.tree & cities.lst %in% cities.veg & cities.lst %in% cities.mask])
length(citiesDone)

# Now compare the done list to what needs to be analyzed
citiesAnalyze <- citiesDone[citiesDone %in% cityStatsRegion$ISOURBID[is.na(cityStatsRegion$LSTmodel.R2adj)]]
length(citiesAnalyze)

# # Start City Loop -----
for(CITY in citiesAnalyze){
  # # Good Test Cities: Sydney (AUS66430)
  # CITY="AUS66430"; CITY="USA26687"
  row.city <- which(cityStatsRegion$ISOURBID==CITY)
  print(CITY)
  citySP <- sdei.urb[sdei.urb$ISOURBID==CITY, ]
  cityBuff <- st_buffer(citySP, dist=10e3)
  # plot(cityBuff[1], add=T); 
  # plot(citySP[1], add=F)
  biome <- st_intersection(ecoregions[,c("BIOME", "biome.name")], cityBuff[,"ISOURBID"])
  summary(biome)
  # plot(ecoregions)
  
  # # data.frame(biome)
  if(nrow(biome)>0){ # Some aren't quite aligning-- we'll figure those out later
    if(nrow(biome)==1){
      cityStatsRegion$biome[row.city] <- biome$biome.name
      cityStatsRegion$biome.prop[row.city] <- 1
    } else {
      biome$area <- st_area(biome)
      biome.sum <- aggregate(area ~ biome.name, data=biome, FUN=sum, na.rm=T)

      if(nrow(biome.sum)>1){
        rowBiome <- which(biome.sum$area==max(biome.sum$area, na.rm=T))
        cityStatsRegion$biome[row.city] <- biome.sum$biome.name[rowBiome]
        cityStatsRegion$biome.prop[row.city] <- biome.sum$area[rowBiome]/sum(biome.sum$area, na.rm=T)
        
      } else {
        cityStatsRegion$biome[row.city] <- biome.sum$biome.name
        cityStatsRegion$biome.prop[row.city] <- 1

      }

      rm(biome.sum)
    } # End if/else
  } # End skipping biomes that don't exist
  
  
  # length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fMASK <- files.mask[grep(CITY, files.mask)]
  fELEV <- files.elev[grep(CITY, files.elev)]
  fLST <- files.lst[grep(CITY, files.lst)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  fVEG <- files.veg[grep(CITY, files.veg)]
  fET <- files.et[grep(CITY, files.et)]
  
  # The length statements will grab the newest file if there's more than one
  maskCity <- raster(file.path(path.EEout, fMASK[length(fMASK)]))
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))
  
  # par(mfrow=c(1,2))
  # plot(elevCity); plot(maskCity)
  # par(mfrow=c(1,1))
  # plot(treeCity)
  # plot(lstCity)
  # plot(etCity)
  
  # lst.mean <- mean(lstCity)
  # tree.mean <- mean(treeCity)
  # veg.mean <- mean(vegCity)
  # par(mfrow=c(2,2))
  # plot(elevCity); plot(lst.mean); plot(tree.mean); plot(veg.mean)
  
  # Elevation should be our most reliable data layer, so lets use that as our base
  coordsCity <- data.frame(coordinates(elevCity)) 
  coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
  coordsCity$elevation <- getValues(elevCity)
  coordsCity$cityBounds <- getValues(maskCity)
  coordsCity$cityBounds <- !is.na(coordsCity$cityBounds) # NA = buffer = FALSE citybounds
  
  # Double Checking the mask 
  coordsMask <- data.frame(coordinates(maskCity))
  coordsMask$location <- paste0("x", coordsMask$x, "y", coordsMask$y)
  
  # In case we're missing some years of LST (likely in the tropics); only pull certain layers
  layers.use <- names(treeCity)[names(treeCity) %in% names(lstCity)]
  
  coordsVeg <- data.frame(coordinates(treeCity))
  coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)
  
  
  # Land Surface Temperature 
  coordsLST <- data.frame(coordinates(lstCity))
  coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)
  
  # Adding ET --> note: This will just be a subset of years!
  layersET <- names(etCity)[names(etCity) %in% names(lstCity)]
  
  coordsET <- data.frame(coordinates(etCity))
  coordsET$location <- paste0("x", coordsET$x, "y", coordsET$y)
  
  # Checkign to see whcih coords match
  # if(!all(coordsCity$location == coordsMask$location)) { stop("Mask and elev coords don't match") } # Elev = mask --> NO!
  # if(!all(coordsVeg$location == coordsCity$location)){ stop("Veg and elev coords don't match")} # Veg = elev --> NO! # Elevation is the bad one!
  # if(!all(coordsVeg$location == coordsMask$location)){ stop("Veg and Mask coords don't match")} # Veg = mask --> YES
  # if(!all(coordsVeg$location == coordsLST$location)){ stop("Veg and LST coords don't match")} # Veg = LST --> YES
  # if(!all(coordsVeg$location == coordsET$location)){ stop("Veg and ET coords don't match")}  #  Veg = ET  --> YES
  # 
  valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
  names(valsCityVeg) <- c("cover.tree", "year")
  valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
  valsCityVeg$x <- coordsVeg$x
  valsCityVeg$y <- coordsVeg$y
  valsCityVeg$location <- coordsVeg$location
  
  # nrow(coordsCity); nrow(coordsVeg)

  # summary(coordsVeg$location == coordsCity$location)
  # summary(coordsVeg[coordsVeg$location==coordsCity$location,])
  # summary(coordsVeg[coordsVeg$location!=coordsCity$location,])
  if(all(coordsVeg$location == coordsCity$location)){
    cityStatsRegion$SpatialMistmatch[row.city] <- F # No problem, we're good
    
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
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      
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
  
  
  valsLST <- stack(data.frame(getValues(lstCity[[layers.use]])))
  names(valsLST) <- c("LST_Day", "year")
  valsLST$x <- coordsLST$x
  valsLST$y <- coordsLST$y
  valsLST$location <- coordsLST$location
  summary(valsLST)
  
  # locLSTAll <- unique(valsLST$location[!is.na(valsLST$LST_Day)])
  
  # nrow(coordsCity); nrow(coordsLST)
  if(all(coordsLST$location == coordsCity$location)){
    valsCity$LST_Day <- valsLST$LST_Day
    # valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
  } else if( nrow(coordsLST) == nrow (coordsCity)) {  
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsCity$x, y1=coordsCity$y, x2 = valsLST$x, y2 = coordsCity$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      valsCity$LST_Day <- valsLST$LST_Day
      
    }  else {
      stop("Something's really off")
    }
  } else if( any(coordsLST$location %in% valsCity$location)) {  
    valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
  } else {
    print(warning("LST coords do not match elev.  Need to re-implment nearest neighbor"))
  }

  
  valsET <- stack(data.frame(getValues(etCity[[layersET]])))
  names(valsET) <- c("ET", "year")
  valsET$x <- coordsET$x
  valsET$y <- coordsET$y
  valsET$location <- coordsET$location
  summary(valsET)
  
  # IMPORTANT: CONVERT ET from 8-day total to per day (mm/8-day to mm/day; 1 mm = 1 kg/m2)
  valsET$ET <- valsET$ET/8
  # locLSTAll <- unique(valsLST$location[!is.na(valsLST$LST_Day)])
  
  # nrow(coordsCity); nrow(coordsLST)
  if(all(coordsET$location == coordsCity$location)){
    valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
    # valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
  } else if( nrow(coordsET) == nrow (coordsCity)) {  
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsCity$x, y1=coordsCity$y, x2 = valsET$x, y2 = valsET$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
      
    }  else {
      stop("Something's really off")
    }
    
    # print(warning("Using a merge to get things together.  Slow, but it should work"))
  } else if( any(coordsET$location %in% valsCity$location)) {
    print(warning("Using a merge to get things together.  Slow, but it should work"))
    valsCity <- merge(valsCity, valsET, all.x=T, all.y=F)
  } else {
    print(warning("LST coords do not match elev.  Need to re-implment nearest neighbor"))
  }
  
  summary(valsCity)
  
  # Doing some conversion etc
  valsCity$year <- as.numeric(substr(valsCity$year, 3, 6))
  valsCity <- valsCity[!is.na(valsCity$elevation) & !is.na(valsCity$cover.tree),]
  summary(valsCity)
  
  if(length(unique(valsCity$location[!is.na(valsCity$LST_Day)]))<50){
    print(warning("LST Spatial mismatch too big; skip city"))
    print("") # Just give a clean return before moving on
    cityStatsRegion$model.R2adj[row.city] <- -9999
    next
  }
  
  
  
  # Don't bother creating a folder for a city until we'll have at least something to save!
  dir.create(file.path(path.cities, CITY), recursive = T, showWarnings = F)
  
  # # Save the full output in case we want to share it for review or other uses
  # write.csv(valsCity, file.path(path.cities, CITY, paste0(CITY, "_CityData_All.csv")), row.names=F)
  
  # Saving some summary stats of our inputs -- I know there's a more elegant way to do this, but hey, this works
  # cityStatsRegion[row.city,]
  cityStatsRegion$n.pixels[row.city] <- length(unique(valsCity$location))
  cityStatsRegion$LST.mean[row.city] <- mean(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$LST.sd[row.city] <- sd(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$LST.min[row.city] <- min(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$LST.max[row.city] <- max(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$tree.mean[row.city] <- mean(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$tree.sd[row.city] <- sd(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$tree.min[row.city] <- min(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$tree.max[row.city] <- max(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$veg.mean[row.city] <- mean(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$veg.sd[row.city] <- sd(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$veg.min[row.city] <- min(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$veg.max[row.city] <- max(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$elev.mean[row.city] <- mean(valsCity$elevation, na.rm=T)
  cityStatsRegion$elev.sd[row.city] <- sd(valsCity$elevation, na.rm=T)
  cityStatsRegion$elev.min[row.city] <- min(valsCity$elevation, na.rm=T)
  cityStatsRegion$elev.max[row.city] <- max(valsCity$elevation, na.rm=T)
  
  # Saving ET params values, but bear in mind they're only a subset of the area!
  cityStatsRegion$ET.mean[row.city] <- mean(valsCity$ET, na.rm=T)
  cityStatsRegion$ET.sd[row.city] <- sd(valsCity$ET, na.rm=T)
  cityStatsRegion$ET.min[row.city] <- min(valsCity$ET, na.rm=T)
  cityStatsRegion$ET.max[row.city] <- max(valsCity$ET, na.rm=T)
  # cityStatsRegion[row.city,]
  

  # Running a null LST model to better get a sense for how much is actually explained by the veg & not just spatial stuff
  modLSTnull <- gam(LST_Day ~ elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modLSTnull <- summary(modLSTnull)
  
  cityStatsRegion$LST.NoVeg.model.R2adj[row.city] <- sum.modLSTnull$r.sq
  cityStatsRegion$LST.NoVeg.model.AIC[row.city] <- AIC(modLSTnull)
  
  valsCity$LST.NoVeg.gam.pred <- predict(modLSTnull, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LST.NoVeg.gam.resid <- valsCity$LST_Day - valsCity$LST.NoVeg.gam.pred # Hand-calculating te residuals... gives the same thing
  save(modLSTnull, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST-NoVegNull_gam.RData")))
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST-NoVegNull_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(modLSTnull)
  hist(valsCity$LST.NoVeg.gam.resid)
  plot(LST.NoVeg.gam.resid ~ LST.NoVeg.gam.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LST.NoVeg.gam.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # plot(modLSTnull)
  
  # Running the LST actual model! Woot Woot
  modLSTCity <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modLSTCity <- summary(modLSTCity)
  valsCity$LSTgam.pred <- predict(modLSTCity, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LSTgam.resid <- valsCity$LST_Day - valsCity$LSTgam.pred # Hand-calculating te residuals... gives the same thing
  save(modLSTCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam.RData")))
  # par(mfrow=c(1,1)); plot(modLSTCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(modLSTCity)
  hist(valsCity$LSTgam.resid)
  plot(LSTgam.resid ~ LSTgam.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LSTgam.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big LST model
  cityStatsRegion$LSTmodel.R2adj[row.city] <- sum.modLSTCity$r.sq
  cityStatsRegion$LSTmodel.AIC[row.city] <- AIC(modLSTCity)
  cityStatsRegion[row.city,c("LSTmodel.tree.slope", "LSTmodel.veg.slope", "LSTmodel.elev.slope")] <- sum.modLSTCity$p.coeff[c("cover.tree", "cover.veg", "elevation")]
  cityStatsRegion[row.city,c("LSTmodel.tree.p", "LSTmodel.veg.p", "LSTmodel.elev.p")] <- sum.modLSTCity$p.pv[c("cover.tree", "cover.veg", "elevation")]
  # cityStatsRegion[row.city,]
  
  

  # ------------
  
  # Adding a quick analysis of how things change through time
  # For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
  summaryYear <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation) ~ year + cityBounds, data=valsCity, FUN=mean)
  names(summaryYear)[names(summaryYear) %in% c("LST_Day", "cover.tree", "cover.veg")] <- c("LST.mean", "tree.mean", "veg.mean")
  summaryYear[,c("LST.sd", "tree.sd", "veg.sd")] <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation) ~ year + cityBounds, data=valsCity, FUN=sd)[,c("LST_Day", "cover.tree", "cover.veg")]
  summary(summaryYear)
  
  # plot(LST.mean ~ year, data=summaryYear)

  # LST
  trendYearLSTMean <-   lm(LST.mean ~ year, data=summaryYear)
  trendYearLSTSD <-   lm(LST.sd ~ year, data=summaryYear)
  cityStatsRegion[row.city, c("trendYear.LST.mean.slope", "trendYear.LST.mean.p")] <- summary(trendYearLSTMean)$coefficients["year", c("Estimate", "Pr(>|t|)")]
  cityStatsRegion[row.city, c("trendYear.LST.sd.slope", "trendYear.LST.sd.p")] <- summary(trendYearLSTSD)$coefficients["year", c("Estimate", "Pr(>|t|)")]
  
  
  # Trees
  trendYearTreeMean <-   lm(tree.mean ~ year, data=summaryYear)
  trendYearTreeSD <-   lm(tree.sd ~ year, data=summaryYear)
  cityStatsRegion[row.city, c("trendYear.tree.mean.slope", "trendYear.tree.mean.p")] <- summary(trendYearTreeMean)$coefficients["year", c("Estimate", "Pr(>|t|)")]
  cityStatsRegion[row.city, c("trendYear.tree.sd.slope", "trendYear.tree.sd.p")] <- summary(trendYearTreeSD)$coefficients["year", c("Estimate", "Pr(>|t|)")]
  

  trendYearVegMean <-   lm(veg.mean ~ year, data=summaryYear)
  trendYearVegSD <-   lm(veg.sd ~ year, data=summaryYear)
  cityStatsRegion[row.city, c("trendYear.veg.mean.slope", "trendYear.veg.mean.p")] <- summary(trendYearVegMean)$coefficients["year", c("Estimate", "Pr(>|t|)")]
  cityStatsRegion[row.city, c("trendYear.veg.sd.slope", "trendYear.veg.sd.p")] <- summary(trendYearVegSD)$coefficients["year", c("Estimate", "Pr(>|t|)")]
  
  # Calculating pixel-based summary stats to do some trend correlations
  # For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
  summaryCity <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation) ~ x+y+location + cityBounds, data=valsCity, FUN=mean)
  names(summaryCity)[names(summaryCity) %in% c("LST_Day", "cover.tree", "cover.veg")] <- c("LST.mean", "tree.mean", "veg.mean")
  summary(summaryCity)


  summaryCity[,c("LST.trend", "LST.p", "LST.R2")] <- NA
  summaryCity[,c("tree.trend", "tree.p", "tree.R2")] <- NA
  summaryCity[,c("veg.trend", "veg.p", "veg.R2")] <- NA
  
  pb.lms <- txtProgressBar(min=0, max=nrow(summaryCity), style=3)
  for(i in 1:nrow(summaryCity)){
  	rowsCity <- which(valsCity$location==summaryCity$location[i])
  	setTxtProgressBar(pb.lms, i)
  	
  	# Skip any analysis if there's less than 10 years of data or our trend doesn't go to the last 5 years of our record
  	if(length(rowsCity)<10 | max(valsCity$year[rowsCity])<=2015) next
  	
  	# The LST Data is going to be noisier, so we'll want to skip over anything without robust data
  	lstGood <- which(valsCity$location==summaryCity$location[i] & !is.na(valsCity$LST_Day))
  	if(length(lstGood)>10 & max(valsCity$year[lstGood])>2015){
    	trend.LST <- lm(LST_Day ~ year, data=valsCity[rowsCity,])
    	sum.LST <- summary(trend.LST)
    	summaryCity[i,c("LST.trend", "LST.p")] <- sum.LST$coefficients["year",c(1,4)]
    	summaryCity[i,"LST.R2"] <- sum.LST $r.squared
  	}
  	
  	trend.tree <- lm(cover.tree ~ year, data=valsCity[rowsCity,])
  	sum.tree <- summary(trend.tree)
  	summaryCity[i,c("tree.trend", "tree.p")] <- sum.tree$coefficients["year",c(1,4)]
  	summaryCity[i,"tree.R2"] <- sum.tree$r.squared
  
  	trend.veg <- lm(cover.veg ~ year, data=valsCity[rowsCity,])
  	sum.veg <- summary(trend.veg)
  	summaryCity[i,c("veg.trend", "veg.p")] <- sum.veg$coefficients["year",c(1,4)]
  	summaryCity[i,"veg.R2"] <- sum.veg$r.squared
  	
  }
  summary(summaryCity)
  write.csv(summaryCity, file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")), row.names=F)
  
  # rm(trend.LST, trend.tree, trend.veg, sum.LST, sum.tree, sum.veg)
  
  # Making some plots with the summary data
  # proj.elev <- projection(elevCity)
  # sp.city2 <- terra::project(x=sp.city, proj.elev)
  # test <- sf::st_as_sf(sp.city)
  # sp.city3 <- st_transform(test, proj.elev)
  plot.lst <- ggplot(data=summaryCity[!is.na(summaryCity$LST.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=LST.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_sf(data=sp.city3, fill=NA) +
    scale_fill_gradientn(name="Summer\nTemp\n(deg. C)", colors=grad.temp) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.elev <- ggplot(data=summaryCity[!is.na(summaryCity$elevation),]) +
    coord_equal() +
    # geom_tile(aes(x=x2, y=y2, fill=temp.summer)) +
    geom_tile(aes(x=x, y=y, fill=elevation)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Elevation\n(m)", colors=grad.elev) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.tree <- ggplot(data=summaryCity[!is.na(summaryCity$tree.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=tree.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
     # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Tree\nCover\n(%)", colors=grad.tree, limits=c(0,100)) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.veg <- ggplot(data=summaryCity[!is.na(summaryCity$veg.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=veg.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Other Veg\nCover (%)", colors=grad.other, limits=c(0,100)) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.et <- ggplot(data=summaryCity[!is.na(summaryCity$ET.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=ET.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="ET (kg/m2)", colors=grad.et) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  
  png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Means.png")), height=10, width=8, units="in", res=120)
  print(
    cowplot::plot_grid(plot.lst, plot.et, plot.tree, plot.veg, plot.elev, ncol=2)
  )
  dev.off()  
  
  if(length(which(!is.na(summaryCity$LST.trend)))>50){
    # cityStatsRegion[row.city,]
    # Calculate the stats for the trends in LST and veg cover
    cityStatsRegion$trend.LST.slope[row.city] <- mean(summaryCity$LST.trend, na.rm=T)
    cityStatsRegion$trend.LST.slope.sd[row.city] <- sd(summaryCity$LST.trend, na.rm=T)
    LST.out <- t.test(summaryCity$LST.trend)
    cityStatsRegion$trend.LST.p[row.city] <- LST.out$p.value
    
    cityStatsRegion$trend.tree.slope[row.city] <- mean(summaryCity$tree.trend, na.rm=T)
    cityStatsRegion$trend.tree.slope.sd[row.city] <- sd(summaryCity$tree.trend, na.rm=T)
    tree.out <- t.test(summaryCity$tree.trend)
    cityStatsRegion$trend.tree.p[row.city] <- tree.out$p.value
    
    cityStatsRegion$trend.veg.slope[row.city] <- mean(summaryCity$veg.trend, na.rm=T)
    cityStatsRegion$trend.veg.slope.sd[row.city] <- sd(summaryCity$veg.trend, na.rm=T)
    veg.out <- t.test(summaryCity$veg.trend)
    cityStatsRegion$trend.veg.p[row.city] <- veg.out$p.value

    if("ET.trend" %in% names(summaryCity) & length(which(!is.na(summaryCity$ET.trend)))>5){
      cityStatsRegion$trend.ET.slope[row.city] <- mean(summaryCity$ET.trend, na.rm=T)
      cityStatsRegion$trend.ET.slope.sd[row.city] <- sd(summaryCity$ET.trend, na.rm=T)
      et.out <- t.test(summaryCity$ET.trend)
      cityStatsRegion$trend.ET.p[row.city] <- et.out$p.value
    }  
    
    # Creating and saving some maps of those trends
    lstlim <- max(abs(summaryCity$LST.trend), na.rm=T)
    plot.lst.trend <- ggplot(data=summaryCity[!is.na(summaryCity$LST.trend),]) +
      coord_equal() +
      geom_tile(aes(x=x, y=y, fill=LST.trend)) +
      geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
      # geom_sf(data=sp.city3, fill=NA) +
      scale_fill_gradientn(name="Summer\nTemp\n(deg. C/yr)", colors=grad.temp, limits=c(-lstlim, lstlim)) +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_blank(),
            axis.text=element_blank())
    
    treelim <- max(abs(summaryCity$tree.trend), na.rm=T)
    plot.tree.trend <- ggplot(data=summaryCity[!is.na(summaryCity$tree.trend),]) +
      coord_equal() +
      geom_tile(aes(x=x, y=y, fill=tree.trend)) +
      geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
      # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
      scale_fill_gradientn(name="Tree\nCover\n(%/yr)", colors=grad.tree, limits=c(-treelim, treelim)) +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_blank(),
            axis.text=element_blank())
    
    veglim <- max(abs(summaryCity$veg.trend), na.rm=T)
    plot.veg.trend <- ggplot(data=summaryCity[!is.na(summaryCity$veg.trend),]) +
      coord_equal() +
      geom_tile(aes(x=x, y=y, fill=veg.trend)) +
      geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
      # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
      scale_fill_gradientn(name="Other Veg\nCover (%/yr)", colors=grad.other, limits=c(-veglim, veglim)) +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_blank(),
            axis.text=element_blank())
    
    if("ET.trend" %in% names(summaryCity)){
      etlim <- max(abs(summaryCity$ET.trend), na.rm=T)
      plot.et.trend <- ggplot(data=summaryCity[!is.na(summaryCity$ET.trend),]) +
        coord_equal() +
        geom_tile(aes(x=x, y=y, fill=ET.trend)) +
        geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
        # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
        scale_fill_gradientn(name="ET (kg/m2/yr)", colors=grad.et, limits=c(-etlim, etlim)) +
        theme(panel.background=element_rect(fill=NA, color="black"),
              panel.grid=element_blank(),
              axis.ticks.length = unit(-0.5, "lines"),
              axis.title=element_blank(),
              axis.text=element_blank())
    } else {
      plot.et.trend <- NULL
    }
    
    png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Trends.png")), height=8, width=8, units="in", res=120)
    print(
      cowplot::plot_grid(plot.lst.trend, plot.et.trend, plot.tree.trend, plot.veg.trend)
    )
    dev.off()  
    
    
    # cityStatsRegion[row.city,]
    # Now calculating the correlations among variables
    tree.lst <- lm(LST.trend ~ tree.trend, data=summaryCity)
    sum.corrTreeLST <- summary(tree.lst)
    cityStatsRegion$corr.LST.tree.slope[row.city] <- sum.corrTreeLST$coefficients["tree.trend",1]
    cityStatsRegion$corr.LST.tree.p[row.city] <- sum.corrTreeLST$coefficients["tree.trend",4]
    cityStatsRegion$corr.LST.tree.Rsq[row.city]  <- sum.corrTreeLST$r.squared
    
    veg.lst <- lm(LST.trend ~ veg.trend, data=summaryCity)
    sum.corrVegLST <- summary(veg.lst)
    cityStatsRegion$corr.LST.veg.slope[row.city] <- sum.corrVegLST$coefficients["veg.trend",1]
    cityStatsRegion$corr.LST.veg.p[row.city] <- sum.corrVegLST$coefficients["veg.trend",4]
    cityStatsRegion$corr.LST.veg.Rsq[row.city]  <- sum.corrVegLST$r.squared
  
    veg.tree <- lm(tree.trend ~ veg.trend, data=summaryCity)
    sum.corrVegTree <- summary(veg.tree)
    cityStatsRegion$corr.tree.veg.slope[row.city] <- sum.corrVegTree$coefficients["veg.trend",1]
    cityStatsRegion$corr.tree.veg.p[row.city] <- sum.corrVegTree$coefficients["veg.trend",4]
    cityStatsRegion$corr.tree.veg.Rsq[row.city]  <- sum.corrVegTree$r.squared
    
    plot.corr.LST.Tree <- ggplot(data=summaryCity, aes(x=tree.trend, y=LST.trend)) +
      geom_point() +
      stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
      labs(x="Tree Trend (%/yr)", y="LST Trend (deg. C/yr)") +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_text(face="bold"),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
    
    plot.corr.LST.Veg <- ggplot(data=summaryCity, aes(x=veg.trend, y=LST.trend)) +
      geom_point() +
      stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
      labs(x="Other Veg Trend (%/yr)", y="LST Trend (deg. C/yr)") +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_text(face="bold"),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
    plot.corr.Tree.Veg <- ggplot(data=summaryCity, aes(x=veg.trend, y=tree.trend)) +
      geom_point() +
      stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
      labs(x="Other Veg Trend (%/yr)", y="Tree Trend (%/yr)") +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_text(face="bold"),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
    png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Correlations_Trends.png")), height=8, width=8, units="in", res=120)
    print(
      cowplot::plot_grid(plot.corr.LST.Tree, plot.corr.LST.Veg, NULL, plot.corr.Tree.Veg)
    )
    dev.off()  
  }
  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  # Write our city stats file each time in case it bonks

  print("") # Just give a clean return before moving on
  
  # Remove a bunch of stuff for our own sanity
  # rm(elevCity, treeCity, vegCity, lstCity, modLSTCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.LST.Tree, plot.corr.LST.Veg, plot.corr.Tree.Veg, plot.lst.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.lst, plot.tree, plot.veg, veg.lst, veg.tree, tree.lst, veg.out, tree.out, sum.corrTreeLST, sum.corrVegLST, sum.corrVegTree, sum.modLSTCity)
  
}	

