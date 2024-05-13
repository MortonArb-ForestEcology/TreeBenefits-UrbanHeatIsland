# Testing and exploring ET models a bit more thoroughly

# Test Cities
# ************************
# 1. Chicago: USA26687; 
# 2. Vancouver: CAN16375; 
# 3. Berlin: DEU10109; 
# 4. Atlanta: USA40447; 
# 5. Sydney: AUS66430; 
# 6. Santiago (Chile): CHL66311; 
# 7. Cairo (AlQahirah): EGY44702; 
# 8. Beijing: CHN31890; 
# 9. Johannesburg (South Africa): ZAF64524; 
# 10. Rio de Janeiro: BRA63739


library(ggplot2)
library(mgcv)

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4")
path.cities <- file.path(path.google, "data_processed_final")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")

# Path where we're saving the preliminary stuff
path.out <- file.path(path.google, "ETmodel_v4_Testing")
if(!dir.exists(path.out)) dir.create(path.out, recursive = T)


cityAll.stats <- read.csv(file.path(path.cities, "..", "city_stats_all.csv"))
summary(cityAll.stats)

cities.cherry <- c("USA26687", "CAN16375", "DEU10109", "USA40447", "AUS66430", "CHL66311", "EGY44702", "CHN31890", "ZAF64524", "BRA63739") # 10 cherry-picked cities
cities.all <- cityAll.stats$ISOURBID[!cityAll.stats$ISOURBID %in% cities.cherry & !is.na(cityAll.stats$ETmodel.R2adj)]
set.seed(525)
cities.random <- sample(cities.all, round(length(cities.all)*.01, 0), replace = F) # Shoudl be 27
cities.test <- c(cities.cherry, cities.random)
cities.test
# i=3


cityClim <- read.csv(file.path(path.google, "city_climatology.csv"))
cityClim$TIME <- as.factor(cityClim$TIME)
summary(cityClim)


# Get a list of the files that are done
# # Note: Some cities (2-3) seems to have >1 file, which is weird.  Can do a spot check or just roll with the last file like I think I have coded in
files.elev <- dir(path.EEout, "elevation")
files.lst <- dir(path.EEout, "LST_Day_Tmean")
files.et <- dir(path.EEout, "ETmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.mask <- dir(path.EEout, "CityMask")
length(files.lst); length(files.et); length(files.tree); length(files.veg); length(files.elev); length(files.mask)
# Note: We're missing 3 cities for ET data

# Figure out which cities have all the layers needed to be analyzed
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.et <- unlist(lapply(files.et, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.mask <- unlist(lapply(files.mask, FUN=function(x){strsplit(x, "_")[[1]][1]}))


# Fitting a series of models to ET to see how we can do
# mod1 <- gam(ET.mean ~ tree.mean + veg.mean + s(x,y), data=valsCity)
# mod2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=valsCity)
# mod3 <- gam(log(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=valsCity)
# mod4 <- gam(ET.mean ~ log(tree.mean) + log(veg.mean) + s(x,y), data=valsCity)
# mod5 <- gam(ET.mean ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=valsCity)
# mod6 <- gam(sqrt(ET.mean) ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=valsCity)
# mod7 <- gam(ET.mean ~  tree.mean*veg.mean + s(x,y), data=valsCity)
# mod8 <- gam(sqrt(ET.mean) ~ tree.mean*veg.mean + s(x,y), data=valsCity)
# lst1 <- gam(ET.mean ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
# lst2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
# lst3 <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y), data=dat.mod)
# lst3b <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
# lst4 <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y), data=dat.mod)
# lst4b <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
# lst5 <- gam(sqrt(ET.mean) ~ s(LST.mean, tree.mean) + s(LST.mean, veg.mean) + s(x,y), data=dat.mod) # # THis one is consistently best
# lst6 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(LST.mean) + s(x,y) , data=dat.mod)
# lst7 <- gam(sqrt(ET.mean) ~ s(tree.mean) + s(veg.mean) + s(LST.mean) + s(x,y), data=dat.mod) # This one is next best; this is easiest to model
# lst8 <- gam(sqrt(ET.mean) ~ s(tree.mean) + s(veg.mean) + LST.mean + s(x,y), data=dat.mod) # 8 hits lower, but does pretty damn well too



mod.names <- c(paste0("mod", 1:8), paste0("lst", 1:9), paste0("lst", 3:4, "b"))
metrics <- c("R2", "AIC", "RMSE")
mods.comp <- data.frame(ISOURBID = rep(cities.test, each=length(mod.names)), model=mod.names, R2=NA, AIC=NA, RMSE=NA)
mods.comp$model <- factor(mods.comp$model, levels=mod.names)
mods.comp$ISOURBID <- factor(mods.comp$ISOURBID)
# cityStats <- dtaa.frame(ISOURBID =, model=mod.names, R2=NA, AIC=NA, RMSE=NA)

for(i in 1:length(cities.test)){
  CITY <- cities.test[i]
  dir.create(file.path(path.out, CITY), recursive=T, showWarnings = F)
  print(CITY)
  
  row.city <- which(cityStatsET$ISOURBID==CITY)
  print(CITY)
  # citySP <- sdei.urb[sdei.urb$ISOURBID==CITY, ]
  # cityBuff <- st_buffer(citySP, dist=10e3)
  
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
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))/8
  
  # Calculating some additional
  
  
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
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
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
    print(warning("LST coords do not match elev.  Need to re-implment nearest neighbor"))
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
  # hist(valsCity$ET.mean)
  # hist(valsCity$tree.mean)
  # hist(valsCity$veg.mean)
  # hist(sqrt(valsCity$ET.mean))
  # hist(log(valsCity$ET.mean))
  
  map.et <- ggplot(data=valsCity ) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=ET.mean))
  map.tree <- ggplot(data=valsCity ) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=tree.mean))
  map.veg <- ggplot(data=valsCity ) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=veg.mean))
  
  hist.et <- ggplot(data=valsCity ) +
    geom_histogram(aes(x=ET.mean))
  hist.tree <- ggplot(data=valsCity ) +
    geom_histogram(aes(x=tree.mean))
  hist.veg <- ggplot(data=valsCity ) +
    geom_histogram(aes(x=veg.mean))
  
  plot.lst <- ggplot(data=valsCity, aes(x=LST.mean, y=ET.mean)) +
    geom_point() +
    stat_smooth(method="lm")
  
  plot.tree <- ggplot(data=valsCity, aes(x=tree.mean, y=ET.mean)) +
    geom_point() +
    stat_smooth(method="lm")
  
  plot.veg <- ggplot(data=valsCity, aes(x=veg.mean, y=ET.mean)) +
    geom_point() +
    stat_smooth(method="lm")
  
  png(file.path(path.out, CITY, paste0(CITY, "_rawGlance.png")), height=8, width=12, units="in", res=120)
  print(cowplot::plot_grid(map.et, map.tree, map.veg, hist.et, hist.tree, hist.veg, plot.lst, plot.tree, plot.veg, byrow = F, ncol=3))
  dev.off()
  
  # Creating a clean thing with no missing data
  dat.mod <- valsCity[!is.na(valsCity$ET.mean) & !is.na(valsCity$tree.mean),]
  summary(dat.mod)
  
  
  # base model
  mod1 <- gam(ET.mean ~ tree.mean + veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod1 <- as.vector(predict(mod1))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod1"] <- summary(mod1)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod1"] <- AIC(mod1)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod1"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod1)^2, na.rm=T))
  
  mod2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod2 <- as.vector(predict(mod2)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod2"] <- summary(mod2)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod2"] <- AIC(mod2)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod2"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod2)^2, na.rm=T))
  
  # mod3 <- gam(log(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.mod)
  # dat.mod$mod3 <- as.vector(exp(predict(mod3)))
  # mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod3"] <- summary(mod3)$r.sq
  # mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod3"] <- AIC(mod3)
  # mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod3"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod3)^2, na.rm=T))
  # 
  # mod4 <- gam(ET.mean ~ log(tree.mean) + log(veg.mean) + s(x,y), data=dat.mod[dat.mod$tree.mean>0 & dat.mod$veg.mean>0,])
  # dat.mod$mod4[dat.mod$tree.mean>0 & dat.mod$veg.mean>0] <- as.vector(predict(mod4))
  # mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod4"] <- summary(mod4)$r.sq
  # mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod4"] <- AIC(mod4)
  # mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod4"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod4)^2, na.rm=T))
  
  
  mod5 <- gam(ET.mean ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.mod)
  dat.mod$mod5 <- as.vector(predict(mod5))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod5"] <- summary(mod5)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod5"] <- AIC(mod5)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod5"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod5)^2, na.rm=T))
  
  mod6 <- gam(sqrt(ET.mean) ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.mod)
  dat.mod$mod6 <- as.vector(predict(mod6)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod6"] <- summary(mod6)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod6"] <- AIC(mod6)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod6"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod6)^2, na.rm=T))
  
  mod7 <- gam(ET.mean ~ tree.mean*veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod7 <- as.vector(predict(mod7))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod7"] <- summary(mod7)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod7"] <- AIC(mod7)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod7"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod7)^2, na.rm=T))
  
  
  mod8 <- gam(sqrt(ET.mean) ~ tree.mean*veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod8 <- as.vector(predict(mod8)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod8"] <- summary(mod8)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod8"] <- AIC(mod8)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod8"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod8)^2, na.rm=T))
  
  lst1 <- gam(ET.mean ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
  dat.mod$mod.lst1 <- as.vector(predict(lst1))
  # summary(lst1)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst1"] <- summary(lst1)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst1"] <- AIC(lst1)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst1"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst1)^2, na.rm=T))
  
  
  lst2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
  dat.mod$mod.lst2 <- as.vector(predict(lst2)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst2"] <- summary(lst2)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst2"] <- AIC(lst2)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst2"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst2)^2, na.rm=T))
  
  # summary(lst2)
  
  lst3 <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
  dat.mod$mod.lst3 <- as.vector(predict(lst3))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3"] <- summary(lst3)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3"] <- AIC(lst3)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst3)^2, na.rm=T))
  
  
  # lst3b <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
  # dat.mod$mod.lst3b <- as.vector(predict(lst3b))
  # mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3b"] <- summary(lst3b)$r.sq
  # mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3b"] <- AIC(lst3b)
  # mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3b"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst3b)^2, na.rm=T))
  # 
  # 
  lst4 <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y), data=dat.mod)
  dat.mod$mod.lst4 <- as.vector(predict(lst4)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4"] <- summary(lst4)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4"] <- AIC(lst4)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst4)^2, na.rm=T))
  
  
  # lst4b <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
  # dat.mod$mod.lst4b <- as.vector(predict(lst4b)^2)
  # mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4b"] <- summary(lst4b)$r.sq
  # mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4b"] <- AIC(lst4b)
  # mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4b"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst4b)^2, na.rm=T))
  # 
  lst5 <- gam(sqrt(ET.mean) ~ s(LST.mean, tree.mean) + s(LST.mean, veg.mean) + s(x,y), data=dat.mod)
  dat.mod$mod.lst5 <- as.vector(predict(lst5)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- summary(lst5)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- AIC(lst5)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst5)^2, na.rm=T))
  
  lst6 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(LST.mean) + s(x,y) , data=dat.mod)
  dat.mod$mod.lst6 <- as.vector(predict(lst6)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst6"] <- summary(lst6)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst6"] <- AIC(lst6)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst6"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst6)^2, na.rm=T))
  
  lst7 <- gam(sqrt(ET.mean) ~ s(tree.mean) + s(veg.mean) + s(LST.mean) + s(x,y), data=dat.mod)
  dat.mod$mod.lst7 <- as.vector(predict(lst7)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst7"] <- summary(lst7)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst7"] <- AIC(lst7)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst7"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst7)^2, na.rm=T))
  
  lst8 <- gam(sqrt(ET.mean) ~ s(tree.mean, k=3) + s(veg.mean, k=3) + s(LST.mean, k=3) + s(x,y), data=dat.mod)
  dat.mod$mod.lst8 <- as.vector(predict(lst8)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst8"] <- summary(lst8)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst8"] <- AIC(lst8)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst8"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst8)^2, na.rm=T))
  
  lst9 <- gam(sqrt(ET.mean) ~ s(tree.mean, k=4) + s(veg.mean, k=4) + s(LST.mean, k=4) + s(x,y), data=dat.mod)
  dat.mod$mod.lst9 <- as.vector(predict(lst9)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst9"] <- summary(lst9)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst9"] <- AIC(lst9)
  mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst9"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst9)^2, na.rm=T))

  # mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- summary(lst5)$r.sq
  # mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- AIC(lst5)
  # mods.comp$RMSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- sqrt(mean((dat.mod$ET.mean - dat.mod$mod.lst5)^2, na.rm=T))
  
  
  summary(dat.mod)
  write.csv(dat.mod, file.path(path.out, CITY, paste0(CITY, "_Predictions.csv")), row.names=F)
  
  colsStack <- names(dat.mod)[grep("mod", names(dat.mod))]
  predStack <- stack(dat.mod[,colsStack])
  predStack[,c("x", "y","ET.mean",  "LST.mean", "tree.mean", "veg.mean")] <- dat.mod[,c("x", "y", "ET.mean", "LST.mean", "tree.mean", "veg.mean")]
  summary(predStack)
  # s
  
  png(file.path(path.out, CITY, paste0(CITY, "_PredObs.png")), height=8, width=12, units="in", res=120)
  print(
  ggplot(data=predStack) +
    facet_wrap(~ind) +
    geom_point(aes(x=values, y=ET.mean)) +
    geom_abline(slope=1, intercept=0, color="red"))
  dev.off()
  
  png(file.path(path.out, CITY, paste0(CITY, "_PredResid.png")), height=8, width=12, units="in", res=120)
  print(
  ggplot(data=predStack) +
    facet_wrap(~ind) +
    geom_point(aes(x=values, y=ET.mean-values)) +
    geom_abline(slope=0, intercept=0, color="red"))
  dev.off()
}

summary(mods.comp)
# mods.comp$RMSE <- mods.comp$MRSE

# Doing some standardization to help evaluate patterns
for(CITY in unique(mods.comp$ISOURBID)){
  rows.city <- which(mods.comp$ISOURBID==CITY)
  mods.comp[rows.city,"R2.diff"] <- mods.comp$R2[rows.city] - mean(mods.comp$R2[rows.city], na.rm=T)
  mods.comp[rows.city,"AIC.diff"] <- mods.comp$AIC[rows.city] - mean(mods.comp$AIC[rows.city], na.rm=T)
  mods.comp[rows.city,"RMSE.diff"] <- mods.comp$RMSE[rows.city] - mean(mods.comp$RMSE[rows.city], na.rm=T)

  mods.comp[rows.city,"AIC.diff7"] <- mods.comp$AIC[which(mods.comp$ISOURBID==CITY & mods.comp$model=="lst7")] - mods.comp$AIC[which(mods.comp$ISOURBID==CITY)]
  mods.comp[rows.city,"R2.diff7"] <- mods.comp$R2[which(mods.comp$ISOURBID==CITY & mods.comp$model=="lst7")] - mods.comp$R2[which(mods.comp$ISOURBID==CITY)]
  mods.comp[rows.city,"RMSE.diff7"] <- mods.comp$RMSE[which(mods.comp$ISOURBID==CITY & mods.comp$model=="lst7")] - mods.comp$RMSE[which(mods.comp$ISOURBID==CITY)]
  
    
  mods.comp[rows.city, "R2.rank"] <- length(rows.city) - rank(mods.comp$R2[rows.city]) +1 # math makes lower Better
  mods.comp[rows.city, "AIC.rank"] <- rank(mods.comp$AIC[rows.city]) # Lower better
  mods.comp[rows.city, "RMSE.rank"] <- rank(mods.comp$RMSE[rows.city]) # Lower better
}
summary(mods.comp)
aggregate(cbind(R2, RMSE) ~ model, data=mods.comp, FUN=mean)
aggregate(cbind(R2, RMSE, AIC) ~ model, data=mods.comp, FUN=median)

# length(which(!is.na(mods.comp$AIC.diff7) & mods.comp$AIC.diff7>0))/length(which(!is.na(mods.comp$AIC.diff7)))
# length(which(!is.na(mods.comp$AIC.diff7) & mods.comp$AIC.diff7<0))/length(which(!is.na(mods.comp$AIC.diff7)))


write.csv(mods.comp, file.path(path.out, "ET_ModelSummaryStats.csv"), row.names=F)


mods.comp <- read.csv(file.path(path.out, "ET_ModelSummaryStats.csv"))

mods.comp$AIC[abs(mods.comp$AIC)==Inf] <- NA
mods.comp$AIC.diff[abs(mods.comp$AIC.diff)==Inf] <- NA
summary(mods.comp)

summary(mods.comp[mods.comp$model=="mod1",])
summary(mods.comp[mods.comp$model=="lst7",])
summary(mods.comp[mods.comp$model=="lst8",])
summary(mods.comp[mods.comp$model=="lst9",])

ggplot(data=mods.comp) +
  facet_wrap(~ISOURBID) +
  geom_bar(aes(x=model, y=R2, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  facet_wrap(~ISOURBID) +
  geom_bar(aes(x=model, y=RMSE, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))


# ggplot(data=mods.comp) +
#   facet_wrap(~ISOURBID) +
#   geom_bar(aes(x=model, y=R2.diff, fill=model), stat="identity") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  facet_wrap(~model) +
  geom_bar(aes(x=ISOURBID, y=R2.diff, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  facet_wrap(~model) +
  geom_bar(aes(x=ISOURBID, y=RMSE.diff, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))


# ggplot(data=mods.comp[mods.comp$model %in% c("lst4", "lst5", "lst7", "lst8"),]) +
#   facet_wrap(~model) +
#   geom_bar(aes(x=ISOURBID, y=AIC.diff, fill=model), stat="identity") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=-45, hjust=0))
# 


# Summarizing across models
ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=R2, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=R2.diff, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=RMSE.diff, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=RMSE.diff/RMSE, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

# Graph higher is better
ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=-R2.rank-1, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

# Graph higher is better better
ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=-RMSE.rank-1, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))


# ggplot(data=mods.comp[mods.comp$model %in% c("lst5", "lst7", "lst8"),]) +
#   geom_boxplot(aes(x=model, y=AIC.diff, fill=model)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=-45, hjust=0))
