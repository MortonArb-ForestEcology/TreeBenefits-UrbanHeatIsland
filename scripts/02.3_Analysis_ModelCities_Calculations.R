library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=F

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsRegion <- file.path(path.cities, "../city_stats_all.csv")

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
# NEED TO IMPORT SOME OF THIS FROM OTHER SCRIPT
# if(!file.exists(file.cityStatsRegion) | overwrite){
#   # cityStatsRegion <- read.csv("../sdei-global-uhi-2013.csv")
#   cols.keep <-  c("ISOURBID", "ISO3", "URBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP")
#   cityStatsRegion <- data.frame(sdei.urb[, cols.keep])[,1:length(cols.keep)]
#   # head(cityStatsRegion)
#   
#   # ------------------
#   # Some summary stats about the inputs at the region scale 
#   # ------------------
#   cityStatsRegion[,"SpatialMistmatch"] <- NA
#   # - number of pixels, mean LST, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
#   cityStatsRegion[,c("biome", "biome.prop", "n.pixels", "LST.mean", "LST.sd", "LST.min", "LST.max", "tree.mean", "tree.sd", "tree.min", "tree.max", "veg.mean", "veg.sd", "veg.min", "veg.max", "elev.mean", "elev.sd", "elev.min", "elev.max", "ET.mean", "ET.sd", "ET.min", "ET.max")] <- NA
#   
#   # Save the key info from the full model
#   cityStatsRegion[,c("LST.NoVeg.model.R2adj", "LST.NoVeg.model.AIC", "LST.NoVeg.model.RMSE")] <- NA
#   cityStatsRegion[,c("LSTmodel.R2adj", "LSTmodel.AIC", "LSTmodel.RMSE")] <- NA
#   cityStatsRegion[,c("LSTmodelSCover.R2adj", "LSTmodelSCover.AIC", "LSTmodelSCover.RMSE")] <- NA
#   cityStatsRegion[,c("LSTmodelLog.R2adj", "LSTmodelLog.AIC", "LSTmodelLog.RMSE")] <- NA
#   
#   cityStatsRegion[,c("LSTmodel.tree.slope", "LSTmodel.veg.slope", "LSTmodel.elev.slope", "LSTmodel.tree.p", "LSTmodel.veg.p", "LSTmodel.elev.p")] <- NA
#   # cityStatsRegion[,c("ETmodel.R2adj", "ETmodel.AIC", "ETmodel.tree.slope", "ETmodel.veg.slope", "ETmodel.elev.slope", "ETmodel.tree.p", "ETmodel.veg.p", "ETmodel.elev.p")] <- NA
#   
#   # I had run this, but removed it because we don't have ET everywhere we need/want it
#   # cityStatsRegion[,c("LST.ET.model.R2adj", "LST.ET.model.df", "LST.ET.model.AIC")] <- NA
#   
#   # Adding some year-based trend that isn't generated by the pixel stats --> this is super necessary for SD as a metric of equity
#   cityStatsRegion[,c("trendYear.LST.mean.slope", "trendYear.LST.mean.p")] <- NA
#   cityStatsRegion[,c("trendYear.LST.sd.slope", "trendYear.LST.sd.p")] <- NA
#   cityStatsRegion[,c("trendYear.tree.mean.slope", "trendYear.tree.mean.p")] <- NA
#   cityStatsRegion[,c("trendYear.tree.sd.slope", "trendYear.tree.sd.p")] <- NA
#   cityStatsRegion[,c("trendYear.veg.mean.slope", "trendYear.veg.mean.p")] <- NA
#   cityStatsRegion[,c("trendYear.veg.sd.slope", "trendYear.veg.sd.p")] <- NA
# 
#     
#   # For each variable calculate the overall trends --> this will need to be separate from the pixel-by-pixel analysis, although we'll do and save that as well
#   cityStatsRegion[,c("trend.LST.slope", "trend.LST.slope.sd", "trend.LST.p")] <- NA
#   cityStatsRegion[,c("trend.tree.slope", "trend.tree.slope.sd", "trend.tree.p")] <- NA
#   cityStatsRegion[,c("trend.veg.slope", "trend.veg.slope.sd", "trend.veg.p")] <- NA
#   cityStatsRegion[,c("trend.ET.slope", "trend.ET.slope.sd", "trend.ET.p")] <- NA
#   
#   # Also look at the correlation between warming and change in tree & veg cover
#   cityStatsRegion[,c("corr.LST.tree.slope", "corr.LST.tree.p", "corr.LST.tree.Rsq")] <- NA
#   cityStatsRegion[,c("corr.LST.veg.slope", "corr.LST.veg.p", "corr.LST.veg.Rsq")] <- NA
#   cityStatsRegion[,c("corr.tree.veg.slope", "corr.tree.veg.p", "corr.tree.veg.Rsq")] <- NA
#   
#   summary(cityStatsRegion)
#   dim(cityStatsRegion)
#   
#   write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  
#   # ------------------
# }

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

  # -----------------------
  # READ IN VALUES AND APPROPRIATE MODEL FROM SCRIPT 2.1
  # -----------------------
  
  
  
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
  summaryCity <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation, ET) ~ x+y+location + cityBounds, data=valsCity, FUN=mean)
  names(summaryCity)[names(summaryCity) %in% c("LST_Day", "cover.tree", "cover.veg", "ET")] <- c("LST.mean", "tree.mean", "veg.mean", "ET.mean")
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

