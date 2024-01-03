# Extract & Format CMIP6 data into a single table for easy use

library(raster); library(tidyr)
library(ggplot2); library(RColorBrewer); library(cowplot)
library(scales)

overwrite=T
###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
# path.cities <- file.path(path.google)
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_GDDP-CMIP6")


path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)


biome.pall.all = c("Taiga"= "#2c5c74", 
                   "Tundra"="#6d8e9d",
                   "Temperate Broadleaf Forest" = "#7f310f",
                   "Temperate Conifer Forest" = "#4d1e10",
                   "Temperate Grassland/Savanna" = "#b09c41",
                   "Montane Grassland/Savanna" = "#a0b8c7",
                   "Mediterranean" = "#bf772e",
                   "Desert" = "#c89948",
                   "Flooded Grassland/Savanna" = "#e0dfa1",
                   "Tropical Grassland/Savanna" = "#a6b39e",
                   "Tropical Dry Broadleaf Forest" = "#7a9c64",
                   "Tropical Conifer Forest" = "#488458",
                   "Tropical Moist Broadleaf Forest"= "#266240",
                   "Mangroves" = "#9c8c94")

world <- map_data("world")
# ##########################################


# ##########################################
# Read in Data; do some cleanup ----
# ##########################################
# Reading in our old, full dataset
cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
summary(as.factor(cityAll.stats$biome))

cityAll.stats$biomeName <- car::recode(cityAll.stats$biome, 
                                       "'boreal forest/taiga'='Taiga';
                                       'tundra'='Tundra';
                                       'montane grassland/savanna'='Montane Grassland/Savanna';
                                       'temperate broadleaf/mixed forest'='Temperate Broadleaf Forest';
                                       'temperate coniferous forest'='Temperate Conifer Forest';
                                       'temperate grassland/savanna'='Temperate Grassland/Savanna';
                                       'mediterranean'='Mediterranean';
                                       'desert/xeric shrublands'='Desert';
                                       'flooded grassland/savanna'='Flooded Grassland/Savanna';
                                       'tropical grassland/savannas'='Tropical Grassland/Savanna';
                                       'tropical dry broadleaf forest'='Tropical Dry Broadleaf Forest';
                                       'tropical coniferous forest'='Tropical Conifer Forest';
                                       'tropical moist broadleaf forest'='Tropical Moist Broadleaf Forest';
                                       'mangroves'='Mangroves'")

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)

modsCMIP6 = c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CESM2', 'CESM2-WACCM', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'CanESM5', 'EC-Earth3', 'EC-Earth3-Veg-LR', 'FGOALS-g3', 'GFDL-CM4', 'GFDL-ESM4', 'GISS-E2-1-G', 'HadGEM3-GC31-LL','IITM-ESM', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR', 'KACE-1-0-G', 'KIOST-ESM', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'NESM3','NorESM2-MM', 'TaiESM1', 'UKESM1-0-LL', 'HadGEM3-GC31-MM',  'NorESM2-LM')
modsCMIP6 <- gsub("-", ".", modsCMIP6)

climCombos <- data.frame(Var1=c("tas", "pr"), Var2="historical", Var3=2014)
climCombos <- rbind(climCombos, expand.grid(c("tas", "pr"), c("ssp245", "ssp585"), c("2020", "2050", "2100")))
names(climCombos) <- c("Var", "Scenario", "Time")
cityAllCMIP6 <- merge(cityAll.stats[,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE")], climCombos, all=T)
cityAllCMIP6[,modsCMIP6] <- NA
dim(cityAllCMIP6)
summary(cityAllCMIP6)
head(cityAllCMIP6)

f.cmip6 <- dir(path.raw, "CMIP6")
head(f.cmip6)

if(overwrite=F) cityAllCMIP6 <- read.csv(file.path(path.cities, "city_stats_all_CMIP6_wide.csv"));
cityAllCMIP6$Var <- as.factor(cityAllCMIP6$Var)
cityAllCMIP6$Scenario <- as.factor(cityAllCMIP6$Scenario)
cityAllCMIP6$Time <- as.factor(cityAllCMIP6$Time)
summary(cityAllCMIP6)

# ##########################################
# The main compilation loop ----
# ##########################################
pb <- txtProgressBar(0, nrow(cityAllCMIP6), style=3)
pbInd = 0
for(CITY in unique(cityAllCMIP6$ISOURBID)){
  # CITY <- cityAllCMIP6$ISOURBID[i]
  print(CITY)
  fCity <- f.cmip6[grep(CITY, f.cmip6)]
  
  if(length(fCity)==0) next 
  
  for(j in 1:length(fCity)){
    pbInd = pbInd+1
    setTxtProgressBar(pb, pbInd)
    
    fNow <- fCity[j]
    fsplit <- strsplit(fNow, "_")[[1]]
    scenNow <- fsplit[3]
    yrNow <- strsplit(fsplit[4], "-")[[1]][2]
    varNow <- strsplit(fsplit[5], "[.]")[[1]][1]
    if(scenNow=="historical") yrNow = 2014
    
    rowInd <- which(cityAllCMIP6$ISOURBID==CITY & cityAllCMIP6$Var==varNow & cityAllCMIP6$Scenario==scenNow & cityAllCMIP6$Time==yrNow)
    
    if(length(rowInd)==0) next
    if(any(!is.na(cityAllCMIP6[rowInd,modsCMIP6]))) next
    
    cityClim <- raster::stack(file.path(path.raw, fNow))
    gcmNames <- names(cityClim)
    gcmMeans <- apply(as.array(cityClim), 3, FUN=mean, na.rm=T)
    names(gcmMeans) <- gcmNames
    # gcmMeans
    
    if(varNow=="pr") gcmMeans <- gcmMeans*60*60*24;
    if(varNow=="tas") gcmMeans <- gcmMeans-273.15;
    
    cityAllCMIP6[rowInd,gcmNames] <- gcmMeans
    
    rm(cityClim)
    
    write.csv(cityAllCMIP6, file.path(path.google, "city_stats_all_CMIP6_wide.csv"), row.names=F)

  }
  # plot(cityClim)
  
  
  
  
}
summary(cityAllCMIP6)
head(cityAllCMIP6)
# ##########################################


# ##########################################
# # Reformat the CMIP6 data to be in a longer format
# ##########################################
cityAllCMIP6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_wide.csv"))
cityAllCMIP6 <- cityAllCMIP6[,names(cityAllCMIP6)[!names(cityAllCMIP6) %in% c("HadGEM3.GC31.MM", "NorESM2.LM")]]
summary(cityAllCMIP6)

cmip6long <- gather(cityAllCMIP6, GCM, value, ACCESS.CM2:UKESM1.0.LL, factor_key = T)
summary(cmip6long)

cmip6 <- spread(cmip6long, Var, value)
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
summary(as.factor(cmip6[is.na(cmip6$tas),"ISOURBID"]))
cmip6[,c("tas.ref", "pr.ref", "tas.diff", "pr.diff", "pr.per")] <- NA
summary(cmip6)

pb <- txtProgressBar(0, length(unique(cmip6$ISOURBID)), style=3)
pbInd = 0

for(CITY in unique(cmip6$ISOURBID)){
  # CITY=unique(cmip6$ISOURBID)[1]
  # print(CITY)
  for(GCM in unique(cmip6$GCM)){
    # GCM = unique(cmip6$GCM)[1]
    rowHist <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM & cmip6$Scenario=="historical")
    for(SCEN in c("ssp245", "ssp585")){
      # # SCEN="ssp245"
      rowsScen <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM & cmip6$Scenario==SCEN)
      
      # Calculate a weighted average for the first 20 years
      rowRef <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM & cmip6$Scenario==SCEN & cmip6$Time==2020)
      if(length(rowRef)==0) next
      
      cmip6$tas.ref[rowsScen] <- (cmip6$tas[rowHist]*14+cmip6$tas[rowRef]*6)/20
      cmip6$pr.ref[rowsScen] <- (cmip6$pr[rowHist]*14+cmip6$pr[rowRef]*6)/20
      
      # # Calculate the absolute diffs
      # cmip6$tas.diff[rowsScen] <- cmip6$tas[rowsScen]-tasRef
      # cmip6$pr.diff[rowsScen] <- cmip6$pr[rowsScen]-prRef
      # cmip6$pr.per[rowsScen] <- cmip6$pr[rowsScen]/prRef
    }
    # rowsGCM <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM)
    # cmip6[rowsGCM ,]
  }
  pbInd = pbInd+1
  setTxtProgressBar(pb, pbInd)
}
summary(cmip6)
summary(cmip6[is.na(cmip6$tas.ref),])
summary(cmip6[!is.na(cmip6$pr) & cmip6$pr.ref==0,])
length(unique(cmip6$ISOURBID[cmip6$pr.ref==0]))

cmip6$tas.diff <- cmip6$tas-cmip6$tas.ref
cmip6$pr.diff <- cmip6$pr-cmip6$pr.ref
cmip6$pr.per <- cmip6$pr/cmip6$pr.ref

cmip6$pr.per[cmip6$pr.per==Inf] <- NA # Get rid of Inf values for the 140 cities that don't get ANY rain in the reference window
summary(cmip6)

write.csv(cmip6, file.path(path.google, "city_stats_all_CMIP6_deviations.csv"), row.names=F)
# ##########################################


# ##########################################
# Do some quick graphing of the CMIP6 temp & precip stats
# ##########################################
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_deviations.csv"))
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
summary(cmip6)

cmip6Agg <- aggregate(cbind(pr, tas, tas.ref, pr.ref, tas.diff, pr.diff, pr.per) ~ ISOURBID + NAME + LATITUDE + LONGITUDE + Scenario + Time, data=cmip6, FUN=mean, na.rm=T)
# su

# Add in historical runs
histAgg <- aggregate(cbind(pr, tas) ~ ISOURBID + NAME + LATITUDE + LONGITUDE + Scenario + Time, data=cmip6[cmip6$Scenario=="historical",], FUN=mean, na.rm=T)
histAgg[,c("tas.ref", "pr.ref", "tas.diff", "pr.diff", "pr.per")] <- NA
summary(histAgg)

cmip6Agg <- rbind(cmip6Agg, histAgg)
cmip6Agg$Scenario <- as.factor(cmip6Agg$Scenario)
cmip6Agg$Time <- as.factor(cmip6Agg$Time)
summary(cmip6Agg)




grad.temp <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
grad.prcp <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") #  ends with teal
world <- map_data("world"); 
world <- world[!world$long>180,]

tasHist <- ggplot(data=cmip6Agg[cmip6Agg$Time==2014,]) +
  facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tas), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nTemp. (deg. C)", colors=grad.temp, n.breaks=13, oob=squish) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(4, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

prBreaks <- round(quantile(cmip6Agg$pr[cmip6Agg$Scenario=="historical"], seq(0.1, 0.9, by=0.1)), 1)
prHist <- ggplot(data=cmip6Agg[cmip6Agg$Time==2014,]) +
  facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=pr), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=grad.prcp, limits=c(0,10), breaks=c(0.1, 1.5, 2.3, 2.7, 3.7, 4.8, 5.8, 7.1, 9.3), oob=squish) + # Using breaks from IPCC AR6 figures
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(4, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

tasFuture <- ggplot(data=cmip6Agg[cmip6Agg$Time %in% c(2050, 2100),]) +
  facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tas.diff), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Temp Change\n(deg. C)", colors=grad.temp, limits=c(-4.5, 4.5), n.breaks=13, oob=squish) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(4, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

prFuture <- ggplot(data=cmip6Agg[cmip6Agg$Time %in% c(2050, 2100),]) +
  facet_grid(Time~Scenario) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=(pr.per-1)*100), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Precip Change\n (%)", colors=grad.prcp, limits=c(-0.8, 0.8)*100, n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(4, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))


png(file.path(path.figs, "Climate_CMIP6_EnsembleMeans.png"), height=8, width=14, units="in", res=320)
cowplot::plot_grid(tasHist, prHist, tasFuture, prFuture, ncol=2, rel_heights = c(0.45, 0.65))
dev.off()
# ##########################################
