# Looking at the ET Models and doing some predictions of water use and greening scenarios
library(ggplot2)

# Script to synthesize the results from all of the individual city models ----
# library(ggplot2); library(RColorBrewer); library(cowplot)
# library(raster); library(tidyr); library(scales)
library(mgcv)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")
path.et <- file.path(path.google, "ET_models_v3")
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v3/")


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

overwrite=T
###########################################

###########################################
# Read in the data
###########################################
cityAnalyStats <- read.csv(file.path(path.google, "city_stats_all_ET.csv"))
# cityAnalyStats[,c("dET.TreeEven", "dET.TreeTargetEven", "dET.TreeTargetBottomUp", "dET.ssp245.2050", "dET.ssp245.2100", "dET.ssp585.2050", "dET.ssp585.2100")] <- cityAnalyStats[,c("modET.TreeEven", "modET.TreeTargetEven", "modET.TreeTargetBottomUp", "modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")] - cityAnalyStats$modET.Base
summary(cityAnalyStats)

# treeDistCurrent <- read.csv(file.path(path.google, "TreeDistribution_Current.csv"))
# treeDistGreen <- read.csv(file.path(path.google, "TreeDistribution_Greening-BottomUp.csv"))
# 
# cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_ET.csv"))
# cmip6$Scenario <- as.factor(cmip6$Scenario)
# cmip6$Time <- as.factor(cmip6$Time)
###########################################


###########################################
# Create a loop to save the partial effects and some stats for each city
# the DECREASE in ET with warming makes it clear we need to summarize the partial effects for different cities
###########################################
# coverIntervals <- seq(0, 100, by=2)
nInterval = 50

if(overwrite | !file.exists(file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree.rds"))){
  
  # First, we need to check the intercepts 
  # -- we'll want to look at the average Intercept and the average LST temperature as well as the max annual temperature
  
  # Intercept.Max = max intercept, [ ].MaxI = year/temp in the year with the max intercept
  # Temp.Max = max annual temperature; [].MaxT = year/intercept in the warmest year 
  dfIntercept <- data.frame(ISOURBID = cityAnalyStats$ISOURBID, Intercept.Mean=NA, Temp.Mean=NA, Intercept.Max=NA, Year.MaxI=NA, Temp.MaxI=NA, Temp.Max=NA, Year.MaxT=NA, Intercept.MaxT=NA)
  
  # Now lets set up holders for the spline partial effects
  # splineTree <- array(dim=c(nrow(cityAnalyStats), length(coverIntervals)))
  # dimnames(splineTree) <- list(ISOURBID=cityAnalyStats$ISOURBID, Cover=coverIntervals)
  # splineVeg <- splineTree
  
  # Figuring out the temperature will be hard because this will vary GREATLY among our cities -- for our mean core temp it goes from 17.5C (63F) to 51.8C (125F)
  # -- Rather than make this an array that will be easy to summarize, lets do a list and let each have a unique scale
  # -- -- after the fact decided to just do this for all to make life easier
  
  splineTemp  <- splineTree <- splineVeg <- list()
} else{ 
  dfIntercept <- read.csv(file.path(path.google, "ETModel_InterceptSummary.csv"))
  
  splineTree <- readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree.rds"))
  splineVeg <- readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverVeg.rds"))
  splineTemp <- readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_LST-Day.rds"))
}



# CITY="CAN15001"
# CITY="USA26687" # Chicago
# rowCity <- which(cityAnalyStats$NAME=="Roanoke" & !is.na(cityAnalyStats$NAME))
# CITY=cityAnalyStats$ISOURBID[rowCity]
# cityAnalyStats[cityAnalyStats$ISOURBID==CITY,]
pb <- txtProgressBar(min=0, max=nrow(cityAnalyStats), style=3)

for(rowCity in 1:nrow(cityAnalyStats)){
  setTxtProgressBar(pb, rowCity)
  
  # test <- splineTree[[CITY]]
  if(!overwrite & !is.null(splineTree[[CITY]])) next # If we've done this city, skip it
  
  CITY=cityAnalyStats$ISOURBID[rowCity]
  # print(CITY)
  
  
  dfCity <- read.csv(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")))
  dfCity <- dfCity[dfCity$cityBounds & !is.na(dfCity$LST.mean),]
  # summary(dfCity)
  
  # dfCityET <- read.csv(file.path(path.et, CITY, paste0(CITY, "_ET_means.csv")))
  # summary(dfCityET)
  
  if(!dir.exists(file.path(path.et, CITY))) next # This city doesn't have any ET model, so just skip it!
  modETCity <- readRDS(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
  # modETCity <- readRDS("~/Desktop/CAN15001_Model-ET_annual_gam.rds")
  
  modETCitySum <- readRDS(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam-summary.rds")))
  # modETCitySum
  
  # ----------------------
  # Storing the interrogation of our intercepts
  # ----------------------
  # Intercept.Max = max intercept, [ ].MaxI = year/temp in the year with the max intercept
  # Temp.Max = max annual temperature; [].MaxT = year/intercept in the warmest year 
  # dfIntercept <- data.frame(ISOURBID = cityAnalyStats$ISOURBID, Intercept.Mean=NA, Temp.Mean=NA, Intercept.Max=NA, Year.MaxI=NA, Temp.MaxI=NA, Temp.Max=NA, Year.MaxT=NA, Intercept.MaxT=NA)
  dfIntercept$Intercept.Mean[rowCity] <- mean(modETCitySum$p.coeff)
  dfIntercept$Temp.Mean[rowCity] <- mean(modETCity$model$LST_Day)
  
  # yrstr <- paste(names(modETCitySum$p.coeff)[1])
  indMaxI <- which(modETCitySum$p.coeff==max(modETCitySum$p.coeff))
  yrMax <- as.numeric(stringr::str_sub(names(modETCitySum$p.coeff)[indMaxI], start=-4))
  dfIntercept$Intercept.Max[rowCity] <- modETCitySum$p.coeff[indMaxI]
  dfIntercept$Year.MaxI[rowCity] <- yrMax
  
  
  # The data frame used to fit the model is stored in modETCity$model
  lstYr <- aggregate( LST_Day ~ `as.factor(year)`, data=modETCity$model, FUN=mean, na.rm=T)
  dfIntercept$Temp.MaxI[rowCity] <- lstYr$LST_Day[indMaxI]
  
  indMaxT <- which(lstYr$LST_Day==max(lstYr$LST_Day))
  dfIntercept$Temp.Max[rowCity] <- lstYr$LST_Day[indMaxT]
  dfIntercept$Year.MaxT[rowCity] <- as.numeric(paste(lstYr$`as.factor(year)`[indMaxT]))
  dfIntercept$Intercept.MaxT[rowCity] <- modETCitySum$p.coeff[indMaxT]
  
  write.csv(dfIntercept, file.path(path.google, "ETModel_InterceptSummary.csv"), row.names=F)
  
  # ----------------------
  
  
  # ------------------
  # Now generating our partial effects dataframes
  # ------------------
  # names(modETCity$model)
  treeMean <- mean(modETCity$model$cover.tree)
  vegMean <- mean(modETCity$model$cover.veg)
  lstMean <- mean(modETCity$model$LST_Day)
  xMean <- mean(modETCity$model$x)
  yMean <- mean(modETCity$model$y)
  
  # Tree Effect!
  dfTree <- data.frame(cover.tree = seq(min(modETCity$model$cover.tree), max(modETCity$model$cover.tree), length.out=nInterval), cover.veg=vegMean, LST_Day=lstMean, x=xMean, y=yMean, year=lstYr$`as.factor(year)`[indMaxT])
  splineTree[[CITY]] <- data.frame(ISOURBID = CITY, Effect=as.numeric(predict(modETCity, type="terms", terms="s(cover.tree)", newdata=dfTree)), cover.tree=dfTree$cover.tree)

  # Non-Tree vegetation
  dfVeg <- data.frame(cover.veg = seq(min(modETCity$model$cover.veg), max(modETCity$model$cover.veg), length.out=nInterval), cover.tree=treeMean, LST_Day=lstMean, x=xMean, y=yMean, year=lstYr$`as.factor(year)`[indMaxT])
  splineVeg[[CITY]] <- data.frame(ISOURBID = CITY, Effect=as.numeric(predict(modETCity, type="terms", terms="s(cover.veg)", newdata=dfVeg)), cover.veg=dfVeg$cover.veg)
  
  # Temperature
  dfTemp <- data.frame(cover.tree = treeMean, cover.veg=vegMean, LST_Day=seq(min(modETCity$model$LST_Day), max(modETCity$model$LST_Day), length.out=nInterval), x=xMean, y=yMean, year=lstYr$`as.factor(year)`[indMaxT])
  splineTemp[[CITY]] <- data.frame(ISOURBID = CITY, Effect=as.numeric(predict(modETCity, type="terms", terms="s(LST_Day)", newdata=dfTemp)), LST_Day=dfTemp$LST_Day)
  
  
  saveRDS(splineTree, file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree.rds"))
  saveRDS(splineVeg, file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverVeg.rds"))
  saveRDS(splineTemp, file=file.path(path.google, "ETModel_Spline_PartialEffects_LST-Day.rds"))
  
  
  # ggplot(data=splineTemp[[CITY]]) +
  #   geom_line(aes(x=LST_Day, y=Effect))

  # ------------------
  
 
}
###########################################


###########################################
# Plotting the partial effects here for now
###########################################

dfSplineTree <- data.table::rbindlist(splineTree)
dfSplineTree <- merge(dfSplineTree, cityAnalyStats[,c("ISOURBID", "biomeName")], all.x=T, all.y=F)
summary(dfSplineTree)

png(file.path(path.figs, "ETmodel_PartialEffects_CoverTree.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTree, aes(x=cover.tree, y=Effect)) +
  facet_wrap(~biomeName) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  coord_cartesian(ylim=c(-0.75, 0.75)) +
  theme_bw()
dev.off()

dfSplineVeg <- data.table::rbindlist(splineVeg)
dfSplineVeg <- merge(dfSplineVeg, cityAnalyStats[,c("ISOURBID", "biomeName")], all.x=T, all.y=F)
summary(dfSplineVeg)

png(file.path(path.figs, "ETmodel_PartialEffects_CoverVeg.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineVeg, aes(x=cover.veg, y=Effect)) +
  facet_wrap(~biomeName) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  coord_cartesian(ylim=c(-0.75, 0.75)) +
  theme_bw()
dev.off()


dfSplineTemp <- data.table::rbindlist(splineTemp)
dfSplineTemp <- merge(dfSplineTemp, cityAnalyStats[,c("ISOURBID", "biomeName")], all.x=T, all.y=F)
summary(dfSplineTemp)

png(file.path(path.figs, "ETmodel_PartialEffects_LST.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTemp[dfSplineTemp$LST_Day>0], aes(x=LST_Day, y=Effect)) +
  facet_wrap(~biomeName) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  geom_vline(xintercept = 35, color="orange2") +
  geom_vline(xintercept = 40, color="red2") +
  coord_cartesian(ylim=c(-0.75, 0.75)) +
  theme_bw()
dev.off()

###########################################
