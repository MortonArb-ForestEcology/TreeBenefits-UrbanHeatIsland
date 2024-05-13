# Looking at the ET Models and doing some predictions of water use and greening scenarios
# This is identical to script X6, but everything will be transformed back to ET by predicting effects under "mean" conditions rather than working in "terms' space
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
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4")
path.cities <- file.path(path.google, "data_processed_final")
path.et <- file.path(path.google, "ET_models_v4")
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v4/")


path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)


biome.pall.all = c("boreal forest/taiga"= "#2c5c74", 
                   "tundra"="#6d8e9d",
                   "temperate broadleaf/mixed forest" = "#7f310f",
                   "temperate coniferous forest" = "#4d1e10",
                   "Temperate Grassland/Savanna" = "#b09c41",
                   "montane grassland/savanna" = "#a0b8c7",
                   "mediterranean" = "#bf772e",
                   "desert/xeric shrublands" = "#c89948",
                   "flooded grassland/savanna" = "#e0dfa1",
                   "tropical grassland/savanna" = "#a6b39e",
                   "tropical dry broadleaf forest" = "#7a9c64",
                   "tropical coniferous forest" = "#488458",
                   "tropical moist broadleaf forest"= "#266240",
                   "mangroves" = "#9c8c94")

world <- map_data("world")

overwrite=T
###########################################

###########################################
# Read in the data
###########################################
cityAnalyStats <- read.csv(file.path(path.google, "city_stats_all_ET.csv"))
# cityAnalyStats[,c("dET.TreeEven", "dET.TreeTargetEven", "dET.TreeTargetBottomUp", "dET.ssp245.2050", "dET.ssp245.2100", "dET.ssp585.2050", "dET.ssp585.2100")] <- cityAnalyStats[,c("modET.TreeEven", "modET.TreeTargetEven", "modET.TreeTargetBottomUp", "modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")] - cityAnalyStats$modET.Base
# cityAnalyStats <- cityAnalyStats[!is.na(cityAnalyStats$ETmodel.R2adj) & cityAnalyStats$ETmodel.R2adj>0,]
summary(cityAnalyStats)

cityStatsBase <- read.csv(file.path(path.google, "city_stats_all.csv"))
summary(cityStatsBase)


# Add in biome designations for our sanity
unique(cityStatsBase$biome)
cityAnalyStats <- merge(cityAnalyStats, cityStatsBase[,c("ISOURBID", "biome")], all.x=T, all.y=F)

unique(cityAnalyStats$biome)

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

if(overwrite | !file.exists(file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree_ET-units.rds"))){
  
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
  dfIntercept <- read.csv(file.path(path.google, "ETModel_InterceptSummary_ET-units.csv"))
  
  splineTree <- readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree_ET-units.rds"))
  splineVeg <- readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverVeg_ET-units.rds"))
  splineTemp <- readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_LST-Day_ET-units.rds"))
}



# CITY="CAN15001"
# CITY="USA26687" # Chicago
# rowCity <- which(cityAnalyStats$NAME=="Roanoke" & !is.na(cityAnalyStats$NAME))
# CITY=cityAnalyStats$ISOURBID[rowCity]
# cityAnalyStats[cityAnalyStats$ISOURBID==CITY,]
pb <- txtProgressBar(min=0, max=nrow(cityAnalyStats), style=3)

for(rowCity in 1:nrow(cityAnalyStats)){
  setTxtProgressBar(pb, rowCity)
  
  CITY=cityAnalyStats$ISOURBID[rowCity]
  # print(CITY)
  
  # test <- splineTree[[CITY]]
  if(!overwrite & !is.null(splineTree[[CITY]])) next # If we've done this city, skip it
  if(is.na(cityAnalyStats$ETmodel.R2adj[rowCity]) | cityAnalyStats$ETmodel.R2adj[rowCity]<0) next # We don't actually have this city; skip it
  
  
  
  dfCity <- read.csv(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")))
  dfCity <- dfCity[dfCity$cityBounds & !is.na(dfCity$LST.mean),]
  # summary(dfCity)
  
  # dfCityET <- read.csv(file.path(path.et, CITY, paste0(CITY, "_ET_means.csv")))
  # summary(dfCityET)
  
  if(!dir.exists(file.path(path.et, CITY))) next # This city doesn't have any ET model, so just skip it!
  modETCity <- readRDS(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam.rds")))
  # modETCity <- readRDS("~/Desktop/CAN15001_Model-ET_annual_gam.rds")
  summary(modETCity$model)
  
  modETCitySum <- readRDS(file.path(path.et, CITY, paste0(CITY, "_Model-ET_annual_gam-summary.rds")))
  # modETCitySum
  
  # ----------------------
  # Storing the interrogation of our intercepts
  # ----------------------
  # Intercept.Max = max intercept, [ ].MaxI = year/temp in the year with the max intercept
  # Temp.Max = max annual temperature; [].MaxT = year/intercept in the warmest year 
  # dfIntercept <- data.frame(ISOURBID = cityAnalyStats$ISOURBID, Intercept.Mean=NA, Temp.Mean=NA, Intercept.Max=NA, Year.MaxI=NA, Temp.MaxI=NA, Temp.Max=NA, Year.MaxT=NA, Intercept.MaxT=NA)
  intYear <- grepl("year", names(modETCitySum$p.coeff))
  dfIntercept$Intercept.Mean[rowCity] <- mean(modETCitySum$p.coeff[intYear])
  dfIntercept$Temp.Mean[rowCity] <- mean(modETCity$model$Tair_f_inst_mean)
  
  intDiffs <- modETCitySum$p.coeff[intYear]-mean(modETCitySum$p.coeff[intYear])
  indMeanI  <- which(intDiffs==min(intDiffs))
  yearMean <- as.numeric(stringr::str_sub(names(modETCitySum$p.coeff[intYear])[indMeanI], start=-4))
  
  
  # yrstr <- paste(names(modETCitySum$p.coeff)[1])
  indMaxI <- which(modETCitySum$p.coeff[intYear]==max(modETCitySum$p.coeff[intYear]))
  yrMax <- as.numeric(stringr::str_sub(names(modETCitySum$p.coeff[intYear])[indMaxI], start=-4))
  dfIntercept$Intercept.Max[rowCity] <- modETCitySum$p.coeff[intYear][indMaxI]
  dfIntercept$Year.MaxI[rowCity] <- yrMax
  
  
  # The data frame used to fit the model is stored in modETCity$model
  lstYr <- aggregate( Tair_f_inst_mean ~ `as.factor(year)`, data=modETCity$model, FUN=mean, na.rm=T)
  dfIntercept$Temp.MaxI[rowCity] <- lstYr$Tair_f_inst_mean[lstYr$`as.factor(year)`==yrMax]
  
  indMaxT <- which(lstYr$Tair_f_inst_mean==max(lstYr$Tair_f_inst_mean))
  yrMaxT <- as.numeric(paste(lstYr$`as.factor(year)`[indMaxT]))
  dfIntercept$Temp.Max[rowCity] <- lstYr$Tair_f_inst_mean[indMaxT]
  dfIntercept$Year.MaxT[rowCity] <- yrMaxT
  dfIntercept$Intercept.MaxT[rowCity] <- modETCitySum$p.coeff[paste0("as.factor(year)", yrMaxT)]
  
  write.csv(dfIntercept, file.path(path.google, "ETModel_InterceptSummary_ET-units.csv"), row.names=F)
  
  # ----------------------
  
  
  # ------------------
  # Now generating our partial effects dataframes
  # ------------------
  # names(modETCity$model)
  treeMean <- mean(modETCity$model$cover.tree)
  vegMean <- mean(modETCity$model$cover.veg)
  tempMean <- mean(modETCity$model$Tair_f_inst_mean)
  xMean <- mean(modETCity$model$x)
  yMean <- mean(modETCity$model$y)
  
  # Tree Effect!
  dfTree <- data.frame(cover.tree = seq(min(modETCity$model$cover.tree), max(modETCity$model$cover.tree), length.out=nInterval), cover.veg=vegMean, Tair_f_inst_mean=tempMean, x=xMean, y=yMean, year=yearMean)
  splineTree[[CITY]] <- data.frame(ISOURBID = CITY, ET=as.numeric(predict(modETCity, newdata=dfTree))^2, cover.tree=dfTree$cover.tree)
  # testTerms <- data.frame(ISOURBID = CITY, Effect=as.numeric(predict(modETCity, type="terms", terms="s(cover.tree)", newdata=dfTree)), cover.tree=dfTree$cover.tree)
  # testResp <- data.frame(ISOURBID = CITY, Effect=as.numeric(predict(modETCity, type="response", terms="s(cover.tree)", newdata=dfTree)), cover.tree=dfTree$cover.tree)
  # testEffect <- data.frame(ISOURBID = CITY, Effect=as.numeric(predict(modETCity, newdata=dfTree)), cover.tree=dfTree$cover.tree)
  # 
  # summary(testTerms)
  # summary(testResp)
  # summary(testEffect)
  
  # plot(Effect ~ cover.tree, data=testTerms)
  # plot(Effect^2 ~ cover.tree, data=testResp)
  # plot(Effect^2 ~ cover.tree, data=testEffect)
  
  # Non-Tree vegetation
  dfVeg <- data.frame(cover.veg = seq(min(modETCity$model$cover.veg), max(modETCity$model$cover.veg), length.out=nInterval), cover.tree=treeMean, Tair_f_inst_mean=tempMean, x=xMean, y=yMean, year=yearMean)
  splineVeg[[CITY]] <- data.frame(ISOURBID = CITY, ET=as.numeric(predict(modETCity, newdata=dfVeg))^2, cover.veg=dfVeg$cover.veg)
  
  # Temperature
  dfTemp <- data.frame(cover.tree = treeMean, cover.veg=vegMean, Tair_f_inst_mean=seq(min(modETCity$model$Tair_f_inst_mean), max(modETCity$model$Tair_f_inst_mean), length.out=nInterval), x=xMean, y=yMean, year=yearMean)
  splineTemp[[CITY]] <- data.frame(ISOURBID = CITY, ET=as.numeric(predict(modETCity, newdata=dfTemp))^2, Tair_f_inst_mean=dfTemp$Tair_f_inst_mean)
  
  
  saveRDS(splineTree, file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree_ET-units.rds"))
  saveRDS(splineVeg, file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverVeg_ET-units.rds"))
  saveRDS(splineTemp, file=file.path(path.google, "ETModel_Spline_PartialEffects_Tair_ET-units.rds"))
  
  
  # ggplot(data=splineTemp[[CITY]]) +
  #   geom_line(aes(x=Tair_f_inst_mean, y=Effect))

  # ------------------
  
 
}
###########################################


###########################################
# Plotting the partial effects here for now
# # This seems useful; worth adding the intercept to these (and then squaring it) so that the y-axis can be ET (kg/m2/day)
# # Ross: anything that can get us towards describing the full effect seems worth a shot
# # Christy: worth doing this for the LST model as well
###########################################

for(CITY in names(splineTree)){
  # splineTree[[CITY]]$Intercept <- splineTree[[CITY]]$ET + dfIntercept$Intercept.Mean[dfIntercept$ISOURBID==CITY]
  splineTree[[CITY]]$ETCent <- splineTree[[CITY]]$ET - mean(splineTree[[CITY]]$ET)
  splineTree[[CITY]]$TreeCent <- splineTree[[CITY]]$cover.tree - mean(splineTree[[CITY]]$cover.tree)
}
dfSplineTree <- data.table::rbindlist(splineTree)
dfSplineTree <- merge(dfSplineTree, cityAnalyStats[,c("ISOURBID", "biome")], all.x=T, all.y=F)
summary(dfSplineTree)

png(file.path(path.figs, "ETmodel_PartialEffects_CoverTree_ET-units.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTree, aes(x=cover.tree, y=ET)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  coord_cartesian(ylim=c(0, 5)) +
  theme_bw()
dev.off()


png(file.path(path.figs, "ETmodel_PartialEffects_CoverTree_Centered-EffectOnly_ET-units.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTree, aes(x=cover.tree, y=ETCent)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  coord_cartesian(ylim=c(-2.5, 2.5)) +
  theme_bw()
dev.off()


png(file.path(path.figs, "ETmodel_PartialEffects_CoverTree_Centered_ET-units.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTree, aes(x=TreeCent, y=ETCent)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  coord_cartesian(ylim=c(-2.5, 2.5)) +
  theme_bw()
dev.off()


dfSplineVeg <- data.table::rbindlist(splineVeg)
dfSplineVeg <- merge(dfSplineVeg, cityAnalyStats[,c("ISOURBID", "biome")], all.x=T, all.y=F)
summary(dfSplineVeg)

png(file.path(path.figs, "ETmodel_PartialEffects_CoverVeg_ET-units.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineVeg, aes(x=cover.veg, y=ET)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth() +
  coord_cartesian(ylim=c(0, 5)) +
  theme_bw()
dev.off()


dfSplineTemp <- data.table::rbindlist(splineTemp)
dfSplineTemp <- merge(dfSplineTemp, cityAnalyStats[,c("ISOURBID", "biome")], all.x=T, all.y=F)
summary(dfSplineTemp)

png(file.path(path.figs, "ETmodel_PartialEffects_Tair_ET-units.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTemp[dfSplineTemp$Tair_f_inst_mean>0], aes(x=Tair_f_inst_mean, y=ET)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  # geom_smooth() +
  # geom_vline(xintercept = 35, color="orange2") +
  # geom_vline(xintercept = 40, color="red2") +
  coord_cartesian(ylim=c(0, 5)) +
  theme_bw()
dev.off()

# Trying to add the intercept alone has very little effect
# splineTempInt <- splineTemp
for(CITY in names(splineTemp)){
  # splineTemp[[CITY]]$ET <- splineTemp[[CITY]]$ET + dfIntercept$Intercept.Mean[dfIntercept$ISOURBID==CITY]
  splineTemp[[CITY]]$ETCent <- splineTemp[[CITY]]$ET - mean(splineTemp[[CITY]]$ET)
  splineTemp[[CITY]]$TairCent <- splineTemp[[CITY]]$Tair_f_inst_mean - mean(splineTemp[[CITY]]$Tair_f_inst_mean)
}

dfSplineTemp <- data.table::rbindlist(splineTemp)
dfSplineTemp <- merge(dfSplineTemp, cityAnalyStats[,c("ISOURBID", "biome")], all.x=T, all.y=F)
summary(dfSplineTemp)

png(file.path(path.figs, "ETmodel_PartialEffects_Tair_Centered_ET-units.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTemp[dfSplineTemp$Tair_f_inst_mean>0], aes(x=TairCent, y=ETCent)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth(method="lm") +
  # geom_vline(xintercept = 35, color="orange2") +
  # geom_vline(xintercept = 40, color="red2") +
  coord_cartesian(ylim=c(-1, 1)) +
  theme_bw()
dev.off()

png(file.path(path.figs, "ETmodel_PartialEffects_Tair_Centered_ET-units_2.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTemp[dfSplineTemp$Tair_f_inst_mean>0], aes(x=TairCent, y=ETCent)) +
  facet_wrap(~biome) +
  geom_line(aes(group=ISOURBID), linewidth=0.1) + 
  geom_smooth(method="lm") +
  # geom_vline(xintercept = 35, color="orange2") +
  # geom_vline(xintercept = 40, color="red2") +
  coord_cartesian(ylim=c(-2.5, 2.5)) +
  theme_bw()
dev.off()


###########################################
