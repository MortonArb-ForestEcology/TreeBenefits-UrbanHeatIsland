# Looking at the ET Models and doing some predictions of water use and greening scenarios
library(ggplot2)

# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
library(raster); library(tidyr); library(scales)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")
path.et <- file.path(path.google, "ET_models_v2")
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

biomeCode.pall.all = c("Tai"= "#2c5c74", 
                       "Tun"="#6d8e9d",
                       "TeBF" = "#7f310f",
                       "TeCF" = "#4d1e10",
                       "TeGS" = "#b09c41",
                       "MGS" = "#a0b8c7",
                       "Med" = "#bf772e",
                       "Des" = "#c89948",
                       "FGS" = "#e0dfa1",
                       "TrGS" = "#a6b39e",
                       "TrDBF" = "#7a9c64",
                       "TrCF" = "#488458",
                       "TrMBF"= "#266240",
                       "Man" = "#9c8c94")

world <- map_data("world")
###########################################

###########################################
# Read in the data
###########################################
cityAnalyStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
cityAnalyStats[,c("dET.TreeEven", "dET.TreeTargetEven", "dET.TreeTargetBottomUp", "dET.ssp245.2050", "dET.ssp245.2100", "dET.ssp585.2050", "dET.ssp585.2100")] <- cityAnalyStats[,c("modET.TreeEven", "modET.TreeTargetEven", "modET.TreeTargetBottomUp", "modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")] - cityAnalyStats$modET.Base
summary(cityAnalyStats)

# test <- read.csv(file.path(path.google, "city_stats_all_ET.csv"))
# summary(test)

treeDistCurrent <- read.csv(file.path(path.google, "TreeDistribution_Current.csv"))
treeDistGreen <- read.csv(file.path(path.google, "TreeDistribution_Greening-BottomUp.csv"))

cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_ET.csv"))
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
cmip6 <- merge(cmip6, cityAnalyStats[,c("ISOURBID", "biomeName", "biomeCode", "modET.Base")], all.x=T, all.y=F)
cmip6$modET.diff <- cmip6$modET - cmip6$modET.Base
summary(cmip6)

###########################################


###########################################
# Things we want to know ----
# 1. Climate Change
#    - How much more water will we need because of climate change?  
#       -- How does that compare to changes in precip?
# 2. Greening
#    - How much water do we need to sustain greening efforts? How does that compare to precip inputs?
#    - How much cooler does our "bottom up" approach get us?
###########################################
# 1. Climate Change

# Starting by aggregating some of the CMIP6 stats to make life easier --> uncertainty will be characterized based on spread seen in cities
cmip6AggMean <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff)~ISOURBID + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=mean, na.rm=T)
summary(cmip6AggMean)

cmip6Biome <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff) ~ biomeName + biomeCode + Scenario + Time, data=cmip6AggMean, FUN=mean)
cmip6Biome

cmip6BiomeMin <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff) ~ biomeName + biomeCode + Scenario + Time, data=cmip6AggMean, FUN=min)
cmip6BiomeMin

cmip6BiomeMax <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff) ~ biomeName + biomeCode + Scenario + Time, data=cmip6AggMean, FUN=max)
cmip6BiomeMax

dfIntercept <- read.csv(file.path(path.google, "ETModel_InterceptSummary.csv"))
splineTemp <- readRDS(file.path(path.google, "ETModel_Spline_PartialEffects_Tair.rds"))
names(dfIntercept)

for(CITY in names(splineTemp)){
  splineTemp[[CITY]]$EffectInt <- splineTemp[[CITY]]$Effect + dfIntercept$Intercept.Mean[dfIntercept$ISOURBID==CITY]
  splineTemp[[CITY]]$EffectCent <- splineTemp[[CITY]]$Effect - mean(splineTemp[[CITY]]$Effect)
  splineTemp[[CITY]]$TairCent <- splineTemp[[CITY]]$Tair_f_inst_mean - mean(splineTemp[[CITY]]$Tair_f_inst_mean)
}

dfSplineTemp <- data.table::rbindlist(splineTemp)
dfSplineTemp <- merge(dfSplineTemp, cityAnalyStats[,c("ISOURBID", "biomeName", "biomeCode")], all.x=F, all.y=F)
summary(dfSplineTemp)

head(dfSplineTemp[is.na(dfSplineTemp$biomeCode),])

png(file.path(path.figs, "ETmodel_PartialEffects_Tair_ClimateChange_2100_BiomeFacet.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTemp[dfSplineTemp$Tair_f_inst_mean>0], aes(x=TairCent, y=EffectCent)) +
  facet_wrap(~biomeName) +
  geom_vline(data=cmip6Biome[cmip6Biome$Time=="2100" & cmip6Biome$Scenario=="ssp245",], aes(xintercept=tas.diff), color="orange2", linewidth=1.5, linetype="dashed") +
  geom_vline(data=cmip6Biome[cmip6Biome$Time=="2100" & cmip6Biome$Scenario=="ssp585",], aes(xintercept=tas.diff), color="red2", linewidth=1.5, linetype="dashed") +
  geom_hline(data=cmip6Biome[cmip6Biome$Time=="2100" & cmip6Biome$Scenario=="ssp245",], aes(yintercept=sqrt(modET)-sqrt(modET.Base)), color="orange3", linewidth=1, linetype="dashed") +
  geom_hline(data=cmip6Biome[cmip6Biome$Time=="2100" & cmip6Biome$Scenario=="ssp585",], aes(yintercept=sqrt(modET)-sqrt(modET.Base)), color="red3", linewidth=1, linetype="dashed") +
  geom_line(aes(group=ISOURBID), linewidth=0.1, color="gray50") + 
  geom_smooth(method="lm", aes(color=biomeCode), linewidth=1.5) +
  scale_color_manual(values=biomeCode.pall.all) +
  labs(x="Tair difference from 2000-2020 mean (deg. c)", y="Effect on sqrt(ET)")+
  guides(color="none") +
  # coord_cartesian(ylim=c(-0.75, 0.75)) +
  theme_bw()
dev.off()


png(file.path(path.figs, "ETmodel_PartialEffects_Tair_ClimateChange_2100_BiomeSlope.png"), height=8, width=10, units="in", res=320)
ggplot(data=dfSplineTemp[dfSplineTemp$Tair_f_inst_mean>0], aes(x=TairCent, y=EffectCent)) +
  # geom_line(aes(group=ISOURBID), linewidth=0.1, color="gray50") + 
  geom_smooth(method="lm", aes(color=biomeCode), linewidth=1.5) +
  scale_color_manual(values=biomeCode.pall.all) +
  labs(x="Tair difference from 2000-2020 mean (deg. c)", y="Effect on sqrt(ET)")+
  # guides(color="none") +
  # coord_cartesian(ylim=c(-0.75, 0.75)) +
  theme_bw()
dev.off()

###########################################
