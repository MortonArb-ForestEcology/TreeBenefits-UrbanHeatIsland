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
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4")
path.cities <- file.path(path.google, "data_processed_final")
path.et <- file.path(path.google, "ET_models_v4")
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v4/")
path.EEout <- file.path("~/Google Drive/My Drive", "UHI_Analysis_Output_Final_v4")


path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)

biome.order = data.frame(biomeName=c("Tundra", "Taiga", "Tropical Conifer Forest", "Mangroves", "Temperate Broadleaf Forest", "Temperate Conifer Forest", "Tropical Moist Broadleaf Forest", "Montane Grassland/Savanna", "Tropical Grassland/Savanna", "Tropical Dry Broadleaf Forest", "Flooded Grassland/Savanna", "Temperate Grassland/Savanna", "Mediterranean", "Desert"),
                         biomeCode=c("Tun", "Tai", "TrCF", "Man", "TeBF", "TeCF", "TrMBF", "MGS", "TrGS", "TrDBF", "FGS", "TeGS", "Med", "Des"))

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

grad.prcp <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") #  ends with teal
grad.temp <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")

world <- map_data("world"); 
world <- world[!world$long>180,]

###########################################

###########################################
# Read in the data
###########################################
cityAnalyStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))

# Create a GLDAS file to make life easier
datGLDAS <- cityAnalyStats[,c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode")]
datGLDAS[,c("Tair.gldas", "pr.gldas")] <- NA
summary(datGLDAS)

files.gldas <- dir(path.EEout, "GLDAS21_annualMeans")

for(i in 1:nrow(datGLDAS)){
  CITY <- datGLDAS$ISOURBID[i]
  fTemp <- files.gldas[grep(CITY, files.gldas)]
  
  if(length(fTemp)==0) next
  
  TempCity <- read.csv(file.path(path.EEout, fTemp[length(fTemp)]))
  TempCity$Tair_f_inst_mean <- TempCity$Tair_f_inst_mean-273.15
  TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")] <- TempCity[,c("Evap_tavg_mean", "Rainf_f_tavg_mean")]*60*60*24
  
  datGLDAS$Tair.gldas[i] <- mean(TempCity$Tair_f_inst_mean, na.rm=T)
  datGLDAS$pr.gldas[i] <- mean(TempCity$Rainf_f_tavg_mean, na.rm=T)
  datGLDAS$ET.gldas[i] <- mean(TempCity$Evap_tavg_mean, na.rm=T)
}
summary(datGLDAS)

# biome.order <- aggregate(Tair.gldas ~ biomeName, data=datGLDAS, FUN=mean)
# biome.order <- biome.order[order(biome.order$Tair.gldas),]
# biome.order2 <- biome.order[order(biome.order$Tair.gldas, decreasing = T),]

# --------------------
# Now read in city stats
# --------------------
cityAnalyStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
cityAnalyStats$biomeName <- factor(cityAnalyStats$biomeName, levels=biome.order$biomeName)
cityAnalyStats[,c("dET.TreeTargetBottom25", "dET.TreeCityBottom50", "dET.TreeCityBottom25", "dET.ssp245.2050", "dET.ssp245.2100", "dET.ssp585.2050", "dET.ssp585.2100")] <- cityAnalyStats[,c("modET.TreeTargetBottom25", "modET.TreeCityBottom50", "modET.TreeCityBottom25", "modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")] - cityAnalyStats$modET.Base
cityAnalyStats$Scenario <- c("Present")
cityAnalyStats$Time <- c("2020")
summary(cityAnalyStats)

# Merging in GLDAS
cityAnalyStats <- merge(cityAnalyStats, datGLDAS, all.x=T, all.y=F)
summary(cityAnalyStats)

# Calculate the ET ratios
cityAnalyStats$gldasET.PR <- cityAnalyStats$ET.gldas/cityAnalyStats$pr.gldas
cityAnalyStats$modET.Precip <- cityAnalyStats$modET.Base/cityAnalyStats$pr.gldas
summary(cityAnalyStats)


# --------------------



# test <- read.csv(file.path(path.google, "city_stats_all_ET.csv"))
# summary(test)

# --------------------
# CMIP6 scenarios
# --------------------
# NOTE that for the cmip6 scenarios, we added the change in temp to the gldas air temp; so lets create a precip correction
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_ET.csv"))
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
cmip6 <- merge(cmip6, cityAnalyStats[,c("ISOURBID", "biomeName", "biomeCode", "modET.Base")], all.x=T, all.y=F)
cmip6$modET.diff <- cmip6$modET - cmip6$modET.Base
summary(cmip6)

cmip6 <- merge(cmip6, datGLDAS, all.x=T, all.y=F)
cmip6$pr.adj <- cmip6$pr.gldas*cmip6$pr.per # Creating an adjusted daily precip by looking at the % change in pr for each run
summary(cmip6)


# Looking at the ET vs Precip Ratio --> NOTE: Using adjusted PR because we adjusted ET
cmip6$modET.Precip <- cmip6$modET/cmip6$pr.adj
cmip6$modET.Precip[cmip6$modET.Precip==Inf] <- NA
# cmip6$modET.PrecipLog <- log(cmip6$modET.Precip)
summary(cmip6)

summary(log(cmip6$modET.Precip))
# --------------------

# Tree distributions if we want to go there
treeDistCurrent <- read.csv(file.path(path.google, "TreeDistribution_Current.csv"))
treeDistGreen <- read.csv(file.path(path.google, "TreeDistribution_Greening-Bottom25.csv"))
treeDistGreenCity50 <- read.csv(file.path(path.google, "TreeDistribution_Greening-CityBottom50.csv"))
treeDistGreenCity25 <- read.csv(file.path(path.google, "TreeDistribution_Greening-CityBottom25.csv"))


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
cmip6AggMed <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.Precip) ~ ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=median, na.rm=T)
summary(cmip6AggMed)
summary(log(cmip6AggMed$modET.Precip))

cmip6AggMean <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.Precip)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=mean, na.rm=T)
cmip6AggMean$biomeName <- factor(cmip6AggMean$biomeName, levels=biome.order$biomeName)
summary(cmip6AggMean)
summary(log(cmip6AggMean$modET.Precip))


cmip6Biome <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff) ~ biomeName + biomeCode + Scenario + Time, data=cmip6AggMean, FUN=mean)
cmip6Biome

cmip6BiomeMin <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff) ~ biomeName + biomeCode + Scenario + Time, data=cmip6AggMean, FUN=min)
cmip6BiomeMin

cmip6BiomeMax <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff) ~ biomeName + biomeCode + Scenario + Time, data=cmip6AggMean, FUN=max)
cmip6BiomeMax


# Looking at ET ratios
summary(cmip6AggMean)
plotRatioLog <- ggplot(data=cmip6AggMean[,]) +
  facet_grid(Time~.) +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(x=biomeName, y=log(modET.Precip), fill=biomeName, alpha=Scenario), scale="width") +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate(geom="text", x=1.25, y=-3, label=c("Precip Surplus"), hjust=0) +
  annotate(geom="text", x=1.25, y=5, label=c("Precip Deficit"), hjust=0) +
  # annotate(geom="text", x=14, y=5, label=c("Precip Deficit"), hjust=1) +
  labs(caption="light = ssp245, dark=ssp585") +
  scale_fill_manual(values=biome.pall.all) +
  scale_alpha_manual(values=c(0.7, 1)) +
  labs(y="log(ET/Precip)", x="Biome") +
  theme_bw() +
  theme(axis.text.x = element_blank())

png(file.path(path.figs, "ETmodel_ET_vs_Precip_CMIP6_Log.png"), height=8, width=10, units="in", res=320)
plotRatioLog
dev.off()

ggplot(data=cityAnalyStats[,]) +
  # facet_grid(Time~.) +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(x=biomeName, y=log(modET.Precip), fill=biomeName), scale="width") +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate(geom="text", x=1.25, y=-3, label=c("Precip Surplus"), hjust=0) +
  annotate(geom="text", x=1.25, y=5, label=c("Precip Deficit"), hjust=0) +
  scale_fill_manual(values=biome.pall.all) +
  labs(y="log(ET/Precip)", x="Biome") +
  theme_bw() +
  theme(axis.text.x = element_blank())

map.ETratio.Now <- ggplot(data=cityAnalyStats[,]) +
  facet_grid(.~Scenario ) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=log(modET.Precip)), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="log(ET/precip)", colors=rev(grad.prcp), limits=c(-3, 3), n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
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
map.ETratio.Now

summary(cmip6AggMean)
map.ETratio.Fut <- ggplot(data=cmip6AggMean[cmip6AggMean$Time=="2100",]) +
  facet_grid(.~Scenario ) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=log(modET.Precip)), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="log(ET/precip)", colors=rev(grad.prcp), limits=c(-3, 3), n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
  theme(legend.position="none",
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
map.ETratio.Fut

# png(file.path(path.figs, "ETmodel_ET_vs_Precip_CMIP6_Log.png"), height=8, width=10, units="in", res=320)

png(file.path(path.figs, "ETmodel_ET_vs_Precip_Now-CMIP6_Map_Log.png"), height=8, width=14, units="in", res=320)
cowplot::plot_grid(map.ETratio.Now, map.ETratio.Fut, ncol=1, rel_heights = c(0.65, 0.45))
dev.off()


# Creating a combined ET data frame
etSummary <- rbind(cityAnalyStats[,c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET.Precip")],
                   cmip6AggMean[cmip6AggMean$Time=="2100",c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET.Precip")])
etSummary$biomeName <- factor(etSummary$biomeName, levels=biome.order$biomeName)
etSummary$biomeCode <- factor(etSummary$biomeCode, levels=rev(biome.order$biomeCode))

summary(etSummary)

map.ETratio.All <- ggplot(data=etSummary[,]) +
  facet_grid(Scenario~. ) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=log(modET.Precip)), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="log(ET/precip)", colors=rev(grad.prcp), limits=c(-3, 3), n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
  theme(legend.position="left",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        legend.key.height = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 2, 0.5, "lines"))
map.ETratio.All

plotRatioLog <- ggplot(data=etSummary[!is.na(etSummary$biomeName),]) +
  facet_grid(Scenario~.) +
  # coord_flip() +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(y=biomeCode, x=log(modET.Precip), fill=biomeName), scale="width") +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate(geom="text", y=14, x=-3, label=c("Precip Surplus"), hjust=0, size=3) +
  annotate(geom="text", y=14, x=3, label=c("Precip Deficit"), hjust=0, size=3) +
  # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
  scale_fill_manual(values=biome.pall.all) +
  labs(x="log(ET/Precip)", y="Biome") +
  guides(fill="none") +
  theme_bw()
plotRatioLog

png(file.path(path.figs, "ETmodel_ET_vs_Precip_Now-CMIP6_Log_Combined.png"), height=8, width=14, units="in", res=320)
cowplot::plot_grid(map.ETratio.All, plotRatioLog, ncol=2, rel_widths = c(0.55, 0.45))
dev.off()
 


# Looking at some partial effects
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
