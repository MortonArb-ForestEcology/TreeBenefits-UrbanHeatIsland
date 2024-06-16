# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
# Key Results: (What Christy needs to get numbers for)
#  3.0. SUPPLEMENT Figure: The amount of warming & precipitation varies across the globe; this is CMIP6, not us, so no need to put in main MS
#. 3.1. Warming will cause an X% increase in ET; X% of cities will see precip not keep pace with this, resulting in XX% of cities 
#  3.2. Although we estimate XX biomes to have the greatest proportion of cities in a canopy water deficit, the biggest shift in the distribution is in temperate forest biomes, particularly cities in Europe and the US

library(ggplot2); library(cowplot); library(scales)

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4")
path.cities <- file.path(path.google, "data_processed_final")

path.figsMS <- file.path(path.google, "figures_manuscript")
path.figsExplore <- file.path(path.google, "figures_exploratory")
dir.create(path.figsMS, recursive=T, showWarnings=F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Read in some base datasets etc.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
source("0_color_palettes_etc.R")

biome.order <-  read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis_BiomeOrder.csv"))
biome.order

StatsCombined <-  read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
StatsCombined$biomeName <- factor(StatsCombined$biomeName, levels=biome.order$biomeName)
StatsCombined$biomeCode <- factor(StatsCombined$biomeCode, levels=biome.order$biomeCode)
StatsCombined$biomeNameRev <- factor(StatsCombined$biomeName, levels=rev(levels(StatsCombined$biomeName)))
StatsCombined$biomeCodeRev <- factor(StatsCombined$biomeCode, levels=rev(levels(StatsCombined$biomeCode)))
summary(StatsCombined)

# Read in the ET base data
cityETStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
cityETStats[,c("dET.ssp245.2050", "dET.ssp245.2100", "dET.ssp585.2050", "dET.ssp585.2100")] <- cityETStats[,c("modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")] - cityETStats$modET.Base
summary(cityETStats)

StatsCombined <- merge(StatsCombined, cityETStats[,c("ISOURBID", "modET.Base")], all.x=T, all.y=F)
StatsCombined$modET.Precip <- StatsCombined$modET.Base/StatsCombined$Precip.GLDAS
StatsCombined$Scenario <- c("Present")
StatsCombined$Time <- c("2020")
summary(StatsCombined)


# Creating a combined ET data frame
# NOTE that for the cmip6 scenarios, we added the change in temp to the gldas air temp; so lets create a precip correction
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_ET.csv"))
cmip6 <- cmip6[cmip6$ISOURBID %in% StatsCombined$ISOURBID,]
cmip6$Scenario <- car::recode(cmip6$Scenario, "'ssp245'='SSP2-4.5'; 'ssp585'='SSP5-8.5'")
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
cmip6 <- merge(cmip6, StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "Tmean.GLDAS", "Precip.GLDAS", "ET.GLDAS", "modET.Base")], all.x=T, all.y=F)
cmip6$modET.diff <- cmip6$modET - cmip6$modET.Base
cmip6$modET.perChange <- cmip6$modET/cmip6$modET.Base
summary(cmip6)

summary(cmip6)


# Looking at the ET vs Precip Ratio --> NOTE: Using adjusted PR because we adjusted ET
cmip6$pr.adj <- cmip6$Precip.GLDAS*cmip6$pr.per # Creating an adjusted daily precip by looking at the % change in pr for each run
cmip6$modET.Precip <- cmip6$modET/cmip6$pr.adj
cmip6$modET.Precip[cmip6$modET.Precip==Inf] <- NA
# cmip6$modET.PrecipLog <- log(cmip6$modET.Precip)
summary(cmip6)

# Checking the distribution of values of the ET/Precip ratio to figure out whether we should rely on mean or median
# This doesn't look bad!
ggplot(data=cmip6) +
  facet_grid(Scenario~.) +
  geom_histogram(aes(x=log(modET.Precip)))

cmip6AggMean <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.perChange, modET.Precip)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=mean, na.rm=T)
cmip6AggMean$biomeName <- factor(cmip6AggMean$biomeName, levels=biome.order$biomeName)
summary(cmip6AggMean)

cmip6AggSD <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.perChange, modET.Precip)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=sd, na.rm=T)
cmip6AggSD$biomeName <- factor(cmip6AggSD$biomeName, levels=biome.order$biomeName)
summary(cmip6AggSD)


ClimSummary <- merge(StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "Tmean.GLDAS", "Precip.GLDAS", "ET.GLDAS")], cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP2-4.5",c("ISOURBID", "tas.diff", "pr.per")], all.x=T)
names(ClimSummary)[names(ClimSummary) %in% c("tas.diff", "pr.per")] <- c("tas.diff.SSP245", "pr.per.SSP245")
ClimSummary <- merge(ClimSummary, cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP5-8.5",c("ISOURBID", "tas.diff", "pr.per")], all.x=T)
names(ClimSummary)[names(ClimSummary) %in% c("tas.diff", "pr.per")] <- c("tas.diff.SSP585", "pr.per.SSP585")
summary(ClimSummary)

ClimSummary$pr.SSP245 <- ClimSummary$Precip.GLDAS*ClimSummary$pr.per.SSP245
ClimSummary$pr.SSP585 <- ClimSummary$Precip.GLDAS*ClimSummary$pr.per.SSP585
ClimSummary$pr.diff.SSP245 <- ClimSummary$Precip.GLDAS - ClimSummary$pr.SSP245
summary(ClimSummary)
median(ClimSummary$Precip.GLDAS[ClimSummary$biomeCode=="Med"], na.rm=T)
median(ClimSummary$pr.diff.SSP245[ClimSummary$biomeCode=="Med"], na.rm=T)
1-median(ClimSummary$pr.per.SSP245[ClimSummary$biomeCode=="Med"], na.rm=T)

# Creating a supplemental table with the climate change stats for each biome
climCurrentMean <- aggregate(cbind(Tmean.GLDAS, Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN = mean)
climCurrentMean[,names(climCurrentMean)[!names(climCurrentMean) %in% c("biomeName", "biomeCode")]] <- round(climCurrentMean[,names(climCurrentMean)[!names(climCurrentMean) %in% c("biomeName", "biomeCode")]], 1)
climCurrentMean

climCurrentSD <- aggregate(cbind(Tmean.GLDAS, Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN = sd)
climCurrentSD[,names(climCurrentSD)[!names(climCurrentSD) %in% c("biomeName", "biomeCode")]] <- round(climCurrentSD[,names(climCurrentSD)[!names(climCurrentSD) %in% c("biomeName", "biomeCode")]], 1)
climCurrentSD

cmip6BiomeMean245 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP2-4.5",], FUN = mean)
cmip6BiomeMean245$tas.diff <- round(cmip6BiomeMean245$tas.diff, 1)
cmip6BiomeMean245$pr.per <- round((cmip6BiomeMean245$pr.per-1)*100, 0)

cmip6BiomeSD245 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP2-4.5",], FUN = sd)
cmip6BiomeSD245$tas.diff <- round(cmip6BiomeSD245$tas.diff, 1)
cmip6BiomeSD245$pr.per <- round((cmip6BiomeSD245$pr.per)*100, 0)

cmip6BiomeMean585 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP5-8.5",], FUN = mean)
cmip6BiomeMean585$tas.diff <- round(cmip6BiomeMean585$tas.diff, 1)
cmip6BiomeMean585$pr.per <- round((cmip6BiomeMean585$pr.per-1)*100, 0)

cmip6BiomeSD585 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP5-8.5",], FUN = sd)
cmip6BiomeSD585$tas.diff <- round(cmip6BiomeSD585$tas.diff, 1)
cmip6BiomeSD585$pr.per <- round((cmip6BiomeSD585$pr.per)*100, 0)

TableS6 <- data.frame(Biome=pasteMeanSD(climCurrentMean$biomeName, climCurrentMean$biomeCode),
                      Tmean.GLDAS = pasteMeanSD(climCurrentMean$Tmean.GLDAS, climCurrentSD$Tmean.GLDAS),
                      Precip.GLDAS = pasteMeanSD(climCurrentMean$Precip.GLDAS, climCurrentSD$Precip.GLDAS),
                      Tmean.diff.245 = pasteMeanSD(cmip6BiomeMean245$tas.diff, cmip6BiomeSD245$tas.diff),
                      Pr.diff.245 = pasteMeanSD(cmip6BiomeMean245$pr.per, cmip6BiomeSD245$pr.per),
                      Tmean.diff.585 = pasteMeanSD(cmip6BiomeMean585$tas.diff, cmip6BiomeSD585$tas.diff),
                      Pr.diff.585 = pasteMeanSD(cmip6BiomeMean585$pr.per, cmip6BiomeSD585$pr.per))
                      
TableS6                      
write.csv(TableS6, file.path(path.figsMS, "TableS6_ClimateStats_CMIP6.csv"), row.names=F)


# # Not a huge difference with median, so lets just roll with mean
# cmip6AggMed <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.Precip)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=median, na.rm=T)
# cmip6AggMed$biomeName <- factor(cmip6AggMed$biomeName, levels=biome.order$biomeName)
# summary(cmip6AggMed)
StatsCombined$modET <- StatsCombined$modET.Base
# cmip6AggMean$modET <- cmip6AggMean$modET

etSummary <- rbind(StatsCombined[,c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET", "modET.Precip")],
                   cmip6AggMean[cmip6AggMean$Time=="2100",c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET", "modET.Precip")])
etSummary$biomeName <- factor(etSummary$biomeName, levels=biome.order$biomeName)
etSummary$biomeCode <- factor(etSummary$biomeCode, levels=rev(biome.order$biomeCode))
etSummary$Scenario <- as.factor(etSummary$Scenario)
summary(etSummary)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 3.1 Main Figure ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Do analyses here!

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
  annotate(geom="text", y=10, x=-3, label=c("Precip Surplus"), hjust=0, size=3) +
  annotate(geom="text", y=10, x=3, label=c("Precip Deficit"), hjust=0, size=3) +
  # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
  scale_fill_manual(values=biome.pall.all) +
  labs(x="log(ET/Precip)", y="Biome") +
  guides(fill="none") +
  theme_bw()
plotRatioLog

png(file.path(path.figsMS, "Figure3_ET_vs_Precip_Now-CMIP6_Log_Combined.png"), height=8, width=14, units="in", res=320)
cowplot::plot_grid(map.ETratio.All, plotRatioLog, ncol=2, rel_widths = c(0.55, 0.45), labels=c("A", "B"))
dev.off()
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 3.2 Supplemental tables & figures breaking down by biome ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
summary(etSummary)
summary(StatsCombined)

#-#-#-#-#-#-#-#-
# 3.2.1. Making a figure showing the spatial patterns of climate & its change ----
#-#-#-#-#-#-#-#-
tasGLDAS <- ggplot(data=StatsCombined[,]) +
  # facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=Tmean.GLDAS), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nTemp. (deg. C)", colors=gradTemp2, n.breaks=13, oob=squish) +
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
tasGLDAS

# prBreaks <- round(quantile(cmip6Agg$pr[cmip6Agg$Scenario=="historical"], seq(0.1, 0.9, by=0.1)), 1)
prGLDAS <- ggplot(data=StatsCombined[,]) +
  # facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=Precip.GLDAS), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=gradPrcp2, n.breaks=13, oob=squish) + 
  # scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=grad.prcp, limits=c(0,10), breaks=c(0.1, 1.5, 2.3, 2.7, 3.7, 4.8, 5.8, 7.1, 9.3), oob=squish) + # Using breaks from IPCC AR6 figures
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
prGLDAS

# figClimPresent <- cowplot:: plot_grid(tasGLDAS, prGLDAS, labels=c("A", "B"), nrow=1)

tasFuture <- ggplot(data=cmip6AggMean[cmip6AggMean$Time==2100,]) +
  facet_grid(.~Scenario)+
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
tasFuture

prFuture <- ggplot(data=cmip6AggMean[cmip6AggMean$Time==2100,]) +
  facet_grid(.~Scenario) +
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
prFuture


png(file.path(path.figsMS, "FigureS6_Climate_GLDAS_CMIP6-EnsembleMeans.png"), height=6, width=14, units="in", res=320)
cowplot::plot_grid(tasGLDAS, prGLDAS, tasFuture, prFuture, ncol=2, rel_heights = c(0.45, 0.65), labels=c("A", "B", "C", "D"))
dev.off()
#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-
# 3.2.2. Summarizing the climate & ET change by biome ----
#-#-#-#-#-#-#-#-
# Adding a figure shoing chang in ET
mapETcurrent <- ggplot(data=StatsCombined[,]) +
  # facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=modET.Base), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nET (mm/day)", colors=gradPrcp2, n.breaks=13, oob=squish) + 
  # scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=grad.prcp, limits=c(0,10), breaks=c(0.1, 1.5, 2.3, 2.7, 3.7, 4.8, 5.8, 7.1, 9.3), oob=squish) + # Using breaks from IPCC AR6 figures
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
mapETcurrent

mapETfuture <- ggplot(data=cmip6AggMean[cmip6AggMean$Time==2100,]) +
  facet_grid(.~Scenario) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=(modET.perChange-1)*100), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="ET Change\n (%)", colors=grad.prcp, limits=c(-0.8, 0.8)*100, n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
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
mapETfuture

png(file.path(path.figsMS, "FigureS7_ET-ETchange_current-CMIP6.png"), height=6, width=9, units="in", res=320)
cowplot::plot_grid(mapETcurrent, mapETfuture, ncol=1, rel_heights = c(0.45, 0.55), labels=c("A", "B"))
dev.off()





violinTas <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=tas.diff, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  coord_cartesian(ylim=c(0, max(cmip6AggMean$tas.diff))) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="Temperature Change (deg. C)") +
  theme_bw()
violinTas

violinPr <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=(pr.per-1)*100, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  coord_cartesian(ylim=c(-1, 1.25)*100) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="Precip Change (%)") +
  theme_bw()
violinPr

violinET <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=modET, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  # coord_cartesian(ylim=c(-1, 1.25)*100) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="ET (mm/day)") +
  theme_bw()
violinET

violinETper <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=(modET.perChange-1)*100, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  # coord_cartesian(ylim=c(-1, 1.25)*100) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="ET Change (%)") +
  theme_bw()
violinETper

summary(cmip6AggMean)

png(file.path(path.figsMS, "FigureS8_Climate-ET-Change_Biomes.png"), height=8, width=8, units="in", res=320)
cowplot::plot_grid(violinTas, violinPr, violinETper, labels=c("A", "B", "C"), ncol=1)
dev.off()


names(StatsCombined)
# We need baseline N Cities; ET; % cities with water risk
etBiomeAggMean <- aggregate(modET ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)
etBiomeAggMean$ET.sd <- aggregate(modET ~ biomeName + biomeCode, data=StatsCombined, FUN=sd)$modET
etBiomeAggMean[,c("modET", "ET.sd")] <- round(etBiomeAggMean[,c("modET", "ET.sd")], 2)
etBiomeAggMean$N.Cities <- aggregate(modET ~ biomeName + biomeCode, data=StatsCombined, FUN=length)$modET
etBiomeAggMean$N.Cities.Risk <- aggregate(modET.Precip ~ biomeName + biomeCode, data=StatsCombined, FUN=function(x){length(which(x>1))})$modET.Precip
etBiomeAggMean

# We'll need % change in ET; % cities drying; # cities with water risk; % cities with water risk
changeBiomeAggMedian <- aggregate(cbind(pr.per, modET, modET.perChange) ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=median)
changeBiomeAggMedian

changeBiomeAggMean <- aggregate(cbind(pr.per, modET, modET.perChange) ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=mean)
changeBiomeAggMean$modET <- round(changeBiomeAggMean$modET, 2)
changeBiomeAggMean[,c("pr.per", "modET.perChange")] <- round(changeBiomeAggMean[,c("pr.per", "modET.perChange")]-1,2)*100
changeBiomeAggMean$N.Cities.Dry <- aggregate(pr.per ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=function(x){length(which(x<1))})$pr.per
changeBiomeAggMean$N.Cities.Risk <- aggregate(modET.Precip ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=function(x){length(which(x>1))})$modET.Precip

changeBiomeAggSD <- aggregate(cbind(pr.per, modET, modET.perChange) ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=sd)
changeBiomeAggSD$modET <- round(changeBiomeAggSD$modET, 2)
changeBiomeAggSD[,c("pr.per", "modET.perChange")] <- round(changeBiomeAggSD[,c("pr.per", "modET.perChange")],2)*100


TableS7 <- data.frame(Biome=pasteMeanSD(etBiomeAggMean$biomeName, etBiomeAggMean$biomeCode),
                      N.Cities = etBiomeAggMean$N.Cities,
                      ET.current = pasteMeanSD(etBiomeAggMean$modET, etBiomeAggMean$ET.sd),
                      current.WaterRisk = paste0(round(etBiomeAggMean$N.Cities.Risk/etBiomeAggMean$N.Cities*100,0),"%"),
                      SSP245.ETchange.per = pasteMeanSD(changeBiomeAggMean$modET.perChange[changeBiomeAggMean$Scenario=="SSP2-4.5"],
                                                        changeBiomeAggSD$modET.perChange[changeBiomeAggMean$Scenario=="SSP2-4.5"]),
                      SSP245.CitiesDry.per = paste0(round(changeBiomeAggMean$N.Cities.Dry[changeBiomeAggMean$Scenario=="SSP2-4.5"]/etBiomeAggMean$N.Cities, 2)*100, "%"),
                      SSP245.CitiesRisk.per = paste0(round(changeBiomeAggMean$N.Cities.Risk[changeBiomeAggMean$Scenario=="SSP2-4.5"]/etBiomeAggMean$N.Cities, 2)*100, "%"),
                      SSP585.ETchange.per = pasteMeanSD(changeBiomeAggMean$modET.perChange[changeBiomeAggMean$Scenario=="SSP5-8.5"],
                                                        changeBiomeAggSD$modET.perChange[changeBiomeAggMean$Scenario=="SSP5-8.5"]),
                      SSP585.CitiesDry.per = paste0(round(changeBiomeAggMean$N.Cities.Dry[changeBiomeAggMean$Scenario=="SSP5-8.5"]/etBiomeAggMean$N.Cities, 2)*100, "%"),
                      SSP585.CitiesRisk.per = paste0(round(changeBiomeAggMean$N.Cities.Risk[changeBiomeAggMean$Scenario=="SSP5-8.5"]/etBiomeAggMean$N.Cities, 2)*100, "%")
                      )
TableS7

write.csv(TableS7, file.path(path.figsMS, "TableS7_ET-Risk_CMIP6.csv"), row.names=F)

# Getting stats on percent changes in ET
changeBiomeAggMedian
median(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5"])
median(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5"])

median(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5"])-median(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5"])

# Getting Stats on number of cities drying
sum(changeBiomeAggMean$N.Cities.Dry[changeBiomeAggMean$Scenario=="SSP2-4.5"])
sum(changeBiomeAggMean$N.Cities.Dry[changeBiomeAggMean$Scenario=="SSP5-8.5"])

# Getting Stats on number of cities with summer precip deficit
sum(changeBiomeAggMean$N.Cities.Risk[changeBiomeAggMean$Scenario=="SSP2-4.5"])/sum(etBiomeAggMean$N.Cities)
sum(changeBiomeAggMean$N.Cities.Risk[changeBiomeAggMean$Scenario=="SSP5-8.5"])/sum(etBiomeAggMean$N.Cities)

# Looking at change in % cities with risk
cbind(TableS7$Biome, as.numeric(gsub("%", "", TableS7$SSP245.CitiesRisk.per)) - as.numeric(gsub("%", "", TableS7$current.WaterRisk)))
cbind(TableS7$Biome, as.numeric(gsub("%", "", TableS7$SSP585.CitiesRisk.per)) - as.numeric(gsub("%", "", TableS7$current.WaterRisk)))

#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
