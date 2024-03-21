# Updated script for manuscript
# Outline
# 01. Trees cool cities & need water to do so: ---- 
#     Trees have a clear, consistent cooling potential on global urban surface temperatures… [treat cooling capacity as a known; we add a tiny bit of nuance; quickly bring in nuance & water]
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
library(ggplot2); library(cowplot)

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
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


# Creating a combined ET data frame
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

cmip6AggMean <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.Precip)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=mean, na.rm=T)
cmip6AggMean$biomeName <- factor(cmip6AggMean$biomeName, levels=biome.order$biomeName)
summary(cmip6AggMean)

etSummary <- rbind(cityAnalyStats[,c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET.Precip")],
                   cmip6AggMean[cmip6AggMean$Time=="2100",c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET.Precip")])
etSummary$biomeName <- factor(etSummary$biomeName, levels=biome.order$biomeName)
etSummary$biomeCode <- factor(etSummary$biomeCode, levels=rev(biome.order$biomeCode))

summary(etSummary)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
# Key Results: (What Christy needs to get numbers for)
#  3.0. SUPPLEMENT Figure: The amount of warming & precipitation varies across the globe; this is CMIP6, not us, so no need to put in main MS
#. 3.1. Warming will cause an X% increase in ET; X% of cities will see precip not keep pace with this, resulting in XX% of cities 
#  3.2. Although we estimate XX biomes to have the greatest proportion of cities in a canopy water deficit, the biggest shift in the distribution is in temperate forest biomes, particularly cities in Europe and the US

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
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
