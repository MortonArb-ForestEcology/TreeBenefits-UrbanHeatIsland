# Updated script for manuscript
# Outline
# 01. Trees cool cities & need water to do so: ---- 
#     Trees have a clear, consistent cooling potential on global urban surface temperatures… [treat cooling capacity as a known; we add a tiny bit of nuance; quickly bring in nuance & water]
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
library(ggplot2)

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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 01. Trees cool cities & need water to do so: ----
#  Trees have a clear, consistent cooling potential on global urban surface temperatures… [treat cooling capacity as a known; we add a tiny bit of nuance; quickly bring in nuance & water]
# 01. Key Results: (What Christy needs to get numbers for) 
#  1.1. Average cooling per percent tree cover of urban trees varies among biomes 
#       -- Trees always do more than non-tree vegetation per percent tree cover
#  1.2. Trees in arid and semi-arid regions do more per percent tree cover but regions that have more trees in their cities have trees doing more total cooling
#       -- How much water is needed to do the cooling varies by biome & relationship
#       -- Our estimates are lower than those produced by other gridded products because we interpolate into the metropolitan core where tree cover is lower, but shows consistent trends ;  
#           -- (SUPPLEMENT Figs/Tables: comparison with gldas)
#       -- The relationships between water use and tree canopy are non-linear
#  1.3. In terms of current peak summer water use vs. output, XXX% of cities receive more water than our 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Do analyses here!


# 
UHIquint <- quantile(StatsCombined$value.LST.diff[StatsCombined$value.LST.diff>0], seq(0.2, 1, by=0.2))
breaksUHI <- c(-rev(UHIquint), 0, UHIquint)
names(breaksUHI) <- round(breaksUHI, 1)

breaksVeg <- seq(0, max(c(StatsCombined$value.tree.core, StatsCombined$value.other.core)), length.out=length(grad.tree))
breaksTree <- c(0, round(quantile(StatsCombined$value.tree.core, seq(0.2, 1, length.out=length(grad.tree))), 0))
breaksOther <- c(0, round(quantile(StatsCombined$value.other.core, seq(0.2, 1, length.out=length(grad.other))), 0))

mapUHI <- ggplot(data=StatsCombined[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=value.LST.diff), size=0.25, alpha=0.8) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_stepsn(colors=grad.lst, breaks=breaksUHI) +
  labs(color="LST Diff.\n(˚C)") +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

png(file.path(path.figsExplore, "Map_UHI.png"), height=8, width=10, units="in", res=320)
mapUHI
dev.off()


mapTree <- ggplot(data=StatsCombined[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=value.tree.core), size=0.25) +
  scale_color_stepsn(name="Tree\nCover (%)", colors=grad.tree, breaks=seq(0, 90, by=10)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))

png(file.path(path.figsExplore, "Map_CoverTree.png"), height=8, width=10, units="in", res=320)
mapTree
dev.off()


# Plotting the temperature partial effect  ----
nInterval = 50

lstEffectList <- list()
for(i in 1:nrow(StatsCombined)){
  dfNow <- data.frame(ISOURBID=StatsCombined$ISOURBID[i], cover.tree=seq(StatsCombined$tree.min[i], StatsCombined$tree.max[i], length.out=nInterval), cover.veg=seq(StatsCombined$veg.min[i], StatsCombined$veg.max[i], length.out=nInterval))
  dfNow$effectLST.tree <- dfNow$cover.tree*StatsCombined$LSTmodel.tree.slope[i]
  dfNow$effectLST.veg <- dfNow$cover.veg*StatsCombined$LSTmodel.veg.slope[i]
  
    
  lstEffectList[[StatsCombined$ISOURBID[i]]] <- dfNow
}
dfEffectLST <- data.table::rbindlist(lstEffectList)
dfEffectLST <- merge(dfEffectLST, StatsCombined[,c("ISOURBID", "biomeName", "biomeCode")], all.x=T, all.y=F)
summary(dfEffectLST)


coolingBiomeTreeVeg <- ggplot(data=dfEffectLST, aes()) +
  facet_wrap(~biomeName) +
  coord_cartesian(ylim=c(-10,5)) +
  geom_line(aes(x=cover.tree, y=effectLST.tree, color="Tree", group=ISOURBID), alpha=0.2, linewidth=0.1)  +
  geom_line(aes(x=cover.veg, y=effectLST.veg, color="Other Vegetation", group=ISOURBID), alpha=0.2, linewidth=0.1)  +
  geom_smooth(method="lm", aes(x=cover.tree, y=effectLST.tree, color="Tree")) +
  geom_smooth(method="lm", aes(x=cover.veg, y=effectLST.veg, color="Other Vegetation")) +
  labs(x="Cover (%)", y="Effect on LST (deg. C)") +
  scale_color_manual(values=c("Tree"="green4", "Other Vegetation"="dodgerblue2")) + 
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figsExplore, "LSTmodel_PartialEffects_CoverTree-CoverVeg.png"), height=8, width=10, units="in", res=320)
coolingBiomeTreeVeg
dev.off()


coolingTreeSummary <- ggplot(data=dfEffectLST, aes()) +
  coord_cartesian(ylim=c(-10,1)) +
  geom_smooth(method="lm", aes(x=cover.tree, y=effectLST.tree, color=biomeName, fill=biomeName)) +
  labs(x="Tree Cover (%)", y="Effect on LST (deg. C)") +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  theme_bw() 

png(file.path(path.figsExplore, "LSTmodel_PartialEffects_CoverTree_Summary.png"), height=8, width=10, units="in", res=320)
coolingTreeSummary
dev.off()

coolingVegSummary <- ggplot(data=dfEffectLST, aes()) +
  coord_cartesian(ylim=c(-3,1)) +
  geom_smooth(method="lm", aes(x=cover.veg, y=effectLST.veg, color=biomeName, fill=biomeName)) +
  labs(x="Other Vegetation Cover (%)", y="Effect on LST (deg. C)") +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  theme_bw() 

png(file.path(path.figsExplore, "LSTmodel_PartialEffects_CoverVeg_Summary.png"), height=8, width=10, units="in", res=320)
coolingVegSummary
dev.off()


splineTree <-  readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree_ET-units.rds"))
dfSplineTree <- data.table::rbindlist(splineTree)
dfSplineTree <- merge(dfSplineTree, StatsCombined[,c("ISOURBID", "biomeName", "biomeCode")], all.x=F, all.y=T)
summary(dfSplineTree)

etBiomeTree <- ggplot(data=dfSplineTree, aes()) +
  facet_wrap(~biomeName) +
  coord_cartesian(ylim=c(0,5)) +
  geom_line(aes(x=cover.tree, y=ET, group=ISOURBID), alpha=0.2, linewidth=0.1)  +
  geom_smooth(method="gam", aes(x=cover.tree, y=ET, color=biomeName)) +
  labs(x="Cover (%)", y="ET (mm/day)") +
  scale_color_manual(values=biome.pall.all) + 
  guides(color="none") +
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figsExplore, "ETmodel_PartialEffects_CoverTree.png"), height=8, width=10, units="in", res=320)
etBiomeTree
dev.off()


etTreeSummary <- ggplot(data=dfSplineTree, aes()) +
  coord_cartesian(ylim=c(0,5)) +
  geom_smooth(method="gam", aes(x=cover.tree, y=ET, color=biomeName, fill=biomeName)) +
  labs(x="Tree Cover (%)", y="ET (mm/day)") +
  scale_color_manual(name="Biome", values=biome.pall.all) + 
  scale_fill_manual(name="Biome", values=biome.pall.all) + 
  theme_bw() 

png(file.path(path.figsExplore, "ETmodel_PartialEffects_CoverTree_Summary.png"), height=8, width=10, units="in", res=320)
etTreeSummary
dev.off()

plotMaps <- cowplot::plot_grid(mapUHI, mapTree, nrow=1, labels=c("A", "B"))
plotPartialEffects <- cowplot::plot_grid(coolingTreeSummary, etTreeSummary+guides(color="none", fill="none"), labels=c("C", "D"), nrow=1, rel_widths = c(0.67,0.33))

png(file.path(path.figsMS, "Fig1_Current_UHI-Tree-Cool-Water.png"), height=8, width=10, units="in", res=320)
cowplot::plot_grid(plotMaps, plotPartialEffects, nrow=2)
dev.off()
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


