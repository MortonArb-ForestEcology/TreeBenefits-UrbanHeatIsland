# Updated script for manuscript
# Outline
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
library(ggplot2); library(cowplot)
library(ggpmisc)

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
path.cities <- file.path(path.google, "data_processed_final")
path.tower <- file.path(path.google, "../ET Validation")

path.figsMS <- file.path(path.google, "figures_manuscript")
path.figsExplore <- file.path(path.google, "figures_exploratory")
dir.create(path.figsMS, recursive=T, showWarnings=F)
path.MS <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Submission 5 - Nature Climate Change")


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

length(unique(StatsCombined$ISOURBID))
length(unique(StatsCombined$biomeName))

aggTower <- read.csv(file.path(path.tower, "FluxTower_ETcomparison_AllTowers-Aggregated.csv"))
summary(aggTower)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# FIGURES ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 
# cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)

biome.hist <- ggplot(data=StatsCombined) +
  geom_bar(aes(x=biomeName, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(biome.order$n.cities)*1.05)) +
  scale_x_discrete(name="Biome", labels=paste0(biome.order$biomeName, " (", biome.order$biomeCode, ")")) +
  guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle=-30, hjust=0),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"), 
        plot.margin = margin(1, 4.5, 0.5, 1, "lines"))

biome.map <- ggplot(data=StatsCombined[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=biomeName), size=0.5) +
  scale_color_manual(name="biome", values=biome.pall.all) +
  # scale_shape_manual(name="biome", values=1:length(biome.pall.all)) +
  guides(color="none") +
  # theme_bw() +
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

# biome.map
png(file.path(path.figsMS, "Figure1_CityDistribution_Biomes.png"), height=180, width=180, units="mm", res=300)
plot_grid(biome.map, biome.hist, ncol=1, rel_heights = c(0.45, 0.55), labels=c("A", "B"))
dev.off()

pdf(file.path(path.MS, "Figure1_CityDistribution_Biomes.pdf"), height=7, width=7)
plot_grid(biome.map, biome.hist, ncol=1, rel_heights = c(0.45, 0.55), labels=c("A", "B"))
dev.off()




 mapLSTR2 <- ggplot(data=StatsCombined[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=LSTmodelFinal.R2adj), size=0.25, alpha=0.8) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_stepsn(colors=grad.modfit, limits=c(0,1)) +
  labs(color="LST model\nadj. R2") +
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

mapETR2 <- ggplot(data=StatsCombined[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=ETmodel.R2adj), size=0.25, alpha=0.8) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_color_stepsn(colors=grad.modfit, limits=c(0,1)) +
  labs(color="ET model\nadj. R2") +
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


png(file.path(path.figsMS, "FigureS1_Map_ModelR2_LST-ET.png"), height=10, width=8, units="in", res=320)
cowplot::plot_grid(mapLSTR2, mapETR2, ncol=1, labels=c("A", "B"))
dev.off()

pdf(file.path(path.MS, "FigureS1_Map_ModelR2_LST-ET.pdf"), height=10, width=8)
cowplot::plot_grid(mapLSTR2, mapETR2, ncol=1, labels=c("A", "B"))
dev.off()


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
  dfNow$effectLST.tree <- dfNow$cover.tree*StatsCombined$LSTslope.tree[i]
  dfNow$effectLST.veg <- dfNow$cover.veg*StatsCombined$LSTslope.veg[i]
  
    
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
  scale_color_manual(name="Vegetation Type", values=c("Tree"="green4", "Other Vegetation"="dodgerblue2")) + 
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figsMS, "FigureS2_LSTmodel_PartialEffects_CoverTree-CoverVeg.png"), height=8, width=10, units="in", res=320)
coolingBiomeTreeVeg
dev.off()

pdf(file.path(path.MS, "FigureS2_LSTmodel_PartialEffects_CoverTree-CoverVeg.pdf"), height=8, width=10)
coolingBiomeTreeVeg
dev.off()

coolingTreeSummary <- ggplot(data=dfEffectLST, aes()) +
  # coord_cartesian(ylim=c(-10,1)) +
  geom_smooth(method="lm", aes(x=cover.tree, y=effectLST.tree, color=biomeCode, fill=biomeCode)) +
  labs(x="Tree Cover (%)", y="Effect on LST (deg. C)") +
  scale_color_manual(name="Biome", values=biomeCode.pall.all) + 
  scale_fill_manual(name="Biome", values=biomeCode.pall.all) + 
  theme_bw() 

png(file.path(path.figsExplore, "LSTmodel_PartialEffects_CoverTree_Summary.png"), height=8, width=10, units="in", res=320)
coolingTreeSummary
dev.off()

coolingVegSummary <- ggplot(data=dfEffectLST, aes()) +
  coord_cartesian(ylim=c(-3,1)) +
  geom_smooth(method="lm", aes(x=cover.veg, y=effectLST.veg, color=biomeCode, fill=biomeCode)) +
  labs(x="Other Vegetation Cover (%)", y="Effect on LST (deg. C)") +
  scale_color_manual(name="Biome", values=biomeCode.pall.all) + 
  scale_fill_manual(name="Biome", values=biomeCode.pall.all) + 
  theme_bw() 

png(file.path(path.figsExplore, "LSTmodel_PartialEffects_CoverVeg_Summary.png"), height=8, width=10, units="in", res=320)
coolingVegSummary
dev.off()


splineTree <-  readRDS(file=file.path(path.google, "ETModel_Spline_PartialEffects_CoverTree_ET-units.rds"))
dfSplineTree <- data.table::rbindlist(splineTree)
dfSplineTree <- merge(dfSplineTree, StatsCombined[,c("ISOURBID", "biomeName", "biomeCode")], all.x=F, all.y=T)
summary(dfSplineTree)

etBiomeTree <- ggplot(data=dfSplineTree, aes()) +
  facet_wrap(~biomeName, ncol=3) +
  coord_cartesian(ylim=c(0,5)) +
  geom_line(aes(x=cover.tree, y=ET, group=ISOURBID), alpha=0.2, linewidth=0.1)  +
  geom_smooth(method="gam", aes(x=cover.tree, y=ET, color=biomeCode)) +
  labs(x="Cover (%)", y="ET (mm/day)") +
  scale_color_manual(values=biomeCode.pall.all) + 
  guides(color="none") +
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figsMS, "FigureS3_ETmodel_PartialEffects_CoverTree.png"), height=10, width=8, units="in", res=320)
etBiomeTree
dev.off()

pdf(file.path(path.MS, "FigureS3_ETmodel_PartialEffects_CoverTree.pdf"), height=10, width=8)
etBiomeTree
dev.off()


etTreeSummary <- ggplot(data=dfSplineTree, aes()) +
  coord_cartesian(ylim=c(0,5)) +
  geom_smooth(method="gam", aes(x=cover.tree, y=ET, color=biomeCode, fill=biomeCode)) +
  labs(x="Tree Cover (%)", y="ET (mm/day)") +
  scale_color_manual(name="Biome", values=biomeCode.pall.all) + 
  scale_fill_manual(name="Biome", values=biomeCode.pall.all) + 
  theme_bw() 

png(file.path(path.figsExplore, "ETmodel_PartialEffects_CoverTree_Summary.png"), height=8, width=10, units="in", res=320)
etTreeSummary
dev.off()

plotMaps <- cowplot::plot_grid(mapUHI, mapTree, nrow=1, labels=c("A", "B"))
plotPartialEffects <- cowplot::plot_grid(coolingTreeSummary, etTreeSummary+guides(color="none", fill="none"), labels=c("C", "D"), nrow=1, rel_widths = c(0.55,0.45))

png(file.path(path.figsMS, "Figure2_Current_UHI-Tree-Cool-Water.png"), height=8, width=11, units="in", res=320)
cowplot::plot_grid(plotMaps, plotPartialEffects, nrow=2, rel_heights = c(0.5, 0.5))
dev.off()

pdf(file.path(path.MS, "Figure2_Current_UHI-Tree-Cool-Water.pdf"), height=8, width=11)
cowplot::plot_grid(plotMaps, plotPartialEffects, nrow=2, rel_heights = c(0.5, 0.5))
dev.off()


# Looking at the fluxtower validation
aggTower <- merge(aggTower, StatsCombined[,c("ISOURBID", "biomeName", "biomeCode")], all.x=T, all.y=F)
summary(aggTower)


aggTowerStack <- stack(aggTower[,c("RMSE.pixel", "RMSE.modis", "RMSE.gldas")])
names(aggTowerStack) <- c("RMSE", "dataset")
aggTowerStack[,c("ISOURBID","biomeName","biomeCode", "IGBP")] <- aggTower[,c("ISOURBID", "biomeName","biomeCode", "IGBP")]
# aggTowerStack$R2 <- stack(aggTower[,c("R2.pixel", "R2.modis", "R2.gldas")])[,"values"]
aggTowerStack$dataset <- car::recode(aggTowerStack$dataset, "'RMSE.pixel'='Model'; 'RMSE.modis'='MODIS'; 'RMSE.gldas'='GLDAS'")
aggTowerStack$dataset <- factor(aggTowerStack$dataset, levels=c("Model", "MODIS", "GLDAS"))
summary(aggTowerStack)

# aggTowerStack2 <- stack(aggTowerStack[,c("RMSE", "R2")])
# aggTowerStack2[,c("dataset", "biomeName", "biomeCode", "IGBP", "ISOURBID")] <- aggTowerStack[,c("dataset", "biomeName", "biomeCode", "IGBP", "ISOURBID")]
# summary(aggTowerStack2)

# valMeans <- aggregate(values ~ dataset + ind, data=aggTowerStack2, FUN=mean, na.rm=T)

ggplot(data=aggTowerStack) +
  facet_wrap(~dataset) +
  geom_histogram(aes(x=RMSE, fill=IGBP)) +
  # geom_vline(data=valMeans, aes(xintercept=values), linetype="dashed", linewidth=1.5) +
  scale_fill_manual(values=c("WET" = "#6c9fb8", "EBF" = "#1c5f2c", "DBF" = "#68ab5f", "MF" = "#b5c58f", "OSH" = "#ccb879", "GRA" = "#dfdfc2", "CRO" = "#ab6c28", "URB" = "#eb0000"),
                    labels=c("Wetland", "Forest, Evg. Broad.", "Forest, Dec. Broad", "Forest, Mixed", "Shrubland", "Grassland", "Cropland", "Urban")) +
  theme_bw()

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

plot.model <- ggplot(data=aggTower, aes(x=ET.pixel, y=ET)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = T) + # Line of best fit
  geom_point(aes(color=biomeCode)) +
  scale_color_manual(name="Biome Code", values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(aggTower$ET)+c(0,1))+
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) + # Display equation and R^2
  guides(color="none") +
  annotate(
    "text",
    x = min(aggTower$ET.pixel),
    y = max(aggTower$ET)-0.5,
    label = paste0("RMSE: ", round(rmse(aggTower$ET, predict(lm(ET ~ ET.pixel, aggTower))), 2)),
    hjust = 0,
    color = "black"
  ) + # Display RMSE
  labs(
    x = "Model ET (mm/day)",
    y = "Tower ET (mm/day)"
  ) +
  theme_bw()

plot.modis <- ggplot(data=aggTower, aes(x=ET.modis, y=ET)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = T) + # Line of best fit
  geom_point(aes(color=biomeCode)) +
  scale_color_manual(name="Biome Code", values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(aggTower$ET)+c(0,1))+
  guides(color="none") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) + # Display equation and R^2
  annotate(
    "text",
    x = min(aggTower$ET.modis, na.rm=T),
    y = max(aggTower$ET)-0.5,
    label = paste0("RMSE: ", round(rmse(aggTower$ET[!is.na(aggTower$ET.modis)], predict(lm(ET ~ ET.modis, aggTower[!is.na(aggTower$ET.modis),]))), 2)),
    hjust = 0,
    color = "black"
  ) + # Display RMSE
  labs(
    x = "MODIS ET (mm/day)",
    y = "Tower ET (mm/day)"
  ) +
  theme_bw()

plot.gldas <- ggplot(data=aggTower, aes(x=ET.gldas, y=ET)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = T) + # Line of best fit
  geom_point(aes(color=biomeCode)) +
  scale_color_manual(name="Biome Code", values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(aggTower$ET)+c(0,1))+
  guides(color="none") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) + # Display equation and R^2
  annotate(
    "text",
    x = min(aggTower$ET.gldas),
    y = max(aggTower$ET)-0.5,
    label = paste0("RMSE: ", round(rmse(aggTower$ET, predict(lm(ET ~ ET.gldas, aggTower))), 2)),
    hjust = 0,
    color = "black"
  ) + # Display RMSE
  labs(
    x = "GLDAS ET (mm/day)",
    y = "Tower ET (mm/day)"
  ) +
  theme_bw()

plot.modelVgldas <- ggplot(data=aggTower, aes(x=ET.pixel, y=ET.gldas)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = T) + # Line of best fit
  geom_point(aes(color=biomeCode)) +
  scale_color_manual(name="Biome Code", values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(aggTower$ET)+c(0,1))+
  guides(color="none") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) + # Display equation and R^2
  annotate(
    "text",
    x = min(aggTower$ET.pixel),
    y = max(aggTower$ET)-0.5,
    label = paste0("RMSE: ", round(rmse(aggTower$ET.gldas, predict(lm(ET.gldas ~ ET.pixel, aggTower))), 2)),
    hjust = 0,
    color = "black"
  ) + # Display RMSE
  labs(
    x = "Model ET (mm/day)",
    y = "GLDAS ET (mm/day)"
  ) +
  theme_bw()

plot.modelVgldas2 <- ggplot(data=StatsCombined, aes(x=ETpred.mean, y=ET.GLDAS)) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_point(aes(color=biomeCode)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = T) + # Line of best fit
  scale_color_manual(name="Biome Code", values=biomeCode.pall.all) +
  scale_y_continuous(limits=range(StatsCombined$ET.GLDAS)+c(0,1))+
  guides(color="none") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE
  ) + # Display equation and R^2
  annotate(
    "text",
    x = min(StatsCombined$ETpred.mean),
    y = max(StatsCombined$ET.GLDAS)-0.5,
    label = paste0("RMSE: ", round(rmse(StatsCombined$ET.GLDAS, predict(lm(ET.GLDAS ~ ETpred.mean, StatsCombined))), 2)),
    hjust = 0,
    color = "black"
  ) + # Display RMSE
  labs(
    x = "Model ET (mm/day)",
    y = "GLDAS ET (mm/day)"
  ) +
  theme_bw()

plot.model
plot.modis
plot.gldas
plot.modelVgldas
plot.modelVgldas2

png(file.path(path.figsMS, "FigureS4_ETmodel_ValidationSummaries.png"), height=8, width=8, units="in", res=320)
cowplot::plot_grid(plot.model, plot.gldas, plot.modis, plot.modelVgldas, plot.modelVgldas2,  ncol=3, labels=c("A", "B", "C", "D",  "E"))
dev.off()


pdf(file.path(path.MS, "FigureS4_ETmodel_ValidationSummaries.pdf"), height=8, width=8)
cowplot::plot_grid(plot.model, plot.gldas, plot.modis, plot.modelVgldas, plot.modelVgldas2,  ncol=3, labels=c("A", "B", "C", "D",  "E"))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# NUMBERS! ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Supplemental Table 2 ----
# Biome, Number Cities Analyzed, Mean/SD Temp; Mean/SD Tree Cover; mean/sd LST diff; mean/sd TreeDiff
tableS3N <- aggregate(value.LST.core~biomeName + biomeCode, data=StatsCombined, FUN=length)
names(tableS3N)[3] <- "N Cities"

tableS3Means <- aggregate(cbind(value.LST.core, value.tree.core, value.LST.diff, value.tree.diff)~biomeName + biomeCode, data=StatsCombined, FUN=mean)
tableS3Means[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")] <- round(tableS3Means[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")],2)

tableS3SDs <- aggregate(cbind(value.LST.core, value.tree.core, value.LST.diff, value.tree.diff)~biomeName + biomeCode, data=StatsCombined, FUN=sd)
tableS3SDs[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")] <- round(tableS3SDs[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")],2)

TableS3 <- data.frame(Biome=paste0(tableS3N$biomeName, " (", tableS3N$biomeCode, ")"),
                      N.Cities = tableS3N$`N Cities`,
                      LST = paste0(tableS3Means$value.LST.core, " (", tableS3SDs$value.LST.core, ")"),
                      TreeCover = paste0(tableS3Means$value.tree.core, " (", tableS3SDs$value.tree.core, ")"),
                      LST.Diff = paste0(tableS3Means$value.LST.diff, " (", tableS3SDs$value.LST.diff, ")"),
                      Tree.Diff = paste0(tableS3Means$value.tree.diff, " (", tableS3SDs$value.tree.diff, ")"))
TableS3
write.csv(TableS3, file.path(path.figsMS, "TableS3_Biome_LST-Tree_Stats.csv"), row.names=F)

# Supplemental Table3 ----
# LSTmdoel R2, ET modelR2; Tree Cooling; Mean/SD Current ET (); 
names(StatsCombined)

StatsCombined$TreeCool <- StatsCombined$LSTslope.tree*StatsCombined$value.tree.core

tableS4Means <- aggregate(cbind(LSTmodelFinal.R2adj, LSTmodelFinal.RMSE, ETmodel.R2adj, ETmodel.RMSE, LSTslope.tree, LSTslope.veg, TreeCool, ETpred.mean)~biomeName + biomeCode, data=StatsCombined, FUN=mean)
tableS4Means[,c("LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE", "ETmodel.R2adj", "ETmodel.RMSE", "LSTslope.tree", "LSTslope.veg", "TreeCool", "ETpred.mean")] <- round(tableS4Means[,c("LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE", "ETmodel.R2adj", "ETmodel.RMSE", "LSTslope.tree", "LSTslope.veg", "TreeCool", "ETpred.mean")],2)

tableS4SDs <- aggregate(cbind(LSTmodelFinal.R2adj, LSTmodelFinal.RMSE, ETmodel.R2adj, ETmodel.RMSE, LSTslope.tree, LSTslope.veg, TreeCool, ETpred.mean)~biomeName + biomeCode, data=StatsCombined, FUN=sd)
tableS4SDs[,c("LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE", "ETmodel.R2adj", "ETmodel.RMSE", "LSTslope.tree", "LSTslope.veg", "TreeCool", "ETpred.mean")] <- round(tableS4SDs[,c("LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE", "ETmodel.R2adj", "ETmodel.RMSE", "LSTslope.tree", "LSTslope.veg", "TreeCool", "ETpred.mean")],2)

TableS4 <- data.frame(Biome=paste0(tableS4Means$biomeName, " (", tableS4Means$biomeCode, ")"),
                      LSTmodel.R2 = paste0(tableS4Means$LSTmodelFinal.R2adj, " (", tableS4SDs$LSTmodelFinal.R2adj, ")"),
                      LSTmodel.RMSE = paste0(tableS4Means$LSTmodelFinal.RMSE, " (", tableS4SDs$LSTmodelFinal.RMSE, ")"),
                      ETmodel.R2 = paste0(tableS4Means$ETmodel.R2adj, " (", tableS4SDs$ETmodel.R2adj, ")"),
                      ETmodel.RMSE = paste0(tableS4Means$ETmodel.RMSE, " (", tableS4SDs$ETmodel.RMSE, ")"),
                      LSTmodel.TreeSlope  = paste0(tableS4Means$LSTslope.tree, " (", tableS4SDs$LSTslope.tree, ")"),
                      LSTmodel.VegSlope  = paste0(tableS4Means$LSTslope.veg, " (", tableS4SDs$LSTslope.veg, ")"),
                      TreeCool.degC = paste0(tableS4Means$TreeCool, " (", tableS4SDs$TreeCool, ")"),
                      ET.mmDay = paste0(tableS4Means$ETpred.mean, " (", tableS4SDs$ETpred.mean, ")"))
TableS4
write.csv(TableS4, file.path(path.figsMS, "TableS4_Biome_LST-ET_ModelStats.csv"), row.names=F)


#-#-#-#-#-#-#-
# Getting some specific numbers for cooling comparisons
#-#-#-#-#-#-#-
#  How much trees cool cities globally on average
mean(StatsCombined$value.LST.diff); sd(StatsCombined$value.LST.diff); median(StatsCombined$value.LST.diff) #UHIs
# Tree cooling
mean(StatsCombined$TreeCool); sd(StatsCombined$TreeCool); 
median(StatsCombined$TreeCool); quantile(StatsCombined$TreeCool, 0.025, 0.95) # Tree Cooling 
# Calculate a bootstrapped median 
bootTreeCool <- vector(length=1000)
set.seed(1221)
for(i in 1:length(bootTreeCool)){
  bootTreeCool[i] <- median(sample(StatsCombined$TreeCool, length(StatsCombined$TreeCool)/3*2))
}
summary(bootTreeCool); round(quantile(-bootTreeCool, c(0.5, 0.025, 0.975)), 2)


LstNoTrees <- (StatsCombined$value.LST.diff - StatsCombined$TreeCool)/StatsCombined$value.LST.diff -1
median(LstNoTrees)

bootLstNoTrees <- vector(length=1000)
set.seed(1230)
for(i in 1:length(bootLstNoTrees)){
  bootLstNoTrees[i] <- median(sample(LstNoTrees, length(LstNoTrees)/3*2))
}
summary(bootLstNoTrees); round(quantile(bootLstNoTrees*100, c(0.5, 0.025, 0.975)), 0)



mean(((StatsCombined$value.LST.diff - StatsCombined$TreeCool)/StatsCombined$value.LST.diff -1))

summary(StatsCombined[,c("LSTslope.tree", "LSTslope.veg")])
summary(StatsCombined$LSTslope.tree/StatsCombined$LSTslope.veg)
median(StatsCombined$LSTslope.tree/StatsCombined$LSTslope.veg)

# Comparing tree versus non-tree veg cooling
coolRatio <- (StatsCombined$LSTslope.tree/StatsCombined$LSTslope.veg)
treeCool <- StatsCombined$LSTslope.tree<0 & StatsCombined$LSTmodelFinal.tree.p<0.05
vegCool <- StatsCombined$LSTslope.veg<0 & StatsCombined$LSTmodelFinal.veg.p<0.05


bootcoolRatio <- vector(length=1000)
set.seed(1233)
for(i in 1:length(bootcoolRatio)){
  bootcoolRatio[i] <- median(sample(coolRatio, length(coolRatio)/3*2))
}
summary(bootcoolRatio); 
round(quantile(bootcoolRatio, c(0.5, 0.025, 0.975)), 1)


length(which(treeCool))/length(treeCool)

summary(coolRatio[treeCool & vegCool])
length(coolRatio[treeCool & vegCool])

# Comparing 
median(StatsCombined$LSTslope.tree[StatsCombined$biomeCode=="Des"])/median(StatsCombined$LSTslope.tree[StatsCombined$biomeCode=="TeCF"])

diffSlope <- vector(length=1000)
set.seed(1554)
for(i in 1:length(diffSlope)){
  datDes <- sample(StatsCombined$LSTslope.tree[StatsCombined$biomeCode=="Des"], length(which(StatsCombined$biomeCode=="Des"))/3*2)
  datTeCF <- sample(StatsCombined$LSTslope.tree[StatsCombined$biomeCode=="TeCF"], length(which(StatsCombined$biomeCode=="TeCF"))/3*2)
  
  diffSlope[i] <- median(datDes)/median(datTeCF)
}
round(quantile(diffSlope, c(0.5, 0.025, 0.975)), 1)



# Cities with lower tree cover
treeLowAll <- StatsCombined$value.tree.diff<0
length(which(treeLowAll))

treeLowSig <- StatsCombined$value.tree.diff<0 & StatsCombined$value.tree.diff.sig
length(which(treeLowSig))/length(treeLowSig)
length(unique(StatsCombined$biomeName[treeLowSig]))

#-#-#-#-#-#-#-



#-#-#-#-#-#-#-
# ET Summary Values ----
#-#-#-#-#-#-#-
names(StatsCombined)
# Checking the R2 at city scale for the ET model
lmETModis <- lm(ETobs.mean ~ ETpred.mean, data=StatsCombined)
summary(lmETModis)

lmETGLDAS <- lm(ET.GLDAS ~ ETpred.mean, data=StatsCombined)
summary(lmETGLDAS)

# Getting some rough numbers for ET at different levels
# Note: Because of how we did the splines, we'll need to give it some bands around the values we want to pull and aggregate to the city first
summary(dfSplineTree); head(dfSplineTree)

etSplineCity10 <- aggregate(ET ~ ISOURBID + biomeName + biomeCode, data=dfSplineTree[dfSplineTree$cover.tree>=9 & cover.tree <=11,], FUN=median) 
names(etSplineCity10)[which(names(etSplineCity10)=="ET")] <- "ET10"
etSplineBiome10 <- aggregate(ET10 ~ biomeName + biomeCode, data=etSplineCity10, FUN=mean)

etSplineCity50 <- aggregate(ET ~ ISOURBID + biomeName + biomeCode, data=dfSplineTree[dfSplineTree$cover.tree>=49 & cover.tree <=51,], FUN=median) 
names(etSplineCity50)[which(names(etSplineCity50)=="ET")] <- "ET50"
etSplineBiome50 <- aggregate(ET50 ~ biomeName + biomeCode, data=etSplineCity50, FUN=mean)

summary(etSplineCity10)
summary(etSplineCity50)

etSplineCityComb <- merge(etSplineCity10, etSplineCity50, all=F)
summary(etSplineCityComb)

etSplineBiomeComb <- aggregate(cbind(ET10, ET50) ~ biomeName + biomeCode, data=etSplineCityComb, FUN=mean)
etSplineBiomeComb

# NOTE: Decision point: We can evaluate the difference in water use within each city for the comparisons we make in the paper OR the biome-trends that we show in our figures; I think we should go with the figures, but we can test both
# Starting with the Biome averages (etSplineBiomeComb)
summary(etSplineCityComb)
indFor <- grep("Forest", etSplineCityComb$biomeName) 
bootETFor <- vector(length=1000)
for(i in 1:length(bootETFor)){
  indRand <- sample(indFor, length(indFor)/3*2)
  bootETFor[i] <- median(etSplineCityComb$ET50[indRand]/etSplineCityComb$ET10[indRand], na.rm=T)
}
round(quantile(bootETFor, c(0.5, 0.025, 0.975)), 1)


indArid <- which(etSplineCityComb$biomeName %in% c("Desert", "Mediterranean"))
bootETArid <- vector(length=1000)
for(i in 1:length(bootETArid)){
  indRand <- sample(indArid, length(indArid)/3*2)
  bootETArid[i] <- median(etSplineCityComb$ET50[indRand]/etSplineCityComb$ET10[indRand], na.rm=T)
}
round(quantile(bootETArid, c(0.5, 0.025, 0.975)), 1)


# 
#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
