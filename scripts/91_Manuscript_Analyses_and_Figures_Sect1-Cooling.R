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
library(ggplot2)

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

length(unique(StatsCombined$ISOURBID))
length(unique(StatsCombined$biomeName))
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# FIGURES ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 
mapLSTR2 <- ggplot(data=StatsCombined[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=LSTmodel.R2adj), size=0.25, alpha=0.8) +
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

png(file.path(path.figsMS, "FigureS2_LSTmodel_PartialEffects_CoverTree-CoverVeg.png"), height=8, width=10, units="in", res=320)
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
  facet_wrap(~biomeName, ncol=3) +
  coord_cartesian(ylim=c(0,5)) +
  geom_line(aes(x=cover.tree, y=ET, group=ISOURBID), alpha=0.2, linewidth=0.1)  +
  geom_smooth(method="gam", aes(x=cover.tree, y=ET, color=biomeName)) +
  labs(x="Cover (%)", y="ET (mm/day)") +
  scale_color_manual(values=biome.pall.all) + 
  guides(color="none") +
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figsMS, "FigureS4_ETmodel_PartialEffects_CoverTree.png"), height=10, width=8, units="in", res=320)
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
plotPartialEffects <- cowplot::plot_grid(coolingTreeSummary, etTreeSummary+guides(color="none", fill="none"), labels=c("C", "D"), nrow=1, rel_widths = c(0.6,0.4))

png(file.path(path.figsMS, "Figure1_Current_UHI-Tree-Cool-Water.png"), height=8, width=11, units="in", res=320)
cowplot::plot_grid(plotMaps, plotPartialEffects, nrow=2, rel_heights = c(0.5, 0.5))
dev.off()



modisET <- ggplot(data=StatsCombined) +
  # coord_equal() +
  geom_point(aes(x=ETpred.mean, y=ETobs.mean, color=biomeCode)) +
  geom_abline(slope=1, intercept=0) +
  annotate(geom="text", x=c(0.25, 4.75), y=c(4.5, 0.5), label=c("Observed (MODIS) higher", "Predicted higher"), hjust=c(0, 1)) +
  scale_color_manual(name="Biome", values=biomeCode.pall.all) +
  labs(x="modeled ET (mm/day)", y="MODIS ET (mm/day)") +
  theme_bw() +
  theme(legend.position="top")

gldasET <- ggplot(data=StatsCombined) +
  # coord_equal() +
  geom_point(aes(x=ETpred.mean, y=ET.GLDAS, color=biomeCode)) +
  geom_abline(slope=1, intercept=0) +
  annotate(geom="text", x=c(0.25, 4.75), y=c(4.5, 0.5), label=c("GLDAS (27 km) higher", "Predicted (1 km) higher"), hjust=c(0, 1)) +
  scale_color_manual(name="Biome", values=biomeCode.pall.all) +
  labs(x="modeled ET (mm/day)", y="GLDAS ET (mm/day)") +
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figsMS, "FigureS3_ETmodel_ValidationSummaries.png"), height=8, width=8, units="in", res=320)
cowplot::plot_grid(modisET, gldasET, ncol=1, labels=c("A", "B"))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# NUMBERS! ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Supplemental Table 2 ----
# Biome, Number Cities Analyzed, Mean/SD Temp; Mean/SD Tree Cover; mean/sd LST diff; mean/sd TreeDiff
tableS2N <- aggregate(value.LST.core~biomeName + biomeCode, data=StatsCombined, FUN=length)
names(tableS2N)[3] <- "N Cities"

tableS2Means <- aggregate(cbind(value.LST.core, value.tree.core, value.LST.diff, value.tree.diff)~biomeName + biomeCode, data=StatsCombined, FUN=mean)
tableS2Means[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")] <- round(tableS2Means[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")],2)

tableS2SDs <- aggregate(cbind(value.LST.core, value.tree.core, value.LST.diff, value.tree.diff)~biomeName + biomeCode, data=StatsCombined, FUN=sd)
tableS2SDs[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")] <- round(tableS2SDs[,c("value.LST.core", "value.tree.core", "value.LST.diff", "value.tree.diff")],2)

TableS2 <- data.frame(Biome=paste0(tableS2N$biomeName, " (", tableS2N$biomeCode, ")"),
                      N.Cities = tableS2N$`N Cities`,
                      LST = paste0(tableS2Means$value.LST.core, " (", tableS2SDs$value.LST.core, ")"),
                      TreeCover = paste0(tableS2Means$value.tree.core, " (", tableS2SDs$value.tree.core, ")"),
                      LST.Diff = paste0(tableS2Means$value.LST.diff, " (", tableS2SDs$value.LST.diff, ")"),
                      Tree.Diff = paste0(tableS2Means$value.tree.diff, " (", tableS2SDs$value.tree.diff, ")"))
TableS2
write.csv(TableS2, file.path(path.figsMS, "TableS2_Biome_LST-Tree_Stats.csv"), row.names=F)

# Supplemental Table3 ----
# LSTmdoel R2, ET modelR2; Tree Cooling; Mean/SD Current ET (); 
names(StatsCombined)

StatsCombined$TreeCool <- StatsCombined$LSTmodel.tree.slope*StatsCombined$value.tree.core

tableS3Means <- aggregate(cbind(LSTmodel.R2adj, ETmodel.R2adj, LSTmodel.tree.slope, LSTmodel.veg.slope, TreeCool, ETpred.mean)~biomeName + biomeCode, data=StatsCombined, FUN=mean)
tableS3Means[,c("LSTmodel.R2adj", "ETmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "TreeCool", "ETpred.mean")] <- round(tableS3Means[,c("LSTmodel.R2adj", "ETmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "TreeCool", "ETpred.mean")],2)

tableS3SDs <- aggregate(cbind(LSTmodel.R2adj, ETmodel.R2adj, LSTmodel.tree.slope, LSTmodel.veg.slope, TreeCool, ETpred.mean)~biomeName + biomeCode, data=StatsCombined, FUN=sd)
tableS3SDs[,c("LSTmodel.R2adj", "ETmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "TreeCool", "ETpred.mean")] <- round(tableS3SDs[,c("LSTmodel.R2adj", "ETmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "TreeCool", "ETpred.mean")],2)

TableS3 <- data.frame(Biome=paste0(tableS3Means$biomeName, " (", tableS3Means$biomeCode, ")"),
                      LSTmodel.R2 = paste0(tableS3Means$LSTmodel.R2adj, " (", tableS3SDs$LSTmodel.R2adj, ")"),
                      ETmodel.R2 = paste0(tableS3Means$ETmodel.R2adj, " (", tableS3SDs$ETmodel.R2adj, ")"),
                      LSTmodel.TreeSlope  = paste0(tableS3Means$LSTmodel.tree.slope, " (", tableS3SDs$LSTmodel.tree.slope, ")"),
                      LSTmodel.VegSlope  = paste0(tableS3Means$LSTmodel.veg.slope, " (", tableS3SDs$LSTmodel.veg.slope, ")"),
                      TreeCool.degC = paste0(tableS3Means$TreeCool, " (", tableS3SDs$TreeCool, ")"),
                      ET.mmDay = paste0(tableS3Means$ETpred.mean, " (", tableS3SDs$ETpred.mean, ")"))
TableS3
write.csv(TableS3, file.path(path.figsMS, "TableS3_Biome_LST-ET_ModelStats.csv"), row.names=F)


#-#-#-#-#-#-#-
# Getting some specific numbers for cooling comparisons
#-#-#-#-#-#-#-
#  How much trees cool cities globally on average
mean(StatsCombined$value.LST.diff); sd(StatsCombined$value.LST.diff); median(StatsCombined$value.LST.diff) #UHIs
mean(StatsCombined$TreeCool); sd(StatsCombined$TreeCool); median(StatsCombined$TreeCool) # Tree Cooling 

LstNoTrees <- (StatsCombined$value.LST.diff - StatsCombined$TreeCool)/StatsCombined$value.LST.diff -1
median(LstNoTrees)
mean(((StatsCombined$value.LST.diff - StatsCombined$TreeCool)/StatsCombined$value.LST.diff -1))

median(StatsCombined$LSTmodel.tree.slope[StatsCombined$biomeCode=="Des"])/median(StatsCombined$LSTmodel.tree.slope[StatsCombined$biomeCode=="TeCF"])


summary(StatsCombined[,c("LSTmodel.tree.slope", "LSTmodel.veg.slope")])
summary(StatsCombined$LSTmodel.tree.slope/StatsCombined$LSTmodel.veg.slope)
median(StatsCombined$LSTmodel.tree.slope/StatsCombined$LSTmodel.veg.slope)

# Sig Cooling Cities; ----
#adding fitlers for sig tree & veg cooling because otherwise the numbers get wonky
coolRatio <- (StatsCombined$LSTmodel.tree.slope/StatsCombined$LSTmodel.veg.slope)
treeCool <- StatsCombined$LSTmodel.tree.slope<0 & StatsCombined$LSTmodel.tree.p<0.05
vegCool <- StatsCombined$LSTmodel.veg.slope<0 & StatsCombined$LSTmodel.veg.p<0.05

length(which(treeCool))/length(treeCool)

summary(coolRatio[treeCool & vegCool])
length(coolRatio[treeCool & vegCool])


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
# Startign with the Biome averages (etSplineBiomeComb)
mean(etSplineBiomeComb$ET50[grep("Forest", etSplineBiomeComb$biomeName)]/etSplineBiomeComb$ET10[grep("Forest", etSplineBiomeComb$biomeName)])
mean(etSplineBiomeComb$ET50[etSplineBiomeComb$biomeName %in% c("Desert", "Mediterranean")]/etSplineBiomeComb$ET10[etSplineBiomeComb$biomeName %in% c("Desert", "Mediterranean")])

# # The city-level version <-- these are ghigher, but I think the biome-scale numbers are the way to go here
# mean(etSplineBiome50$ET50[grep("Forest", etSplineBiome50$biomeName)]/etSplineBiome10$ET10[grep("Forest", etSplineBiome10$biomeName)])
# mean(etSplineBiome50$ET50[etSplineBiome50$biomeName %in% c("Desert", "Mediterranean")]/etSplineBiome10$ET10[etSplineBiome10$biomeName %in% c("Desert", "Mediterranean")])
# 
#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
