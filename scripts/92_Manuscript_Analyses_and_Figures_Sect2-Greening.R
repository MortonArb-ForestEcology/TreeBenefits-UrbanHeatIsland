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

cityETStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
summary(cityETStats)

# StatsCombined <- merge(StatsCombined, cityETStats[,c("ISOURBID", "tree.mean.TreeTargetBottomUp", "modET.Base", "modET.TreeTargetBottomUp")], all.x=T, all.y=F)
StatsCombined <- merge(StatsCombined, cityETStats[,c("ISOURBID", "tree.mean.TreeTargetBottom25", "tree.mean.TreeCityBottom25", "tree.mean.TreeCityBottom50", "modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50")], all.x=T, all.y=F)
summary(StatsCombined)


# # Currently commented out because I think we'd have to go back and redo the biome targets
# # NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! 
# # Here we're going to JUST look at cities with UHIs
# # NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! 
# citiesUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01)
# length(citiesUHI)
# 
# StatsUHI <- StatsCombined[citiesUHI,]
# summary(StatsUHI)
# # NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! NOTE! 



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 02. Key Results: (What Christy needs to get numbers for)
#  2.1. UHI greatest in regions where the “tree deficit” is greatest; Need XX% more tree cover to offset UHI
#       -- (SUPPLEMENT: Difference in tree cover; will link up with why our ET estimate lower than gldas)
#  2.2. Because the relationship between water use & tree cover is non-linear, how you accomplish the goal will impact the amount of water needed
#       2.2.1. Bottom-up approach: bringing things 50% below the biome target 
#              -- Results in X% more tree Canopy
#              -- Results in X˚ Cooling
#              -- Requires X amount more water
#       2.2.2. Uniform canopy water estimates; Reaching the same mean canopy level as should have the same effects on LST because we used linear relationship, but will require XX% more water
#       2.2.3. Benchmark against current precip
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#- 
# Do all necessary calculations up front
#-#-#-#-#-#-#-#- 
# Cooling contributed by current tree cover
# Breaking down contributions of tree cover to LST & UHI ----
StatsCombined$EffectLST.tree <- StatsCombined$LSTmodel.tree.slope*StatsCombined$value.tree.core # Cooling of trees in core
StatsCombined$EffectLST.other <- StatsCombined$LSTmodel.veg.slope*StatsCombined$value.other.core # coolign of other veg in core
StatsCombined$ContribUHI.tree <- StatsCombined$LSTmodel.tree.slope*StatsCombined$value.tree.diff # temp effect of tree diff w/ buffer
StatsCombined$ContribUHI.other <- StatsCombined$LSTmodel.veg.slope*StatsCombined$value.other.diff # temp effect of other veg diff w/ buffer
summary(StatsCombined)


# How much more trees would there need to be to offset UHI?
StatsCombined$TreeCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$LSTmodel.tree.slope # How much MORE tree cover you'd need
StatsCombined$OtherCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$LSTmodel.veg.slope # How much MORE other veg cover you'd need
StatsCombined$TreeCoverTargetUHI <- StatsCombined$TreeCoverUHINeed + StatsCombined$value.tree.core # Add MORE to CURRENT
StatsCombined$OtherCoverTargetUHI <- StatsCombined$OtherCoverUHINeed + StatsCombined$value.other.core # Add MORE to CURRENT
summary(StatsCombined)

# Calculating cooling trees added from bottom-up scenario and associated cooling
# cityETStats[,c("ISOURBID", "tree.mean.TreeTargetBottom25", "tree.mean.TreeCityBottom25", "tree.mean.TreeCityBottom50", "modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50")]
# Doing this for the biome Target
StatsCombined$greening.treeAddBiomeTarget <- StatsCombined$tree.mean.TreeTargetBottom25 - StatsCombined$value.tree.core
StatsCombined$greening.coolingAddBiomeTarget <- StatsCombined$greening.treeAddBiomeTarget*StatsCombined$LSTmodel.tree.slope
StatsCombined$greening.treeTotalBiomeTarget <- StatsCombined$tree.mean.TreeTargetBottom25 # Yeah, just renaming, but it will help below
StatsCombined$greening.coolingTotalBiomeTarget <- StatsCombined$tree.mean.TreeTargetBottom25*StatsCombined$LSTmodel.tree.slope
StatsCombined$greening.remainUHIBiomeTarget <- StatsCombined$value.LST.diff + StatsCombined$greening.coolingAddBiomeTarget
summary(StatsCombined)


# Doing this for the city 25 scenario
StatsCombined$greening.treeAddCity25 <- StatsCombined$tree.mean.TreeCityBottom25 - StatsCombined$value.tree.core
StatsCombined$greening.coolingAddCity25 <- StatsCombined$greening.treeAddCity25*StatsCombined$LSTmodel.tree.slope
StatsCombined$greening.treeTotalCity25 <- StatsCombined$tree.mean.TreeCityBottom25 # Yeah, just renaming, but it will help below
StatsCombined$greening.coolingTotalCity25 <- StatsCombined$tree.mean.TreeCityBottom25*StatsCombined$LSTmodel.tree.slope
StatsCombined$greening.remainUHICity25 <- StatsCombined$value.LST.diff + StatsCombined$greening.coolingAddCity25
summary(StatsCombined)
summary(StatsCombined[StatsCombined$value.LST.diff>0,c("value.tree.core", "value.LST.diff", "LSTmodel.tree.slope", names(StatsCombined)[grep("City25", names(StatsCombined))])])


# Doing this for the city 50 scenario
StatsCombined$greening.treeAddCity50 <- StatsCombined$tree.mean.TreeCityBottom50 - StatsCombined$value.tree.core
StatsCombined$greening.coolingAddCity50 <- StatsCombined$greening.treeAddCity50*StatsCombined$LSTmodel.tree.slope
StatsCombined$greening.treeTotalCity50 <- StatsCombined$tree.mean.TreeCityBottom50 # Yeah, just renaming, but it will help below
StatsCombined$greening.coolingTotalCity50 <- StatsCombined$tree.mean.TreeCityBottom50*StatsCombined$LSTmodel.tree.slope
StatsCombined$greening.remainUHICity50 <- StatsCombined$value.LST.diff + StatsCombined$greening.coolingAddCity50
summary(StatsCombined)




# Water Ratios
# "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50"
StatsCombined$ETcurrent.Precip <- StatsCombined$modET.Base/StatsCombined$Precip.GLDAS
StatsCombined$ETgreenBiomeTarget.Precip <- StatsCombined$modET.TreeTargetBottom25/StatsCombined$Precip.GLDAS
StatsCombined$ETgreenCity25.Precip <- StatsCombined$modET.TreeCityBottom25/StatsCombined$Precip.GLDAS
StatsCombined$ETgreenCity50.Precip <- StatsCombined$modET.TreeCityBottom50/StatsCombined$Precip.GLDAS
summary(StatsCombined)

length(which(StatsCombined$ETcurrent.Precip<1 & !is.na(StatsCombined$ETcurrent.Precip)))
length(which(StatsCombined$ETgreenTarget.Precip<1 & !is.na(StatsCombined$ETgreenTarget.Precip)))
# Only ~ 220 cities lose their sustainability from greening!
#-#-#-#-#-#-#-#- 


# Comparing how much the 50% does to get you to the UHI offset target; uses a biome-average
summary(StatsCombined[,c("value.tree.core", "TreeCoverUHINeed", "greening.treeAddBiomeTarget", "greening.treeAddCity25", "greening.treeAddCity50")]) 

summary((StatsCombined$greening.treeAddBiomeTarget+StatsCombined$value.tree.core)/StatsCombined$value.tree.core) # This would be a ratio of current canopy 
summary((StatsCombined$greening.treeAddCity25+StatsCombined$value.tree.core)/StatsCombined$value.tree.core) # This would be a ratio of current canopy 
summary((StatsCombined$greening.treeAddCity50+StatsCombined$value.tree.core)/StatsCombined$value.tree.core) # This would be a ratio of current canopy 
summary((StatsCombined$TreeCoverUHINeed+StatsCombined$value.tree.core)/StatsCombined$value.tree.core) # This would be a ratio of current canopy 

summary(StatsCombined$greening.treeAddBiomeTarget/StatsCombined$TreeCoverUHINeed) # Note: this is the same as doing it with temperature because the linear relationship & how our goal is set up



TreeCoverTarget <- ggplot(data=StatsCombined[,], aes(x=biomeCode, y=TreeCoverTargetUHI, fill="Biome Target", color="Biome Target")) +  
  geom_bar(stat="summary", fun="median") +
  geom_hline(yintercept=30, linetype="dashed") +
  # geom_segment(yend=0, aes(xend=biomeCode), stat="summary", fun="median", size=2) +
  geom_violin(aes(x=biomeCode, y=value.tree.core, fill="Current", color="Current"), scale="width", alpha=0.8) +
  # geom_violin(aes(x=biomeCode, y=tree.mean.TreeTargetBottom25, fill="Biome Target, 25% Greening", color="Biome Target 25% Greening"), scale="width", alpha=0.5) +
  geom_violin(aes(x=biomeCode, y=tree.mean.TreeCityBottom25, fill="City Mean, 25% Greening", color="City Mean, 25% Greening"), scale="width", alpha=0.5) +
  # geom_violin(aes(x=biomeCode, y=tree.mean.TreeCityBottom25, fill="City Mean, 25% Greening", color="City Mean, 25% Greening"), scale="width", alpha=0.5) +
  # geom_violin(aes(x=biomeCode, y=tree.mean.TreeCityBottom50, fill="City Mean, 50% Greening", color="City Mean, 50% Greening"), scale="width", alpha=0.5) +
  annotate("text", x=10.5, y=33, label="30%", hjust=1, vjust=0) +
  # geom_point(stat="summary", fun="median", size=5) +
  scale_fill_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4], "City Mean, 25% Greening"=grad.tree[6])) +
  scale_color_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4], "City Mean, 25% Greening"=grad.tree[6])) +
  labs(x="Biome", y="Tree Cover (%)") +
  scale_y_continuous(limits=c(0,70), expand=c(0,0)) +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face='bold'),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

png(file.path(path.figsExplore, "TreeCover_Targets.png"), height=6, width=6, units="in", res=320)
TreeCoverTarget
dev.off()


# Now looking at the cooling vs. UHI of the current and our scenario
# citiesUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01)
summary(StatsCombined[,c("EffectLST.tree", "value.LST.diff", "greening.coolingTotalBiomeTarget", "greening.remainUHIBiomeTarget", "greening.coolingTotalCity25", "greening.remainUHICity25", "greening.coolingTotalCity50", "greening.remainUHICity50")])

# "greening.coolingTotalCity25", "greening.remainUHICity25", "greening.coolingTotalCity50", "greening.remainUHICity50"
effectsUHI <- stack(StatsCombined[,c("EffectLST.tree", "value.LST.diff")])
names(effectsUHI) <- c("Current", "effect")
effectsUHI$GreeningBiomeTarget <- stack(StatsCombined[,c("greening.coolingTotalBiomeTarget", "greening.remainUHIBiomeTarget")])[,1]
effectsUHI$GreeningCity25 <- stack(StatsCombined[,c("greening.coolingTotalCity25", "greening.remainUHICity25")])[,1]
effectsUHI$GreeningCity50 <- stack(StatsCombined[,c("greening.coolingTotalCity50", "greening.remainUHICity50")])[,1]
effectsUHI$effect <- as.character(effectsUHI$effect)
effectsUHI$effect[grep("tree", effectsUHI$effect)] <- "Tree"
effectsUHI$effect[grep("LST", effectsUHI$effect)] <- "Remaining UHI"
effectsUHI$effect <- factor(effectsUHI$effect, levels=c("Tree", "Remaining UHI"))
summary(effectsUHI)

effectsUHI[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")] <- StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")]
summary(effectsUHI)

effectsUHI2 <- stack(effectsUHI[,c("Current", "GreeningBiomeTarget", "GreeningCity25", "GreeningCity50")])
names(effectsUHI2)[2] <- c("Scenario")
effectsUHI2[,c("effect", "ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")] <- effectsUHI[,c("effect", "ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")]
summary(effectsUHI2)


# "#67001f" "#b2182b" "#d6604d" "#f4a582" "#fbbdc7" "#d1e5f0" "#92c5de" "#4393c3" "#2166ac" "#053061"
# "#f03b20"    "#ef3b2c"
plotTempEffects <- ggplot(data=effectsUHI2, aes(x=Scenario, y=values, fill=effect)) + 
  facet_wrap(~biomeCode) +
  geom_bar(stat="summary", fun="mean", position="dodge") +
  stat_summary(fun = mean, 
               fun.min=function(x){mean(x)-sd(x)}, 
               fun.max=function(x){mean(x)+sd(x)}, 
               geom="pointrange", position=position_dodge(0.9)) +
  # stat_summary(fun.data = "mean_cl_normal", conf.int=0.95, geom="pointrange", position=position_dodge(0.9)) +
  geom_hline(yintercept=0, linewidth=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  scale_color_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  geom_text(x="Des", y=-7, hjust=1, label="Cooling Effect") +
  geom_text(x="Des", y=2.5, hjust=1, label="Warming Effect") +
  # coord_cartesian(ylim=c(-7.5, 2.5)) +
  labs(x="Biome", y="Temperature Effect (˚C)") +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face="bold"),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

png(file.path(path.figsExplore, "TreeCover_Targets-Cooling.png"), height=6, width=6, units="in", res=320)
plotTempEffects
dev.off()


# Making a poster-ready figure where we're only looking at cities with UHI & showign teh Greening City 25 scenario
cityUHI <- StatsCombined$ISOURBID[StatsCombined$value.LST.diff>0]
plotTempEffectsUHIonly <- ggplot(data=effectsUHI2[effectsUHI2$ISOURBID %in% cityUHI & effectsUHI2$Scenario %in% c("Current", "GreeningCity50"),], aes(x=Scenario, y=values, fill=effect)) + 
  facet_wrap(~biomeCode) +
  geom_bar(stat="summary", fun="mean") +
  stat_summary(fun = mean, 
               fun.min=function(x){mean(x)-sd(x)}, 
               fun.max=function(x){mean(x)+sd(x)}, 
               geom="pointrange") +
  # stat_summary(fun.data = "mean_cl_normal", conf.int=0.95, geom="pointrange", position=position_dodge(0.9)) +
  geom_hline(yintercept=0, linewidth=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  scale_color_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  geom_text(x="Des", y=-7, hjust=1, label="Cooling Effect") +
  geom_text(x="Des", y=2.5, hjust=1, label="Warming Effect") +
  # coord_cartesian(ylim=c(-7.5, 2.5)) +
  scale_x_discrete(labels=c("Current", "Greening")) +
  labs(y="Temperature Effect (˚C)") +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face="bold"),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

png(file.path(path.figsExplore, "TreeCover_Targets-Cooling_UHIOnly_City25.png"), height=6.5, width=9, units="in", res=320)
plotTempEffectsUHIonly
dev.off()

# Generating some quick stats that shows how much of the UHI is reduced
summary(StatsCombined[StatsCombined$value.LST.diff>0, c("value.LST.diff", "greening.remainUHICity25")])
summary(1-StatsCombined$greening.remainUHICity25[StatsCombined$value.LST.diff>0]/StatsCombined$value.LST.diff[StatsCombined$value.LST.diff>0])
hist(1-StatsCombined$greening.remainUHICity25[StatsCombined$value.LST.diff>0]/StatsCombined$value.LST.diff[StatsCombined$value.LST.diff>0])

summary(StatsCombined$greening.treeAddCity25[StatsCombined$value.LST.diff>0])

summary(1-StatsCombined$greening.remainUHICity50[StatsCombined$value.LST.diff>0]/StatsCombined$value.LST.diff[StatsCombined$value.LST.diff>0])
summary(StatsCombined$greening.treeAddCity50[StatsCombined$value.LST.diff>0])


# Compare current vs. scenario with water use
plotPrecip <- ggplot(data=StatsCombined[,]) +
  geom_violin(aes(x=biomeCode, y=Precip.GLDAS, fill=biomeName), scale="width") +
  scale_fill_manual(values=biome.pall.all) +
  scale_y_continuous(limits=c(0, max(effectsET$Precip.GLDAS, na.rm=T))) +
  labs(y="GLDAS Precip (kg/m2/day)", x="Biome") +
  guides(fill="none") +
  theme_bw() 
plotPrecip

effectsET <- stack(StatsCombined[,c("modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50")])
names(effectsET) <- c("ET", "Scenario")
effectsET$ETratio <- stack(StatsCombined[,c("ETcurrent.Precip", "ETgreenBiomeTarget.Precip", "ETgreenCity25.Precip", "ETgreenCity50.Precip")])[,1]
effectsET$Scenario <- as.character(effectsET$Scenario)
effectsET$Scenario[grepl("Base", effectsET$Scenario)] <- "Current"
effectsET$Scenario[grepl("TargetBottom", effectsET$Scenario)] <- "Biome Target, 25% Greening"
effectsET$Scenario[grepl("CityBottom25", effectsET$Scenario)] <- "City Mean, 25% Greening"
effectsET$Scenario[grepl("CityBottom50", effectsET$Scenario)] <- "City Mean, 50% Greening"
effectsET[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "Precip.GLDAS")] <- StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "Precip.GLDAS")]
summary(effectsET)

# "Current"="#005a32", "Biome Target"=grad.tree[4], "50% Greening"=grad.tree[6])

plotET <- ggplot(data=effectsET[,]) +
  geom_violin(aes(x=biomeCode, y=ET, fill=Scenario), scale="width") +
  # scale_fill_manual(values=c("Current"="#005a32", "GreeningTarget"=grad.tree[6])) +
  labs(y="ET (kg/m2/day)", x="Biome") +
  scale_y_continuous(limits=c(0, max(effectsET$Precip.GLDAS, na.rm=T))) +
  # guides(fill="none") +
  theme_bw() + theme(plot.background = element_blank())

plotET

plotETratio <- ggplot(data=effectsET[,]) +
  geom_violin(aes(x=biomeCode, y=log(ETratio), fill=Scenario), scale="width") +
  # scale_fill_manual(values=c("Current"="#005a32", "GreeningTarget"=grad.tree[6])) +
  geom_hline(yintercept=log(1), linetype="dashed") +
  annotate(geom="text", x=1.25, y=c(-2.5, 3), label=c("Precip Surplus", "Precip Deficit"), hjust=0) +
  labs(y="log(ET/Precip) (unitless)", x="Biome") +
  # guides(fill="none") +
  theme_bw() + theme(plot.background = element_blank())

plotETratio

# plotETcomb <- cowplot::plot_grid(plotET,NA, plotETratio, NA, ncol=2, rel_widths=c(0.75, 0.25))

png(file.path(path.figsExplore, "TreeCover_Targets-ET.png"), height=6, width=8, units="in", res=320)
cowplot::plot_grid(plotPrecip, plotET,plotETratio, ncol=1, rel_heights=c(0.4, 0.3, 0.3))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

