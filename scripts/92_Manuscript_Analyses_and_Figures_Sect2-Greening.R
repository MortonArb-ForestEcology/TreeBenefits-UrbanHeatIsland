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
# Do analyses here!



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

