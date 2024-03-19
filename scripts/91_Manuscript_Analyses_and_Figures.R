# Updated script for manuscript
# Outline
# 01. Trees cool cities & need water to do so: ---- 
#     Trees have a clear, consistent cooling potential on global urban surface temperatures… [treat cooling capacity as a known; we add a tiny bit of nuance; quickly bring in nuance & water]
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")

path.figs <- file.path(path.google, "figures_manuscript")
dir.create(path.figs, recursive=T, showWarnings=F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Read in some base datasets etc.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
source("0_color_palettes_etc.R")

biome.order <-  read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis_BiomeOrder.csv"))
biome.order

StatsCombined <-  read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
StatsCombined$biomeName <- factor(StatsCombined$biomeName, levels=biome.order$biomeName)
StatsCombined$biomeCode <- factor(StatsCombined$biomeCode, levels=biome.order$biomeCode)
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




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
# Key Results: (What Christy needs to get numbers for)
#  3.0. SUPPLEMENT Figure: The amount of warming & precipitation varies across the globe; this is CMIP6, not us, so no need to put in main MS
#. 3.1. Warming will cause an X% increase in ET; X% of cities will see precip not keep pace with this, resulting in XX% of cities 
#  3.2. Although we estimate XX biomes to have the greatest proportion of cities in a canopy water deficit, the biggest shift in the distribution is in temperate forest biomes, particularly cities in Europe and the US

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Do analyses here!

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
