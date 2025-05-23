
grad.prcp <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") #  ends with teal
grad.temp <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
gradTemp2 <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")
gradPrcp2 <- c("#ffffcc", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")


# grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with red
# grad.lst <- c("#2166ac", "#67a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2182b") # ends with red
grad.lst <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
# grad.lstHot <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026") # ends with red
grad.lstHot <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026") # ends with red


# grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
# grad.treeDiff <- c("#8c510a", "#d8b365", "#f6e8c3", "#f5f5f5", "#c7eae5", "#5ab4ac", "#01665e") # ends with teal
grad.treeDiff <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") # ends with teal
# grad.tree <- c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#31a354", "#006837") # ends with green
grad.tree <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529") # ends with green


# grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
# grad.otherDiff <- c("#c51b7d", "#e0a3c0", "#fde0ef", "#f7f7f7", "#e6f5d0", "#a1d76a", "#4d9221") # Green
grad.otherDiff <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e7f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419") # Green

# grad.other <- c("#f0f9e8", "#ccebc5", "#a8ddb5", "#7bccc4", "#43a2ca", "#0868ac") # ends with Blue
grad.other <- c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081")

grad.modfit <- c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177", "#49006a")



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


# Color-blind friendly versions
library(RColorBrewer)

# display.brewer.all(colorblindFriendly = T)
# brewer.pal(10, "Paired")
# [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F"
# [8] "#FF7F00" "#CAB2D6" "#6A3D9A"
biome.pall.ShortCB = c("Taiga"= "#1F78B4", 
                   "Montane Grassland/Savanna" = "#A6CEE3",
                   "Temperate Conifer Forest" = "#6A3D9A",
                   "Temperate Broadleaf Forest" = "#CAB2D6",
                   "Temperate Grassland/Savanna" = "#FF7F00",
                   "Tropical Grassland/Savanna" = "#FDBF6F",
                   "Mediterranean" = "#FB9A99",
                   "Tropical Moist Broadleaf Forest"= "#33A02C",
                   "Tropical Dry Broadleaf Forest" = "#B2DF8A",
                   "Desert" = "#E31A1C"
                   )
biomeCode.pall.ShortCB = c("Tai"= "#1F78B4", 
                           "MGS" = "#A6CEE3",
                           "TeCF" = "#6A3D9A",
                           "TeBF" = "#CAB2D6",
                           "TeGS" = "#FF7F00",
                           "TrGS" = "#FDBF6F",
                           "Med" = "#FB9A99",
                           "TrMBF"= "#33A02C",
                           "TrDBF" = "#B2DF8A",
                           "Des" = "#E31A1C"
                           )


nlcd.palette <- data.frame(value=c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90,95),
                           color=c("#466b9f", "#d1def8", "#dec5c5", "#d99282", "#eb0000", "#ab0000", "#b3ac9f", "#68ab5f", "#1c5f2c", "#b5c58f", "#af963c", "#ccb879", "#dfdfc2", "#d1d182", "#a3cc51", "#82ba9e", "#dcd939", "#ab6c28", "#b8d9eb", "#6c9fb8"),
                           lcType = c("water", "ice/snow", "developed, open", "developed, low", "developed, medium", "developed, high", "barren", "forest, deciduous", "forest, evergreen", "forest, mixed", "scrub, dwarf", "shrub/scrub", "grassland", "sedge/herb", "lichens", "moss", "pasture/hay", "crops", "wetlands, woody", "wetlands, herbaceous"))

world <- ggplot2::map_data("world"); 
world <- world[!world$long>180,]


# function to format pasting of means & sd
pasteMeanSD <- function(x, y){ paste0(x, " (", y, ")")}
