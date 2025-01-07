library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=T

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsRegion <- file.path(path.cities, "../city_stats_model.csv")

# Which model to use based off of script 2
modelUse <- "SCover"
nInterval = 50 # Number of divisions for plotting partial effects

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")

# Some color palettes for later
grad.temp <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
grad.elev <- c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4")
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange

grad.et <- rev(c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101")) # Ends with orange


# If we don't have our summary file yet, create it and create all the column names we're going to want
if(!file.exists(file.cityStatsRegion) | overwrite){
  cityStatsBase <- read.csv(file.path(path.cities, "../city_stats_model-selection.csv"))
  cityStatsRegion <- cityStatsBase[, !grepl("model", names(cityStatsBase))]
  cityStatsRegion[,c("LSTmodelFinal.R2adj", "LSTmodelFinal.RMSE")] <- cityStatsBase[,paste0("LSTmodel", modelUse, c(".R2adj", ".RMSE"))]
  cityStatsRegion[,c("LSTmodelFinal.tree.p", "LSTmodelFinal.veg.p", "LSTmodelFinal.elev.p", "LSTmodelFinal.Intercept.Mean")] <- NA
  cityStatsRegion[,c("LSTEffect.tree", "LSTEffect.veg")] <- NA

  # Also look at the correlation tree & veg cover
  cityStatsRegion[,c("corr.tree.veg.slope", "corr.tree.veg.p", "corr.tree.veg.Rsq")] <- NA

  # summary(cityStatsRegion)
  # dim(cityStatsRegion)
  
  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)
  
  splineTree <- splineVeg <- list()
  
}


# Read in Summary file -----
cityStatsRegion <- read.csv(file.cityStatsRegion)
summary(cityStatsRegion); dim(cityStatsRegion)



# Find the cities that we need to analyze
rowsAnalyze <- which(is.na(cityStatsRegion$LSTmodelFinal.tree.p) & !is.na(cityStatsRegion$n.pixels))
length(citiesAnalyze)

pb <- txtProgressBar(min=0, max=length(rowsAnalyze), style=3)
# # Start City Loop -----
for(i in seq_along(rowsAnalyze)){
  setTxtProgressBar(pb, i)
  # # Good Test Cities: Sydney (AUS66430)
  # CITY="AUS66430"; CITY="USA26687"
  # print(CITY)
  rowCity <- rowsAnalyze[i]
  CITY=cityStatsRegion$ISOURBID[rowCity]
  
  # -----------------------
  # READ IN VALUES AND APPROPRIATE MODEL FROM SCRIPT 2.1
  # -----------------------
  valsCity <- read.csv(file.path(path.cities, CITY, paste0(CITY, "_values-All.csv")))
  summary(valsCity)
  
  # modLSTCitySCover
  load(file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam-SCover.RData")))
  summary(modLSTCitySCover)
  
  saveRDS(modLSTCitySCover, file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam-Final.RDS")))
  
  # Quick check on the correlation between tree & veg cover
  # plot(cover.veg ~ cover.tree, data=valsCity)
  corrTreeVeg <- lm(cover.veg ~ cover.tree, data=valsCity)
  sumTreeVeg <- summary(corrTreeVeg)
  
  # c("corr.tree.veg.slope", "corr.tree.veg.p", "corr.tree.veg.Rsq")
  cityStatsRegion$corr.tree.veg.slope[rowCity] <- sumTreeVeg$coefficients["cover.tree", "Estimate"]
  cityStatsRegion$corr.tree.veg.p[rowCity] <- sumTreeVeg$coefficients["cover.tree", "Pr(>|t|)"]
  cityStatsRegion$corr.tree.veg.Rsq[rowCity] <- sumTreeVeg$r.squared
  
  # Extract the p-value for our key effects --> with the spline we do NOT have handy coefficients to pull out
  modSum <- summary(modLSTCitySCover)
  
  cityStatsRegion[rowCity, c("LSTmodelFinal.tree.p", "LSTmodelFinal.veg.p")] <- modSum$s.table[c("s(cover.tree)", "s(cover.veg)"), "p-value"]
  cityStatsRegion$LSTmodelFinal.elev.p <- modSum$p.table["elevation", "Pr(>|t|)"]
  

  # Pulling out some intercept info to help with modeling
  intYear <- grepl("year", names(modSum$p.coeff))
  intMean <- mean(modSum$p.coeff[intYear])
  
  intDiffs <- abs(modSum$p.coeff[intYear]-mean(modSum$p.coeff[intYear]))
  indMeanI  <- which(intDiffs==min(intDiffs))
  yearMean <- as.numeric(stringr::str_sub(names(modSum$p.coeff[intYear])[indMeanI], start=-4))
  
  cityStatsRegion[rowCity,"LSTmodelFinal.Intercept.Mean"] <- intMean
  # cityStatsRegion
  # cityStatsRegion[,c("LSTmodelFinal.tree.p", "LSTmodelFinal.veg.p", "LSTmodelFinal.elev.p")] <- NA
  
    # ------------
  

  # Calculating pixel-based summary stats to do some trend correlations
  # For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
  summaryCity <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation, ET) ~ x+y+location + cityBounds, data=valsCity, FUN=mean)
  summaryCity$year <- yearMean
  # names(summaryCity)[names(summaryCity) %in% c("LST_Day", "cover.tree", "cover.veg", "ET")] <- c("LST.mean", "tree.mean", "veg.mean", "ET.mean")
  summary(summaryCity)

  
  # Worth pulling out the partial effects here for each pixel as well as the range of values like we do for ET
  effCity <- data.frame(predict(modLSTCitySCover, type="terms", newdata=summaryCity, exclude="as.factor(year)"))
  effCity$intercept <- intMean
  summary(effCity)
  summaryCity$LST.predict <- apply(effCity, 1, sum)
  summaryCity$LST.resid <- summaryCity$LST_Day - summaryCity$LST.predict
  summaryCity[,c("LSTEffect.elevation", "LSTEffect.tree", "LSTEffect.veg", "LSTeffect.xy")] <- effCity[,c("elevation", "s.cover.tree.", "s.cover.veg.", "s.x.y.")]
  summary(summaryCity)
  
  # plot(LST_Day ~ LST.predict, data=summaryCity); abline(a=0, b=1, col="red2")
  # plot(LST.resid ~ LST.predict, data=summaryCity); abline(a=0, b=0, col="red2")
  
  summary(summaryCity[, !names(summaryCity) %in% c("year")])
  
  cityStatsRegion[rowCity, c("LSTEffect.tree", "LSTEffect.veg")] <- apply(summaryCity[,c("LSTEffect.tree", "LSTEffect.veg")], 2, mean)
  cityStatsRegion[rowCity, c("LSTEffect.tree.city", "LSTEffect.veg.city")] <- apply(summaryCity[summaryCity$cityBounds, c("LSTEffect.tree", "LSTEffect.veg")], 2, mean)
  
  dfTree <- data.frame(cover.tree=seq(min(modLSTCitySCover$model$cover.tree), max(modLSTCitySCover$model$cover.tree), length.out=nInterval),
                      cover.veg=mean(modLSTCitySCover$model$cover.veg),
                      elevation=mean(modLSTCitySCover$model$elevation),
                      x=mean(modLSTCitySCover$model$x),
                      y=mean(modLSTCitySCover$model$y),
                      year=yearMean)
  splineTree[[CITY]] <- data.frame(ISOURBID=CITY, cover.tree=dfTree$cover.tree, LST.pred=predict(modLSTCitySCover, newdata=dfTree),
                                   effect.tree = as.vector(predict(modLSTCitySCover, newdata=dfTree, type="terms", exclude=c("s(cover.veg)", "s(x,y)", "elevation", "as.factor(year)"))))
  summary(splineTree[[CITY]])

  dfVeg <- data.frame(cover.veg=seq(min(modLSTCitySCover$model$cover.veg), max(modLSTCitySCover$model$cover.veg), length.out=nInterval),
                       cover.tree=mean(modLSTCitySCover$model$cover.tree),
                       elevation=mean(modLSTCitySCover$model$elevation),
                       x=mean(modLSTCitySCover$model$x),
                       y=mean(modLSTCitySCover$model$y),
                       year=yearMean)
  splineVeg[[CITY]] <- data.frame(ISOURBID=CITY, cover.veg=dfVeg$cover.veg, LST.pred=predict(modLSTCitySCover, newdata=dfVeg),
                                   effect.veg = as.vector(predict(modLSTCitySCover, newdata=dfVeg, type="terms", exclude=c("s(cover.tree)", "s(x,y)", "elevation", "as.factor(year)"))))
  summary(splineVeg[[CITY]])
  
      
  write.csv(summaryCity[, !names(summaryCity) %in% c("year")], file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")), row.names=F)
  
  saveRDS(splineTree, file=file.path(path.cities, "../LSTModel_Spline_PartialEffects_CoverTree.rds"))
  saveRDS(splineVeg, file=file.path(path.cities, "../LSTModel_Spline_PartialEffects_CoverVeg.rds"))
  
  
  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  # Write our city stats file each time in case it bonks

  # print("") # Just give a clean return before moving on
  
  # Remove a bunch of stuff for our own sanity
  # rm(elevCity, treeCity, vegCity, lstCity, modLSTCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.LST.Tree, plot.corr.LST.Veg, plot.corr.Tree.Veg, plot.lst.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.lst, plot.tree, plot.veg, veg.lst, veg.tree, tree.lst, veg.out, tree.out, sum.corrTreeLST, sum.corrVegLST, sum.corrVegTree, sum.modLSTCity)
  
}	

