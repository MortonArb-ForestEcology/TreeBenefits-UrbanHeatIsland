# Compile GLDAS climatology into a single file to make life easier (this takes a while so pulling it out of a different file)
library(ggplot2)

# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
library(raster)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
# path.google <- file.path(path.google)
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v3/")

# Pulling our base file to get the names etc
cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
summary(cityAll.stats)

cityAllClim <- cityAll.stats[,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biome", "LST.mean", "LST.sd", "LST.min", "LST.max")]
summary(cityAllClim)

f.gldas <- dir(path.raw, "GLDAS")
head(f.gldas)

pb <- txtProgressBar(0, nrow(cityAllClim), style=3)
for(i in 1:nrow(cityAllClim)){
  setTxtProgressBar(pb, i)
  
  CITY <- cityAllClim$ISOURBID[i]
  fCity <- grep(CITY, f.gldas)
  
  if(length(fCity)==0) next 
  
  cityClim <- stack(file.path(path.raw, f.gldas[fCity]))
  # plot(cityClim)
  
  etNow <- getValues(cityClim[["Evap_tavg_mean"]])*60*60*24
  precipNow <- getValues(cityClim[["Rainf_f_tavg_mean"]])*60*60*24
  tempNow <- getValues(cityClim[["Tair_f_inst_mean"]])-273.15
  
  etNow <- etNow[!is.na(etNow)]
  precipNow <- precipNow[!is.na(precipNow)]
  tempNow <- tempNow[!is.na(tempNow)]
  
  if(length(etNow)==0) next
  
  if(length(etNow)==1){
    cityAllClim$ET.GLDAS[i] <- etNow
    cityAllClim$Precip.GLDAS[i] <- precipNow
    cityAllClim$Tmean.GLDAS[i] <- tempNow
  } else {
    cityAllClim$ET.GLDAS[i] <- mean(etNow)
    cityAllClim$Precip.GLDAS[i] <- mean(precipNow)
    cityAllClim$Tmean.GLDAS[i] <- mean(tempNow)
  }
  
  rm(cityClim)
  
}
summary(cityAllClim)
head(cityAllClim[is.na(cityAllClim$ET.GLDAS),])

write.csv(cityAllClim, file.path(path.google, "city_stats_all_GLDAS21_climatology_2001-2020.csv"), row.names=F)
