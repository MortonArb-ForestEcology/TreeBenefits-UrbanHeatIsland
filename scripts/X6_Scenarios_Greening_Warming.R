# Doing some warming and greening projections on cities
# Vegetation: 1) As is; 2) Green Blob (even at target) 3) Uniform increase 4) bottom-up
# -- Vegetation goal: How much water will a city need to meet its goals?  How does canopy equality affect water needs?
# -- Vegetation Methods: apply scenario to each pixel; for 4) ???
# -- Save along the way: percent of each city in 5% bins so we can graph the changes in distribution
# Climate: 1) current; 2) mid-century: ssp 245, 585; 3) end-century: ssp 245, 585
# -- Climate Goal: How will the water needs of the canopy change with climate change; how does that compare to changes in precip 
# -- Climate Method: look at the delta in temperature; add uniformly to area (acknowledge we're dealing with air vs. lst temps here, but it's a start!); compare to delta in precip; all deltas calculated as a change from weighted-average temp & precip for each GCM x scenario; for precip consider both absolute and % change
# -- Save along the way: climate change projection stats for each city: average warming, drying/wetting per city 
#        

###########################################
# Load Packages, set paths ----
###########################################
library(tidyr)

library(ggplot2); library(RColorBrewer); library(cowplot)
library(ggalt); library(sf)
library(mapproj)

path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")
###########################################


###########################################
# Read in city data
###########################################
# cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
# cityAll.stats <- cityAll.stats[,!grepl("ETmodel", names(cityAll.stats))]
# cityAll.stats <- cityAll.stats[,!grepl("corr.", names(cityAll.stats))]
# cityAll.stats <- cityAll.stats[,!grepl("trend", names(cityAll.stats))]
# summary(cityAll.stats)
# 
# summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])

StatsCombined <- read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
StatsCombined <- StatsCombined[,!(grepl("ETmodel", names(StatsCombined)))]
summary(StatsCombined)

cityAll.ET <- read.csv(file.path(path.google, "city_stats_all_ET-Clim.csv"))
length(cityAll.ET$ISOURBID[which(cityAll.ET$Precip.GLDAS<0.01)])

cityAll.ET <- cityAll.ET[cityAll.ET$ISOURBID %in% StatsCombined$ISOURBID & cityAll.ET$Precip.GLDAS>0.01,]
summary(cityAll.ET)

# Read in CMIP6 metadata 
cmip6Raw <- read.csv(file.path(path.google, "city_stats_all_CMIP6_wide.csv"))
cmip6Raw <- cmip6Raw[!is.na(cmip6Raw$ACCESS.CM2),names(cmip6Raw)[!names(cmip6Raw) %in% c("HadGEM3.GC31.MM", "NorESM2.LM")]]
summary(cmip6Raw)

cmip6long <- gather(cmip6Raw, GCM, value, ACCESS.CM2:UKESM1.0.LL, factor_key = T)
summary(cmip6long)

cmip6 <- spread(cmip6long, Var, value)
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
summary(as.factor(cmip6[is.na(cmip6$tas),"ISOURBID"]))
cmip6[,c("tas.ref", "pr.ref", "tas.diff", "pr.diff", "pr.per")] <- NA
summary(cmip6)

pb <- txtProgressBar(0, length(unique(cmip6$ISOURBID)), style=3)
pbInd = 0

for(CITY in unique(cmip6$ISOURBID)){
  # CITY=unique(cmip6$ISOURBID)[1]
  # print(CITY)
  for(GCM in unique(cmip6$GCM)){
    # GCM = unique(cmip6$GCM)[1]
    rowHist <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM & cmip6$Scenario=="historical")
    for(SCEN in c("ssp245", "ssp585")){
      # # SCEN="ssp245"
      rowsScen <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM & cmip6$Scenario==SCEN)
      
      # Calculate a weighted average for the first 20 years
      rowRef <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM & cmip6$Scenario==SCEN & cmip6$Time==2020)
      if(length(rowRef)==0) next
      
      cmip6$tas.ref[rowsScen] <- (cmip6$tas[rowHist]*14+cmip6$tas[rowRef]*6)/20
      cmip6$pr.ref[rowsScen] <- (cmip6$pr[rowHist]*14+cmip6$pr[rowRef]*6)/20
      
      # # Calculate the absolute diffs
      # cmip6$tas.diff[rowsScen] <- cmip6$tas[rowsScen]-tasRef
      # cmip6$pr.diff[rowsScen] <- cmip6$pr[rowsScen]-prRef
      # cmip6$pr.per[rowsScen] <- cmip6$pr[rowsScen]/prRef
    }
    # rowsGCM <- which(cmip6$ISOURBID==CITY & cmip6$GCM==GCM)
    # cmip6[rowsGCM ,]
  }
  pbInd = pbInd+1
  setTxtProgressBar(pb, pbInd)
}
summary(cmip6)

cmip6$tas.diff <- cmip6$tas-cmip6$tas.ref
cmip6$pr.diff <- cmip6$pr-cmip6$pr.ref
cmip6$pr.per <- cmip6$pr/cmip6$pr.ref-1
summary(cmip6)

cmip6Agg <- aggregate(cbind(pr, tas, tas.ref, pr.ref, tas.diff, pr.diff, pr.per) ~ ISOURBID + NAME + LATITUDE + LONGITUDE + Scenario + Time, data=cmip6, FUN=mean)
summary(cmip6Agg)

grad.temp <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
grad.prcp <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") #  ends with teal
world <- map_data("world"); 
world <- world[!world$long>180,]

ggplot(data=cmip6Agg[cmip6Agg$Time!=2020,]) +
  facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  coord_map("merc") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tas.diff), size=0.25, alpha=0.8) +
  scale_color_stepsn(name="Temp Change", colors=grad.temp, limits=c(-4.5, 4.5), n.breaks=13) +
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

ggplot(data=cmip6Agg[cmip6Agg$Time!=2020,]) +
  facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=pr.per), size=0.25, alpha=0.8) +
  scale_color_stepsn(name="Precip Change", colors=grad.prcp, limits=c(-0.8, 0.8), n.breaks=13) + # Using breaks from IPCC AR6 figures
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(4, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))
###########################################
