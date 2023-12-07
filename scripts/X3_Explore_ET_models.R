# Testing and exploring ET models a bit more thoroughly

# Test Cities
# ************************
# 1. Chicago: USA26687; 
# 2. Vancouver: CAN16375; 
# 3. Berlin: DEU10109; 
# 4. Atlanta: USA40447; 
# 5. Sydney: AUS66430; 
# 6. Santiago (Chile): CHL66311; 
# 7. Cairo (AlQahirah): EGY44702; 
# 8. Beijing: CHN31890; 
# 9. Johannesburg (South Africa): ZAF64524; 
# 10. Rio de Janeiro: BRA63739


library(ggplot2)
library(mgcv)

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3/data_processed_final")
path.out <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3/ETmodel_Testing")
if(!dir.exists(path.out)) dir.create(path.out, recursive = T)

cities.all <- dir(path.cities)
cities.cherry <- c("USA26687", "CAN16375", "DEU10109", "USA40447", "AUS66430", "CHL66311", "EGY44702", "CHN31890", "ZAF64524", "BRA63739") # 10 cherry-picked cities
set.seed(525)
cities.random <- sample(cities.all[!cities.all %in% cities.cherry], round((length(cities.all)-length(cities.cherry))*.01, 0), replace = F) # Shoudl be 27
cities.test <- c(cities.cherry, cities.random)
# i=3

# Fitting a series of models to ET to see how we can do
# mod1 <- gam(ET.mean ~ tree.mean + veg.mean + s(x,y), data=dat.test)
# mod2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.test)
# mod3 <- gam(log(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.test)
# mod4 <- gam(ET.mean ~ log(tree.mean) + log(veg.mean) + s(x,y), data=dat.test)
# mod5 <- gam(ET.mean ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.test)
# mod6 <- gam(sqrt(ET.mean) ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.test)
# mod7 <- gam(ET.mean ~  tree.mean*veg.mean + s(x,y), data=dat.test)
# mod8 <- gam(sqrt(ET.mean) ~ tree.mean*veg.mean + s(x,y), data=dat.test)
# lst1 <- gam(ET.mean ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
# lst2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
# lst3 <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y), data=dat.mod)
# lst3b <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
# lst4 <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y), data=dat.mod)
# lst4b <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
# lst5 <- gam(sqrt(ET.mean) ~ s(LST.mean, tree.mean) + s(LST.mean, veg.mean) + s(x,y), data=dat.mod)
# lst6 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(LST.mean) + s(x,y) , data=dat.mod)
# lst7 <- gam(sqrt(ET.mean) ~ s(tree.mean) + s(veg.mean) + s(LST.mean) + s(x,y)n, data=dat.mod)



mod.names <- c(paste0("mod", 1:8), paste0("lst", 1:7), paste0("lst", 3:4, "b"))
metrics <- c("R2", "AIC", "MRSE")
mods.comp <- data.frame(ISOURBID = rep(cities.test, each=length(mod.names)), model=mod.names, R2=NA, AIC=NA, MRSE=NA)
mods.comp$model <- factor(mods.comp$model, levels=mod.names)
mods.comp$ISOURBID <- factor(mods.comp$ISOURBID)
# cityStats <- dtaa.frame(ISOURBID =, model=mod.names, R2=NA, AIC=NA, MRSE=NA)

for(i in 1:length(cities.test)){
  CITY <- cities.test[i]
  dir.create(file.path(path.out, CITY), recursive=T, showWarnings = F)
  print(CITY)
  
  dat.test <- read.csv(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")))
  summary(dat.test)
  
  # hist(dat.test$ET.mean)
  # hist(dat.test$tree.mean)
  # hist(dat.test$veg.mean)
  # hist(sqrt(dat.test$ET.mean))
  # hist(log(dat.test$ET.mean))
  
  map.et <- ggplot(data=dat.test ) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=ET.mean))
  map.tree <- ggplot(data=dat.test ) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=tree.mean))
  map.veg <- ggplot(data=dat.test ) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=veg.mean))
  
  hist.et <- ggplot(data=dat.test ) +
    geom_histogram(aes(x=ET.mean))
  hist.tree <- ggplot(data=dat.test ) +
    geom_histogram(aes(x=tree.mean))
  hist.veg <- ggplot(data=dat.test ) +
    geom_histogram(aes(x=veg.mean))
  
  plot.lst <- ggplot(data=dat.test, aes(x=LST.mean, y=ET.mean)) +
    geom_point() +
    stat_smooth(method="lm")
  
  plot.tree <- ggplot(data=dat.test, aes(x=tree.mean, y=ET.mean)) +
    geom_point() +
    stat_smooth(method="lm")
  
  plot.veg <- ggplot(data=dat.test, aes(x=veg.mean, y=ET.mean)) +
    geom_point() +
    stat_smooth(method="lm")
  
  png(file.path(path.out, CITY, paste0(CITY, "_rawGlance.png")), height=8, width=12, units="in", res=120)
  print(cowplot::plot_grid(map.et, map.tree, map.veg, hist.et, hist.tree, hist.veg, plot.lst, plot.tree, plot.veg, byrow = F, ncol=3))
  dev.off()
  
  # Creating a clean thing with no missing data
  dat.mod <- dat.test[!is.na(dat.test$ET.mean) & !is.na(dat.test$tree.mean),]
  summary(dat.mod)
  
  
  # base model
  mod1 <- gam(ET.mean ~ tree.mean + veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod1 <- as.vector(predict(mod1))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod1"] <- summary(mod1)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod1"] <- AIC(mod1)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod1"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod1)^2), na.rm=T)
  
  mod2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod2 <- as.vector(predict(mod2)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod2"] <- summary(mod2)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod2"] <- AIC(mod2)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod2"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod2)^2), na.rm=T)
  
  mod3 <- gam(log(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod3 <- as.vector(exp(predict(mod3)))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod3"] <- summary(mod3)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod3"] <- AIC(mod3)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod3"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod3)^2), na.rm=T)
  
  mod4 <- gam(ET.mean ~ log(tree.mean) + log(veg.mean) + s(x,y), data=dat.mod[dat.mod$tree.mean>0 & dat.mod$veg.mean>0,])
  dat.mod$mod4[dat.mod$tree.mean>0 & dat.mod$veg.mean>0] <- as.vector(predict(mod4))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod4"] <- summary(mod4)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod4"] <- AIC(mod4)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod4"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod4)^2), na.rm=T)
  
  
  mod5 <- gam(ET.mean ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.mod)
  dat.mod$mod5 <- as.vector(predict(mod5))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod5"] <- summary(mod5)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod5"] <- AIC(mod5)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod5"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod5)^2), na.rm=T)
  
  mod6 <- gam(sqrt(ET.mean) ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.mod)
  dat.mod$mod6 <- as.vector(predict(mod6)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod6"] <- summary(mod6)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod6"] <- AIC(mod6)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod6"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod6)^2), na.rm=T)
  
  mod7 <- gam(ET.mean ~ tree.mean*veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod7 <- as.vector(predict(mod7))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod7"] <- summary(mod7)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod7"] <- AIC(mod7)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod7"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod7)^2), na.rm=T)
  
  
  mod8 <- gam(sqrt(ET.mean) ~ tree.mean*veg.mean + s(x,y), data=dat.mod)
  dat.mod$mod8 <- as.vector(predict(mod8)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="mod8"] <- summary(mod8)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="mod8"] <- AIC(mod8)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="mod8"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod8)^2), na.rm=T)
  
  lst1 <- gam(ET.mean ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
  dat.mod$mod.lst1 <- as.vector(predict(lst1))
  # summary(lst1)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst1"] <- summary(lst1)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst1"] <- AIC(lst1)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst1"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst1)^2), na.rm=T)
  
  
  lst2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + LST.mean + s(x,y), data=dat.mod)
  dat.mod$mod.lst2 <- as.vector(predict(lst2)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst2"] <- summary(lst2)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst2"] <- AIC(lst2)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst2"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst2)^2), na.rm=T)
  
  # summary(lst2)
  
  lst3 <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
  dat.mod$mod.lst3 <- as.vector(predict(lst3))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3"] <- summary(lst3)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3"] <- AIC(lst3)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst3)^2), na.rm=T)
  
  
  lst3b <- gam(ET.mean ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
  dat.mod$mod.lst3b <- as.vector(predict(lst3b))
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3b"] <- summary(lst3b)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3b"] <- AIC(lst3b)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst3b"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst3b)^2), na.rm=T)
  
  
  lst4 <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y), data=dat.mod)
  dat.mod$mod.lst4 <- as.vector(predict(lst4)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4"] <- summary(lst4)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4"] <- AIC(lst4)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst4)^2), na.rm=T)
  
  
  lst4b <- gam(sqrt(ET.mean) ~ (tree.mean + veg.mean)*LST.mean + s(x,y) - tree.mean - veg.mean - LST.mean, data=dat.mod)
  dat.mod$mod.lst4b <- as.vector(predict(lst4b)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4b"] <- summary(lst4b)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4b"] <- AIC(lst4b)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst4b"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst4b)^2), na.rm=T)

  lst5 <- gam(sqrt(ET.mean) ~ s(LST.mean, tree.mean) + s(LST.mean, veg.mean) + s(x,y), data=dat.mod)
  dat.mod$mod.lst5 <- as.vector(predict(lst5)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- summary(lst5)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- AIC(lst5)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst5)^2), na.rm=T)
  
  lst6 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(LST.mean) + s(x,y) , data=dat.mod)
  dat.mod$mod.lst6 <- as.vector(predict(lst6)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst6"] <- summary(lst6)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst6"] <- AIC(lst6)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst6"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst6)^2), na.rm=T)
  
  lst7 <- gam(sqrt(ET.mean) ~ s(tree.mean) + s(veg.mean) + s(LST.mean) + s(x,y)n, data=dat.mod)
  dat.mod$mod.lst7 <- as.vector(predict(lst7)^2)
  mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst7"] <- summary(lst7)$r.sq
  mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst7"] <- AIC(lst7)
  mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst7"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst7)^2), na.rm=T)
  
  # mods.comp$R2[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- summary(lst5)$r.sq
  # mods.comp$AIC[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- AIC(lst5)
  # mods.comp$MRSE[mods.comp$ISOURBID==CITY & mods.comp$model=="lst5"] <- mean(sqrt((dat.mod$ET.mean - dat.mod$mod.lst5)^2), na.rm=T)
  
  
  summary(dat.mod)
  write.csv(dat.mod, file.path(path.out, CITY, paste0(CITY, "_Predictions.csv")), row.names=F)
  
  colsStack <- names(dat.mod)[grep("mod", names(dat.mod))]
  predStack <- stack(dat.mod[,colsStack])
  predStack[,c("x", "y","ET.mean",  "LST.mean", "tree.mean", "veg.mean")] <- dat.mod[,c("x", "y", "ET.mean", "LST.mean", "tree.mean", "veg.mean")]
  summary(predStack)
  # s
  
  png(file.path(path.out, CITY, paste0(CITY, "_PredObs.png")), height=8, width=12, units="in", res=120)
  print(
  ggplot(data=predStack) +
    facet_wrap(~ind) +
    geom_point(aes(x=values, y=ET.mean)) +
    geom_abline(slope=1, intercept=0, color="red"))
  dev.off()
  
  png(file.path(path.out, CITY, paste0(CITY, "_PredResid.png")), height=8, width=12, units="in", res=120)
  print(
  ggplot(data=predStack) +
    facet_wrap(~ind) +
    geom_point(aes(x=values, y=ET.mean-values)) +
    geom_abline(slope=0, intercept=0, color="red"))
  dev.off()
}

summary(mods.comp)

# Doing some standardization to help evaluate patterns
for(CITY in unique(mods.comp$ISOURBID)){
  rows.city <- which(mods.comp$ISOURBID==CITY)
  mods.comp[rows.city,"R2.diff"] <- mods.comp$R2[rows.city] - mean(mods.comp$R2[rows.city], na.rm=T)
  mods.comp[rows.city,"AIC.diff"] <- mods.comp$AIC[rows.city] - mean(mods.comp$AIC[rows.city], na.rm=T)
  mods.comp[rows.city,"MRSE.diff"] <- mods.comp$MRSE[rows.city] - mean(mods.comp$MRSE[rows.city], na.rm=T)
  
  mods.comp[rows.city, "R2.rank"] <- length(rows.city) - rank(mods.comp$R2[rows.city]) +1 # math makes lower Better
  mods.comp[rows.city, "AIC.rank"] <- rank(mods.comp$AIC[rows.city]) # Lower better
  mods.comp[rows.city, "MRSE.rank"] <- rank(mods.comp$MRSE[rows.city]) # Lower better
}
summary(mods.comp)
aggregate(cbind(R2, MRSE) ~ model, data=mods.comp, FUN=mean)
aggregate(cbind(R2, MRSE) ~ model, data=mods.comp, FUN=median)

write.csv(mods.comp, file.path(path.out, "ET_ModelSummaryStats.csv"), row.names=F)

ggplot(data=mods.comp) +
  facet_wrap(~ISOURBID) +
  geom_bar(aes(x=model, y=R2, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  facet_wrap(~ISOURBID) +
  geom_bar(aes(x=model, y=MRSE, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))


# ggplot(data=mods.comp) +
#   facet_wrap(~ISOURBID) +
#   geom_bar(aes(x=model, y=R2.diff, fill=model), stat="identity") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  facet_wrap(~model) +
  geom_bar(aes(x=ISOURBID, y=R2.diff, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  facet_wrap(~model) +
  geom_bar(aes(x=ISOURBID, y=MRSE.diff, fill=model), stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))



# Summarizing across models
ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=R2, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=R2.diff, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=MRSE.diff, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=MRSE.diff/MRSE, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

# Graph higher is better
ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=-R2.rank-1, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))

# Graph higher is better better
ggplot(data=mods.comp) +
  geom_boxplot(aes(x=model, y=-MRSE.rank-1, fill=model)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))
