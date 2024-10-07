# Doing the full quantitative comparisons of Flux Tower data and our 1 km interpolated MODIS product

library(ggplot2)
library(mgcv)
library(nlme)

# file paths for where to put the processed data
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4/data_processed_final")
path.tower <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/ET Validation")

datTower <- read.csv(file.path(path.tower, "FluxTower_ETcomparison_AllTowers.csv"))
summary(datTower)

datTower$Error.Pixel <- datTower$ET.pixel - datTower$ET
datTower$Error.Modis <- datTower$ET.modis - datTower$ET
datTower$Error.PixelSD <- datTower$Error.Pixel/datTower$ET
summary(datTower)

summary(datTower[abs(datTower$Error.Pixel)>abs(datTower$Error.Modis) & !is.na(datTower$Error.Modis),])
summary(datTower[abs(datTower$Error.Pixel)>1,])

ggplot(data=datTower) +
  geom_histogram(aes(x=Error.Pixel, fill=IGBP)) +
  geom_vline(xintercept=0, linetype="dashed") +
  theme_bw()

# ggplot(data=datTower) +
#   geom_histogram(aes(x=Error.PixelSD, fill=IGBP)) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   # coord_cartesian(xlim=c(-5,10)) +
#   theme_bw()

ggplot(data=datTower) +
  geom_point(aes(x=ET.pixel, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")

ggplot(data=datTower) +
  geom_point(aes(x=ET.modis, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")

ggplot(data=datTower) +
  geom_point(aes(x=TA.gldas, y=TA)) +
  geom_abline(intercept=0, slope=1, color="black")

lmTA <- lm(TA ~ TA.gldas, data=datTower)
summary(lmTA)

lmETgldas <- lm(ET ~ ET.gldas, data=datTower)
summary(lmETgldas)

lmETmodis <- lm(ET ~ ET.modis, data=datTower[,])
summary(lmETmodis)

# When comparing the fit with MODIS, important to make sure we're doing apples-to-apples
lmETpixel <- lm(ET ~ ET.pixel, data=datTower[!is.na(datTower$ET.modis),])
summary(lmETpixel)

lmETpg <- lm(ET.gldas ~ ET.pixel, data=datTower)
summary(lmETpg)


lmETtower <- lm(ET ~ ET.modTower, data=datTower[!is.na(datTower$ET.modis),])
summary(lmETtower)

lmETtowerAll <- lm(ET ~ ET.modTower, data=datTower)
summary(lmETtowerAll)


aggTower <- aggregate(cbind(ET, TA, ET.pixel, ET.modis, ET.gldas, Error.Pixel)~ISOURBID + ISO3 + NAME + SITE_ID + IGBP + TOWER_LAT + TOWER_LONG, data=datTower, FUN=mean)

lmETavg <- lm(ET ~ ET.modTower, data=aggTower, na.action=na.omit)
summary(lmETavg)

lmeETavg <- lme(ET ~ ET.modTower, random=list(SITE_ID=~1), data=aggTower, na.action=na.omit)
summary(lmeETavg)
MuMIn::r.squaredGLMM(lmeETavg)

ggplot(data=aggTower) +
  geom_point(aes(x=ET.pixel, y=ET)) +
  geom_abline(intercept=0, slope=1, color="black")


lmeETpix <- lme(ET ~ ET.pixel, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmeETpix)
MuMIn::r.squaredGLMM(lmeETpix)

lmeETpix2 <- lme(ET ~ ET.pixel, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower[!is.na(datTower$ET.modis),], na.action=na.omit)
# summary(lmeETpix2)
MuMIn::r.squaredGLMM(lmeETpix2)

lmeETmodis <- lme(ET ~ ET.modis, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmeETmodis)
MuMIn::r.squaredGLMM(lmETmodis)


lmETgldas <- lme(ET ~ ET.gldas, random=list(ISOURBID=~1, SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmETgldas)
MuMIn::r.squaredGLMM(lmETgldas)

lmETgldas2 <- lme(ET.gldas ~ ET.modTower, random=list(SITE_ID=~1), data=datTower, na.action=na.omit)
# summary(lmETgldas2)
MuMIn::r.squaredGLMM(lmETgldas2)


png(file.path(path.tower, "FluxTower_ETcomparison_AllTowers_scatter.png"), height=8, width=8, units="in", res=220)
ggplot(data=datTower) +
  geom_point(aes(x=ET.pixel, y=ET, color=Dataset)) +
  geom_abline(intercept=0, slope=1, color="black")
dev.off()

png(file.path(path.tower, "FluxTower_ETcomparison_AllTowers-IGBP_scatter.png"), height=8, width=8, units="in", res=220)
ggplot(data=datTower) +
  geom_point(aes(x=ET.pixel, y=ET, color=IGBP)) +
  geom_abline(intercept=0, slope=1, color="black")
dev.off()

png(file.path(path.tower, "FluxTower_ETcomparison_Urban.png"), height=8, width=8, units="in", res=220)
ggplot(data=datTower[datTower$IGBP=="URB",]) +
  geom_point(aes(x=ET.pixel, y=ET, color=IGBP)) +
  geom_abline(intercept=0, slope=1, color="black")
dev.off()


lmeETpixUrbMod <- lme(ET ~ ET.modis, random=list(SITE_ID=~1), data=datTower[datTower$IGBP=="URB",], na.action=na.omit)
summary(lmeETpixUrbMod)
MuMIn::r.squaredGLMM(lmeETpixUrbMod) 

lmeETpixUrb <- lme(ET ~ ET.pixel, random=list(SITE_ID=~1), data=datTower[datTower$IGBP=="URB",], na.action=na.omit)
summary(lmeETpixUrb)
MuMIn::r.squaredGLMM(lmeETpixUrb) 

lmeETpixUrb2 <- lme(ET ~ ET.pixel, random=list(SITE_ID=~1), data=datTower[datTower$IGBP=="URB" & !is.na(datTower$ET.modis),], na.action=na.omit)
summary(lmeETpixUrb2)
MuMIn::r.squaredGLMM(lmeETpixUrb2) 

lmETpixUrb <- lm(ET ~ ET.pixel, data=datTower[datTower$IGBP=="URB",], na.action=na.omit)
summary(lmETpixUrb)
# MuMIn::r.squaredGLMM(lmeETpixUrb)
