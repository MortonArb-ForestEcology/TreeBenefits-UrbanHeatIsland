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

cities.test <- c("USA26687", "CAN16375", "DEU10109", "USA40447", "AUS66430", "CHL66311", "EGY44702", "CHN31890", "ZAF64524", "BRA63739")

dat.test <- read.csv(file.path(path.cities, cities.test[2], paste0(cities.test[2], "_CityStats_Pixels.csv")))
summary(dat.test)
dat.test <- dat.test[!is.na(dat.test$ET.mean) & !is.na(dat.test$tree.mean),]
summary(dat.test)

hist(dat.test$ET.mean)
hist(dat.test$tree.mean)
hist(dat.test$veg.mean)
hist(sqrt(dat.test$ET.mean))
hist(log(dat.test$ET.mean))

plot(ET.mean ~ tree.mean, data=dat.test); abline(lm(ET.mean ~ tree.mean, data=dat.test), col="red")
plot(sqrt(ET.mean) ~ tree.mean, data=dat.test); abline(lm(sqrt(ET.mean) ~ tree.mean, data=dat.test), col="red")
# plot(ET.mean ~ log(tree.mean), data=dat.test); abline(lm(ET.mean ~ log(tree.mean), data=dat.test), col="red")
# plot(log(ET.mean) ~ tree.mean, data=dat.test)
plot(ET.mean ~ sqrt(tree.mean), data=dat.test)
plot(ET.mean ~ log(tree.mean), data=dat.test)

# base model
mod1 <- gam(ET.mean ~ tree.mean + veg.mean + s(x,y), data=dat.test)
summary(mod1)
pred1 <- predict(mod1)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred1); abline(a=0, b=1, col="red")
plot(mod1)
hist(resid(mod1))
plot(resid(mod1) ~ predict(mod1))

# mod1b <- gam(ET.mean ~ tree.mean + veg.mean + s(x,y)-1, data=dat.test)
# summary(mod1b)
# pred1b <- predict(mod1b); summary(pred1b)
# plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred1b); abline(a=0, b=1, col="red")
# plot(mod1b)
# hist(resid(mod1b))
# plot(resid(mod1b) ~ predict(mod1b))


mod2 <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.test)
summary(mod2)
# plot(mod2)
pred2 <- predict(mod2)^2; summary(pred2)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred2); abline(a=0, b=1, col="red")
hist(resid(mod2))
plot(resid(mod2) ~ predict(mod2))
# 
# mod2b <- gam(sqrt(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.test)
# summary(mod2b)
# # plot(mod2)
# pred2b <- predict(mod2b)^2; summary(pred2b)
# plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred2b); abline(a=0, b=1, col="red")
# hist(resid(mod2b))
# plot(resid(mod2b) ~ predict(mod2b))


mod3 <- gam(log(ET.mean) ~ tree.mean + veg.mean + s(x,y), data=dat.test)
summary(mod3)
# plot(mod2)
pred3 <- exp(predict(mod3)); summary(pred3)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred3); abline(a=0, b=1, col="red")
hist(resid(mod3))
plot(resid(mod3) ~ predict(mod3))

# mod3b <- gam(log(ET.mean) ~ tree.mean + veg.mean + s(x,y)-1, data=dat.test)
# summary(mod3b)
# # plot(mod2)
# pred3b <- exp(predict(mod3b))
# plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred3b); abline(a=0, b=1, col="red")
# # hist(resid(mod3b))
# # plot(resid(mod3b) ~ predict(mod3b))


# ATL produces negative values!
mod4 <- gam(ET.mean ~ log(tree.mean) + log(veg.mean) + s(x,y), data=dat.test)
summary(mod4)
pred4 <- predict(mod4); summary(pred4)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred4); abline(a=0, b=1, col="red")
# plot(mod4)
# hist(resid(mod4))
# plot(resid(mod4) ~ predict(mod4))

mod4b <- gam(ET.mean ~ log(tree.mean) + log(veg.mean) + s(x,y)-1, data=dat.test)
summary(mod4b)
pred4b <- predict(mod4b); summary(pred4b)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred4b); abline(a=0, b=1, col="red")
# plot(mod4)
# hist(resid(mod4b))
# plot(resid(mod4b) ~ predict(mod4b))

mod5 <- gam(ET.mean ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.test)
summary(mod5)
# plot(mod5)
pred5 <- predict(mod5); summary(pred5)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred5); abline(a=0, b=1, col="red")
# hist(resid(mod5))
# plot(resid(mod5) ~ predict(mod5))

mod5b <- gam(ET.mean ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y)-1, data=dat.test)
summary(mod5b)
# plot(mod5)
pred5b <- predict(mod5b); summary(pred5b)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred5b); abline(a=0, b=1, col="red")

mod6 <- gam(sqrt(ET.mean) ~ sqrt(tree.mean) + sqrt(veg.mean) + s(x,y), data=dat.test)
summary(mod6)
pred6 <- predict(mod6)^2
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred6); abline(a=0, b=1, col="red")
# plot(mod6)
# plot(resid(mod6) ~ predict(mod6))
# hist(resid(mod6))


mod7 <- gam(ET.mean ~ tree.mean*veg.mean + s(x,y), data=dat.test)
summary(mod7)
pred7 <- predict(mod7)
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred7); abline(a=0, b=1, col="red")
plot(mod7)
hist(resid(mod7))
plot(resid(mod7) ~ predict(mod7))

mod8 <- gam(sqrt(ET.mean) ~ tree.mean*veg.mean + s(x,y), data=dat.test)
summary(mod8)
plot(resid(mod8) ~ predict(mod8))
pred8 <- predict(mod8)^2
plot(dat.test$ET.mean[!is.na(dat.test$ET.mean)] ~ pred8); abline(a=0, b=1, col="red")
plot(mod8)
hist(resid(mod8))
