  # Creating a script to do the cross-validation 
  # I'll do 2 sets of cross validation:
  # 1. removing 20% of pixels and calculating bias, RMSE, and R2; I'll iterate through this 50 times for each city
  # 2. Temporal CV: use the first 16 years to predict the last 4 (20% of time; with future prediction emphasis); this will only happen once
  # Note: Will have separate scripts for LST & ET to make them easier to run simultaneously (although someone more motivated than me could set this up to be parallelized and run much faster)
  
  library(mgcv); library(doParallel); library(foreach)
  
  path.google <- file.path("~/Google Drive/")
  path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")
  fsave <- file.path(path.cities, "../CrossValidation-LST.csv")
  
  overwrite=F # whether to overwrite our file or not
  niter=50 # Number of iterations for our bootstrap
  pDat = 0.2 # Proportion of data to withhold fo crossvalidation
  
  # Setting up the cluster to parallelize comptuation
  n_cores <- detectCores()-4 # leave a couple cores free for other processes
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # 0. read in datasets
  
  if(!file.exists(fsave) | overwrite){
    # Create a dataframe to store the stats we care about
  # City ID; mean error (bias); sd error (bias); mean RMSE, sd RMSE
    cityStatsBase <- read.csv(file.path(path.cities, "../city_stats_model-selection.csv"))
    
    xValidResults <- data.frame(ISOURBID=unique(cityStatsBase$ISOURBID), spatError.mean = NA, spatError.sd = NA, spatRMSE.mean = NA, spatRMSE.sd = NA, timeError=NA, timeRMSE=NA)
    xValidResults <- xValidResults[order(xValidResults$ISOURBID),]
    
    write.csv(xValidResults, fsave, row.names=F)
  } 
  
  xValidResults <- read.csv(fsave)
  xValidResults <- xValidResults[order(xValidResults$ISOURBID),]
  
  summary(xValidResults)
  
  citiesAnalyze <- xValidResults$ISOURBID[is.na(xValidResults$spatError.mean)]
  length(citiesAnalyze)
  
  # Loop through cities -- LST at same time
  for(CITY in citiesAnalyze){
    print(CITY)
    rowCity = which(xValidResults$ISOURBID==CITY)
    
    # set up a dataframe
    xValidSpat <- data.frame(error=rep(NA, niter), RMSE=rep(NA, niter))
    
    # 1.1 Load in data -- 
    valsCity <- read.csv(file.path(path.cities, CITY, paste0(CITY, "_values-All.csv")))
    summary(valsCity)
    
     # - create an x/y coordinate column to make subsetting easy
     # - create a vector of just the coordinates
    cityCoord <- unique(valsCity$location)
    ncoord <- length(cityCoord)
    
    # 2 - Spatial xValidation ----
    set.seed(1221) # Just going ahead and using the same seed for all cities
    xValidSpat <- foreach(iter=1:niter, .combine="rbind", .packages="mgcv") %dopar% {
      # 2.1. Select random pixels
      coordLO <- sample(cityCoord, pDat*ncoord, replace=F)
      
      # 2.2 subset dataframe
      datTrain <- valsCity[!valsCity$location %in% coordLO,]
      datValid <- valsCity[valsCity$location %in% coordLO,]
  
      modLSTCityS3D <- gam(LST_Day ~ cover.tree + s(x,y, elevation) + as.factor(year)-1, data=datTrain)
      # sum.modLSTCityS3D <- summary(modLSTCityS3D
      datValid$LSTgamS3D.pred <- predict(modLSTCityS3D, newdata=datValid) #
      datValid$LSTgamS3D.resid <- datValid$LST_Day - datValid$LSTgamS3D.pred 
      
      # xValidSpat$error[i] <- mean(datValid$LSTgamS3D.resid, na.rm=T)
      # xValidSpat$RMSE[i] <- sqrt(mean(datValid$LSTgamS3D.resid^2, na.rm=T))
      c("error"=mean(datValid$LSTgamS3D.resid, na.rm=T), "RMSE"=sqrt(mean(datValid$LSTgamS3D.resid^2, na.rm=T)))
      
  
    } # End spatial iteration loop
    
    xValidResults$spatError.mean[rowCity] <- mean(xValidSpat[,"error"])
    xValidResults$spatError.sd[rowCity] <- sd(xValidSpat[,"error"])
    xValidResults$spatRMSE.mean[rowCity] <- mean(xValidSpat[,"RMSE"])
    xValidResults$spatRMSE.sd[rowCity] <- sd(xValidSpat[,"RMSE"])
    
    # 3. Temporal validation ----
    yrsNow <- unique(valsCity$year)[order(unique(valsCity$year))]
    yrsTrain <- yrsNow[1:(length(yrsNow)*(1-pDat))]
    
    datTrain <- valsCity[valsCity$year %in% yrsTrain,]
    datValid <- valsCity[!valsCity$year %in% yrsTrain,]
    
    modLSTCityS3D <- gam(LST_Day ~ cover.tree + s(x,y, elevation) + as.factor(year)-1, data=datTrain)
    modSum <- summary(modLSTCityS3D)
    
    # Because we can't use year to fit into the future, we need to do what we'd done for our climate change scenarios, which is using the mean intercept
    intYear <- which(grepl("year", names(modSum$p.coeff)))
    cityIntercept <- mean(modSum$p.coeff[intYear]) # Taking the mean year intercept
    # yrstr <- paste(names(modETCitySum$p.coeff)[1])
    yrUse <- as.numeric(stringr::str_sub(names(modSum$p.coeff)[intYear[1]], start=-4)) # Using a dummy year just to get the model to run
    
    datValid$Intercept <- cityIntercept
    datValid$yearReal <- datValid$year
    datValid$yearDummy <- yrUse
    datValid$year <- yrUse
    
    # sum.modLSTCityS3D <- summary(modLSTCityS3D
    datValid$LSTgamS3D.pred <- predict(modLSTCityS3D, type="link", exclude="as.factor(year)", newdata=datValid) + datValid$Intercept
    datValid$LSTgamS3D.resid <- datValid$LST_Day - datValid$LSTgamS3D.pred 
    
    xValidResults$timeError[rowCity] <- mean(datValid$LSTgamS3D.resid, na.rm=T)
    xValidResults$timeRMSE[rowCity] <- sqrt(mean(datValid$LSTgamS3D.resid^2, na.rm=T))
    
    # Save our cross-validation results
    write.csv(xValidResults, fsave, row.names=F)
  
  }
  stopCluster(cl)
