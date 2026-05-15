# Calibrate per-city Kc (crop coefficients) from ERA5 total ET and FAO-56 ET0.
#
# For each city, fits a linear mixed-effects model using non-urban buffer pixels:
#   Kc_pixel_year = et_total_mm / ET0_mean_daily
#   lmer(Kc ~ frac_tree + frac_veg + (1 | year), REML = TRUE)
#
# Model parameterization note:
#   frac_tree + frac_veg + frac_bare = 1 by construction, so including all three
#   fractions as predictors causes perfect rank deficiency. The solution is to use
#   an intercept and drop frac_bare:
#     Intercept = Kc_bare
#     beta_tree = Kc_tree - Kc_bare  (contrast over bare)
#     beta_veg  = Kc_veg  - Kc_bare
#   Derived: Kc_tree = Kc_bare + beta_tree; Kc_veg = Kc_bare + beta_veg
#
# Uncertainty propagation (for use in 14b):
#   ET = ET0 * (Kc_bare + frac_tree * beta_tree + frac_veg * beta_veg)
#   SE(ET/ET0) = sqrt(t(f) %*% vcov(fit) %*% f),  where f = c(1, frac_tree, frac_veg)
#   Six VCV elements are saved: var_Kc_bare, var_beta_tree, var_beta_veg,
#   cov_bare_tree, cov_bare_veg, cov_tree_veg
#
# Inputs:
#   ERA5 ET rasters (GDrive v5):   {ISOURBID}_ERA5_et_raster_*.tif
#     20 bands (et_total_2001 - et_total_2020); summer-mean daily mm
#     Source: GEE total_evaporation_sum (TRUE total ET, unaffected by band-label swap)
#   Vegetation rasters (GDrive v4): {ISOURBID}_Vegetation_Percent{Tree,OtherVeg}.tif
#     20 bands (YR2001 - YR2020); MODIS sinusoidal ~926 m; values in percent (0-100)
#   Buffer mask (GDrive v4):        {ISOURBID}_Buffer-NoUrb.tif
#     1 = non-urban buffer, NaN = city interior
#   ET0 summary (Shared Drive):     city_ET0_summary.csv
#     Columns: ISOURBID, year, ET0_mean_daily
#   SDEI shapefile:                 sdei-global-uhi-2013.shp
#
# Outputs:
#   Per-city model:  {ISOURBID}_Kc_model.rds      → GDrive v5
#   Summary table:   city_Kc_params.csv            → Shared Drive Analysis_v5

if (!grepl("scripts", getwd())) setwd("scripts")

library(terra)
library(lme4)
library(MuMIn)
library(lubridate)


# ---- Paths ------------------------------------------------------------------
path.google <- file.path("~/Google Drive/My Drive")
path.v4     <- file.path(path.google, "UHI_Analysis_Output_Final_v4")
path.v5     <- file.path(path.google, "UHI_Analysis_Output_Final_v5")
path.proc   <- file.path("~/Google Drive/Shared Drives",
                         "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v5")
path.shp    <- file.path("..", "data_raw", "sdei-global-uhi-2013-shp", "shp",
                         "sdei-global-uhi-2013.shp")

file.et0    <- file.path(path.proc, "city_ET0_summary.csv")
file.kc.out <- file.path(path.proc, "city_Kc_params.csv")

overwrite <- T


# ---- Settings ---------------------------------------------------------------
years      <- 2001:2020
kc.min     <- 0     # Kc below this flagged as outlier and excluded
kc.max     <- 1.5   # Kc above this flagged as outlier and excluded
min.pixels <- 3     # minimum ERA5 pixels required to attempt a model fit


# ---- Load city metadata and ET0 ---------------------------------------------
sdei.df <- data.frame(terra::vect(path.shp))
sdei.df <- sdei.df[sdei.df$ES00POP >= 100e3 & sdei.df$SQKM_FINAL >= 1e2, ]

et0.df <- read.csv(file.et0, stringsAsFactors = FALSE)


# ---- Identify cities with all required inputs -------------------------------
cities.et  <- sub("_ERA5_et_raster.*\\.tif$", "",
                  dir(path.v5, pattern = "_ERA5_et_raster.*\\.tif$"))
cities.tree <- sub("_Vegetation_PercentTree\\.tif$", "",
                  dir(path.v4, pattern = "_Vegetation_PercentTree\\.tif$"))
cities.veg <- sub("_Vegetation_PercentOtherVeg\\.tif$", "",
                  dir(path.v4, pattern = "_Vegetation_PercentOtherVeg\\.tif$"))
cities.buf <- sub("_Buffer-NoUrb\\.tif$", "",
                  dir(path.v4, pattern = "_Buffer-NoUrb\\.tif$"))

cities.use <- Reduce(intersect,
                     list(cities.et, cities.tree, cities.veg, cities.buf,
                          unique(et0.df$ISOURBID), sdei.df$ISOURBID))
message(length(cities.use), " cities with all required inputs.")

if (!overwrite) {
  done <- sub("_Kc_model\\.rds$", "", dir(path.v5, pattern = "_Kc_model\\.rds$"))
  cities.use <- cities.use[!cities.use %in% done]
  message(length(cities.use), " cities remaining after skipping completed.")
}
if (length(cities.use) == 0) stop("All cities complete. Set overwrite=TRUE to rerun.")


# ---- Helper: fit Kc model for one city --------------------------------------
fit_city_kc <- function(CITY) {

  # -- ERA5 ET raster (20 bands: et_total_2001 - et_total_2020)
  et.files <- dir(path.v5,
                  pattern = paste0("^", CITY, "_ERA5_et_raster.*\\.tif$"),
                  full.names = TRUE)
  if (length(et.files) == 0) { warning(CITY, ": ET raster not found"); return(NULL) }
  r.et <- terra::rast(et.files[1])
  if (nlyr(r.et) != length(years)) {
    warning(CITY, ": ET raster has ", nlyr(r.et), " bands, expected ", length(years))
    return(NULL)
  }
  names(r.et) <- paste0("et_", years)

  # -- Vegetation rasters (MODIS sinusoidal ~926 m; 20 bands: YR2001 - YR2020)
  r.tree <- terra::rast(file.path(path.v4, paste0(CITY, "_Vegetation_PercentTree.tif")))
  r.veg  <- terra::rast(file.path(path.v4, paste0(CITY, "_Vegetation_PercentOtherVeg.tif")))
  names(r.tree) <- paste0("tree_", years)
  names(r.veg)  <- paste0("veg_",  years)

  # -- Buffer mask (1 = non-urban buffer, NaN = city interior)
  r.buf <- terra::rast(file.path(path.v4, paste0(CITY, "_Buffer-NoUrb.tif")))

  # -- Reproject vegetation and buffer mask to ERA5 grid
  #    terra::project handles sinusoidal → WGS84 reprojection and ~12:1 aggregation.
  #    method="average" gives mean fractional cover per ERA5 pixel.
  r.tree.era5 <- terra::project(r.tree, r.et, method = "average") / 100
  r.veg.era5  <- terra::project(r.veg,  r.et, method = "average") / 100
  # r.buf.era5  <- terra::project(r.buf,  r.et, method = "average")

  # Working with whatever data we have -- not just buffer!
  # buf.mask <- r.buf.era5 > 0.5
  n.pixels <- sum(!is.na(terra::values(r.tree.era5[[1]])))

  if (n.pixels < min.pixels) {
    message("  ", CITY, ": only ", n.pixels, " buffer pixel(s) — skipping model fit.")
    return(data.frame(ISOURBID         = CITY,
                      NAME             = sdei.df$NAME[sdei.df$ISOURBID == CITY],
                      LATITUDE         = sdei.df$LATITUDE[sdei.df$ISOURBID == CITY],
                      LONGITUDE        = sdei.df$LONGITUDE[sdei.df$ISOURBID == CITY],
                      WWF_BIOME        = sdei.df$WWF_BIOME[sdei.df$ISOURBID == CITY],
                      WWF_ECO          = sdei.df$WWF_ECO[sdei.df$ISOURBID == CITY],
                      model_type       = "skipped_too_few_pixels",
                      n_pixels         = n.pixels,
                      flag_n_pixels_low = TRUE,
                      stringsAsFactors = FALSE))
  }

  # -- ET0 for this city by year
  et0.city <- et0.df[et0.df$ISOURBID == CITY, c("year", "ET0_mean_daily")]

  # -- Extract buffer pixel values for each year → long-format data frame
  pixel.list <- lapply(seq_along(years), function(j) {
    yr     <- years[j]
    et0.yr <- et0.city$ET0_mean_daily[et0.city$year == yr]
    if (length(et0.yr) == 0 || is.na(et0.yr) || et0.yr <= 0) return(NULL)

    stk <- c(r.et[[j]], r.tree.era5[[j]], r.veg.era5[[j]])
    names(stk) <- c("et_mm", "frac_tree", "frac_veg")

    df <- as.data.frame(stk, na.rm = TRUE)
    df <- df[!is.nan(df$et_mm), ]
    if (nrow(df) == 0) return(NULL)

    df$Kc        <- df$et_mm / et0.yr
    df$frac_tree <- pmax(0, pmin(1, df$frac_tree))
    df$frac_veg  <- pmax(0, pmin(1, df$frac_veg))
    df$frac_bare <- pmax(0, 1 - df$frac_tree - df$frac_veg)
    df$year      <- factor(yr)
    df[, c("year", "Kc", "frac_tree", "frac_veg", "frac_bare")]
  })

  pixel.df <- do.call(rbind, pixel.list[!sapply(pixel.list, is.null)])
  if (is.null(pixel.df) || nrow(pixel.df) == 0) {
    warning(CITY, ": no valid pixel-year observations after extraction")
    return(NULL)
  }


  ## CR -- skipping this because there's not a clear reason for 1.5 at this point since my ET0 could have issues
  # # -- Remove Kc outliers before fitting
  # n.raw     <- nrow(pixel.df)
  # pixel.df  <- pixel.df[pixel.df$Kc >= kc.min & pixel.df$Kc <= kc.max &
  #                         !is.na(pixel.df$Kc), ]
  # n.removed <- n.raw - nrow(pixel.df)
  # if (nrow(pixel.df) < min.pixels) {
  #   warning(CITY, ": too few observations after outlier removal")
  #   return(NULL)
  # }

  mean.frac.tree <- mean(pixel.df$frac_tree, na.rm = TRUE)
  mean.frac.veg  <- mean(pixel.df$frac_veg,  na.rm = TRUE)
  mean.frac.bare <- mean(pixel.df$frac_bare, na.rm = TRUE)

  # -- Fit lmer: intercept = Kc_bare; slopes = contrast of tree/veg over bare.
  #    All three fractions cannot be included simultaneously (sum-to-one collinearity).
  model.type <- "lmer"
  fit <- tryCatch(
    lme4::lmer(Kc ~ frac_tree + frac_veg + (1 | year),
               data    = pixel.df,
               REML    = TRUE,
               control = lmerControl(optimizer = "bobyqa")),
    error = function(e) {
      warning(CITY, ": lmer failed (", conditionMessage(e), "), falling back to lm")
      NULL
    }
  )

  if (is.null(fit)) {
    model.type <- "lm_fallback"
    fit <- tryCatch(
      lm(Kc ~ frac_tree + frac_veg, data = pixel.df),
      error = function(e) { warning(CITY, ": lm fallback also failed"); NULL }
    )
  }
  if (is.null(fit)) return(NULL)

  # -- Save model object
  saveRDS(fit, file.path(path.v5, paste0(CITY, "_Kc_model.rds")))

  # -- Extract fixed effects and VCV matrix
  is.lmer <- inherits(fit, "lmerMod")
  fe  <- if (is.lmer) lme4::fixef(fit) else coef(fit)
  V   <- as.matrix(vcov(fit))   # 3×3: rows/cols = (Intercept), frac_tree, frac_veg
  se  <- sqrt(diag(V))
  t.vals <- fe / se

  # -- Derived Kc values (intercept = Kc_bare; add betas to get Kc_tree/Kc_veg)
  Kc.bare <- unname(fe["(Intercept)"])
  Kc.tree <- unname(fe["(Intercept)"] + fe["frac_tree"])
  Kc.veg  <- unname(fe["(Intercept)"] + fe["frac_veg"])

  # Delta-method SEs for derived Kc_tree and Kc_veg
  SE.Kc.bare <- unname(se["(Intercept)"])
  SE.Kc.tree <- sqrt(V["(Intercept)","(Intercept)"] +
                       V["frac_tree","frac_tree"] +
                       2 * V["(Intercept)","frac_tree"])
  SE.Kc.veg  <- sqrt(V["(Intercept)","(Intercept)"] +
                       V["frac_veg","frac_veg"] +
                       2 * V["(Intercept)","frac_veg"])

  # -- City-level weighted Kc and SE using mean buffer fractions
  #    ET = ET0 * t(f) %*% fe,  f = c(1, mean_frac_tree, mean_frac_veg)
  f.mean       <- c(1, mean.frac.tree, mean.frac.veg)
  Kc.city.mean <- sum(f.mean * fe)
  SE.Kc.city   <- sqrt(as.numeric(t(f.mean) %*% V %*% f.mean))

  # -- Model fit metrics
  if (is.lmer) {
    r2 <- tryCatch(MuMIn::r.squaredGLMM(fit)[1, c("R2m", "R2c")],
                   error = function(e) c(R2m = NA_real_, R2c = NA_real_))
    vc     <- lme4::VarCorr(fit)
    sd.yr  <- as.data.frame(vc)$sdcor[1]
    sig    <- sigma(fit)
    icc    <- sd.yr^2 / (sd.yr^2 + sig^2)
    flag.singular   <- lme4::isSingular(fit)
    flag.converged  <- length(fit@optinfo$conv$lme4$messages) == 0
  } else {
    s   <- summary(fit)
    r2  <- c(R2m = s$r.squared, R2c = NA_real_)
    sd.yr <- NA_real_; icc <- NA_real_; sig <- sigma(fit)
    flag.singular  <- FALSE
    flag.converged <- TRUE
  }

  fit.df <- data.frame(
    # City metadata
    ISOURBID             = CITY,
    NAME                 = sdei.df$NAME[sdei.df$ISOURBID == CITY],
    LATITUDE             = sdei.df$LATITUDE[sdei.df$ISOURBID == CITY],
    LONGITUDE            = sdei.df$LONGITUDE[sdei.df$ISOURBID == CITY],
    model_type           = model.type,
    # Sample size
    n_pixels             = n.pixels,
    n_pixel_years        = nrow(pixel.df),
    # n_outliers_removed   = n.removed,
    # Raw model parameters (use these in 14b for delta-method ET uncertainty)
    Kc_bare              = Kc.bare,
    beta_tree            = unname(fe["frac_tree"]),
    beta_veg             = unname(fe["frac_veg"]),
    SE_Kc_bare           = SE.Kc.bare,
    SE_beta_tree         = unname(se["frac_tree"]),
    SE_beta_veg          = unname(se["frac_veg"]),
    t_bare               = unname(t.vals["(Intercept)"]),
    t_tree               = unname(t.vals["frac_tree"]),
    t_veg                = unname(t.vals["frac_veg"]),
    # VCV matrix elements — all six unique entries needed for propagation
    var_Kc_bare          = V["(Intercept)","(Intercept)"],
    var_beta_tree        = V["frac_tree","frac_tree"],
    var_beta_veg         = V["frac_veg","frac_veg"],
    cov_bare_tree        = V["(Intercept)","frac_tree"],
    cov_bare_veg         = V["(Intercept)","frac_veg"],
    cov_tree_veg         = V["frac_tree","frac_veg"],
    # Derived Kc values for reporting and interpretation
    Kc_tree              = Kc.tree,
    Kc_veg               = Kc.veg,
    SE_Kc_tree           = SE.Kc.tree,
    SE_Kc_veg            = SE.Kc.veg,
    # City-level weighted Kc (mean buffer fractions × model parameters)
    mean_frac_tree       = mean.frac.tree,
    mean_frac_veg        = mean.frac.veg,
    mean_frac_bare       = mean.frac.bare,
    Kc_city_mean         = Kc.city.mean,
    SE_Kc_city           = SE.Kc.city,
    # Model fit
    R2_marginal          = unname(r2["R2m"]),
    R2_conditional       = unname(r2["R2c"]),
    sigma_resid          = sig,
    sd_year              = sd.yr,
    icc_year             = icc,
    # Flags
    flag_converged       = flag.converged,
    flag_singular        = flag.singular,
    flag_n_pixels_low    = n.pixels < min.pixels,
    flag_kc_bare_neg     = Kc.bare < 0,
    flag_kc_tree_neg     = Kc.tree < 0,
    flag_kc_veg_neg      = Kc.veg  < 0,
    stringsAsFactors     = FALSE
  )

  return(fit.df)
}


# ---- Main loop --------------------------------------------------------------
summary.list <- vector("list", length(cities.use))
pb <- txtProgressBar(min = 0, max = length(cities.use), style = 3)

for (i in seq_along(cities.use[1:5])) {
  setTxtProgressBar(pb, i)
  summary.list[[i]] <- tryCatch(
    fit_city_kc(cities.use[i]),
    error = function(e) {
      warning(cities.use[i], ": unexpected error — ", conditionMessage(e))
      NULL
    }
  )
}
close(pb)


# ---- Compile and write summary CSV ------------------------------------------
# Merge new results with any previously saved rows (when overwrite=FALSE)
new.df <- do.call(rbind, summary.list[!sapply(summary.list, is.null)])

if (!overwrite && file.exists(file.kc.out)) {
  old.df <- read.csv(file.kc.out, stringsAsFactors = FALSE)
  # Remove stale rows for cities just reprocessed, then append fresh results
  old.df <- old.df[!old.df$ISOURBID %in% new.df$ISOURBID, ]
  out.df <- rbind(old.df, new.df)
} else {
  out.df <- new.df
}


summary(new.df)

out.df <- out.df[order(out.df$ISOURBID), ]
write.csv(out.df, file.kc.out, row.names = FALSE)
message("Kc parameter table saved: ", file.kc.out,
        " (", nrow(out.df), " cities, ",
        sum(out.df$model_type == "lmer",       na.rm = TRUE), " lmer, ",
        sum(out.df$model_type == "lm_fallback", na.rm = TRUE), " lm fallback, ",
        sum(grepl("skipped", out.df$model_type), na.rm = TRUE), " skipped)")


# ---- VERIFICATION (run manually after full run) -----------------------------
# Quick single-city test:
# CITY <- "USA4201"   # Chicago — temperate broadleaf buffer, ~10 pixels expected
# result <- fit_city_kc(CITY)
# str(result)
# result[, c("Kc_bare","Kc_tree","Kc_veg","SE_Kc_tree","R2_marginal","flag_singular")]
#
# Sanity check ranges by biome (from FAO-56 / FLUXNET literature):
#   Kc_tree: 0.5–1.0 temperate broadleaf, 0.1–0.5 desert
#   Kc_veg:  0.4–0.9 temperate, 0.1–0.4 arid
#   Kc_bare: 0.05–0.3 (should be lowest Kc)
#
# Inspect all cities with negative Kc or singular fits:
# out.df[out.df$flag_kc_tree_neg | out.df$flag_singular, c("ISOURBID","NAME","WWF_BIOME",
#   "Kc_bare","Kc_tree","Kc_veg","n_pixels","flag_singular")]
#
# Distribution of city-mean Kc by biome:
# boxplot(Kc_city_mean ~ WWF_BIOME, data=out.df, las=2)
#
# Check propagated SE magnitude relative to Kc_city_mean:
# hist(out.df$SE_Kc_city / out.df$Kc_city_mean)   # CV of Kc estimate per city
