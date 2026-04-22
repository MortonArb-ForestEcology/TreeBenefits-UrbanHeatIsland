# ERA5-Land per-city raster extraction for all CDS variables.
# Replaces the variable-specific 01f2_ERA_ET_Rasters_CDS.R with a general handler.
#
# Workflow (per variable):
#   1. Check that all 80 nc files are present (20 years x 2 months x NH+SH).
#   2. Build a 20-band annual-mean stack (year-by-year to bound memory).
#   3. Crop to each city + 10 km buffer and write a GeoTIFF.
#   4. Once all cities are extracted, compress the 80 nc files and delete them.
#
# Input files (from 01f_ERA_GetData_CDS.R):
#   data_raw/era5_cds_temp/era5_{NH|SH}_{year}_{MM}_{short}.nc
#
# Output files (to Google Drive UHI_Analysis_Output_Final_v5):
#   {ISOURBID}_{var}_ERA5-Land_CDS.tif
#   - 20 bands, one per year 2001-2020 (names: "{var}_2001" ... "{var}_2020")
#   - CRS: WGS84 (ERA5 native); FLT4S datatype; clipped to city + 10 km buffer
#
# Compressed archives:
#   data_raw/era5_cds_archived/era5_{var}_ERA5-Land_CDS.tar.gz
#
# UNIT CONVENTIONS (applied after seasonal mean over all hourly layers):
#   t2m, d2m : K -> degrees C  (subtract 273.15)
#   sp       : Pa -> kPa       (divide by 1e3)
#   u10, v10 : m/s             (identity)
#   tp       : m/h -> mm/day   (x 24 x 1e3)
#   ssrd,strd: J/m2/h -> MJ/m2/day (x 24 / 1e6)
#   e, evbs  : m/h -> mm/day, positive  (x 24 x -1e3)
#
# Seasonal windows:
#   Northern Hemisphere (LATITUDE >= 0): July + August (months 7, 8)
#   Southern Hemisphere (LATITUDE <  0): January + February (months 1, 2)

if (!grepl("scripts", getwd())) setwd("scripts")

library(terra)
library(ncdf4)

# ---- Paths ----
path.google      <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
dir.out     <- file.path(path.google, GoogleFolderSave)
dir.temp    <- file.path("../data_raw", "era5_cds_temp")
dir.archive <- file.path("../data_raw", "era5_cds_archived")
dir.create(dir.out,     showWarnings = FALSE, recursive = TRUE)
dir.create(dir.archive, showWarnings = FALSE, recursive = TRUE)

overwrite <- FALSE
years_all <- 2001:2020

# ---- Variable map (matches 01f_ERA_GetData_CDS.R) ----
vars_map <- data.frame(
  short = c("t2m", "d2m", "sp", "u10", "v10", "tp", "ssrd", "strd", "e", "evbs"),
  stringsAsFactors = FALSE
)

# Unit conversion functions applied to the seasonal-mean raster (1 layer).
# For flux vars: mean(hourly layers) * 24 = mean daily total (mathematically equivalent
# to computing daily totals first, then averaging).
unit_fns <- list(
  t2m  = function(r) r - 273.15,
  d2m  = function(r) r - 273.15,
  sp   = function(r) r / 1e3,
  u10  = function(r) r,
  v10  = function(r) r,
  tp   = function(r) r * 24 * 1e3,
  ssrd = function(r) r * 24 / 1e6,
  strd = function(r) r * 24 / 1e6,
  e    = function(r) r * 24 * (-1e3),
  evbs = function(r) r * 24 * (-1e3)
)

# ---- Load and filter cities ----
shp.path  <- file.path("..", "data_raw", "sdei-global-uhi-2013-shp", "shp",
                        "sdei-global-uhi-2013.shp")
sdei.df   <- data.frame(terra::vect(shp.path))
sdei.df   <- sdei.df[sdei.df$ES00POP >= 100e3 & sdei.df$SQKM_FINAL >= 1e2, ]

sdei.vect <- terra::vect(shp.path)
sdei.vect <- sdei.vect[sdei.vect$ES00POP >= 100e3 & sdei.vect$SQKM_FINAL >= 1e2, ]

# 10 km buffer — slow to compute (~minutes), so cache to disk after the first run.
buf_cache <- file.path("..", "data_raw", "sdei_buf_10km.gpkg")
if (file.exists(buf_cache)) {
  message("Loading cached 10 km city buffers: ", buf_cache)
  sdei.buf <- terra::vect(buf_cache)
} else {
  message("Computing 10 km city buffers (slow — cached after this run)...")
  sdei.buf <- terra::buffer(sdei.vect, width = 10000)
  terra::writeVector(sdei.buf, buf_cache, overwrite = TRUE)
  message("  Cached to: ", buf_cache)
}

idx_NH <- which(sdei.df$LATITUDE >= 0)
idx_SH <- which(sdei.df$LATITUDE <  0)

message(nrow(sdei.df), " cities total (", length(idx_NH), " NH, ", length(idx_SH), " SH).")


# ---- Helper: expected nc file paths for one variable ----
expected_nc_files <- function(var) {
  nh <- file.path(dir.temp,
                  paste0("era5_NH_", rep(years_all, each = 2), "_",
                         sprintf("%02d", c(7, 8)), "_", var, ".nc"))
  sh <- file.path(dir.temp,
                  paste0("era5_SH_", rep(years_all, each = 2), "_",
                         sprintf("%02d", c(1, 2)), "_", var, ".nc"))
  c(nh, sh)
}


# ---- Helper: temporal mean of one nc file via ncdf4 + rowSums ----
# Returns a single-layer SpatRaster of the time-mean values.
#
# WHY ncdf4 instead of terra::mean():
#   ERA5 nc files are stored time-first (one compressed spatial chunk per time step).
#   terra::mean() reads in spatial blocks, requiring 700+ scattered decompression
#   seeks per block — extremely slow. ncdf4 reads sequentially in the file's natural
#   time-step order, and rowSums() computes the accumulation with a single C-level
#   vectorized call instead of an R-level apply() loop.
#
# chunk_hours: number of time steps read per iteration. 168 = one week.
#   NH domain (3600x950 pixels): ~2.3 GB per chunk. Reduce if RAM is limited.
nc_temporal_mean <- function(nc_path, var_short, chunk_hours = 168L) {
  
  trast <- terra::rast(nc_path)
  trast
  plot(trast[[1]])


  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))

  lon    <- ncdf4::ncvar_get(nc, "longitude")
  lat    <- ncdf4::ncvar_get(nc, "latitude")
  nlon   <- length(lon)
  nlat   <- length(lat)
  ntimes <- nc$dim$valid_time$len

  r_sum <- numeric(nlon * nlat)   # flat accumulator, longitude-major order

  tInd <- 1L
  while (tInd <= ntimes) {
    cnt   <- min(chunk_hours, ntimes - tInd + 1L)
    chunk <- ncdf4::ncvar_get(nc, var_short,
                              start = c(1L, 1L, tInd),
                              count = c(nlon, nlat, cnt))
    # chunk is [nlon, nlat, cnt]; reshape to [pixels, time] and sum across time
    
    r_sum <- r_sum + rowSums(matrix(chunk, nrow = nlon * nlat, ncol = cnt),
                             na.rm = TRUE)
    tInd <- tInd + cnt
  }

  r_mean_vec <- r_sum / ntimes

  # Build SpatRaster with correct WGS84 extent.
  # ERA5 lat is typically descending (e.g. 90 → -5); min()/max() gives correct bounds.
  res <- abs(lon[2] - lon[1])
  r_out <- terra::rast(
    nrows = nlat, ncols = nlon,
    xmin  = min(lon) - res / 2, xmax = max(lon) + res / 2,
    ymin  = min(lat) - res / 2, ymax = max(lat) + res / 2,
    crs   = "EPSG:4326"
  )
  # terra::values() is row-major from ymax downward.
  # r_mean_vec is [nlon*nlat] in (lon, lat) order with lat[1] = northernmost.
  # Reshape to [nlat, nlon], read as vector → north-first.
  terra::values(r_out) <- as.vector(matrix(r_mean_vec, nrow = nlon, ncol = nlat))
  r_out
}


# ---- Helper: build 20-band annual-mean stack for one hemisphere ----
# Returns a SpatRaster backed by a single GeoTIFF in dir.temp for fast city cropping.
# Caller is responsible for unlinking the file when done (see main loop).
build_annual_stack <- function(var, hemi, MO) {
  conv_fn    <- unit_fns[[var]]
  stack_path <- file.path(dir.temp, paste0("_stack_", var, "_", hemi, ".tif"))
  annual_layers <- vector("list", length(years_all))

  for (yi in seq_along(years_all)) {
    yr       <- years_all[yi]
    nc_paths <- file.path(dir.temp,
                          paste0("era5_", hemi, "_", yr, "_",
                                 sprintf("%02d", MO), "_", var, ".nc"))

    r_months <- lapply(nc_paths, nc_temporal_mean, var_short = var)
    r_mean   <- Reduce("+", r_months) / length(r_months)
    r_conv   <- conv_fn(r_mean)
    names(r_conv) <- paste0(var, "_", yr)
    r_conv[r_conv < -200] <- NA
    annual_layers[[yi]] <- r_conv
    message("    ", hemi, " ", yr, " done.")
  }

  # Write the 20-band stack to one GeoTIFF so city crops read from a single
  # contiguous file rather than from 40 scattered nc files.
  terra::writeRaster(terra::rast(annual_layers), stack_path,
                     overwrite = TRUE, datatype = "FLT4S")
  terra::rast(stack_path)
}


# ---- Helper: extract and save per-city GeoTIFFs ----
# Returns a logical vector: TRUE for each city that has a complete output file.
extract_city_rasters <- function(r_stack, hemi_idx, var) {
  city_ids  <- sdei.df$ISOURBID[hemi_idx]
  city_bufs <- sdei.buf[hemi_idx, ]
  success   <- logical(length(city_ids))

  pb <- txtProgressBar(min = 0, max = length(city_ids), style = 3)
  for (i in seq_along(city_ids)) {
    setTxtProgressBar(pb, i)
    cityID  <- city_ids[i]
    outFile <- file.path(dir.out, paste0(cityID, "_", var, "_ERA5-Land_CDS.tif"))

    if (!overwrite && file.exists(outFile)) {
      success[i] <- TRUE
      next
    }

    r_city <- tryCatch(
      terra::crop(r_stack, city_bufs[i, ]),
      error = function(e) {
        warning("Crop failed for ", cityID, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(r_city)) next

    tryCatch({
      terra::writeRaster(r_city, outFile, filetype = "GTiff",
                         datatype = "FLT4S", overwrite = overwrite)
      success[i] <- TRUE
    }, error = function(e) {
      warning("writeRaster failed for ", cityID, ": ", conditionMessage(e))
    })
  }
  close(pb)
  success
}


# ---- Helper: compress and remove nc files for one variable ----
archive_variable <- function(var) {
  nc_paths <- expected_nc_files(var)
  present  <- nc_paths[file.exists(nc_paths)]
  if (length(present) == 0) {
    message("  No nc files found on disk for ", var, "; nothing to archive.")
    return(invisible(FALSE))
  }

  # Stage files in a temporary subdirectory so tar can use relative paths
  stage_dir <- file.path(dir.temp, paste0("_stage_", var))
  dir.create(stage_dir, showWarnings = FALSE)
  for (f in present) file.rename(f, file.path(stage_dir, basename(f)))

  archive_path <- file.path(dir.archive, paste0("era5_", var, "_ERA5-Land_CDS.tar.gz"))
  staged_names <- basename(present)

  ok <- tryCatch({
    tar(archive_path,
        files       = file.path(stage_dir, staged_names),
        compression = "gzip",
        tar         = "internal")
    TRUE
  }, error = function(e) {
    warning("tar() failed for ", var, ": ", conditionMessage(e))
    FALSE
  })

  if (ok && file.exists(archive_path) && file.info(archive_path)$size > 0) {
    unlink(stage_dir, recursive = TRUE)
    sz_mb <- round(file.info(archive_path)$size / 1e6, 1)
    message("  Archived ", length(present), " files -> ", basename(archive_path),
            " (", sz_mb, " MB)")
    return(invisible(TRUE))
  } else {
    # Archive failed or empty — move files back so they aren't lost
    warning("Archive verification failed for ", var, "; restoring nc files.")
    staged <- dir(stage_dir, full.names = TRUE)
    for (f in staged) file.rename(f, file.path(dir.temp, basename(f)))
    unlink(stage_dir, recursive = TRUE)
    return(invisible(FALSE))
  }
}


# ==============================================================================
# ---- Main loop: process each variable ----
# ==============================================================================

for (var in vars_map$short) {
  message("\n=== Variable: ", var, " ===")

  # Step 1: Check if this variable is already fully archived
  archive_path <- file.path(dir.archive, paste0("era5_", var, "_ERA5-Land_CDS.tar.gz"))
  if (file.exists(archive_path)) {
    message("  Already archived: ", basename(archive_path), " — skipping.")
    next
  }

  # Step 2: Check that all 80 nc files are present
  nc_all    <- expected_nc_files(var)
  nc_missing <- nc_all[!file.exists(nc_all)]
  if (length(nc_missing) > 0) {
    message("  ", length(nc_missing), " / 80 nc files missing — skipping ",
            var, " (run 01f_ERA_GetData_CDS.R to finish downloads).")
    if (length(nc_missing) <= 10)
      message("  Missing: ", paste(basename(nc_missing), collapse = ", "))
    next
  }
  message("  All 80 nc files present.")

  # Step 3: Check which output city TIFs already exist
  expected_tifs_nh <- file.path(dir.out,
                                 paste0(sdei.df$ISOURBID[idx_NH], "_", var,
                                        "_ERA5-Land_CDS.tif"))
  expected_tifs_sh <- file.path(dir.out,
                                 paste0(sdei.df$ISOURBID[idx_SH], "_", var,
                                        "_ERA5-Land_CDS.tif"))

  nh_done <- all(file.exists(expected_tifs_nh))
  sh_done <- all(file.exists(expected_tifs_sh))

  if (nh_done && sh_done && !overwrite) {
    message("  All city TIFs already exist — proceeding to archive.")
  } else {
    # Step 4: Build annual-mean stacks and extract cities

    if (!nh_done || overwrite) {
      message("  Building NH annual stack (Jul+Aug) for ", var, "...")
      r_nh <- build_annual_stack(var, hemi = "NH", MO = c(7, 8))
      message("  Extracting ", length(idx_NH), " NH cities...")
      nh_success <- extract_city_rasters(r_nh, idx_NH, var)
      rm(r_nh)
      unlink(file.path(dir.temp, paste0("_stack_", var, "_NH.tif")))
      gc()
      message("  NH: ", sum(nh_success), " / ", length(idx_NH), " cities written.")
    }

    if (!sh_done || overwrite) {
      message("  Building SH annual stack (Jan+Feb) for ", var, "...")
      r_sh <- build_annual_stack(var, hemi = "SH", MO = c(1, 2))
      message("  Extracting ", length(idx_SH), " SH cities...")
      sh_success <- extract_city_rasters(r_sh, idx_SH, var)
      rm(r_sh)
      unlink(file.path(dir.temp, paste0("_stack_", var, "_SH.tif")))
      gc()
      message("  SH: ", sum(sh_success), " / ", length(idx_SH), " cities written.")
    }
  }

  # Step 5: Archive nc files only after confirming all city TIFs are complete
  nh_complete <- all(file.exists(expected_tifs_nh))
  sh_complete <- all(file.exists(expected_tifs_sh))

  if (nh_complete && sh_complete) {
    message("  All cities extracted. Compressing nc files for ", var, "...")
    archive_variable(var)
  } else {
    n_missing_nh <- sum(!file.exists(expected_tifs_nh))
    n_missing_sh <- sum(!file.exists(expected_tifs_sh))
    warning("  Extraction incomplete for ", var,
            " (NH missing: ", n_missing_nh, ", SH missing: ", n_missing_sh,
            ") — nc files NOT archived. Re-run to retry missing cities.")
  }
}

message("\n=== Done. ===")
message("City TIFs: ", dir.out)
message("Archives : ", dir.archive)


# ---- VERIFICATION (run manually on test cities after first variable completes) ----
# r <- terra::rast(file.path(dir.out, "USA4201_t2m_ERA5-Land_CDS.tif"))
# nlyr(r)            # 20
# names(r)           # t2m_2001 ... t2m_2020
# terra::crs(r)      # WGS84
# summary(terra::values(r[[1]]))  # Chicago Jul/Aug 2001 mean temp: ~18-25 degrees C
#
# For e or evbs (mm/day, positive):
# r2 <- terra::rast(file.path(dir.out, "USA4201_e_ERA5-Land_CDS.tif"))
# summary(terra::values(r2[[1]]))  # should be ~0.5-4 mm/day
#
# Verify archive can be listed (not corrupted):
# system2("tar", c("-tzf",
#   file.path(dir.archive, "era5_t2m_ERA5-Land_CDS.tar.gz")))  # 80 nc files
