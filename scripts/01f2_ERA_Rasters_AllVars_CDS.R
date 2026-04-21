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

# ---- Paths ----
path.google      <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
dir.out     <- file.path(path.google, GoogleFolderSave)
dir.temp    <- file.path(dirname(getwd()), "data_raw", "era5_cds_temp")
dir.archive <- file.path(dirname(getwd()), "data_raw", "era5_cds_archived")
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
sdei.buf  <- terra::buffer(sdei.vect, width = 10000)  # 10 km buffer, matches GEE

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


# ---- Helper: build 20-band annual-mean stack for one hemisphere ----
# Loops year-by-year (loads 2 monthly nc files at a time) to keep memory bounded.
build_annual_stack <- function(var, hemi, months) {
  conv_fn <- unit_fns[[var]]
  annual_layers <- vector("list", length(years_all))

  for (yi in seq_along(years_all)) {
    yr <- years_all[yi]
    nc_paths <- file.path(dir.temp,
                          paste0("era5_", hemi, "_", yr, "_",
                                 sprintf("%02d", months), "_", var, ".nc"))
    r_list <- lapply(nc_paths, terra::rast)
    r_season <- terra::rast(r_list)           # all hourly layers for both months
    r_mean   <- terra::mean(r_season, na.rm = TRUE)  # mean hourly value
    r_conv   <- conv_fn(r_mean)               # apply unit conversion
    names(r_conv) <- paste0(var, "_", yr)
    annual_layers[[yi]] <- r_conv
    rm(r_list, r_season, r_mean, r_conv)
    terra::tmpFiles(remove = TRUE)            # flush terra temp files each year
  }

  terra::rast(annual_layers)  # 20-band stack
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
      r_nh <- build_annual_stack(var, hemi = "NH", months = c(7, 8))
      message("  Extracting ", length(idx_NH), " NH cities...")
      nh_success <- extract_city_rasters(r_nh, idx_NH, var)
      rm(r_nh); terra::tmpFiles(remove = TRUE)
      message("  NH: ", sum(nh_success), " / ", length(idx_NH), " cities written.")
    }

    if (!sh_done || overwrite) {
      message("  Building SH annual stack (Jan+Feb) for ", var, "...")
      r_sh <- build_annual_stack(var, hemi = "SH", months = c(1, 2))
      message("  Extracting ", length(idx_SH), " SH cities...")
      sh_success <- extract_city_rasters(r_sh, idx_SH, var)
      rm(r_sh); terra::tmpFiles(remove = TRUE)
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
