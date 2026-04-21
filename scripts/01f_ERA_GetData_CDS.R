# 01f_ERA_GetData_CDS.R — Script A of 3-script CDS pipeline
# Submit and download ERA5-Land hourly data from Copernicus CDS.
# Followed by: 01f_ERA_Aggregate_CDS.R (hourly->daily), 01f_ERA_Extract_CDS.R (city extraction)
#
# 800 total jobs: 10 vars x (NH-Jul, NH-Aug, SH-Jan, SH-Feb) x 20 years
# One variable per request avoids CDS file-size limits (~1-2 GB each).
# CDS caps queued requests at 150; this script fills up to MAX_QUEUED then refills
# one-for-one as each job completes, keeping the queue near capacity throughout.
# Resumable: saves job handles to cds_jobs.rds after every change. Re-run to continue.
#
# FIRST-TIME SETUP (run once in R console before executing this script):
#   install.packages("ecmwfr")
#   ecmwfr::wf_set_key(key = "YOUR_CDS_PERSONAL_ACCESS_TOKEN")
#   # Token: https://cds.climate.copernicus.eu -> user icon -> API tokens

if (!grepl("scripts", getwd())) setwd("scripts")
library(ecmwfr)

MAX_QUEUED <- 140   # stay safely under CDS's 150-request limit

# ---- Paths ----
dir.temp    <- file.path(dirname(getwd()), "data_raw", "era5_cds_temp")
dir.archive <- file.path(dirname(getwd()), "data_raw", "era5_cds_archived")
dir.create(dir.temp, showWarnings = FALSE, recursive = TRUE)
jobs_rds <- file.path(dir.temp, "cds_jobs.rds")

# ---- Variable map: CDS API name -> short name used in filenames ----
vars_map <- data.frame(
  cds_name = c(
    "2m_temperature",
    "2m_dewpoint_temperature",
    "surface_pressure",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "total_precipitation",
    "surface_solar_radiation_downwards",
    "surface_thermal_radiation_downwards",
    "total_evaporation",
    "evaporation_from_bare_soil"
  ),
  short = c("t2m", "d2m", "sp", "u10", "v10", "tp", "ssrd", "strd", "e", "evbs"),
  stringsAsFactors = FALSE
)

# ---- Job grid: 800 rows (10 vars x 20 years x 4 month-hemisphere combos) ----
nh_base      <- expand.grid(year = 2001:2020, month = c(7L, 8L), stringsAsFactors = FALSE)
nh_base$hemi <- "NH"
sh_base      <- expand.grid(year = 2001:2020, month = c(1L, 2L), stringsAsFactors = FALSE)
sh_base$hemi <- "SH"

hemi_grid <- rbind(nh_base, sh_base)
job_grid  <- merge(hemi_grid, vars_map, by = NULL)   # full cross-join

job_grid$key    <- paste0(job_grid$hemi, "_", job_grid$year, "_",
                          sprintf("%02d", job_grid$month), "_", job_grid$short)
job_grid$target <- file.path(dir.temp, paste0("era5_", job_grid$key, ".nc"))

message(nrow(job_grid), " total jobs in grid (", nrow(vars_map),
        " vars x 80 month-hemi-year combos).")

# ---- Resume: reload job handles from a prior session ----
jobs_list <- if (file.exists(jobs_rds)) {
  message("Loading saved job handles from: ", jobs_rds)
  readRDS(jobs_rds)
} else {
  list()
}

# Status constants — defined early for startup reconciliation
SUCCESS_STATUS  <- c("successful", "completed")
TERMINAL_STATUS <- c("failed", "dismissed", "deleted", "expired")

# Disk is ground truth: promote any file already on disk to "downloaded"
for (k in names(jobs_list)) {
  if (file.exists(jobs_list[[k]]$target) && jobs_list[[k]]$status != "downloaded")
    jobs_list[[k]]$status <- "downloaded"
}

# Archive is also ground truth: if a variable's tar.gz exists in era5_cds_archived/,
# treat all 80 of its jobs as done so they are excluded from the submit queue.
# This lets you run extraction + archival first, then resume downloads for remaining vars.
if (dir.exists(dir.archive)) {
  tarballs      <- dir(dir.archive, pattern = "^era5_.+_ERA5-Land_CDS\\.tar\\.gz$")
  archived_vars <- sub("^era5_(.+)_ERA5-Land_CDS\\.tar\\.gz$", "\\1", tarballs)
  if (length(archived_vars) > 0) {
    message("Archived variables (", paste(archived_vars, collapse = ", "),
            ") — marking all their jobs as done.")
    archived_keys <- job_grid$key[job_grid$short %in% archived_vars]
    new_keys      <- archived_keys[!archived_keys %in% names(jobs_list)]
    for (k in new_keys) {
      jobs_list[[k]] <- list(job    = NULL,
                             target = job_grid$target[job_grid$key == k],
                             status = "downloaded")
    }
    if (length(new_keys) > 0) saveRDS(jobs_list, jobs_rds)
  }
}

# For still-pending entries, poll CDS to distinguish live jobs from dead ones.
# Only clear jobs CDS confirms as terminal; keep live handles so we can download them.
pending_keys <- names(jobs_list)[sapply(jobs_list, function(x) x$status == "pending")]
if (length(pending_keys) > 0) {
  message("Reconciling ", length(pending_keys),
          " pending handles with CDS (1 sec/job — may take a few minutes)...")
  for (k in pending_keys) {
    entry <- jobs_list[[k]]
    st <- tryCatch({
      entry$job$update_status()
      entry$job$get_status()
    }, error = function(e) "unknown")

    if (st %in% SUCCESS_STATUS) {
      ok <- tryCatch({ entry$job$download(); TRUE }, error = function(e) FALSE)
      jobs_list[[k]]$status <- if (ok && file.exists(entry$target)) "downloaded" else "failed"
      if (jobs_list[[k]]$status == "downloaded") message("  Recovered on reconcile: ", k)
    } else if (st %in% TERMINAL_STATUS) {
      jobs_list[[k]]$status <- "failed"  # mark for clearing below
    }
    # "accepted" / "running" / "unknown" → leave as "pending" (keep the handle)
    Sys.sleep(1)  # stay under CDS rate limit
  }
  saveRDS(jobs_list, jobs_rds)
}

# Clear only confirmed-dead entries; live pending jobs stay in jobs_list
dead <- names(jobs_list)[sapply(jobs_list, function(x) x$status == "failed")]
if (length(dead) > 0) {
  message("Clearing ", length(dead), " dead handles (dismissed/failed) for resubmission...")
  jobs_list[dead] <- NULL
  saveRDS(jobs_list, jobs_rds)
}

n_on_disk <- sum(file.exists(job_grid$target))
n_active <- ifelse(length(jobs_list)>0, sum(sapply(jobs_list, function(x) x$status == "pending")), 0)
# n_active  <- sum(sapply(jobs_list, function(x) x$status == "pending"))

# Jobs not yet submitted and not yet downloaded, ET variables front-loaded
submit_queue <- job_grid[
  !job_grid$key %in% names(jobs_list) & !file.exists(job_grid$target), ,
  drop = FALSE
]
et_first     <- submit_queue$short %in% c("e", "evbs")
submit_queue <- rbind(submit_queue[et_first, ], submit_queue[!et_first, ])
rownames(submit_queue) <- NULL
submit_ptr <- 1L   # index of next row in submit_queue to submit

message(n_on_disk, " files on disk | ", n_active, " currently queued on CDS | ",
        nrow(submit_queue), " waiting to submit (",
        sum(et_first), " ET jobs front-loaded).")

# ---- Helper: submit the next job from submit_queue ----
# Uses <<- to update jobs_list and submit_ptr in the script environment.
submit_next_job <- function() {
  if (submit_ptr > nrow(submit_queue)) return(invisible(FALSE))
  row  <- submit_queue[submit_ptr, ]
  area <- if (row$hemi == "NH") c(90, -180, -5, 180) else c(5, -180, -60, 180)

  request <- list(
    dataset_short_name = "reanalysis-era5-land",
    product_type       = "reanalysis",
    variable           = row$cds_name,
    year               = as.character(row$year),
    month              = sprintf("%02d", row$month),
    day                = sprintf("%02d", 1:31),
    time               = sprintf("%02d:00", 0:23),
    area               = area,                   # N, W, S, E
    data_format        = "netcdf",
    download_format    = "unarchived",
    target             = basename(row$target)
  )

  message("  [", submit_ptr, "/", nrow(submit_queue), "] Submitting: ", row$key)
  job <- tryCatch(
    wf_request(request = request, path = dir.temp, transfer = FALSE, verbose = FALSE),
    error = function(e) {
      warning("Submission failed for ", row$key, ": ", conditionMessage(e))
      NULL
    }
  )

  submit_ptr <<- submit_ptr + 1L

  if (!is.null(job)) {
    jobs_list[[row$key]] <<- list(job = job, target = row$target, status = "pending")
    saveRDS(jobs_list, jobs_rds)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# ---- Initial batch: fill the queue up to MAX_QUEUED ----
n_slots <- max(0L, MAX_QUEUED - n_active)
message("Filling initial queue: submitting up to ", n_slots, " jobs...")
for (s in seq_len(n_slots)) {
  if (submit_ptr > nrow(submit_queue)) break
  submit_next_job()
  if (submit_ptr <= nrow(submit_queue)) Sys.sleep(2)  # avoid 429 rate-limit
}

# Short initial wait if this is a fresh batch; skip on pure resume
n_queued_now <- sum(sapply(jobs_list, function(x) x$status == "pending"))
if (n_queued_now > 0 && n_active == 0) {
  message("Initial batch submitted. Waiting 5 minutes before polling...")
  Sys.sleep(5 * 60)
} else if (n_queued_now > 0) {
  message("Polling ", n_queued_now, " queued jobs + ", max(0, nrow(submit_queue) - submit_ptr + 1),
          " waiting to submit...")
}

# ---- Poll + download + refill loop ----
# job$get_status() returns: "accepted" | "running" | "successful" | "failed" | "dismissed"
# job$is_failed() only catches "failed", NOT "dismissed" — check get_status() directly.
# SUCCESS_STATUS and TERMINAL_STATUS defined above for reuse here.

t_start <- proc.time()["elapsed"]

repeat {
  n_still_pending <- 0

  for (k in names(jobs_list)) {
    entry <- jobs_list[[k]]

    if (entry$status %in% c("downloaded", "failed")) next

    if (file.exists(entry$target)) {
      jobs_list[[k]]$status <- "downloaded"
      saveRDS(jobs_list, jobs_rds)
      next
    }

    # Poll CDS for current status — update_status() mutates the R6 object in place;
    # get_status() reads it back. jobs_list[[k]]$status is our own tracking field
    # and must be explicitly updated below.
    tryCatch(
      entry$job$update_status(),
      error = function(e) warning("update_status() error for ", k, ": ", conditionMessage(e))
    )
    st <- tryCatch(entry$job$get_status(), error = function(e) "unknown")

    slot_freed <- FALSE

    if (st %in% SUCCESS_STATUS) {
      message("Downloading: ", k, " ...")
      # job$download() pulls the file to the path set at wf_request() time (dir.temp).
      # This is the actual download trigger — transfer=FALSE only deferred it.
      ok <- tryCatch({
        entry$job$download()
        TRUE
      }, error = function(e) {
        warning("download() failed for ", k, ": ", conditionMessage(e))
        FALSE
      })

      if (ok && file.exists(entry$target)) {
        jobs_list[[k]]$status <- "downloaded"
        message("  -> saved: ", basename(entry$target))
      } else {
        jobs_list[[k]]$status <- "failed"
        warning("  -> file missing after download(): ", entry$target)
      }
      saveRDS(jobs_list, jobs_rds)
      slot_freed <- TRUE

    } else if (st %in% TERMINAL_STATUS) {
      warning("CDS job '", st, "': ", k)
      jobs_list[[k]]$status <- "failed"
      saveRDS(jobs_list, jobs_rds)
      slot_freed <- TRUE

    } else {
      # still "accepted" or "running" or "unknown"
      n_still_pending <- n_still_pending + 1
    }

    # A slot opened — submit the next waiting job immediately
    if (slot_freed && submit_ptr <= nrow(submit_queue)) {
      Sys.sleep(1)
      submit_next_job()
      if (submit_ptr <= nrow(submit_queue)) Sys.sleep(2)
    }
  }

  n_waiting <- max(0L, nrow(submit_queue) - submit_ptr + 1L)
  if (n_still_pending == 0 && n_waiting == 0) break

  elapsed <- round((proc.time()["elapsed"] - t_start) / 60, 1)
  message(format(Sys.time(), "%H:%M:%S"), " — ", n_still_pending, " running/queued | ",
          n_waiting, " waiting to submit | elapsed: ", elapsed, " min | next poll in 30 sec...")
  Sys.sleep(30)
}

# ---- Final summary ----
n_downloaded <- sum(file.exists(job_grid$target))
statuses     <- sapply(jobs_list, `[[`, "status")
n_failed     <- sum(statuses == "failed")

message("\n=== ERA5-Land CDS download complete ===")
message("Files on disk : ", n_downloaded, " / ", nrow(job_grid))
message("Failed jobs   : ", n_failed)
if (n_failed > 0) {
  message("Failed keys: ", paste(names(statuses)[statuses == "failed"], collapse = ", "))
  message("Re-run this script to retry failed jobs.")
}
message("Output directory: ", dir.temp)
message("Next step: run 01f_ERA_Aggregate_CDS.R")
