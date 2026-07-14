# Refresh the caches behind the Sensors page.
#
# Rendering the site always reads the JSON caches under data/ -- it never calls
# an API or scans a Parquet file. This script is the only thing that rewrites
# them. Run it deliberately, check the numbers, and commit the result.
#
#   Rscript R/update_sensor_cache.R                    # every dataset
#   Rscript R/update_sensor_cache.R coverage           # just one
#   Rscript R/update_sensor_cache.R impact capture     # a subset
#
# Parameters (dates, radii, thresholds) live in `update_sensor_pages_cache` in
# R/portal_config.R -- edit them there, not here. The `update` flag in that list
# guards this script: it must be TRUE, either by editing the config or by
# setting OPDI_UPDATE_CACHE=true for a single run:
#
#   OPDI_UPDATE_CACHE=true Rscript R/update_sensor_cache.R
#
# See README.md for what each dataset needs and how long it takes.

suppressPackageStartupMessages(library(jsonlite))

.root <- local({
  d <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(d, "_quarto.yml"))) break
    d <- dirname(d)
  }
  d
})

source(file.path(.root, "R", "portal_config.R"))

CFG <- update_sensor_pages_cache

# Each dataset: what it needs, and how to rebuild it. Kept declarative so adding
# one is a single entry rather than a new branch in the runner below.
DATASETS <- list(

  coverage = list(
    label = "sensor register + reception coverage",
    needs = "PocketBase (public) and OpenSky OAuth (OPENSKY_CLIENT_ID/SECRET)",
    cache = "data/sensors/coverage.json",
    run = function(p) {
      source(file.path(.root, "R", "sensor_coverage.R"), local = TRUE)
      r <- get_sensor_coverage(refresh = TRUE, lookback_days = p$lookback_days)
      if (!identical(r$source, "live")) {
        stop("could not reach PocketBase/OpenSky; cache left unchanged")
      }
      message("  ", nrow(r$sensors), " sensors, ", length(r$coverage), " coverage polygons")
      invisible(r)
    }
  ),

  flight_hours = list(
    label = "observed flight hours per day",
    needs = "OpenSky S3 (OSN_USERNAME/OSN_KEY) and osninterface",
    cache = "data/sensors/flight_hours.json",
    run = function(p) {
      source(file.path(.root, "R", "flight_hours.R"), local = TRUE)
      r <- compute_flight_hours(p$date, p$max_gap_seconds)
      message(sprintf("  %.1f flight hours from %d aircraft on %s",
                      r$flight_hours, r$aircraft, p$date))
      invisible(r)
    }
  ),

  impact = list(
    label = "added coverage benefit per airport",
    needs = "OpenSky S3 (OSN_USERNAME/OSN_KEY) and osninterface",
    cache = "data/sensors/airport_impact.json",
    run = function(p) {
      source(file.path(.root, "R", "airport_impact.R"), local = TRUE)
      r <- compute_airport_impact(p$date, p$radius_nm, p$max_gap_seconds)
      message("  ", nrow(r), " airports")
      invisible(r)
    }
  ),

  capture = list(
    label = "annual capture rate vs reported traffic",
    needs = "nothing -- both sources are open",
    cache = "data/reference/airport_capture_{year}.json",
    run = function(p) {
      source(file.path(.root, "R", "airport_capture.R"), local = TRUE)
      r <- compute_airport_capture(p$year)
      message("  ", nrow(r), " airports, overall ",
              round(100 * sum(r$opdi_movements) / sum(r$reported_movements), 1), "%")
      invisible(r)
    }
  ),

  capture_daily = list(
    label = "daily capture rate over recent months",
    needs = "nothing -- both sources are open",
    cache = "data/reference/airport_capture_daily.json",
    run = function(p) {
      source(file.path(.root, "R", "airport_capture_daily.R"), local = TRUE)
      r <- compute_airport_capture_daily(
        year = p$year,
        months = p$first_month:p$last_month,
        gap_threshold_pct = p$gap_day_threshold_pct,
        outage_recovery_pct = p$outage_recovery_pct,
        outage_min_days = p$outage_min_days)
      message("  ", nrow(r), " airport-days")
      invisible(r)
    }
  )
)

update_sensor_cache <- function(which = CFG$datasets) {
  if (!isTRUE(CFG$update)) {
    stop("update_sensor_pages_cache$update is FALSE.\n",
         "  Set it TRUE in R/portal_config.R, or run:\n",
         "    OPDI_UPDATE_CACHE=true Rscript R/update_sensor_cache.R",
         call. = FALSE)
  }

  unknown <- setdiff(which, names(DATASETS))
  if (length(unknown)) {
    stop("unknown dataset(s): ", paste(unknown, collapse = ", "),
         "\n  available: ", paste(names(DATASETS), collapse = ", "), call. = FALSE)
  }

  results <- list()
  for (nm in which) {
    d <- DATASETS[[nm]]
    message("\n=== ", nm, ": ", d$label)
    message("    needs: ", d$needs)
    t0 <- Sys.time()
    ok <- tryCatch({ d$run(CFG[[nm]]); TRUE },
                   error = function(e) { message("  FAILED: ", conditionMessage(e)); FALSE })
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
    message(sprintf("    %s in %ds", if (ok) "done" else "failed", secs))
    results[[nm]] <- ok
  }

  message("\n--- summary ---")
  for (nm in names(results)) {
    message(sprintf("  %-14s %s", nm, if (results[[nm]]) "ok" else "FAILED"))
  }
  failed <- names(results)[!unlist(results)]
  if (length(failed)) {
    message("\nRe-run the failures once their credentials are in place:")
    message("  OPDI_UPDATE_CACHE=true Rscript R/update_sensor_cache.R ",
            paste(failed, collapse = " "))
  } else {
    message("\nAll caches refreshed. Review the JSON under data/, then commit.")
  }
  invisible(results)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  which <- if (length(args)) args else CFG$datasets
  update_sensor_cache(which)
}
