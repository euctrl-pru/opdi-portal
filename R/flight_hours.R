# Observed flight hours per day across the OPDI sensor network.
#
# This is a MANUAL recalculation script, not part of the site render: it scans a
# full day of OpenSky state vectors on S3 (~90s) and the result is pasted into
# `sensors$flight_hours_per_day` in R/portal_config.R.
#
#   Rscript R/flight_hours.R              # yesterday-1, the default
#   Rscript R/flight_hours.R 2026-07-08   # a specific day
#   Rscript R/flight_hours.R 2026-07-06 2026-07-08   # a range, one row per day
#
# Requirements:
#   - euctrl-pru/osn-interface  (remotes::install_github("euctrl-pru/osn-interface"))
#   - OSN_USERNAME / OSN_KEY in the environment (OpenSky S3 credentials)
#
# Method
# ------
# The `serials` column of the state-vector parquet lists every receiver that saw
# a given fix, as a space-padded, stringified list ("[  80706008 -1407996758]",
# or "[]" when none is recorded). We cast it to BIGINT[] and keep rows where any
# element is one of our sensors.
#
# Observed flight time is then the sum of intervals between consecutive fixes of
# the same aircraft, counting only gaps <= MAX_GAP_SECONDS. The data is 5-second
# downsampled, so a 5s gap is normal sampling and counts; a large gap means the
# aircraft left our coverage and returned, and that time was not observed.
#
# The cutoff is not a sensitive parameter. On 2026-07-08, 99.1% of gaps were
# exactly 5s and the 99th percentile was 5s; varying MAX_GAP_SECONDS from 30 to
# 300 moved the total by 3.3% (2348.6 -> 2427.4 hours).
#
# Note the network total is much less than the sum of per-sensor hours: an
# aircraft seen by two receivers at once counts once for the network but once
# for each sensor. The network figure is the one reported on the site.

suppressPackageStartupMessages({
  library(jsonlite)
})

# Only jsonlite is needed to READ the cache, which is all the site does at render
# time. osninterface/DBI load lazily, so a machine without the OpenSky tooling
# can still build the page.
.require_osn_fh <- function() {
  for (p in c("osninterface", "DBI")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(p, " is required to compute flight hours. Install with:\n",
           "  remotes::install_github(\"euctrl-pru/osn-interface\")")
    }
  }
  suppressPackageStartupMessages({ library(osninterface); library(DBI) })
}

if (!exists("opdi_project_root")) {
  opdi_project_root <- function(start = getwd()) {
    d <- normalizePath(start, mustWork = FALSE)
    for (i in 1:6) {
      if (file.exists(file.path(d, "_quarto.yml"))) return(d)
      parent <- dirname(d)
      if (parent == d) break
      d <- parent
    }
    start
  }
}

MAX_GAP_SECONDS <- 60
FLIGHT_HOURS_CACHE <- file.path(opdi_project_root(), "data", "sensors",
                                "flight_hours.json")

#' Read the cached figure, or NULL when it has never been computed.
read_flight_hours <- function() {
  if (!file.exists(FLIGHT_HOURS_CACHE)) return(NULL)
  tryCatch(read_json(FLIGHT_HOURS_CACHE, simplifyVector = TRUE),
           error = function(e) NULL)
}

#' Compute for `date` and write the cache.
compute_flight_hours <- function(date, max_gap = MAX_GAP_SECONDS) {
  .require_osn_fh()
  r <- flight_hours_for_day(date, max_gap = max_gap)
  dir.create(dirname(FLIGHT_HOURS_CACHE), recursive = TRUE, showWarnings = FALSE)
  write_json(list(
    date            = format(as.Date(date)),
    max_gap_seconds = max_gap,
    generated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    flight_hours    = r$flight_hours,
    aircraft        = r$aircraft,
    state_vectors   = r$state_vectors
  ), FLIGHT_HOURS_CACHE, auto_unbox = TRUE, digits = 4, pretty = TRUE)
  message("wrote ", FLIGHT_HOURS_CACHE)
  r
}

# `serials` -> BIGINT[]: strip brackets and padding, drop empty elements (the
# "[]" case), then cast. Without the filter DuckDB errors on ''.
.SERIALS_ARRAY <- paste0(
  "list_transform(",
  "  list_filter(string_split_regex(trim(serials, '[] '), '\\s+'), x -> x <> ''),",
  "  x -> CAST(x AS BIGINT))"
)

#' Our sensor serials, from the PocketBase register or its committed cache.
.opdi_serials <- function() {
  # Anchor on the project root; see the note in sensor_coverage.R.
  root <- getwd()
  for (i in 1:6) {
    if (file.exists(file.path(root, "_quarto.yml"))) break
    root <- dirname(root)
  }
  source(file.path(root, "R", "sensor_coverage.R"), local = TRUE)
  sensors <- get_sensors()
  if (is.null(sensors) || !nrow(sensors)) {
    stop("no sensor serials available (PocketBase and cache both unreachable)")
  }
  as.integer(sensors$serial)
}

#' Observed flight hours and distinct aircraft for a single day.
#'
#' @param date A Date or "YYYY-MM-DD" string.
#' @param serials Integer sensor serials. Defaults to the OPDI register.
#' @param con An open osn_connect() connection, or NULL to open one.
#' @param max_gap Seconds; gaps longer than this are treated as coverage exits.
#' @return data.frame(date, flight_hours, aircraft, state_vectors)
flight_hours_for_day <- function(date,
                                 serials = .opdi_serials(),
                                 con = NULL,
                                 max_gap = MAX_GAP_SECONDS) {
  .require_osn_fh()
  own_con <- is.null(con)
  if (own_con) con <- osn_connect()
  on.exit(if (own_con) osn_disconnect(con), add = TRUE)

  date <- as.Date(date)
  view <- sprintf("sv_%s", format(date, "%Y%m%d"))
  invisible(osn_fetch_day(date, con))

  lit <- sprintf("[%s]::BIGINT[]", paste(serials, collapse = ", "))
  where <- sprintf(
    "serials IS NOT NULL AND serials <> '[]' AND list_has_any(%s, %s)",
    .SERIALS_ARRAY, lit
  )

  res <- dbGetQuery(con, sprintf("
    WITH obs AS (
      SELECT DISTINCT icao24, time FROM %s WHERE %s
    ),
    gaps AS (
      SELECT icao24,
             time - lag(time) OVER (PARTITION BY icao24 ORDER BY time) AS gap
      FROM obs
    )
    SELECT sum(CASE WHEN gap <= %d THEN gap ELSE 0 END) / 3600.0 AS flight_hours,
           count(DISTINCT icao24) AS aircraft,
           count(*) + 1           AS state_vectors
    FROM gaps WHERE gap IS NOT NULL", view, where, max_gap))

  data.frame(date = date,
             flight_hours = round(res$flight_hours, 1),
             aircraft = res$aircraft,
             state_vectors = res$state_vectors)
}

#' Per-sensor breakdown for a day. Hours here sum to more than the network
#' total, because an aircraft seen by several receivers is counted once each.
flight_hours_by_sensor <- function(date,
                                   serials = .opdi_serials(),
                                   con = NULL,
                                   max_gap = MAX_GAP_SECONDS) {
  .require_osn_fh()
  own_con <- is.null(con)
  if (own_con) con <- osn_connect()
  on.exit(if (own_con) osn_disconnect(con), add = TRUE)

  date <- as.Date(date)
  view <- sprintf("sv_%s", format(date, "%Y%m%d"))
  invisible(osn_fetch_day(date, con))

  dbGetQuery(con, sprintf("
    WITH obs AS (
      SELECT DISTINCT s AS serial, icao24, time
      FROM %s, unnest(%s) AS t(s)
      WHERE serials IS NOT NULL AND serials <> '[]' AND s IN (%s)
    ),
    gaps AS (
      SELECT serial, icao24,
             time - lag(time) OVER (PARTITION BY serial, icao24 ORDER BY time) AS gap
      FROM obs
    )
    SELECT serial,
           round(sum(CASE WHEN gap <= %d THEN gap ELSE 0 END) / 3600.0, 1) AS flight_hours,
           count(DISTINCT icao24) AS aircraft
    FROM gaps WHERE gap IS NOT NULL
    GROUP BY 1 ORDER BY flight_hours DESC",
    view, .SERIALS_ARRAY, paste(serials, collapse = ", "), max_gap))
}

# ---- Rscript entry point -----------------------------------------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  dates <- if (length(args) == 0) {
    Sys.Date() - 2
  } else if (length(args) == 1) {
    as.Date(args[1])
  } else {
    seq(as.Date(args[1]), as.Date(args[2]), by = "day")
  }

  .require_osn_fh()
  serials <- .opdi_serials()
  message("sensors: ", length(serials), "   max_gap: ", MAX_GAP_SECONDS, "s")

  con <- osn_connect()
  on.exit(osn_disconnect(con), add = TRUE)

  out <- do.call(rbind, lapply(dates, function(d) {
    r <- flight_hours_for_day(d, serials, con)
    message(sprintf("  %s  flight_hours=%8.1f  aircraft=%d",
                    format(r$date), r$flight_hours, r$aircraft))
    r
  }))

  if (nrow(out) > 1) {
    message("\nmean flight_hours/day: ", round(mean(out$flight_hours), 1))
  }
  print(out, row.names = FALSE)
}
