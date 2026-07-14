# Added coverage benefit of the OPDI sensors, per airport.
#
# This is a MANUAL analysis script, not part of the site render: it scans a full
# day of OpenSky state vectors on S3 and writes data/sensors/airport_impact.json,
# which is read by portal_config.R and rendered under "Impact on Airport
# Monitoring" on the Sensors page.
#
#   Rscript R/airport_impact.R              # default day (see ANALYSIS_DATE)
#   Rscript R/airport_impact.R 2026-07-08   # a specific day
#
# Requirements:
#   - euctrl-pru/osn-interface  (remotes::install_github("euctrl-pru/osn-interface"))
#   - OSN_USERNAME / OSN_KEY in the environment (OpenSky S3 credentials)
#
# Question
# --------
# Of the flight hours observed within RADIUS_NM of an airport, what share would
# have gone unobserved without the OPDI sensors there?
#
# Method
# ------
# The airport reference point is its ICAO coordinate (osn_airport_coords), not
# the sensor centroid -- the two differ by under 1.2 km at every OPDI site.
#
# Within the radius, each state vector's `serials` column lists every receiver
# that saw that fix. A fix is EXCLUSIVE when that list contains an OPDI sensor
# and nothing else: no other receiver in the OpenSky Network saw it, so without
# our sensor the fix would not exist.
#
# Flight hours are summed the same way as R/flight_hours.R: the interval between
# consecutive fixes of the same aircraft, counting only gaps <= MAX_GAP_SECONDS,
# since the data is 5-second downsampled and a long gap means the aircraft left
# coverage rather than sat still. Exclusive hours use only the exclusive fixes.
#
#   added_pct = exclusive_hours / total_hours   (within the radius)
#
# Fixes with an empty `serials` list ("[]", ~8% of fixes) carry no receiver
# attribution and are excluded from both numerator and denominator. `total_hours`
# therefore means "hours observed by at least one identified receiver", not "all
# hours flown in the radius".
#
# A site whose sensors contributed no fixes at all is reported as
# `status = "commissioning"` with `added_pct = NA`, rather than 0%: it is being
# set up, not failing.
#
# Radius
# ------
# 40 NM is the terminal area: the approach, departure and low-altitude regime
# that the OPDI sensors were deployed to observe. It is well inside the measured
# median reception distance of every OPDI receiver (30-163 NM), and far short of
# the radio horizon (~200 NM at FL250), where aircraft are at cruise and are
# seen by many receivers, so exclusivity there would be near zero regardless.

# Only jsonlite is needed to READ the cache, which is all the site does at
# render time. osninterface/DBI are loaded lazily by the compute functions, so
# a machine without the OpenSky tooling can still build the page.
suppressPackageStartupMessages(library(jsonlite))

.require_osn <- function() {
  for (p in c("osninterface", "DBI")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(p, " is required to compute the impact analysis. Install with:\n",
           "  remotes::install_github(\"euctrl-pru/osn-interface\")")
    }
  }
  suppressPackageStartupMessages({
    library(osninterface)
    library(DBI)
  })
}

RADIUS_NM       <- 40
MAX_GAP_SECONDS <- 60          # matches R/flight_hours.R
ANALYSIS_DATE   <- "2026-07-08"  # a representative summer day

# Anchor on the project root: `sys.frame(1)$ofile` is unset under knitr, so a
# script-relative path resolves against the page's directory and the cache read
# silently misses.
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
.here <- file.path(opdi_project_root(), "R")
IMPACT_CACHE <- file.path(opdi_project_root(), "data", "sensors", "airport_impact.json")

# `serials` -> BIGINT[]: strip brackets and padding, drop the empty elements
# produced by "[]", then cast. Without the filter DuckDB errors on ''.
.SERIALS_ARRAY <- paste0(
  "list_transform(",
  "  list_filter(string_split_regex(trim(serials, '[] '), '\\s+'), x -> x <> ''),",
  "  x -> CAST(x AS BIGINT))"
)

#' Observed and exclusively-observed flight hours within `radius_nm` of one
#' airport, for a single day.
airport_impact_for_day <- function(icao, serials, date, con,
                                   radius_nm = RADIUS_NM,
                                   max_gap = MAX_GAP_SECONDS) {
  .require_osn()
  date <- as.Date(date)
  view <- sprintf("sv_%s", format(date, "%Y%m%d"))
  invisible(osn_fetch_day(date, con))

  coords   <- osn_airport_coords(icao)
  radius_m <- radius_nm * 1852
  dlat <- radius_nm / 60
  dlon <- radius_nm / (60 * cos(coords$lat * pi / 180))

  arr  <- .SERIALS_ARRAY
  ours <- sprintf("[%s]::BIGINT[]", paste(serials, collapse = ", "))

  # A fix is ours when any OPDI serial appears in its list; exclusive when the
  # list contains nothing else.
  sql <- sprintf("
    WITH in_radius AS (
      SELECT icao24, time, %s AS ser
      FROM %s
      WHERE lat BETWEEN %f AND %f
        AND lon BETWEEN %f AND %f
        AND serials IS NOT NULL AND serials <> '[]'
        AND ST_Distance_Sphere(ST_Point(lon, lat), ST_Point(%f, %f)) <= %f
    ),
    tagged AS (
      SELECT DISTINCT icao24, time,
             list_has_any(ser, %s)                                  AS is_ours,
             len(list_filter(ser, x -> NOT list_contains(%s, x))) = 0 AS is_exclusive
      FROM in_radius
    ),
    all_gaps AS (
      SELECT time - lag(time) OVER (PARTITION BY icao24 ORDER BY time) AS gap
      FROM (SELECT DISTINCT icao24, time FROM tagged)
    ),
    ours_gaps AS (
      SELECT time - lag(time) OVER (PARTITION BY icao24 ORDER BY time) AS gap
      FROM (SELECT DISTINCT icao24, time FROM tagged WHERE is_ours)
    ),
    excl_gaps AS (
      SELECT time - lag(time) OVER (PARTITION BY icao24 ORDER BY time) AS gap
      FROM (SELECT DISTINCT icao24, time FROM tagged WHERE is_ours AND is_exclusive)
    )
    SELECT
      (SELECT sum(CASE WHEN gap <= %d THEN gap ELSE 0 END)/3600.0 FROM all_gaps)  AS total_hours,
      (SELECT sum(CASE WHEN gap <= %d THEN gap ELSE 0 END)/3600.0 FROM ours_gaps) AS opdi_hours,
      (SELECT sum(CASE WHEN gap <= %d THEN gap ELSE 0 END)/3600.0 FROM excl_gaps) AS exclusive_hours,
      (SELECT count(DISTINCT icao24) FROM tagged)                                 AS aircraft,
      (SELECT count(DISTINCT icao24) FROM tagged WHERE is_ours AND is_exclusive)  AS aircraft_exclusive",
    arr, view,
    coords$lat - dlat, coords$lat + dlat,
    coords$lon - dlon, coords$lon + dlon,
    coords$lon, coords$lat, radius_m,
    ours, ours,
    max_gap, max_gap, max_gap)

  r <- dbGetQuery(con, sql)
  zero <- function(x) if (is.null(x) || is.na(x)) 0 else x

  data.frame(
    icao              = icao,
    total_hours       = round(zero(r$total_hours), 1),
    opdi_hours        = round(zero(r$opdi_hours), 1),
    exclusive_hours   = round(zero(r$exclusive_hours), 1),
    added_pct         = if (zero(r$total_hours) > 0)
                          round(100 * zero(r$exclusive_hours) / zero(r$total_hours), 1)
                        else NA_real_,
    aircraft          = zero(r$aircraft),
    aircraft_exclusive = zero(r$aircraft_exclusive),
    stringsAsFactors  = FALSE
  )
}

#' Run the analysis for every airport that hosts OPDI sensors and cache it.
compute_airport_impact <- function(date = ANALYSIS_DATE,
                                   radius_nm = RADIUS_NM,
                                   max_gap = MAX_GAP_SECONDS) {
  .require_osn()
  source(file.path(.here, "sensor_coverage.R"), local = TRUE)
  sensors <- get_sensors()
  if (is.null(sensors) || !nrow(sensors)) {
    stop("no sensor register available (PocketBase and cache both unreachable)")
  }
  serials <- as.integer(sensors$serial)
  icaos   <- unique(sensors$airport_icao)

  con <- osn_connect()
  on.exit(osn_disconnect(con), add = TRUE)

  rows <- lapply(icaos, function(i) {
    message("  ", i, " ...")
    r <- airport_impact_for_day(i, serials, date, con, radius_nm, max_gap)
    # carry the airport label through for rendering
    r$name    <- sensors$airport_name[match(i, sensors$airport_icao)]
    r$country <- sensors$country_name[match(i, sensors$airport_icao)]
    r$sensors <- sum(sensors$airport_icao == i)
    # A site whose sensors contributed no fixes at all is being commissioned,
    # not performing badly: distinguish it so the page never reports 0% added.
    r$status  <- if (r$opdi_hours == 0) "commissioning" else "reporting"
    message(sprintf("    total=%.1fh  exclusive=%.1fh  added=%s%%  [%s]",
                    r$total_hours, r$exclusive_hours,
                    ifelse(is.na(r$added_pct), "-", r$added_pct), r$status))
    r
  })
  out <- do.call(rbind, rows)
  # Reporting sites first, best added benefit at the top; commissioning last.
  out <- out[order(out$status != "reporting", -out$added_pct, out$icao), ]
  rownames(out) <- NULL

  dir.create(dirname(IMPACT_CACHE), recursive = TRUE, showWarnings = FALSE)
  # na = "null" keeps the schema stable: without it jsonlite omits the key
  # entirely for a commissioning site's NA added_pct, and readers see no field.
  write_json(list(
    analysis_date = format(as.Date(date)),
    radius_nm     = radius_nm,
    max_gap_s     = max_gap,
    generated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    airports      = out
  ), IMPACT_CACHE, auto_unbox = TRUE, digits = 4, pretty = TRUE, na = "null")

  message("\nwrote ", IMPACT_CACHE)
  out
}

#' Read the cached analysis, or NULL when it has never been run.
read_airport_impact <- function() {
  if (!file.exists(IMPACT_CACHE)) return(NULL)
  tryCatch({
    d <- read_json(IMPACT_CACHE, simplifyVector = TRUE)
    d$airports <- as.data.frame(d$airports)
    d
  }, error = function(e) NULL)
}

# ---- Rscript entry point -----------------------------------------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  date <- if (length(args)) args[1] else ANALYSIS_DATE
  message("airport impact: ", date, "  radius ", RADIUS_NM, " NM")
  out <- compute_airport_impact(date)
  print(out, row.names = FALSE)
}
