# Capture rate: OPDI-observed movements vs. reported movements, per airport, 2025.
#
# This is a MANUAL analysis script, not part of the site render. It streams the
# twelve 2025 OPDI flight-list parquet files straight from the EUROCONTROL
# download server with DuckDB (no bulk download) and compares the result to the
# published airport traffic statistics. The output is cached to
# data/reference/airport_capture_2025.json, which the site reads.
#
#   Rscript R/airport_capture.R          # recompute and refresh the cache
#   Rscript R/airport_capture.R 2024     # a different year
#
# Requirements: duckdb, DBI, jsonlite. No credentials -- both sources are open.
#
# Sources
# -------
# OPDI flight list (one parquet per month):
#   https://www.eurocontrol.int/performance/data/download/OPDI/v002/flight_list/
#     flight_list_{YYYYMM}.parquet
# Reported traffic (one CSV per year, daily rows per airport):
#   https://www.eurocontrol.int/performance/data/download/csv/airport_traffic_{YYYY}.csv
#
# Method
# ------
# A movement is one departure or one arrival. In the OPDI flight list each row
# is a flight, contributing one movement at `adep` and one at `ades`; a flight
# is counted at both endpoints, exactly as the reference CSV counts FLT_DEP_1
# and FLT_ARR_1.
#
# `adep`/`ades` hold the detected airport. We deliberately ignore `adep_p` and
# `ades_p`: they are present on ~1% of rows, always disagree with adep/ades, and
# refer to a different place (LEBL vs ES-0109, LFPG vs LFPB), so they are not a
# fallback for a missing value.
#
#   capture_rate = opdi_movements / reported_movements
#
# Caveats
# -------
# 1. About 47% of OPDI flights have no `adep` (44% no `ades`): the trajectory did
#    not start or end inside the ADS-B coverage used to build the flight list.
#    Those flights contribute no movement to any airport.
#
# 2. The two sources count different populations. The reference CSV reports IFR
#    commercial traffic; the OPDI flight list contains every transponder-equipped
#    flight, including VFR and general aviation. At small aerodromes this pushes
#    the ratio above 100% -- Murcia San Javier reports 335 movements against
#    4,971 in OPDI. The rate is therefore only interpretable at airports with
#    meaningful scheduled traffic; `over_reference` flags the rest.
#
#    Measured over 2025: airports with >=50k reported movements have a median
#    capture of 97.9%; those under 5k, 47.2%, with a third exceeding 100%.
#
# 3. An airport with zero OPDI movements is an ADS-B coverage gap, not a data
#    error. In 2025 there were 43 such airports (323,560 reported movements),
#    concentrated in the Canaries and northern Norway.

suppressPackageStartupMessages(library(jsonlite))

# Anchor on the project root: `sys.frame(1)$ofile` is unset under knitr, so a
# script-relative path resolves against the rendering page's directory.
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

CAPTURE_YEAR  <- 2025

#' Cache path for a given year. Keep this a function rather than a fixed
#' constant: otherwise compute_airport_capture(2024) writes 2024's numbers into
#' the 2025 file.
capture_cache_path <- function(year = CAPTURE_YEAR) {
  file.path(opdi_project_root(), "data", "reference",
            sprintf("airport_capture_%d.json", year))
}

OPDI_FLIGHT_LIST <- paste0(
  "https://www.eurocontrol.int/performance/data/download/OPDI/v002/",
  "flight_list/flight_list_%s.parquet"
)
REPORTED_TRAFFIC <- paste0(
  "https://www.eurocontrol.int/performance/data/download/csv/",
  "airport_traffic_%d.csv"
)

.require_duckdb <- function() {
  for (p in c("duckdb", "DBI")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(p, " is required to compute the capture rate. install.packages(\"", p, "\")")
    }
  }
  suppressPackageStartupMessages({ library(duckdb); library(DBI) })
}

#' Movements per airport in the OPDI flight list for one year.
#' Each flight contributes one movement at its departure and one at its arrival.
opdi_movements <- function(year = CAPTURE_YEAR, con) {
  months <- sprintf("%d%02d", year, 1:12)
  urls <- sprintf(OPDI_FLIGHT_LIST, months)
  url_list <- paste(sprintf("'%s'", urls), collapse = ", ")

  sql <- sprintf("
    WITH fl AS (
      SELECT nullif(trim(adep), '') AS adep,
             nullif(trim(ades), '') AS ades
      FROM read_parquet([%s])
    ),
    moves AS (
      SELECT adep AS icao, 'dep' AS kind FROM fl WHERE adep IS NOT NULL
      UNION ALL
      SELECT ades AS icao, 'arr' AS kind FROM fl WHERE ades IS NOT NULL
    )
    SELECT icao,
           count(*)                                  AS opdi_movements,
           sum(CASE WHEN kind = 'dep' THEN 1 ELSE 0 END) AS opdi_departures,
           sum(CASE WHEN kind = 'arr' THEN 1 ELSE 0 END) AS opdi_arrivals
    FROM moves
    GROUP BY icao", url_list)

  dbGetQuery(con, sql)
}

#' Total flights in the OPDI flight list, and how many carry an airport at each
#' end. Gives the ceiling that the capture rate is measured against.
opdi_attribution <- function(year = CAPTURE_YEAR, con) {
  months <- sprintf("%d%02d", year, 1:12)
  url_list <- paste(sprintf("'%s'", sprintf(OPDI_FLIGHT_LIST, months)), collapse = ", ")
  dbGetQuery(con, sprintf("
    SELECT count(*)                                              AS flights,
           sum(nullif(trim(adep),'') IS NOT NULL)::BIGINT        AS with_adep,
           sum(nullif(trim(ades),'') IS NOT NULL)::BIGINT        AS with_ades
    FROM read_parquet([%s])", url_list))
}

#' Reported movements per airport from the published traffic statistics.
reported_movements <- function(year = CAPTURE_YEAR, con) {
  dbGetQuery(con, sprintf("
    SELECT APT_ICAO       AS icao,
           any_value(APT_NAME)   AS airport,
           any_value(STATE_NAME) AS state,
           sum(FLT_TOT_1)::BIGINT AS reported_movements,
           sum(FLT_DEP_1)::BIGINT AS reported_departures,
           sum(FLT_ARR_1)::BIGINT AS reported_arrivals
    FROM read_csv_auto('%s')
    WHERE YEAR = %d
    GROUP BY APT_ICAO", sprintf(REPORTED_TRAFFIC, year), year))
}

#' Join both sides and compute the capture rate per airport.
compute_airport_capture <- function(year = CAPTURE_YEAR) {
  .require_duckdb()
  CAPTURE_CACHE <- capture_cache_path(year)
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  message("reading reported traffic ", year, " ...")
  rep <- reported_movements(year, con)
  message("  ", nrow(rep), " airports")

  message("streaming 12 OPDI flight-list files ...")
  opdi <- opdi_movements(year, con)
  message("  ", nrow(opdi), " airports seen")

  attrib <- opdi_attribution(year, con)

  # Reported traffic is the reference set: an airport OPDI saw but EUROCONTROL
  # does not report is outside the reference scope, not a capture success.
  m <- merge(rep, opdi, by = "icao", all.x = TRUE)
  m$opdi_movements[is.na(m$opdi_movements)] <- 0
  m$opdi_departures[is.na(m$opdi_departures)] <- 0
  m$opdi_arrivals[is.na(m$opdi_arrivals)] <- 0
  m$capture_rate <- ifelse(m$reported_movements > 0,
                           round(100 * m$opdi_movements / m$reported_movements, 1),
                           NA_real_)

  # OPDI counts VFR/GA traffic the IFR-only reference omits, so a rate above
  # 100% means the populations differ, not that capture exceeded the total.
  m$over_reference <- !is.na(m$capture_rate) & m$capture_rate > 100
  # Zero attributed movements at a reported airport is an ADS-B coverage gap.
  m$coverage_gap <- m$opdi_movements == 0 & m$reported_movements > 0
  m$size_tier <- cut(m$reported_movements,
                     breaks = c(-Inf, 5000, 50000, Inf),
                     labels = c("small", "medium", "large"))

  m <- m[order(-m$reported_movements), ]
  rownames(m) <- NULL

  dir.create(dirname(CAPTURE_CACHE), recursive = TRUE, showWarnings = FALSE)
  write_json(list(
    year            = year,
    generated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_opdi     = sprintf(OPDI_FLIGHT_LIST, "{YYYYMM}"),
    source_reported = sprintf(REPORTED_TRAFFIC, year),
    opdi_flights    = attrib$flights,
    opdi_with_adep  = attrib$with_adep,
    opdi_with_ades  = attrib$with_ades,
    totals = list(
      airports           = nrow(m),
      reported_movements = sum(m$reported_movements),
      opdi_movements     = sum(m$opdi_movements),
      capture_rate       = round(100 * sum(m$opdi_movements) / sum(m$reported_movements), 1),
      coverage_gaps      = sum(m$coverage_gap),
      over_reference     = sum(m$over_reference)
    ),
    # Two views per size band, because they answer different questions.
    # `median_capture_rate` weights every airport equally: the typical airport.
    # `capture_rate` weights by movements: the share of actual traffic captured.
    # They diverge -- a handful of large airports with poor capture pull the
    # weighted rate down, while the >100% GA aerodromes pull the small band up.
    by_size = lapply(levels(m$size_tier), function(tier) {
      g <- m[m$size_tier == tier, ]
      list(tier = tier, airports = nrow(g),
           median_capture_rate = round(median(g$capture_rate, na.rm = TRUE), 1),
           capture_rate = round(100 * sum(g$opdi_movements) / sum(g$reported_movements), 1),
           reported_movements = sum(g$reported_movements),
           opdi_movements = sum(g$opdi_movements),
           over_reference = sum(g$over_reference))
    }),
    airports = m
  ), CAPTURE_CACHE, auto_unbox = TRUE, digits = 4, pretty = TRUE, na = "null")

  message("\nwrote ", CAPTURE_CACHE)
  m
}

#' Read the cached comparison, or NULL when it has never been computed.
read_airport_capture <- function(year = CAPTURE_YEAR) {
  path <- capture_cache_path(year)
  if (!file.exists(path)) return(NULL)
  tryCatch({
    d <- read_json(path, simplifyVector = TRUE)
    d$airports <- as.data.frame(d$airports)
    d
  }, error = function(e) NULL)
}

# ---- Rscript entry point -----------------------------------------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  year <- if (length(args)) as.integer(args[1]) else CAPTURE_YEAR
  m <- compute_airport_capture(year)
  cat("\nTop 15 airports by reported movements:\n")
  print(head(m[, c("icao", "airport", "reported_movements",
                   "opdi_movements", "capture_rate")], 15), row.names = FALSE)

  cat("\nMedian capture by airport size:\n")
  for (tier in levels(m$size_tier)) {
    g <- m[m$size_tier == tier, ]
    cat(sprintf("  %-7s n=%3d  median %5.1f%%  (%d over reference)\n",
                tier, nrow(g), median(g$capture_rate, na.rm = TRUE),
                sum(g$over_reference)))
  }
  cat(sprintf("\nOverall: %s of %s movements captured (%.1f%%)\n",
              format(sum(m$opdi_movements), big.mark = ","),
              format(sum(m$reported_movements), big.mark = ","),
              100 * sum(m$opdi_movements) / sum(m$reported_movements)))
  cat(sprintf("Coverage gaps (0 OPDI movements): %d airports, %s reported movements\n",
              sum(m$coverage_gap),
              format(sum(m$reported_movements[m$coverage_gap]), big.mark = ",")))
}
