# Daily capture rate per airport, for a run of recent months.
#
# A time-series companion to R/airport_capture.R. Where that script answers
# "how much of 2025's traffic does OPDI see at each airport?", this one answers
# "how has that changed day by day?" -- which makes a sensor coming online, or a
# coverage outage, visible as a step in the curve rather than a shifted average.
#
# This is a MANUAL analysis script, not part of the site render. It streams the
# monthly OPDI flight-list parquet files with DuckDB and caches the result to
# data/reference/airport_capture_daily.json, which the site reads.
#
#   Rscript R/airport_capture_daily.R                  # default window
#   Rscript R/airport_capture_daily.R 2026 1 5         # year, first month, last month
#
# Requirements: duckdb, DBI, jsonlite. No credentials -- both sources are open.
#
# Sources (identical to R/airport_capture.R)
# ------------------------------------------
# OPDI flight list:  .../OPDI/v002/flight_list/flight_list_{YYYYMM}.parquet
# Reported traffic:  .../csv/airport_traffic_{YYYY}.csv  (one row per airport per day)
#
# Method
# ------
# A movement is one departure or one arrival. Each OPDI flight contributes one
# movement at `adep` and one at `ades`, on its day of flight `dof` -- verified to
# equal the date of `first_seen` for every row. The reference CSV gives
# FLT_DEP_1 + FLT_ARR_1 per airport per FLT_DATE. Joining on (airport, date):
#
#   capture_rate(airport, day) = opdi_movements / reported_movements * 100
#
# Only airports hosting OPDI sensors are kept, since the point is to show what a
# deployment does to the observability of its own airport.
#
# Caveats (see R/airport_capture.R for the full discussion)
# --------------------------------------------------------
# `adep`/`ades` are inferred from the trajectory, not reported by the aircraft, so
# the rate reflects the detection algorithm as well as receiver coverage. The two
# sources also count different populations -- reported traffic is IFR, the OPDI
# flight list includes VFR and general aviation -- so a small aerodrome can exceed
# 100%. Daily rates are noisier than annual ones; a 7-day rolling mean is stored
# alongside the raw value for plotting.

suppressPackageStartupMessages(library(jsonlite))

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

DAILY_YEAR        <- 2026
DAILY_FIRST_MONTH <- 1
DAILY_LAST_MONTH  <- 5

# A day on which the OPDI flight list captures less than this share of reported
# movements *across every sensor airport at once* is an ingest gap, not a
# coverage event: no receiver failure is simultaneous in Sweden, Lithuania and
# Georgia. On clean days the network-wide rate sits near 92%, and the gap days
# sit near 0%, so the threshold is not sensitive -- anything from 5% to 50%
# selects the same set.
GAP_DAY_THRESHOLD_PCT <- 20

# Minimum consecutive days below that threshold, at a single airport while the
# others keep reporting, before we call it an event worth annotating. Two days
# filters out one-off dips without hiding the real outages.
OUTAGE_MIN_DAYS <- 2

# A spell opens when the rate collapses below GAP_DAY_THRESHOLD_PCT and closes
# only once it recovers past this. These airports normally run at 80-95%, so 50%
# sits clear of both the healthy band and the collapse, and it stops a single
# partial day mid-outage from splitting the spell in two.
OUTAGE_RECOVERY_PCT <- 50
DAILY_CACHE <- file.path(opdi_project_root(), "data", "reference",
                         "airport_capture_daily.json")

OPDI_FLIGHT_LIST <- paste0(
  "https://www.eurocontrol.int/performance/data/download/OPDI/v002/",
  "flight_list/flight_list_%s.parquet"
)
REPORTED_TRAFFIC <- paste0(
  "https://www.eurocontrol.int/performance/data/download/csv/",
  "airport_traffic_%d.csv"
)

.require_duckdb_daily <- function() {
  for (p in c("duckdb", "DBI")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(p, " is required. install.packages(\"", p, "\")")
    }
  }
  suppressPackageStartupMessages({ library(duckdb); library(DBI) })
}

#' Centred 7-day rolling mean. NA-safe at the edges and across missing days;
#' a window containing no observations at all yields NA rather than NaN.
.roll7 <- function(x) {
  n <- length(x)
  vapply(seq_len(n), function(i) {
    w <- x[max(1, i - 3):min(n, i + 3)]
    w <- w[!is.na(w)]
    if (!length(w)) NA_real_ else mean(w)
  }, numeric(1))
}

#' Daily movements per airport per day from the OPDI flight list.
opdi_daily <- function(year, months, icaos, con) {
  urls <- sprintf(OPDI_FLIGHT_LIST, sprintf("%d%02d", year, months))
  url_list <- paste(sprintf("'%s'", urls), collapse = ", ")
  icao_list <- paste(sprintf("'%s'", icaos), collapse = ", ")

  dbGetQuery(con, sprintf("
    WITH fl AS (
      SELECT CAST(dof AS DATE)      AS day,
             nullif(trim(adep), '') AS adep,
             nullif(trim(ades), '') AS ades
      FROM read_parquet([%s])
    ),
    moves AS (
      SELECT day, adep AS icao FROM fl WHERE adep IS NOT NULL
      UNION ALL
      SELECT day, ades        FROM fl WHERE ades IS NOT NULL
    )
    SELECT icao, day, count(*) AS opdi_movements
    FROM moves
    WHERE icao IN (%s)
    GROUP BY icao, day", url_list, icao_list))
}

#' Daily reported movements per airport per day.
reported_daily <- function(year, months, icaos, con) {
  icao_list <- paste(sprintf("'%s'", icaos), collapse = ", ")
  dbGetQuery(con, sprintf("
    SELECT APT_ICAO AS icao,
           CAST(FLT_DATE AS DATE) AS day,
           any_value(APT_NAME) AS airport,
           sum(FLT_TOT_1)::BIGINT AS reported_movements
    FROM read_csv_auto('%s')
    WHERE APT_ICAO IN (%s)
      -- MONTH_NUM arrives as zero-padded text ('01'), so cast before comparing.
      AND CAST(MONTH_NUM AS INTEGER) BETWEEN %d AND %d
    GROUP BY APT_ICAO, CAST(FLT_DATE AS DATE)",
    sprintf(REPORTED_TRAFFIC, year), icao_list, min(months), max(months)))
}

compute_airport_capture_daily <- function(year = DAILY_YEAR,
                                          months = DAILY_FIRST_MONTH:DAILY_LAST_MONTH,
                                          gap_threshold_pct = GAP_DAY_THRESHOLD_PCT,
                                          outage_recovery_pct = OUTAGE_RECOVERY_PCT,
                                          outage_min_days = OUTAGE_MIN_DAYS) {
  .require_duckdb_daily()
  source(file.path(opdi_project_root(), "R", "sensor_coverage.R"), local = TRUE)
  sensors <- get_sensors()
  if (is.null(sensors) || !nrow(sensors)) stop("no sensor register available")
  icaos <- sort(unique(sensors$airport_icao))

  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

  message("reported traffic ", year, " months ", min(months), "-", max(months), " ...")
  rep <- reported_daily(year, months, icaos, con)
  message("  ", nrow(rep), " airport-days")

  message("streaming ", length(months), " OPDI flight-list files ...")
  opdi <- opdi_daily(year, months, icaos, con)
  message("  ", nrow(opdi), " airport-days")

  m <- merge(rep, opdi, by = c("icao", "day"), all.x = TRUE)
  m$opdi_movements[is.na(m$opdi_movements)] <- 0
  m$capture_rate <- ifelse(m$reported_movements > 0,
                           round(100 * m$opdi_movements / m$reported_movements, 1),
                           NA_real_)

  # Some days are missing from the OPDI flight list entirely -- the trajectory
  # ingest failed, not the receivers. They show up as a network-wide collapse to
  # near zero on the same date at every airport, which no sensor event can cause.
  # Flag them so the site can exclude them rather than draw a phantom recovery.
  net <- aggregate(cbind(reported_movements, opdi_movements) ~ day, m, sum)
  net$net_rate <- 100 * net$opdi_movements / net$reported_movements
  gap_days <- net$day[net$net_rate < gap_threshold_pct]
  m$data_gap <- m$day %in% gap_days
  if (length(gap_days)) {
    message("  ", length(gap_days), " day(s) flagged as OPDI data gaps: ",
            paste(format(gap_days), collapse = ", "))
  }

  m <- m[order(m$icao, m$day), ]

  # Smooth within each airport, never across the boundary between two, and treat
  # gap days as missing so a zero does not drag down the six days around it.
  m$rate_for_smoothing <- ifelse(m$data_gap, NA_real_, m$capture_rate)
  m$capture_rate_7d <- unlist(lapply(split(m$rate_for_smoothing, m$icao), .roll7),
                              use.names = FALSE)
  m$capture_rate_7d <- round(m$capture_rate_7d, 1)
  m$rate_for_smoothing <- NULL
  m$day <- format(m$day)
  rownames(m) <- NULL

  dir.create(dirname(DAILY_CACHE), recursive = TRUE, showWarnings = FALSE)
  # Airport-specific events, distinguished from the network-wide ingest gaps
  # above: a run of days where one airport alone collapses while the others keep
  # reporting. A run that ends before the window closes is an outage; one that
  # runs from the first day up to a recovery is a sensor coming online.
  events <- do.call(rbind, lapply(sort(unique(m$icao)), function(a) {
    g <- m[m$icao == a & !m$data_gap, ]
    g <- g[order(g$day), ]
    # Hysteresis: a spell opens when the rate collapses below the gap threshold
    # and stays open until it recovers past OUTAGE_RECOVERY_PCT. Without this a
    # single partial day (Tbilisi, 29% on 19 Jan) splits one outage into two and
    # the annotation undershoots the visible dip.
    depressed <- !is.na(g$capture_rate) & g$capture_rate < outage_recovery_pct
    collapsed <- !is.na(g$capture_rate) & g$capture_rate < gap_threshold_pct
    low <- logical(nrow(g)); open <- FALSE
    for (i in seq_len(nrow(g))) {
      if (!open && collapsed[i]) open <- TRUE
      else if (open && !depressed[i]) open <- FALSE
      low[i] <- open
    }
    if (!any(low)) return(NULL)
    r <- rle(low)
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    sel <- which(r$values & r$lengths >= outage_min_days)
    if (!length(sel)) return(NULL)
    do.call(rbind, lapply(sel, function(i) data.frame(
      icao  = a,
      from  = format(g$day[starts[i]]),
      to    = format(g$day[ends[i]]),
      days  = r$lengths[i],
      # A spell starting on day one is the sensor not yet reporting; the day it
      # recovers is the activation.
      kind  = if (starts[i] == 1L) "activation" else "outage",
      resumed = if (ends[i] < nrow(g)) format(g$day[ends[i] + 1L]) else NA_character_,
      stringsAsFactors = FALSE
    )))
  }))

  clean <- m[!m$data_gap, ]
  write_json(list(
    year            = year,
    months          = range(months),
    date_from       = min(m$day),
    date_to         = max(m$day),
    generated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_opdi     = sprintf(OPDI_FLIGHT_LIST, "{YYYYMM}"),
    source_reported = sprintf(REPORTED_TRAFFIC, year),
    # Record the parameters, so a cache always says how it was made.
    params = list(gap_day_threshold_pct = gap_threshold_pct,
                  outage_recovery_pct   = outage_recovery_pct,
                  outage_min_days       = outage_min_days),
    airports        = sort(unique(m$icao)),
    gap_days        = sort(unique(m$day[m$data_gap])),
    n_gap_days      = length(unique(m$day[m$data_gap])),
    events          = if (is.null(events)) list() else events,
    network_capture_clean = round(
      100 * sum(clean$opdi_movements) / sum(clean$reported_movements), 1),
    daily           = m
  ), DAILY_CACHE, auto_unbox = TRUE, digits = 4, pretty = TRUE, na = "null")

  message("\nwrote ", DAILY_CACHE)
  m
}

read_airport_capture_daily <- function() {
  if (!file.exists(DAILY_CACHE)) return(NULL)
  tryCatch({
    d <- read_json(DAILY_CACHE, simplifyVector = TRUE)
    d$daily <- as.data.frame(d$daily)
    d$daily$day <- as.Date(d$daily$day)
    d
  }, error = function(e) NULL)
}

# ---- Rscript entry point -----------------------------------------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  year   <- if (length(args) >= 1) as.integer(args[1]) else DAILY_YEAR
  months <- if (length(args) >= 3) as.integer(args[2]):as.integer(args[3]) else
                                   DAILY_FIRST_MONTH:DAILY_LAST_MONTH
  m <- compute_airport_capture_daily(year, months)

  gaps <- sort(unique(m$day[m$data_gap]))
  if (length(gaps)) {
    cat(sprintf("\n%d day(s) excluded as OPDI ingest gaps: %s\n",
                length(gaps), paste(gaps, collapse = ", ")))
  }

  ev <- read_airport_capture_daily()$events
  if (length(ev) && is.data.frame(ev) && nrow(ev)) {
    cat("\nAirport-specific events detected:\n")
    print(ev, row.names = FALSE)
  }

  clean <- m[!m$data_gap, ]
  cat("\nMean daily capture rate by airport (gap days excluded):\n")
  agg <- aggregate(capture_rate ~ icao, clean, mean, na.rm = TRUE)
  agg$capture_rate <- round(agg$capture_rate, 1)
  print(agg[order(agg$capture_rate), ], row.names = FALSE)

  cat("\nFirst vs last 14 clean days (a sensor coming online shows here):\n")
  for (a in sort(unique(clean$icao))) {
    g <- clean[clean$icao == a & !is.na(clean$capture_rate), ]
    if (nrow(g) < 28) next
    cat(sprintf("  %-5s  first14 %5.1f%%   last14 %5.1f%%\n", a,
                mean(head(g$capture_rate, 14)), mean(tail(g$capture_rate, 14))))
  }
}
