# Fetch OPDI sensor metadata (PocketBase) and their reception coverage
# (OpenSky Network), with an on-disk cache so the site always renders.
#
# Data flow:
#   1. PocketBase collection `opensky_sensor_details` -> serials + positions.
#      World-readable, so no credentials needed.
#   2. OpenSky GET /api/range/days?days=<midnight-utc-epoch>&serials=... ->
#      a 360-point coverage polygon per sensor, as [bearing, lat, lon].
#      Requires OAuth2 client credentials (OPENSKY_CLIENT_ID/SECRET).
#
# Credentials live in .env (see .env.template). When they are absent the
# cache under data/sensors/ is used instead; when the cache is also absent the
# caller gets sensors with no coverage and renders markers only.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# Find the project root by walking up to the directory holding _quarto.yml.
# `sys.frame(1)$ofile` is unset under knitr, so a script-relative path silently
# becomes relative to the rendering page's directory (content/) and every cache
# read misses. Anchoring on the project root works from any working directory.
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

SENSOR_CACHE_DIR <- file.path(opdi_project_root(), "data", "sensors")
SENSOR_CACHE <- file.path(SENSOR_CACHE_DIR, "coverage.json")

# OpenSky returns no coverage for a sensor that reported nothing that day, so
# look back until we find a day with data rather than showing an empty map.
COVERAGE_LOOKBACK_DAYS <- 5

.midnight_utc <- function(days_ago = 1) {
  t <- as.numeric(Sys.time()) - days_ago * 86400
  as.integer(t - t %% 86400)
}

#' Sensor metadata from the PocketBase collection.
fetch_sensors <- function(base_url = Sys.getenv("OPDI_SENSORS_PH_URL",
                                                "https://opdi.pockethost.io")) {
  tryCatch({
    resp <- request(base_url) |>
      req_url_path("/api/collections/opensky_sensor_details/records") |>
      req_url_query(perPage = 500) |>
      req_timeout(30) |>
      req_perform()

    items <- resp_body_json(resp)$items
    if (!length(items)) return(NULL)

    do.call(rbind, lapply(items, function(r) data.frame(
      serial       = as.numeric(r$sensor_serial),
      airport_icao = r$airport_icao %||% NA_character_,
      airport_name = r$airport_name %||% NA_character_,
      country_name = r$country_name %||% NA_character_,
      country_iso3 = r$country_iso3 %||% NA_character_,
      latitude     = as.numeric(r$latitude),
      longitude    = as.numeric(r$longitude),
      stringsAsFactors = FALSE
    )))
  }, error = function(e) {
    message("sensor_coverage: PocketBase fetch failed: ", conditionMessage(e))
    NULL
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' OAuth2 client-credentials token, or NULL when unconfigured.
.opensky_token <- function() {
  id <- Sys.getenv("OPENSKY_CLIENT_ID", "")
  secret <- Sys.getenv("OPENSKY_CLIENT_SECRET", "")
  if (!nzchar(id) || !nzchar(secret)) return(NULL)

  tryCatch({
    resp <- request(paste0("https://auth.opensky-network.org/auth/realms/",
                           "opensky-network/protocol/openid-connect/token")) |>
      req_body_form(grant_type = "client_credentials",
                    client_id = id, client_secret = secret) |>
      req_timeout(30) |>
      req_perform()
    resp_body_json(resp)$access_token
  }, error = function(e) {
    message("sensor_coverage: OpenSky auth failed: ", conditionMessage(e))
    NULL
  })
}

#' Coverage polygons for `serials`, as a named list keyed by serial.
#' Each element is a data.frame(bearing, latitude, longitude).
.fetch_coverage_day <- function(token, serials, day) {
  # The API wants one `serials=` parameter per sensor; req_url_query() would
  # overwrite rather than repeat, so explode the vector into repeated keys.
  req <- request("https://opensky-network.org/api/range/days") |>
    req_auth_bearer_token(token) |>
    req_timeout(90) |>
    req_url_query(days = day) |>
    req_url_query(serials = trimws(format(serials, scientific = FALSE)),
                  .multi = "explode")

  body <- resp_body_json(req_perform(req))
  out <- list()
  for (day_entries in body) {
    for (e in day_entries) {
      pts <- e$ranges
      if (!length(pts)) next
      out[[as.character(e$serial)]] <- data.frame(
        bearing   = vapply(pts, function(p) as.numeric(p[[1]]), numeric(1)),
        latitude  = vapply(pts, function(p) as.numeric(p[[2]]), numeric(1)),
        longitude = vapply(pts, function(p) as.numeric(p[[3]]), numeric(1))
      )
    }
  }
  out
}

fetch_coverage <- function(serials, lookback_days = COVERAGE_LOOKBACK_DAYS) {
  token <- .opensky_token()
  if (is.null(token)) return(NULL)

  acc <- list()
  for (d in seq_len(lookback_days)) {
    missing <- setdiff(as.character(serials), names(acc))
    if (!length(missing)) break
    got <- tryCatch(
      .fetch_coverage_day(token, as.numeric(missing), .midnight_utc(d)),
      error = function(e) {
        message("sensor_coverage: range/days failed (day -", d, "): ",
                conditionMessage(e))
        list()
      }
    )
    acc <- c(acc, got[setdiff(names(got), names(acc))])
  }
  if (length(acc)) acc else NULL
}

.write_cache <- function(sensors, coverage) {
  dir.create(SENSOR_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  write_json(
    list(fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         sensors = sensors, coverage = coverage),
    SENSOR_CACHE, auto_unbox = TRUE, digits = 6
  )
}

.read_cache <- function() {
  if (!file.exists(SENSOR_CACHE)) return(NULL)
  tryCatch({
    d <- read_json(SENSOR_CACHE, simplifyVector = TRUE)
    # jsonlite gives back a list of data.frames for coverage
    if (is.list(d$coverage) && length(d$coverage)) {
      d$coverage <- lapply(d$coverage, as.data.frame)
    }
    d
  }, error = function(e) NULL)
}

#' Sensors + coverage. Reads the cache by default -- rendering the site must
#' never depend on a network call. Pass `refresh = TRUE` (what
#' R/update_sensor_cache.R does) to fetch from PocketBase and OpenSky and
#' rewrite the cache.
#'
#' Returns list(sensors, coverage, fetched_at, source).
get_sensor_coverage <- function(refresh = FALSE,
                                lookback_days = COVERAGE_LOOKBACK_DAYS) {
  if (isTRUE(refresh)) {
    sensors <- fetch_sensors()

    if (!is.null(sensors)) {
      coverage <- fetch_coverage(sensors$serial, lookback_days)
      if (!is.null(coverage)) {
        .write_cache(sensors, coverage)
        return(list(sensors = sensors, coverage = coverage,
                    fetched_at = format(Sys.time(), "%Y-%m-%d", tz = "UTC"),
                    source = "live"))
      }
      # PocketBase answered but OpenSky did not: keep the fresh sensor register
      # and reuse whatever coverage we cached last time.
      cached <- .read_cache()
      if (!is.null(cached)) {
        return(list(sensors = sensors, coverage = cached$coverage,
                    fetched_at = substr(cached$fetched_at, 1, 10),
                    source = "cache"))
      }
    }
  }

  cached <- .read_cache()
  if (!is.null(cached)) {
    return(list(sensors = cached$sensors, coverage = cached$coverage,
                fetched_at = substr(cached$fetched_at, 1, 10),
                source = "cache"))
  }

  # No cache. Only reach the network as a last resort, so a fresh clone that has
  # not yet run the updater still renders something rather than nothing.
  sensors <- fetch_sensors()

  # Last resort: markers only, no coverage.
  list(sensors = sensors, coverage = NULL, fetched_at = NA_character_,
       source = if (is.null(sensors)) "none" else "sensors-only")
}

#' Sensor records alone, live when PocketBase is reachable and from the cache
#' otherwise. Cheaper than get_sensor_coverage() for pages that only need
#' counts, and it does not require OpenSky credentials.
get_sensors <- function(refresh = FALSE) {
  if (isTRUE(refresh)) {
    sensors <- fetch_sensors()
    if (!is.null(sensors)) return(sensors)
  }

  cached <- .read_cache()
  if (!is.null(cached)) return(as.data.frame(cached$sensors))

  # No cache yet: fall back to the register so a fresh clone still works.
  fetch_sensors()
}

#' Headline counts and the per-airport deployment table, derived from the
#' PocketBase collection so the site never disagrees with the register.
#' Returns NULL when no sensor data is reachable at all.
sensor_stats <- function(sensors = get_sensors()) {
  if (is.null(sensors) || !nrow(sensors)) return(NULL)

  by_airport <- aggregate(
    list(sensors = sensors$serial),
    by = list(icao    = sensors$airport_icao,
              name    = sensors$airport_name,
              country = sensors$country_name),
    FUN = length
  )
  # Busiest airports first, then alphabetically for a stable order.
  by_airport <- by_airport[order(-by_airport$sensors, by_airport$icao), ]
  rownames(by_airport) <- NULL

  list(
    total_sensors  = nrow(sensors),
    total_airports = length(unique(sensors$airport_icao)),
    countries      = length(unique(sensors$country_name)),
    airports       = by_airport
  )
}
