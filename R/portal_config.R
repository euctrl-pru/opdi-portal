# Central place to update release metadata and coverage dates for the portal.
# Update this file for each data refresh; the QMD files pull values from here.

# Load .env if present. Pages source this file from both the project root
# (index.qmd) and content/, so resolve the path from this script's location
# rather than the working directory. Absent .env is not an error: every
# consumer below falls back to a default.
local({
  here <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, mustWork = TRUE)),
    error = function(e) NULL
  )
  env_file <- if (is.null(here)) ".env" else file.path(here, "..", ".env")
  if (file.exists(env_file)) readRenviron(env_file)
})

# Read an env var, falling back when unset or empty.
portal_env <- function(key, default = "") {
  v <- Sys.getenv(key, unset = "")
  if (nzchar(v)) v else default
}

# ---------------------------------------------------------------------------
# Sensor pages: cache control
# ---------------------------------------------------------------------------
# Every figure on the Sensors page is computed by a script in R/ and stored in a
# cache under data/. Rendering the site ALWAYS reads those caches -- it never
# calls an API or scans a Parquet file -- so a build is fast, reproducible, and
# works offline.
#
# Flip `update` to TRUE (or set OPDI_UPDATE_CACHE=true) to refresh the caches
# from the live sources instead. That is a deliberate, occasional act: the scans
# take minutes and some need credentials. Set it back to FALSE afterwards and
# commit the regenerated JSON.
#
#   Rscript R/update_sensor_cache.R            # refresh everything
#   Rscript R/update_sensor_cache.R coverage   # refresh one dataset
#
# See README.md for what each dataset is, what it needs, and how long it takes.
update_sensor_pages_cache <- list(

  # Master switch. TRUE = fetch live and rewrite the caches. FALSE = read only.
  # Never commit this as TRUE: a contributor without credentials would then be
  # unable to render the site.
  update = identical(tolower(portal_env("OPDI_UPDATE_CACHE", "false")), "true"),

  # --- what to refresh, when `update` is TRUE -----------------------------
  datasets = c("coverage", "flight_hours", "impact", "capture", "capture_daily"),

  # --- sensor register and reception coverage (PocketBase + OpenSky) -------
  coverage = list(
    # Days to look back for a coverage polygon before giving a sensor up as
    # silent. A sensor that reported nothing yesterday may have reported today.
    lookback_days = 5
  ),

  # --- observed flight hours per day (OpenSky S3 via osninterface) ---------
  flight_hours = list(
    # The day to measure. A representative summer day, not a holiday.
    date = "2026-07-08",
    # Gaps longer than this between consecutive fixes of one aircraft mean it
    # left coverage and returned; that time was not observed. The data is 5 s
    # downsampled, so 99% of gaps are exactly 5 s and the result is insensitive
    # to this choice between 30 s and 300 s.
    max_gap_seconds = 60
  ),

  # --- added coverage benefit per airport (OpenSky S3) ---------------------
  impact = list(
    date = "2026-07-08",
    # The terminal area. Well inside every OPDI receiver's median reception
    # distance (30-163 NM) and far short of the radio horizon (~200 NM at
    # FL250), where aircraft are seen by many receivers anyway.
    radius_nm = 40,
    max_gap_seconds = 60
  ),

  # --- annual capture rate vs reported traffic (open Parquet + CSV) --------
  capture = list(
    year = 2025
  ),

  # --- daily capture rate over recent months (open Parquet + CSV) ----------
  capture_daily = list(
    year = 2026,
    first_month = 1,
    last_month = 5,
    # A day on which the flight list captures less than this share of reported
    # movements at EVERY sensor airport at once is an ingest gap, not a coverage
    # event: no receiver failure is simultaneous in Sweden, Lithuania and
    # Georgia. Clean days sit near 92%, gap days near 0%, so anything from 5 to
    # 50 selects the same set.
    gap_day_threshold_pct = 20,
    # A spell at one airport opens below gap_day_threshold_pct and closes only
    # once the rate recovers past this, so a single partial day mid-outage does
    # not split one event into two.
    outage_recovery_pct = 50,
    # Consecutive days before a dip counts as an event worth annotating.
    outage_min_days = 2
  )
)

portal_cfg <- list(
  # ---------------------------------------------------------------------------
  # Releases
  # ---------------------------------------------------------------------------
  # The release the Data and Methods sections describe.
  release = "v2.0.0",

  # The release the site presents as the one to use. It stays "v0.0.2" until
  # the files of release v2.0.0 have been produced for the whole first period
  # and can be reached; then set it to "v2.0.0". The home page, the Data
  # section and the Roadmap change their wording with it.
  current_release = "v0.0.2",

  # The previous release. Its monthly files stay available for download.
  previous_release = "v0.0.2",

  # Days covered by release v2.0.0. `start` is the first day of the first
  # production run. Set `end` to the last published day when switching
  # `current_release`; while it is NA the pages say "to the latest published
  # day".
  release_coverage = list(
    start = as.Date("2025-01-01"),
    end   = as.Date(NA)
  ),
  # Date on which release v2.0.0 is announced, as text, for example
  # "2027-01-15". Empty: shown as "to be announced" on the Roadmap.
  release_date = "",
  # Number of flights in release v2.0.0, as shown on the home page once
  # `current_release` is "v2.0.0", for example "+120M". Empty: "to be announced".
  release_counts = list(
    flights = ""
  ),

  # Access to the files of release v2.0.0.
  access = list(
    # The OpenSky Network Trino instance. The catalog and schema names below
    # are the placeholders used in the pipeline's table definitions
    # (src/opdi/sql/trino/tables.sql in the opdi repository). Replace them by
    # the names used on the instance.
    trino_catalog = "minio",
    trino_schema = "opdi",
    # Set to TRUE once the two names above are the real ones. While FALSE, the
    # Access page says that they are placeholders.
    trino_names_final = FALSE,
    # Address of the Trino file browser. Leave empty while it is not known:
    # the pages then say "to be announced".
    browser_url = "",
    # Public address of the published folder (the folder that holds
    # flight_list/ and flight_events/). Leave empty while it is not known.
    download_url = "",
    # Where researchers ask the OpenSky Network for access to its historical
    # database, the Trino instance.
    opensky_access_url = "https://opensky-network.org/data/impala"
  ),

  # ---------------------------------------------------------------------------
  # Release v0.0.2 (monthly and ten-day files)
  # ---------------------------------------------------------------------------
  # Month of the latest refresh of the v0.0.2 files, the period they cover and
  # their size. Update these for each refresh of release v0.0.2.
  refresh_label = "October 2026",
  coverage = list(
    start = as.Date("2022-01-01"),
    end   = as.Date("2026-09-30"),
    snapshot_end = as.Date("2026-09-30")
  ),
  counts = list(
    flights = "+79M",
    events = "+1.9B",
    measurements = "+3.8B"
  ),

  resources = list(
    publications = 6,
    software = 6
  ),

  community = list(
    members = 8,
    # Distinct countries; EUROCONTROL and the PRC are both Belgium-based.
    countries = 7
  ),

  challenges = list(
    editions = 3,
    # The totals cover the 2024 and 2025 editions only: the 2026 totals have
    # not been published. Pages show this label next to each total.
    totals_label = "2024-2025",
    total_teams = "300+",
    total_countries = 51,
    total_submissions = "2,600+",
    items = list(
      list(
        id = "dc2024",
        title = "Actual Takeoff Weight Prediction",
        year = 2024,
        status = "Completed",
        teams = 132,
        finalists = 43,
        # Countries are only published for the finalist teams, not all 132.
        countries = 22,
        # Sum of the "Version" column on the public leaderboard (42 ranked teams);
        # a floor, since unranked teams' submissions are not published.
        submissions = "~550",
        description = "Participants predicted the actual takeoff weight (ATOW) of flights using open ADS-B trajectory data, weather information, and aircraft characteristics.",
        url = "https://ansperformance.eu/study/data-challenge/dc2024/",
        github = "https://github.com/prc-data-challenge-2024"
      ),
      list(
        id = "dc2025",
        title = "Fuel Burn Estimation",
        year = 2025,
        status = "Completed",
        teams = 179,
        # Teams that submitted predictions (179 signed up).
        finalists = 53,
        countries = 48,
        submissions = "2,127",
        description = "Participants estimated the fuel burnt by an aircraft over given intervals of a flight, using open data sources, as a basis for estimating emissions.",
        url = "https://ansperformance.eu/study/data-challenge/dc2025/",
        github = "https://github.com/prc-data-challenge-2025"
      ),
      list(
        id = "dc2026",
        title = "Taxi-Out Time Prediction",
        year = 2026,
        status = "Completed",
        dates = "1 September - 14 October 2026",
        # Team, country and submission totals have not been published.
        teams = NA_integer_,
        finalists = NA_integer_,
        countries = NA_integer_,
        submissions = NA_character_,
        movements = 4167797,
        description = "Participants predicted the taxi-out time (from off-block to take-off, in seconds) of departing flights at ten major European airports, from airport movement data matched to the Network Manager flight list.",
        url = "https://ansperformance.eu/study/data-challenge/dc2026/",
        github = "https://github.com/prc-data-challenge-2026"
      )
    )
  ),

  # Fallback only. The live figures and the deployment table are derived from
  # the PocketBase `opensky_sensor_details` collection via R/sensor_coverage.R;
  # these values are used solely when neither the API nor the cache is reachable.
  sensors = list(
    total_sensors = 12,
    total_airports = 7,
    countries = 4,

    # NOTE: the figures below are FALLBACKS, used only when no cache is present.
    # The Sensors page reads data/sensors/*.json and data/reference/*.json, all
    # regenerated by `Rscript R/update_sensor_cache.R`. See README.md.
    airports = data.frame(
      icao    = c("ESSA", "EYVI", "EYPA", "UGTB", "UGSB", "UGKO", "SBSP"),
      name    = c("Stockholm Arlanda", "Vilnius", "Palanga", "Tbilisi", "Batumi", "Kutaisi", "São Paulo Congonhas"),
      country = c("Sweden", "Lithuania", "Lithuania", "Georgia", "Georgia", "Georgia", "Brazil"),
      sensors = c(4L, 2L, 1L, 2L, 1L, 1L, 1L),
      stringsAsFactors = FALSE
    )
  )
)

# Helpers for the release switch. `cfg` is portal_cfg.
portal_release <- list(
  # TRUE once release v2.0.0 is the one presented as current.
  is_current = function(cfg) identical(cfg$current_release, cfg$release),
  # A value from the access block, or "to be announced" while it is empty.
  value_or_tba = function(value) {
    if (is.null(value) || !nzchar(value)) "to be announced" else value
  },
  # A Markdown link, or "to be announced" while the address is empty.
  link_or_tba = function(url, text = url) {
    if (is.null(url) || !nzchar(url)) "to be announced" else paste0("[", text, "](", url, ")")
  },
  # A date in words, for example "1 January 2025".
  day_text = function(d) sub("^0", "", format(d, "%d %B %Y")),
  # The days covered by release v2.0.0, in words.
  coverage_text = function(cfg) {
    day_text <- function(d) sub("^0", "", format(d, "%d %B %Y"))
    start <- day_text(cfg$release_coverage$start)
    end <- cfg$release_coverage$end
    if (is.na(end)) paste("from", start, "to the latest published day")
    else paste("from", start, "to", day_text(end))
  },
  # The Trino catalog and schema, as written before a table name.
  trino_prefix = function(cfg) {
    paste0(cfg$access$trino_catalog, ".", cfg$access$trino_schema)
  }
)

# Helper accessors for formatted labels
portal_labels <- list(
  month_range = function(start_date, end_date) {
    paste(format(start_date, "%B %Y"), "-", format(end_date, "%B %Y"))
  },
  short_month_range = function(start_date, end_date) {
    paste(format(start_date, "%m/%Y"), "-", format(end_date, "%m/%Y"))
  },
  date_range = function(start_date, end_date) {
    paste(format(start_date, "%B %d, %Y"), "-", format(end_date, "%B %d, %Y"))
  }
)
