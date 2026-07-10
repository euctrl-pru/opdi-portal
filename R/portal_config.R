# Central place to update release metadata and coverage dates for the portal.
# Update this file for each monthly refresh; the QMD files pull values from here.

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

portal_cfg <- list(
  version = "v0.0.2",
  refresh_label = "June 2026",
  coverage = list(
    start = as.Date("2022-01-01"),
    end   = as.Date("2026-05-30"),
    snapshot_end = as.Date("2026-05-30")
  ),
  counts = list(
    flights = "+73M",
    events = "+1.7B",
    measurements = "+3.4B"
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
        title = "Fuel Flow Estimation",
        year = 2025,
        status = "Completed",
        teams = 179,
        finalists = 53,
        countries = 48,
        submissions = "2,127",
        description = "Participants estimated per-second fuel flow along flight trajectories, enabling granular emissions analysis using open data sources.",
        url = "https://ansperformance.eu/study/data-challenge/dc2025/",
        github = "https://github.com/prc-data-challenge-2025"
      ),
      list(
        id = "dc2026",
        title = NA_character_,
        year = 2026,
        status = "Announced",
        teams = NA_integer_,
        finalists = NA_integer_,
        countries = NA_integer_,
        submissions = NA_character_,
        description = "The next edition of the PRC Data Challenge is in development. The topic and dataset will be announced in due course.",
        url = "https://ansperformance.eu/study/data-challenge/",
        github = NA_character_
      )
    )
  ),

  sensors = list(
    total_sensors = 12,
    total_airports = 7,
    countries = 4,
    airports = data.frame(
      icao    = c("ESSA", "EYVI", "EYPA", "UGTB", "UGSB", "UGKO", "SBSP"),
      name    = c("Stockholm Arlanda", "Vilnius", "Palanga", "Tbilisi", "Batumi", "Kutaisi", "São Paulo Congonhas"),
      country = c("Sweden", "Lithuania", "Lithuania", "Georgia", "Georgia", "Georgia", "Brazil"),
      sensors = c(4L, 2L, 1L, 2L, 1L, 1L, 1L),
      stringsAsFactors = FALSE
    )
  )
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
