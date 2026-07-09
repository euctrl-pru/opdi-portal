# Central place to update release metadata and coverage dates for the portal.
# Update this file for each monthly refresh; the QMD files pull values from here.

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

  challenges = list(
    editions = 2,
    total_teams = "100+",
    items = list(
      list(
        id = "dc2024",
        title = "Actual Takeoff Weight Prediction",
        year = 2024,
        status = "Completed",
        teams = 53,
        submissions = "1,600+",
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
        submissions = "2,127",
        description = "Participants estimated per-second fuel flow along flight trajectories, enabling granular emissions analysis using open data sources.",
        url = "https://ansperformance.eu/study/data-challenge/dc2025/",
        github = "https://github.com/prc-data-challenge-2025"
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
