# Central place to update release metadata and coverage dates for the portal.
# Update this file for each monthly refresh; the QMD files pull values from here.

portal_cfg <- list(
  version = "v0.0.2",
  refresh_label = "February 2026",
  coverage = list(
    start = as.Date("2022-01-01"),
    end   = as.Date("2026-01-31"),
    snapshot_end = as.Date("2026-01-31")
  ),
  counts = list(
    flights = "+67M",
    events = "+1.6B",
    measurements = "+3.2B"
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
