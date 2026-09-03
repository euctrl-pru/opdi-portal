# OPDI Portal

Quarto sources for the [Open Performance Data Initiative](https://opdi.aero)
portal.

## Getting started

```bash
git clone https://github.com/euctrl-pru/opdi-portal
cd opdi-portal
quarto add rchaput/acronyms@master   # required extension
quarto render
```

`quarto render` needs no credentials and makes no network calls: every number on
the site is read from a cache committed to the repo. See
[Refreshing the sensor data](#refreshing-the-sensor-data) below.

### Layout

```
index.qmd                 home page (must stay at the root)
content/                  all other pages
assets/                   logos, images, CSS, CSL, bibliography
R/                        configuration and analysis scripts
data/sensors/             caches for the OPDI sensor network
data/reference/           caches derived from open EUROCONTROL datasets
archive/                  retired files, not rendered
_site/                    build output (committed; the site is served from it)
```

### Environment

Copy `.env.template` to `.env` and fill in what you need. **`.env` is gitignored;
never commit it.** Nothing in `.env` is required to render the site — only to
refresh caches from the live sources.

| Variable | Needed for |
|---|---|
| `OPDI_UPDATE_CACHE` | guards the cache refresh; see below |
| `OPENSKY_CLIENT_ID`, `OPENSKY_CLIENT_SECRET` | sensor reception coverage (OpenSky REST) |
| `OSN_USERNAME`, `OSN_KEY` | flight hours and airport impact (OpenSky S3 — a *different* credential pair) |
| `OPDI_SENSORS_PH_URL` | PocketBase sensor register (world-readable; the default works) |

`R/portal_config.R` loads `.env` on every source, so the scripts see these
without any shell setup. An empty value in `.env` is ignored rather than
overriding one already exported in your shell.

The OpenSky S3 scans also need the
[`osninterface`](https://github.com/euctrl-pru/osn-interface) package:

```r
remotes::install_github("euctrl-pru/osn-interface")
```

## Refreshing the sensor data

Every figure, table, map and chart on the **Sensors** page is produced by a
script in `R/` and stored as JSON under `data/`. Rendering the site *only ever
reads those caches*. That keeps a build fast, reproducible, and possible for
anyone who clones the repo — including people without OpenSky credentials.

Refreshing a cache is a separate, deliberate act.

### The switch

`R/portal_config.R` holds a single object, `update_sensor_pages_cache`. Its
`update` flag guards every refresh:

```r
update_sensor_pages_cache <- list(
  update   = FALSE,                     # TRUE = fetch live and rewrite caches
  datasets = c("coverage", "flight_hours", "impact", "capture", "capture_daily"),
  ...
)
```

Leave `update = FALSE` in the repo. Override it for a single run instead:

```bash
# refresh everything
OPDI_UPDATE_CACHE=true Rscript R/update_sensor_cache.R

# refresh one dataset, or a subset
OPDI_UPDATE_CACHE=true Rscript R/update_sensor_cache.R coverage
OPDI_UPDATE_CACHE=true Rscript R/update_sensor_cache.R impact capture
```

Then review the regenerated JSON, re-render, and commit both.

### The datasets

| Dataset | What it produces | Cache | Needs | Time |
|---|---|---|---|---|
| `coverage` | sensor register and reception polygons | `data/sensors/coverage.json` | PocketBase + OpenSky OAuth | ~10 s |
| `flight_hours` | observed flight hours on one day | `data/sensors/flight_hours.json` | OpenSky S3 + `osninterface` | ~2 min |
| `impact` | added coverage benefit per airport | `data/sensors/airport_impact.json` | OpenSky S3 + `osninterface` | ~40 min |
| `capture` | annual capture rate vs reported traffic | `data/reference/airport_capture_{year}.json` | nothing (open data) | ~2 min |
| `capture_daily` | daily capture rate over recent months | `data/reference/airport_capture_daily.json` | nothing (open data) | ~1 min |

A failed dataset leaves its cache untouched and the run continues, so a missing
credential never destroys good data. The summary at the end names what to re-run.

### The parameters

Analysis choices — dates, radii, thresholds — live beside the switch in
`update_sensor_pages_cache`, not scattered through the scripts. Edit them there.
Each cache also records the parameters it was built with, so a JSON file always
says how it was made.

```r
impact = list(
  date      = "2026-07-08",   # a representative summer day
  radius_nm = 40,             # the terminal area
  max_gap_seconds = 60
)
```

Two are worth understanding before you change them.

**`max_gap_seconds` (60).** Flight hours are summed as the interval between
consecutive receptions of the same aircraft. A longer gap means the aircraft left
coverage and returned; that time was not observed. The state-vector data is
5-second downsampled, so 99% of gaps are exactly 5 s — the result barely moves
between a 30 s and a 300 s cutoff.

**`gap_day_threshold_pct` (20).** A day on which the flight list captures less
than this share of reported movements *at every sensor airport simultaneously* is
an ingest gap, not a coverage event: no receiver failure happens at once in
Sweden, Lithuania and Georgia. Such days are excluded from the daily chart.
Clean days sit near 92% and gap days near 0%, so anything from 5 to 50 selects
the same set.

### Adding a dataset

Add an entry to `DATASETS` in `R/update_sensor_cache.R` with a `label`, `needs`,
`cache` path and a `run(params)` function, and a matching parameter list in
`update_sensor_pages_cache`. Nothing else needs to change.

Each analysis script is also runnable on its own, which is useful while
developing:

```bash
Rscript R/flight_hours.R 2026-07-08          # print, and cache, one day
Rscript R/airport_capture.R 2024             # a different year
Rscript R/airport_capture_daily.R 2026 1 5   # year, first month, last month
```

## Monthly data refresh

0. `git checkout -b YYYYMM-release`
1. Update **only** `R/portal_config.R` with the new:
   - coverage start/end dates (site, flight list, flight events, measurements, snapshot),
   - dataset version tag and refresh label,
   - headline counts (flights / events / measurements).
2. `quarto render` to regenerate `_site/`.
3. Spot-check the rendered pages — `index.qmd`, `content/data*.qmd`,
   `content/data-preview.qmd` — to confirm the new values flow through.

The sensor caches are independent of this cycle; refresh them when the sensor
network changes, not every month.

## Publishing

From the repo root, after the refresh and checks:

```bash
git checkout main
git pull
git merge --squash YYYYMM-release
git commit -m "YYYYMM full release"
git tag -af v9.8.1 -m "YYYYMM full release"
git push origin --follow-tags main
```

The site is served from the committed `_site/` directory, so a release must
include a fresh `quarto render`.
