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

## Release settings

Every value that changes with a data release is in `R/portal_config.R`; the
pages read it from there.

| Setting | Meaning |
|---|---|
| `release` | The release the Data and Methods sections describe (`v2.0.0`). |
| `current_release` | The release presented as the one to use. `"v0.0.2"` until the files of release v2.0.0 are produced and reachable; then `"v2.0.0"`. |
| `previous_release` | The earlier release whose files stay available (`v0.0.2`). |
| `release_coverage` | First and last day of release v2.0.0 (`end` empty means "to the latest published day"). |
| `release_date`, `release_counts$flights` | Shown on the Roadmap and the home page once `current_release` is `"v2.0.0"`; empty values show as "to be announced". |
| `access` | Trino catalog and schema, the Trino file browser address, the public download address and the OpenSky access page. Empty addresses show as "to be announced". |
| `refresh_label`, `coverage`, `counts` | The last refresh of release v0.0.2: its month, the period covered and the headline counts. |

### Switching the current release

When the files of release v2.0.0 have been produced for the first period and
can be reached through Trino:

1. In `R/portal_config.R`, set `current_release = "v2.0.0"`, set
   `release_coverage$end` (or leave it `NA`), `release_date` and
   `release_counts$flights`.
2. Render and check the home page, `content/data/index.qmd`,
   `content/data/access.qmd`, `content/data/releases.qmd`,
   `content/data/v0.0.2.qmd` and `content/roadmap.qmd`: each says which
   release is current, and the wording changes with this one setting.

The pages are written so that both values read correctly. To check the
other wording before the switch, render once with the other value and do not
commit it.

## Rendering and publishing

### Prerequisites

- Quarto, R with the packages the pages load (among them `bslib`, `bsicons`,
  `lubridate`, `tidyverse`, `arrow`, `plotly`), and the `acronyms` extension
  (`quarto add rchaput/acronyms@master`).
- Python 3 for the scripts under `tools/` (no packages needed for the checks;
  pandas and matplotlib for `draw_validation_figures.py`).
- A checkout of the `opdi` repository beside this one, for the data
  dictionary.

### Steps

```bash
# 1. Tables of the data dictionary, from the pipeline's list of columns
python tools/gen_data_dictionary.py --opdi ../opdi/src --check   # or without --check to rewrite

# 2. Only when a study's committed results changed: figures and numbers
python tools/draw_validation_figures.py
python tools/gen_validation_summary.py --opdi ../opdi/src

# 3. Checks: links, sidebar, hidden community page, methods template,
#    numbers, wording, event types; the studies index; the tools' own tests
python tools/check_site.py
python tools/check_site.py --list-types --opdi ../opdi/src
python tools/check_papers_index.py
python -m pytest tools/tests

# 4. Render the site, and the studies (a separate Quarto project)
quarto render
quarto render papers/
```

Rendering the site (`quarto render`) needs no credentials. Rendering the
studies is different: `quarto render papers/` runs the analysis of each newer
study again when the inputs of that study have changed, and then needs
credentials for the computing cluster. To render them from the committed data
instead, run `OPDI_RENDER=allow-stale quarto render papers/`; the study then
marks the figures it could not check against its inputs.

`tools/README.md` explains each script and each check.

### What to look at after rendering

These pages have not yet been rendered; check them by eye in `_site/`:

- Home page (`index.html`): the release note in the Data card and the two
  value boxes.
- Data section (`content/data/`): `index`, `access`, `dictionary`,
  `releases`, `limitations`, `v0.0.2`. On `access`, the `USE` statement and
  the "to be announced" addresses; on `dictionary`, the width of the long
  column tables; on `v0.0.2`, the entity diagram, the two download scripts
  and the three file lists in their collapsed boxes.
- Methods section (`content/methods/`): all seventeen pages, in particular
  the charts under "Error distribution", the diagram on `pipeline`, the
  figure of flight milestones on `index`, and the table on
  `validation-summary`.
- `content/concepts.html` and `content/methodology.html`, now short pages
  that point into the Methods section.
- `content/roadmap.html`, `content/resources.html`, `content/data-challenges.html`
  (the 2026 edition), `content/about.html`, `content/data-preview.html`,
  `content/sensors-expansion.html`.
- `papers/index.html` and every study the Methods pages link to.
- The old addresses still open the right page: `/data.html`,
  `/content/data.html`, `/flight-list-data.html`, `/flight-event-data.html`,
  `/measurement-data.html`, `/methodology.html`, `/concepts.html`.

### The community page

`content/community.qmd` is kept but hidden: it is excluded from rendering in
`_quarto.yml` and is not linked from any page. `tools/README.md`, section "The
community page", lists the four edits that show it again. A `_site/` rendered
before the exclusion still holds `content/community.html` and its entry in
`_site/search.json`; both go away only on the next render. If
`_site/content/community.html` is still there after rendering, delete it.

### Merging

The site is served from the committed `_site/` directory, so a release must
include a fresh `quarto render`. From the repository root, after the checks:

```bash
git checkout main
git pull
git merge --squash feature/new-topics
git commit -m "Release v2.0.0 documentation"
git push origin main
```

`PUBLISHING_CHECKLIST.md` lists what must be supplied or decided before this
merge.

## Refreshing release v0.0.2

Release v0.0.2 is no longer extended. Should its files be refreshed once
more, update in `R/portal_config.R` the coverage dates, `refresh_label` and
the headline counts, render, and check the home page,
`content/data/v0.0.2.qmd` and `content/data-preview.qmd`.
