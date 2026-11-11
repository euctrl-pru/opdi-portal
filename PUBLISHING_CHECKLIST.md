# Publishing checklist

What has to be supplied, decided and checked before the branch
`feature/new-topics` is merged into `main` and the site is published. The
render steps themselves are in `README.md`, section "Rendering and
publishing".

## 1. Values to supply

All in `R/portal_config.R`, block `access` unless stated otherwise. An empty
address is shown on the pages as "to be announced", so the site can be
published before every value is known.

- [ ] **Trino catalog and schema names.** `trino_catalog` and `trino_schema`
  hold the placeholders `minio` and `opdi`, copied from the pipeline's table
  definitions. Set the real names, then set `trino_names_final = TRUE` (the
  Access page stops calling them placeholders). In the `opdi` repository, the
  table definitions are written again with the same names by
  `python -m opdi.sql.render --catalog <catalog> --schema <schema>`.
- [ ] **Trino file browser address**: `browser_url`.
- [ ] **Public download address** of the published folder (the folder that
  holds `flight_list/` and `flight_events/`): `download_url`. The reader in
  the `opdi` package has its own placeholder default
  (`PUBLISHED_BASE_URL` in `src/opdi/published.py`); set it to the same
  address there.
- [ ] **OpenSky access page.** `opensky_access_url` points at
  `https://opensky-network.org/data/impala`, the page this site already cited
  for access to the OpenSky historical database. Confirm with the OpenSky
  Network that it is the right page for access to the Trino instance that
  holds the OPDI tables.

## 2. Facts to confirm

- [ ] **2026 PRC Data Challenge.** Compare the 2026 card on
  `content/data-challenges.qmd` and the `dc2026` entry in
  `R/portal_config.R` with the live pages at
  `https://ansperformance.eu/study/data-challenge/dc2026/` and the GitHub
  organisation `prc-data-challenge-2026`: the task, the dates (1 September to
  14 October 2026), the ten airports, 4,167,797 movements, the metric, the
  three winning teams and their scores, the presentation at the 14th OpenSky
  Symposium (Madrid, 29-30 October 2026) and the planned data paper (first
  quarter of 2027). The team, country and submission totals of 2026 are left
  out because they were not published; add them when they are, and change
  `totals_label` from "2024-2025".
- [ ] **The OPDI software is public.** The Access page tells readers to
  install the reader with
  `pip install "opdi[reader] @ git+https://github.com/euctrl-pru/opdi"`, and
  the Resources page points at the `airflow/` folder of that repository. Both
  need the industrialisation work merged into the default branch of
  `euctrl-pru/opdi` first.
- [ ] **API reference.** Resources links to `https://euctrl-pru.github.io/OPDI/`.
  Check that it is online and describes the current package.
- [ ] **Release v0.0.2 downloads.** Open a few of the links on
  `content/data/v0.0.2.qmd` (first and last month, first and last ten-day
  file) to confirm the files are still served.

## 3. Decisions

- [ ] **When to switch the current release.** `current_release` stays
  `"v0.0.2"` until the files of release v2.0.0 have been produced for the
  first period (from 1 January 2025) and can be reached. Then set
  `current_release = "v2.0.0"`, `release_coverage$end`, `release_date` and
  `release_counts$flights`, render, and look at the pages listed in
  `README.md`, "Switching the current release".
- [ ] **Community page.** It stays hidden until every person and
  organisation shown on `content/community.qmd` has agreed to be listed.
  `tools/README.md`, "The community page", lists the edits that show it again,
  including removing `"!content/community.qmd"` from `project: render:` in
  `_quarto.yml`.
- [ ] **Old community page in `_site/`.** The page is excluded from rendering,
  but an earlier render left `_site/content/community.html` and its entry in
  `_site/search.json`. The search entry goes away at the next full render; if
  `_site/content/community.html` is still there after rendering, delete it.

## 4. Render and check

- [ ] `python tools/gen_data_dictionary.py --opdi ../opdi/src --check` reports
  no difference (run it without `--check` if the pipeline's columns changed).
- [ ] `python tools/check_site.py` reports `OK`.
- [ ] `python tools/check_site.py --list-types --opdi ../opdi/src` lists the
  same event types as the Data dictionary.
- [ ] `python tools/check_papers_index.py` reports `OK`.
- [ ] `python -m pytest tools/tests` passes.
- [ ] `quarto render` finishes without error.
- [ ] `quarto render papers/` has been run for the studies the Methods pages
  link to. At the time of writing, `_site/papers/` has no page for
  `airspace-events-v1` or `track-construction-v2` to `track-construction-v5`,
  so the "Full study" links of Track identity, Holding, Point merge, FIR
  crossings and Measurements lead nowhere until they are rendered.
- [ ] The pages listed in `README.md`, "What to look at after rendering",
  have been looked at in a browser, at full width and on a phone.
- [ ] The old addresses open the right page: `/data.html`,
  `/content/data.html`, `/flight-list-data.html`, `/flight-event-data.html`,
  `/measurement-data.html`, `/methodology.html`, `/concepts.html`.

## 5. Merge

- [ ] Commit the rendered `_site/` on `feature/new-topics`.
- [ ] Merge `feature/new-topics` into `main` as described in `README.md`,
  "Merging", and push.
- [ ] Open `https://www.opdi.aero` and follow the sidebar once from top to
  bottom.
