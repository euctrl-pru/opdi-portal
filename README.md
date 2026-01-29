
# OPDI Portal

<!-- badges: start -->
<!-- badges: end -->

This repo contains the Quarto sources for the OPDI portal.

You need to install the following Quarto extension(s):
* `quarto add rchaput/acronyms@master`

## Monthly Data Refresh (single source of truth)

1) Update only `R/portal_config.R` with the new:
   - coverage start/end dates (site, flight list, flight events, measurements, snapshot),
   - dataset version tag, refresh label,
   - headline counts (Flights/Events/Measurements).
2) Run `quarto render` to regenerate `_site/`.
3) Spot-check the rendered pages (`index.qmd`, `data*.qmd`, `data-preview.qmd`) to confirm the new values flow through.

## Publishing

Run from the repo root after completing the monthly refresh and checks:

```bash
git checkout master
git pull
git merge --squash 202308-release-1
git commit -m "202308 August full release"
git tag -l            # only if you need to inspect existing tags
git tag -af v9.8.1 -m "August 2023 full release"
git push origin --follow-tags master
```



