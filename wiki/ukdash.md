---
title: ukdash
type: project
status: active
created: 2026-10-27
updated: 2026-11-04
sources: []
scope: housing-observatory
tags: [project, frontend]
verified_commit: c629bced9d44ace5491bf4d080c01a9c74d73904
files: [app.R, DESCRIPTION, R, analysis, data-raw, manifest.json]
---
# ukdash

Shiny dashboard of the UK Housing Observatory, written as an R package: price indices, exuberance statistics, uncertainty, reports and downloads.

Part of [[concepts/housing-observatory]]. Embedded in [[projects/housing-site-uk/housing-site-uk]].

## Stack
R Shiny packaged with `DESCRIPTION`/`NAMESPACE` (Imports in `DESCRIPTION` are the source of truth). R 4.6.1 via `rig`; dependencies with `rv` (`rproject.toml` + `rv.lock`), never `install.packages()` or `renv`; format with `air format .`, lint with `jarl check .` before committing (repo `CLAUDE.md`).

## Run and test
`Rscript -e 'shiny::runApp()'`. Entry point `app.R`.

## Layout
- `R/` update pipeline `01-update-download.R` to `06-update-app.R` (download, exuberance, statistics, reports, app), shared `functions.R` and `src-map.R`
- `R/ui-*.R` one module per tab: `overview`, `indices`, `exuberance`, `uncertainty`, `report`, `download`
- `analysis/` data prep (HPU index, map source); `data-raw/shapefiles/` map boundaries
- `manifest.json`, `rsconnect/` deploy metadata; `output/`

## Unknowns
- Host: `CLAUDE.md` says shinyapps.io; the UK site links Posit Connect Cloud (`kvasilopoulos-ukdash.share.connect.posit.cloud`). Same open point as [[projects/idash/idash]].

Related: [[concepts/housing-observatory]], [[conventions]], [[inbox/ukdash]].

## Analytics
GA4 tag `G-ZHNETED6TG` (property "Housing Observatory - GA4"), loaded from `ukdash/wiki/ukdash.md/google-analytics.html` in the app head. The ID lives once, in `scripts/set_ga.py` (`GA_ID`); `python scripts/set_ga.py` rewrites it, `--check` verifies. Replaced the dead Universal Analytics tag (idash) and added the missing tag (ukdash) on 2026-11-04. After a republish, hits show under the `kvasilopoulos-www.share.connect.posit.cloud` hostname.

## Analytics
GA4 tag `G-ZHNETED6TG` (property "Housing Observatory - GA4"), loaded from `ukdash/google-analytics.html` in the app head. The ID lives once, in `scripts/set_ga.py` (`GA_ID`); `python scripts/set_ga.py` rewrites it, `--check` verifies. Replaced the dead Universal Analytics tag (idash) and added the missing tag (ukdash) on 2026-11-04. After a republish, hits show under the `kvasilopoulos-.share.connect.posit.cloud` hostname.
