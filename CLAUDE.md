# ukdash

UK Housing Observatory Shiny dashboard, structured as an R package (`DESCRIPTION`/`NAMESPACE`/`R/`). `app.R` is the entry point; `analysis/` and `data-raw/` prepare the underlying data. Deployed to shinyapps.io via `rsconnect` (`manifest.json`, `rsconnect/`).

## Environment

Dependencies are pinned with `renv` (installer backend: `pak`, enabled via `.Rprofile`). The lockfile was seeded from the deployed `rsconnect` manifest, so it should track deployed versions closely.

- Restore: `Rscript -e 'renv::restore()'`
- Add a dependency: add it to `DESCRIPTION` Imports, then `Rscript -e 'renv::install("pkg"); renv::snapshot()'`
- After changing dependencies: `Rscript -e 'renv::snapshot()'`

## Commands

- Run locally: `Rscript -e 'shiny::runApp()'`
- Deploy: `Rscript -e 'rsconnect::deployApp()'`
