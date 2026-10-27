# ukdash

UK Housing Observatory Shiny dashboard, structured as an R package (`DESCRIPTION`/`NAMESPACE`/`R/`). `app.R` is the entry point; `analysis/` and `data-raw/` prepare the underlying data. Deployed to shinyapps.io via `rsconnect` (`manifest.json`, `rsconnect/`).

## Environment

- R version: managed with `rig` (currently 4.6.1 - `rig default 4.6.1`)
- Dependencies: `DESCRIPTION` Imports is the source of truth; `rv` (`rproject.toml` + lockfile) manages the reproducible dev library on top of it, not renv
  - Setup: `rv sync`
  - Add a dependency: add it to `DESCRIPTION` Imports, then `rv add <pkg>` (or `rv sync` if already added to rproject.toml)
- Format: `air format .` (check only: `air format --check .`)
- Lint: `jarl check .` (autofix: `jarl check . --fix`)

The `rv.lock` lockfile was originally seeded (via renv, before this migration) from the deployed `rsconnect` manifest, so it tracked deployed versions closely. Re-verify `rv sync` against the deployed shinyapps.io manifest at the next deploy to confirm that fidelity still holds.

### Rules
- Never use `install.packages()` or `renv::*` - all dependency changes go through DESCRIPTION + `rv add`/`rv remove`.
- Run `air format .` then `jarl check .` before committing.
- No `# nolint` comments - jarl uses `# jarl-ignore <rule>: <reason>` on the line before the flagged code instead.

## Commands

- Run locally: `Rscript -e 'shiny::runApp()'`
- Deploy: `Rscript -e 'rsconnect::deployApp()'`

<!-- knowledge:start -->
## Knowledge wiki
This repo's wiki lives in `./wiki` (committed with the code; overview: `wiki/ukdash.md`). It is linked into the
shared vault at `./kb` (-> C:\Users\User\knowledge), where `kb/CLAUDE.md` holds the rules (frontmatter, wikilinks, INGEST/QUERY/LINT).
- Session start: read `kb/inbox/ukdash.md`, act on items, move them under `## Done`.
- Write the wiki to the repo wiki standard: `kb/REPO-WIKI.md` (layout, page types, frontmatter, links). Scaffold pages with `python kb/scripts/new_page.py ukdash architecture|decision|runbook|integration|concept "<Title>"`.
- Update the wiki page that describes a behaviour in the same change that alters it; then run `python kb/scripts/reindex.py` and `python kb/scripts/lint.py`.
- Find things with `python kb/scripts/search.py "<terms>"` before browsing `kb/wiki/INDEX.md`. Never edit `kb/raw/`.
<!-- knowledge:end -->
