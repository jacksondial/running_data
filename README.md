# running_data

A Shiny app for marathon training, built on data from a COROS watch: race
readiness, training load, recovery and sleep, a multi-sport training log, and
COROS's own race predictor.

## Running it

Click **Run App** in RStudio with `ui.R` or `server.R` open, or from the repo root:

```r
shiny::runApp("code")
```

## Data

The app reads a point-in-time snapshot of COROS data. The COROS MCP server is
only reachable from Claude Code, not from the app at runtime, so refreshing is
a two-step hand-off:

1. Pull fresh data with the COROS MCP tools and turn the raw output into the
   snapshot files: `python3 code/ingest_coros_mcp.py --help`
2. Rebuild the tidy CSVs the app reads: `Rscript code/parse_coros_snapshot.R`

`data/coros/snapshot/README.md` has the details, including how far back each
metric goes. Everything else under `data/` is personal and stays out of git.

## Layout

| File | Role |
|---|---|
| `code/app.R` | entry point |
| `code/init.R` | shared setup: packages, theme, COROS data and tabs |
| `code/ui.R` | the navbar page |
| `code/coros_data.R` | loads the snapshot, derives metrics |
| `code/coros_plots.R` | interactive (ggiraph) charts |
| `code/ui_coros.R` | the tabs |
| `code/server.R` | outputs |
| `code/ingest_coros_mcp.py` | raw MCP output -> snapshot files |
| `code/parse_coros_snapshot.R` | snapshot files -> tidy CSVs |

The earlier Strava-based version of the app lives in the git history.
