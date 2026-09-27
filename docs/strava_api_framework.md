# Strava API Framework

This repo now includes a Strava API sync framework that replaces manual archive downloads with:

1. OAuth token setup
2. paginated activity sync
3. optional detailed activity hydration
4. export to an archive-style CSV
5. rebuild of `data/activity_dat2.RDS`

## Files

- `code/strava_api_sync.py`
  - Handles OAuth URL generation, token exchange, token refresh, activity sync, raw JSON cache, and CSV export.
- `code/build_activity_dat_from_csv.R`
  - Takes the API compatibility CSV and rebuilds `data/activity_dat2.RDS` in the format your app already uses.

## Environment Variables

Set these before running the sync:

- `STRAVA_CLIENT_ID`
- `STRAVA_CLIENT_SECRET`
- `STRAVA_REDIRECT_URI`
  - Optional. Defaults to `http://localhost:8765/exchange_token`

## First-Time Setup

1. Create a Strava API application in the Strava developer portal.
2. Set the app callback domain / redirect URI to match `STRAVA_REDIRECT_URI`.
3. Export your credentials as environment variables.
4. Print the authorization URL:

```bash
python3 code/strava_api_sync.py auth-url
```

5. Open the URL, approve the app, and copy the `code` query parameter from the redirect URL.
6. Exchange the code for tokens:

```bash
python3 code/strava_api_sync.py exchange-code --code YOUR_CODE
```

## Sync Activity Data

Incremental sync:

```bash
python3 code/strava_api_sync.py sync
```

Historical backfill from a date:

```bash
python3 code/strava_api_sync.py sync --after 2024-01-01T00:00:00 --full-refresh
```

Fast sync using summary activities only:

```bash
python3 code/strava_api_sync.py sync --skip-details
```

Outputs:

- `data/strava/api/token.json`
- `data/strava/api/manifest.json`
- `data/strava/api/raw/activities_summary.json`
- `data/strava/api/raw/activities_detailed.json`
- `data/strava/api_export/activities_api_compat.csv`

## Rebuild The Existing RDS

```bash
Rscript code/build_activity_dat_from_csv.R
```

You can also pass paths explicitly:

```bash
Rscript code/build_activity_dat_from_csv.R \
  data/strava/api_export/activities_api_compat.csv \
  data/activity_dat2.RDS
```

## Compatibility Notes

The API export is designed to preserve the columns your current R pipeline depends on, but it is not identical to the downloadable Strava archive.

Known gaps:

- `Training Load` is not populated by this framework.
- `Intensity` is not populated by this framework.
- `Recovery` is not populated by this framework.
- Most weather fields are not populated by this framework.
- Some archive-only metadata fields remain blank unless Strava exposes an API equivalent.

Where the API field names differ, the export maps them into archive-style names. For example:

- API `distance` in meters -> archive `Distance` in kilometers
- API `average_speed` in meters/second -> archive `Average Speed` in kilometers/hour
- API `suffer_score` -> archive `Relative Effort`

## Operational Guidance

- Use `sync --full-refresh` for the initial backfill.
- After that, plain `sync` should be enough for incremental updates.
- Incremental syncs re-fetch a 14-day overlap window to catch edits to recent activities.
- If Strava changes response fields, inspect the raw JSON cache before changing the compatibility export.
