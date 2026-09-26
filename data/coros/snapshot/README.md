# COROS snapshot

Condensed extracts of COROS MCP server responses, fetched **2026-09-26**.

Built by `code/ingest_coros_mcp.py`, which parses the raw MCP tool output rather
than being typed by hand. Large MCP responses are written to disk by the tool
harness; point the script at those dumps:

```
python3 code/ingest_coros_mcp.py \
    --activities <querySportRecords dump> \
    --sleep      <querySleepOverview dump> \
    --daily-health <queryDailyHealthData dump> \
    --hrv <querySleepHrv dumps...>
```

The Shiny app cannot call the COROS MCP server at runtime, so these files are the
hand-off point: fetch with the MCP tools, drop the values here, then run
`code/parse_coros_snapshot.R` to produce the tidy CSVs the app reads.

| File | MCP tool |
|---|---|
| `fitness_assessment.txt` | `queryFitnessAssessmentOverview` |
| `recovery_status.txt` | `queryRecoveryStatus` |
| `training_load.txt` | `queryTrainingLoadAssessment(days=30)` |
| `resting_hr.txt` | `queryRestingHeartRate(days=30)` |
| `sleep.txt` | `querySleepOverview(days=30)` |
| `stress.txt` | `queryStressLevel(days=30)` |
| `hrv.txt` | `querySleepHrv` (3 x 7-day windows; the tool caps at 7 days per call) |
| `activities.txt` | `querySportRecords(20231201-20260926, sportTypeCodes=[65535])` |

`fitness_assessment.txt` and `recovery_status.txt` are verbatim tool output.
The rest are pipe-delimited condensations of it — the tools return prose blocks,
and the numbers are the only part the app uses.

Dates follow COROS conventions: sleep and HRV are dated by **wake-up day**, so a
value dated D describes the night that *ended* on the morning of D.

Gaps are real: days the watch was not worn (or not synced) are simply absent, and
the parser leaves them as NA rather than interpolating.

## All sports, not just running

`activities.txt` is pulled with `sportTypeCodes=[65535]` (all sports) rather than
the running codes. Pulling only running silently dropped 45 sessions — including
a 333 km ride — and made the training-load chart unreadable, because COROS's load,
recovery and HRV numbers count every sport while a running-only activity list does
not.

The file carries a `sport_group` column (`Run` / `Bike` / `Strength`) and columns
for both pace and speed, because the sports do not report the same fields:

| sport_group | distance | pace | speed |
|---|---|---|---|
| Run | yes | `avg_pace_km` | - |
| Bike | yes | - | `avg_speed_kmh` |
| Strength | - | - | - |

Distance is therefore **not** comparable across sports. Duration and COROS load
are the cross-sport currencies, and the app uses those wherever it mixes sports.


## How far back each stream goes

COROS serves different history depths per metric, which is why the app's
timeframe selector shows a per-stream day count rather than pretending every
chart covers the same window.

| Stream | Depth | Limit |
|---|---|---|
| Activities | 1,105 back to **2023-12-30** | none found; `limit` must be raised (defaults to 20) |
| Stress | 354 days | 365-day cap on `days` |
| Sleep / HRV / resting HR | 273 days, from **2025-12-17** | that is when the watch began recording them |
| Training load | ~31 days | **API caps this near 30 days** regardless of `days` |

Two API quirks worth remembering:

* `queryRestingHeartRate(days=1100)` returns "The date is out of range"; 365 is
  the practical maximum for the day-count tools.
* `querySleepHrv` documents a 7-day maximum, but that applies to the
  *recent-days* mode only. Passing explicit `startDate`/`endDate` returns the
  whole range, so month-sized chunks work.
