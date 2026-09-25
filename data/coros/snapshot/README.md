# COROS snapshot

Condensed extracts of COROS MCP server responses, fetched **2026-09-24**.

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
| `activities.txt` | `querySportRecords(20260713-20260924, run codes 100/101/102/103)` |

`fitness_assessment.txt` and `recovery_status.txt` are verbatim tool output.
The rest are pipe-delimited condensations of it — the tools return prose blocks,
and the numbers are the only part the app uses.

Dates follow COROS conventions: sleep and HRV are dated by **wake-up day**, so a
value dated D describes the night that *ended* on the morning of D.

Gaps are real: days the watch was not worn (or not synced) are simply absent, and
the parser leaves them as NA rather than interpolating.
