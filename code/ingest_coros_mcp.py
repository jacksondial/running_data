"""Turn raw COROS MCP tool output into the pipe-delimited snapshot files.

The COROS MCP tools return prose. Large responses are written to disk by the
tool harness rather than being pasted around, so this script reads those raw
dumps and produces the files in data/coros/snapshot/ that
code/parse_coros_snapshot.R then turns into tidy CSVs.

Usage:
    python3 code/ingest_coros_mcp.py --activities <dump> [--hrv <dump> ...]
                                     [--load <dump>] [--rhr <dump>]
                                     [--sleep <dump>] [--stress <dump>]

Any stream not supplied is left untouched, so streams can be refreshed
independently.
"""

# Py3.9 on this machine: defer annotation evaluation so "str | None" parses.
from __future__ import annotations

from pathlib import Path
import argparse
import re
import sys

SNAP = Path(__file__).resolve().parent.parent / "data" / "coros" / "snapshot"

# COROS numeric sport codes -> the groups the app reports on. Grouping by code
# rather than display name because the names vary ("Cycling", "Gravel Bike",
# "Indoor Cycling" are all riding).
SPORT_GROUPS = {
    "Run": {100, 101, 102, 103},
    "Bike": {200, 201, 202, 203, 204, 205, 299},
    "Strength": {400, 401, 402, 901, 902, 903, 904, 905, 906},
    # everything else (hike 104/105, walk 900, ski 500-503, ...) -> "Other"
}


def sport_group(code: int) -> str:
    for name, codes in SPORT_GROUPS.items():
        if code in codes:
            return name
    return "Other"


def load_dump(path: str) -> str:
    """Read a raw dump, unescaping the \\n the harness writes for newlines."""
    text = Path(path).read_text()
    return text.replace("\\n", "\n")


def parse_activities(path: str) -> list[str]:
    text = load_dump(path)
    rows = []
    # Blocks look like "12. Outdoor Run — 2026-09-24\n   Location: ...".
    block_re = re.compile(
        r"^\s*\d+\.\s+(?P<sport>.+?)\s+—\s+(?P<date>\d{4}-\d{2}-\d{2})\s*$"
    )
    blocks, current = [], None
    for line in text.splitlines():
        m = block_re.match(line)
        if m:
            if current:
                blocks.append(current)
            current = {"sport": m.group("sport").strip(), "date": m.group("date"), "body": []}
        elif current is not None:
            current["body"].append(line)
    if current:
        blocks.append(current)

    for b in blocks:
        body = "\n".join(b["body"])

        def grab(pattern, cast=str):
            m = re.search(pattern, body)
            if not m:
                return ""
            try:
                return cast(m.group(1))
            except ValueError:
                return ""

        code = grab(r"SportType:\s*(\d+)", int)
        if code == "":
            continue
        location = grab(r"Location:\s*(.+)")
        duration = grab(r"Duration:\s*([0-9:]+)")
        distance = grab(r"Distance:\s*([0-9.]+)\s*km")
        # Some short activities report metres instead of km.
        if not distance:
            metres = grab(r"Distance:\s*([0-9.]+)\s*m\b")
            distance = f"{float(metres) / 1000:.3f}" if metres else ""
        pace = grab(r"Average Pace:\s*([0-9:]+)\s*/km")
        speed = grab(r"Average Speed:\s*([0-9.]+)\s*km/h")
        hr = grab(r"Avg HR:\s*(\d+)\s*bpm")
        cal = grab(r"Calories:\s*(\d+)\s*kcal")

        rows.append("|".join([
            b["date"], b["sport"], sport_group(code), location,
            duration, distance, pace, speed, hr, cal,
        ]))

    # Oldest first, and drop any duplicate rows from overlapping pulls.
    seen, unique = set(), []
    for r in sorted(rows):
        if r not in seen:
            seen.add(r)
            unique.append(r)
    return unique


def parse_hrv(paths: list[str]) -> list[str]:
    """Daily HRV assessment only -- the raw per-minute series is not used."""
    rows = {}
    for path in paths:
        text = load_dump(path)
        head = text.split("Sleep HRV Time Series")[0]
        for m in re.finditer(
            r"(\d{4}-\d{2}-\d{2}):\s*\n\s*HRV Avg:\s*(\d+) ms\s*—\s*([^\n]+?)\s*\n"
            r"\s*Normal Range:\s*(\d+)\s*-\s*(\d+) ms\s*\n\s*Baseline:\s*(\d+) ms",
            head,
        ):
            d, avg, status, lo, hi, base = m.groups()
            rows[d] = f"{d}: HRV Avg: {avg} ms — {status.strip()} | Normal Range: {lo} - {hi} ms | Baseline: {base} ms"
    return [rows[d] for d in sorted(rows, reverse=True)]


def parse_load(path: str) -> list[str]:
    text = load_dump(path)
    rows = {}
    for m in re.finditer(
        r"(\d{4}-\d{2}-\d{2})\s*\nComment:\s*(\S+)\s*\nShort-Term Load:\s*(\d+)\s*\n"
        r"Long-Term Load:\s*(\d+)\s*\nLoad Ratio:\s*([0-9.]+)",
        text,
    ):
        d, c, st, lt, r = m.groups()
        rows[d] = f"{d}|{c}|{st}|{lt}|{r}"
    return [rows[d] for d in sorted(rows, reverse=True)]


def parse_rhr(path: str) -> list[str]:
    text = load_dump(path)
    rows = {}
    for m in re.finditer(r"(\d{4}-\d{2}-\d{2}):\s*(\d+) bpm", text):
        rows[m.group(1)] = f"{m.group(1)}: {m.group(2)} bpm"
    return [rows[d] for d in sorted(rows, reverse=True)]


def parse_stress(path: str) -> list[str]:
    text = load_dump(path)
    rows = {}
    for m in re.finditer(r"(\d{4}-\d{2}-\d{2}):\s*\nAverage Stress:\s*(\d+)\s*\(([^)]+)\)", text):
        d, v, lab = m.groups()
        rows[d] = f"{d}|{v}|{lab}"
    return [rows[d] for d in sorted(rows, reverse=True)]


def parse_sleep(path: str) -> list[str]:
    """Sleep overview.

    COROS returns two shapes: a verbose one for short recent-day queries
    ("Main Sleep (asleep):", with an Awake Count line) and a compact one for
    long date ranges ("Main Sleep:", no Awake Count). Both are handled here.
    """
    text = load_dump(path)
    rows = {}
    for block in text.split("\n\n"):
        m_date = re.match(r"\s*(\d{4}-\d{2}-\d{2})\s*$", block.splitlines()[0] if block.strip() else "")
        if not m_date:
            continue
        date = m_date.group(1)

        def grab(pattern, default=""):
            m = re.search(pattern, block)
            return m.group(1) if m else default

        score = grab(r"Sleep Score:\s*(\d+)")
        if not score or score == "0":
            continue
        main = grab(r"Main Sleep(?: \(asleep\))?:\s*([0-9]+h [0-9]+min)")
        if not main:
            continue
        rows[date] = "|".join([
            date, score, main,
            grab(r"Deep Sleep Ratio:\s*(\d+)%", "0"),
            grab(r"Light Sleep Ratio:\s*(\d+)%", "0"),
            grab(r"REM Ratio:\s*(\d+)%", "0"),
            grab(r"Awake Ratio:\s*(\d+)%", "0"),
            grab(r"Awake Time:\s*(\d+) min", "0"),
            grab(r"Awake Count \(>5 min\):\s*(\d+)", "0"),
        ])
    return [rows[d] for d in sorted(rows)]


# COROS's own stress bands, inferred from the labelled values it returns in
# queryStressLevel and used to label the daily-health feed, which gives the
# number without the word.
def stress_label(v: int) -> str:
    if v <= 25:
        return "Relaxed"
    if v <= 50:
        return "Low"
    if v <= 75:
        return "Medium"
    return "High"


def parse_daily_health(path: str) -> list[str]:
    """Stress from the daily-health feed (`--- YYYYMMDD ---` blocks)."""
    text = load_dump(path)
    rows = {}
    for m in re.finditer(r"---\s*(\d{8})\s*---[^\n]*\n(?:[^\n]*\n)?Stress: Avg (\d+)", text):
        raw, val = m.groups()
        d = f"{raw[0:4]}-{raw[4:6]}-{raw[6:8]}"
        rows[d] = f"{d}|{val}|{stress_label(int(val))}"
    return [rows[d] for d in sorted(rows, reverse=True)]


def write(name: str, header: str | None, rows: list[str]) -> None:
    out = SNAP / name
    body = ("\n".join([header] + rows) if header else "\n".join(rows)) + "\n"
    out.write_text(body)
    print(f"  {name:22} {len(rows):5d} rows")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--activities")
    ap.add_argument("--hrv", nargs="*", default=[])
    ap.add_argument("--load")
    ap.add_argument("--rhr")
    ap.add_argument("--sleep")
    ap.add_argument("--stress")
    ap.add_argument("--daily-health")
    args = ap.parse_args()

    if args.activities:
        write("activities.txt",
              "date|sport|sport_group|location|duration|distance_km|avg_pace_km|avg_speed_kmh|avg_hr|calories",
              parse_activities(args.activities))
    if args.hrv:
        write("hrv.txt", None, parse_hrv(args.hrv))
    if args.load:
        write("training_load.txt", "date|comment|short_term_load|long_term_load|load_ratio",
              parse_load(args.load))
    if args.rhr:
        write("resting_hr.txt", "Resting Heart Rate\n========================\n",
              parse_rhr(args.rhr))
    if args.stress:
        write("stress.txt", "date|avg_stress|label", parse_stress(args.stress))
    if args.daily_health:
        write("stress.txt", "date|avg_stress|label", parse_daily_health(args.daily_health))
    if args.sleep:
        write("sleep.txt",
              "date|sleep_score|main_sleep|deep_pct|light_pct|rem_pct|awake_pct|awake_min|awake_count",
              parse_sleep(args.sleep))


if __name__ == "__main__":
    main()
