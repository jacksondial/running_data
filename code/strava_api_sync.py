#!/usr/bin/env python3
"""
Strava API sync framework for this repo.

Goals:
1. Replace manual archive downloads with OAuth + API sync.
2. Export an archive-compatible CSV shape for the existing R pipeline.
3. Keep enough raw JSON on disk to debug schema drift later.

This script intentionally uses only the Python standard library.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Iterable, List, Optional


REPO_ROOT = Path(__file__).resolve().parent.parent
DATA_ROOT = REPO_ROOT / "data" / "strava" / "api"
RAW_ROOT = DATA_ROOT / "raw"
EXPORT_ROOT = REPO_ROOT / "data" / "strava" / "api_export"
TOKEN_PATH = DATA_ROOT / "token.json"
MANIFEST_PATH = DATA_ROOT / "manifest.json"
SUMMARY_CACHE_PATH = RAW_ROOT / "activities_summary.json"
DETAIL_CACHE_PATH = RAW_ROOT / "activities_detailed.json"
EXPORT_CSV_PATH = EXPORT_ROOT / "activities_api_compat.csv"

AUTH_URL = "https://www.strava.com/oauth/authorize"
TOKEN_URL = "https://www.strava.com/oauth/token"
API_BASE = "https://www.strava.com/api/v3"
DEFAULT_LOOKBACK_SECONDS = 14 * 24 * 60 * 60

# Archive headers found in the manual export. Unsupported API fields are left blank.
ARCHIVE_HEADERS = [
    "Activity ID",
    "Activity Date",
    "Activity Name",
    "Activity Type",
    "Activity Description",
    "Elapsed Time",
    "Distance",
    "Max Heart Rate",
    "Relative Effort",
    "Commute",
    "Activity Private Note",
    "Activity Gear",
    "Filename",
    "Athlete Weight",
    "Bike Weight",
    "Moving Time",
    "Max Speed",
    "Average Speed",
    "Elevation Gain",
    "Elevation Loss",
    "Elevation Low",
    "Elevation High",
    "Max Grade",
    "Average Grade",
    "Average Positive Grade",
    "Average Negative Grade",
    "Max Cadence",
    "Average Cadence",
    "Average Heart Rate",
    "Max Watts",
    "Average Watts",
    "Calories",
    "Max Temperature",
    "Average Temperature",
    "Total Work",
    "Number of Runs",
    "Uphill Time",
    "Downhill Time",
    "Other Time",
    "Perceived Exertion",
    "Type",
    "Start Time",
    "Weighted Average Power",
    "Power Count",
    "Prefer Perceived Exertion",
    "Perceived Relative Effort",
    "Total Weight Lifted",
    "From Upload",
    "Grade Adjusted Distance",
    "Weather Observation Time",
    "Weather Condition",
    "Weather Temperature",
    "Apparent Temperature",
    "Dewpoint",
    "Humidity",
    "Weather Pressure",
    "Wind Speed",
    "Wind Gust",
    "Wind Bearing",
    "Precipitation Intensity",
    "Sunrise Time",
    "Sunset Time",
    "Moon Phase",
    "Bike",
    "Gear",
    "Precipitation Probability",
    "Precipitation Type",
    "Cloud Cover",
    "Weather Visibility",
    "UV Index",
    "Weather Ozone",
    "Jump Count",
    "Total Grit",
    "Average Flow",
    "Flagged",
    "Average Elapsed Speed",
    "Dirt Distance",
    "Newly Explored Distance",
    "Newly Explored Dirt Distance",
    "Activity Count",
    "Total Steps",
    "Carbon Saved",
    "Pool Length",
    "Training Load",
    "Intensity",
    "Average Grade Adjusted Pace",
    "Timer Time",
    "Total Cycles",
    "Recovery",
    "With Pet",
    "Competition",
    "Long Run",
    "For a Cause",
    "With Kid",
    "Downhill Distance",
    "Media",
]


def ensure_dirs() -> None:
    for path in (DATA_ROOT, RAW_ROOT, EXPORT_ROOT):
        path.mkdir(parents=True, exist_ok=True)


def load_json(path: Path, default):
    if not path.exists():
        return default
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def write_json(path: Path, payload) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2, sort_keys=True)


def required_env(name: str) -> str:
    value = os.environ.get(name)
    if not value:
        raise SystemExit(f"Missing required environment variable: {name}")
    return value


def redirect_uri() -> str:
    return os.environ.get("STRAVA_REDIRECT_URI", "http://localhost:8765/exchange_token")


def api_request(
    method: str,
    url: str,
    *,
    payload: Optional[dict] = None,
    access_token: Optional[str] = None,
) -> dict | list:
    data = None
    headers = {
        "Accept": "application/json",
    }
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"
    if access_token:
        headers["Authorization"] = f"Bearer {access_token}"

    request = urllib.request.Request(url, data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(request) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        raise SystemExit(f"HTTP {exc.code} calling {url}\n{body}") from exc


def build_auth_url() -> str:
    query = urllib.parse.urlencode(
        {
            "client_id": required_env("STRAVA_CLIENT_ID"),
            "redirect_uri": redirect_uri(),
            "response_type": "code",
            "approval_prompt": "auto",
            "scope": "read,activity:read_all",
        }
    )
    return f"{AUTH_URL}?{query}"


def exchange_code(code: str) -> dict:
    payload = {
        "client_id": required_env("STRAVA_CLIENT_ID"),
        "client_secret": required_env("STRAVA_CLIENT_SECRET"),
        "code": code,
        "grant_type": "authorization_code",
    }
    token = api_request("POST", TOKEN_URL, payload=payload)
    write_json(TOKEN_PATH, token)
    return token


def refresh_token_if_needed() -> dict:
    token = load_json(TOKEN_PATH, {})
    if not token:
        raise SystemExit("No token file found. Run `auth-url` and `exchange-code` first.")

    expires_at = int(token.get("expires_at", 0))
    if time.time() < (expires_at - 300):
        return token

    payload = {
        "client_id": required_env("STRAVA_CLIENT_ID"),
        "client_secret": required_env("STRAVA_CLIENT_SECRET"),
        "grant_type": "refresh_token",
        "refresh_token": token["refresh_token"],
    }
    refreshed = api_request("POST", TOKEN_URL, payload=payload)
    write_json(TOKEN_PATH, refreshed)
    return refreshed


def parse_api_datetime(value: Optional[str]) -> Optional[datetime]:
    if not value:
        return None
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def format_archive_datetime(value: Optional[str]) -> str:
    parsed = parse_api_datetime(value)
    if parsed is None:
        return ""
    return parsed.strftime("%m/%d/%Y %H:%M:%S")


def format_archive_time(value: Optional[str]) -> str:
    parsed = parse_api_datetime(value)
    if parsed is None:
        return ""
    return parsed.strftime("%H:%M:%S")


def meters_to_km(value: Optional[float]) -> str:
    if value is None:
        return ""
    return f"{value / 1000.0:.6f}"


def meters_per_second_to_kmh(value: Optional[float]) -> str:
    if value is None:
        return ""
    return f"{value * 3.6:.6f}"


def bool_to_archive(value: Optional[bool]) -> str:
    if value is None:
        return ""
    return "TRUE" if value else "FALSE"


def number_to_text(value) -> str:
    if value is None:
        return ""
    return str(value)


def epoch_from_iso(value: Optional[str]) -> Optional[int]:
    parsed = parse_api_datetime(value)
    if parsed is None:
        return None
    return int(parsed.timestamp())


def get_activities(access_token: str, after_epoch: Optional[int], before_epoch: Optional[int], page_size: int) -> List[dict]:
    activities: List[dict] = []
    page = 1
    while True:
        params = {
            "page": page,
            "per_page": page_size,
        }
        if after_epoch is not None:
            params["after"] = after_epoch
        if before_epoch is not None:
            params["before"] = before_epoch
        url = f"{API_BASE}/athlete/activities?{urllib.parse.urlencode(params)}"
        chunk = api_request("GET", url, access_token=access_token)
        if not chunk:
            break
        activities.extend(chunk)
        if len(chunk) < page_size:
            break
        page += 1
    return activities


def get_activity_detail(access_token: str, activity_id: int) -> dict:
    url = f"{API_BASE}/activities/{activity_id}?include_all_efforts=false"
    return api_request("GET", url, access_token=access_token)


def merge_by_id(existing: Iterable[dict], incoming: Iterable[dict]) -> List[dict]:
    merged: Dict[int, dict] = {}
    for item in existing:
        merged[int(item["id"])] = item
    for item in incoming:
        merged[int(item["id"])] = item
    return sorted(merged.values(), key=lambda row: row.get("start_date", ""))


def normalize_activity(summary: dict, detail: dict) -> dict:
    record = {header: "" for header in ARCHIVE_HEADERS}
    start_date_local = detail.get("start_date_local") or summary.get("start_date_local")

    record["Activity ID"] = number_to_text(detail.get("id") or summary.get("id"))
    record["Activity Date"] = format_archive_datetime(start_date_local)
    record["Activity Name"] = detail.get("name") or summary.get("name") or ""
    record["Activity Type"] = detail.get("type") or summary.get("type") or detail.get("sport_type") or summary.get("sport_type") or ""
    record["Activity Description"] = detail.get("description") or ""
    record["Elapsed Time"] = number_to_text(detail.get("elapsed_time") or summary.get("elapsed_time"))
    record["Distance"] = meters_to_km(detail.get("distance") or summary.get("distance"))
    record["Max Heart Rate"] = number_to_text(detail.get("max_heartrate"))
    record["Relative Effort"] = number_to_text(detail.get("suffer_score") or summary.get("suffer_score"))
    record["Commute"] = bool_to_archive(detail.get("commute"))
    record["Activity Gear"] = detail.get("gear_id") or summary.get("gear_id") or ""
    record["Moving Time"] = number_to_text(detail.get("moving_time") or summary.get("moving_time"))
    record["Max Speed"] = meters_per_second_to_kmh(detail.get("max_speed") or summary.get("max_speed"))
    record["Average Speed"] = meters_per_second_to_kmh(detail.get("average_speed") or summary.get("average_speed"))
    record["Elevation Gain"] = number_to_text(detail.get("total_elevation_gain") or summary.get("total_elevation_gain"))
    record["Elevation Low"] = number_to_text(detail.get("elev_low"))
    record["Elevation High"] = number_to_text(detail.get("elev_high"))
    record["Max Grade"] = number_to_text(detail.get("max_grade"))
    record["Average Cadence"] = number_to_text(detail.get("average_cadence") or summary.get("average_cadence"))
    record["Average Heart Rate"] = number_to_text(detail.get("average_heartrate") or summary.get("average_heartrate"))
    record["Max Watts"] = number_to_text(detail.get("max_watts"))
    record["Average Watts"] = number_to_text(detail.get("average_watts") or summary.get("average_watts"))
    record["Calories"] = number_to_text(detail.get("calories"))
    record["Type"] = detail.get("type") or summary.get("type") or ""
    record["Start Time"] = format_archive_time(start_date_local)
    record["Weighted Average Power"] = number_to_text(detail.get("weighted_average_watts"))
    record["Power Count"] = number_to_text(detail.get("kilojoules"))
    record["From Upload"] = bool_to_archive(detail.get("manual"))
    record["Gear"] = detail.get("gear_id") or summary.get("gear_id") or ""
    record["Flagged"] = bool_to_archive(detail.get("flagged"))
    record["Average Elapsed Speed"] = meters_per_second_to_kmh(
        safe_divide(detail.get("distance") or summary.get("distance"), detail.get("elapsed_time") or summary.get("elapsed_time"))
    )
    record["Average Grade Adjusted Pace"] = ""
    record["Timer Time"] = number_to_text(detail.get("moving_time") or summary.get("moving_time"))
    record["Training Load"] = ""
    record["Intensity"] = ""
    record["Recovery"] = ""
    record["Media"] = number_to_text((detail.get("photos") or {}).get("count"))
    return record


def safe_divide(numerator: Optional[float], denominator: Optional[float]) -> Optional[float]:
    if numerator is None or denominator in (None, 0):
        return None
    return numerator / denominator


def write_export_csv(records: List[dict]) -> None:
    EXPORT_ROOT.mkdir(parents=True, exist_ok=True)
    with EXPORT_CSV_PATH.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=ARCHIVE_HEADERS)
        writer.writeheader()
        writer.writerows(records)


def command_auth_url(_: argparse.Namespace) -> int:
    print(build_auth_url())
    return 0


def command_exchange_code(args: argparse.Namespace) -> int:
    token = exchange_code(args.code)
    print(f"Token saved to {TOKEN_PATH}")
    print(f"Access token expires at epoch {token['expires_at']}")
    return 0


def command_sync(args: argparse.Namespace) -> int:
    ensure_dirs()
    token = refresh_token_if_needed()
    manifest = load_json(MANIFEST_PATH, {})
    summary_cache = load_json(SUMMARY_CACHE_PATH, [])
    detail_cache = load_json(DETAIL_CACHE_PATH, {})

    after_epoch = None
    if args.after:
        after_epoch = int(datetime.fromisoformat(args.after).replace(tzinfo=timezone.utc).timestamp())
    elif not args.full_refresh:
        previous = manifest.get("last_sync_after_epoch")
        if previous is not None:
            after_epoch = max(0, int(previous) - DEFAULT_LOOKBACK_SECONDS)

    before_epoch = None
    if args.before:
        before_epoch = int(datetime.fromisoformat(args.before).replace(tzinfo=timezone.utc).timestamp())

    summaries = get_activities(token["access_token"], after_epoch, before_epoch, args.page_size)
    merged_summaries = merge_by_id(summary_cache, summaries)
    write_json(SUMMARY_CACHE_PATH, merged_summaries)

    details_to_fetch = merged_summaries if args.full_refresh else summaries
    if not args.skip_details:
        for summary in details_to_fetch:
            detail_cache[str(summary["id"])] = get_activity_detail(token["access_token"], int(summary["id"]))
    write_json(DETAIL_CACHE_PATH, detail_cache)

    records = []
    for summary in merged_summaries:
        detail = detail_cache.get(str(summary["id"]), summary)
        records.append(normalize_activity(summary, detail))
    write_export_csv(records)

    latest_epoch = None
    if merged_summaries:
        latest_epoch = max(epoch_from_iso(item.get("start_date")) or 0 for item in merged_summaries)
    manifest.update(
        {
            "last_sync_at_utc": datetime.now(timezone.utc).isoformat(),
            "last_sync_after_epoch": latest_epoch,
            "incremental_overlap_seconds": DEFAULT_LOOKBACK_SECONDS,
            "export_csv": str(EXPORT_CSV_PATH.relative_to(REPO_ROOT)),
            "activity_count": len(merged_summaries),
            "detailed_activity_count": len(detail_cache),
        }
    )
    write_json(MANIFEST_PATH, manifest)

    print(f"Synced {len(summaries)} activities this run.")
    print(f"Total cached activities: {len(merged_summaries)}")
    print(f"Export written to {EXPORT_CSV_PATH}")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Strava API sync framework for this repo.")
    subparsers = parser.add_subparsers(dest="command", required=True)

    auth_url_parser = subparsers.add_parser("auth-url", help="Print the Strava OAuth URL.")
    auth_url_parser.set_defaults(func=command_auth_url)

    exchange_parser = subparsers.add_parser("exchange-code", help="Exchange a one-time code for tokens.")
    exchange_parser.add_argument("--code", required=True, help="Authorization code returned by Strava.")
    exchange_parser.set_defaults(func=command_exchange_code)

    sync_parser = subparsers.add_parser("sync", help="Sync activities and write an archive-style CSV export.")
    sync_parser.add_argument("--after", help="ISO-8601 UTC timestamp to backfill after, e.g. 2024-01-01T00:00:00")
    sync_parser.add_argument("--before", help="ISO-8601 UTC timestamp to stop before, e.g. 2026-01-01T00:00:00")
    sync_parser.add_argument("--page-size", type=int, default=100, help="Strava page size, max 200.")
    sync_parser.add_argument("--full-refresh", action="store_true", help="Refetch detailed activity JSON for the full cache.")
    sync_parser.add_argument("--skip-details", action="store_true", help="Skip per-activity detail requests and export from summaries only.")
    sync_parser.set_defaults(func=command_sync)

    return parser


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
