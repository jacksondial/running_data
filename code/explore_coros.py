"""explore_coros.py

Small CLI to parse FIT files in a folder, produce per-file CSVs, simple summaries,
and quick plots (heart rate / speed vs time).

Usage examples:
  python code/explore_coros.py --dir data/coros/13_Jan_2025_extract --out parsed --plot --save-csv

Dependencies: fitparse, pandas, numpy, matplotlib, seaborn
"""

from __future__ import annotations
import argparse
import os
from pathlib import Path
import logging
from typing import List, Dict, Any

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

try:
    from fitparse import FitFile
except Exception as e:
    FitFile = None
    logging.warning("fitparse not available: %s", e)


logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")


def semicircles_to_degrees(value: Any) -> Any:
    try:
        # FIT uses semicircles where 2**31 corresponds to 180 degrees
        return float(value) * (180.0 / (2 ** 31))
    except Exception:
        return value


def parse_fit_to_df(path: Path) -> pd.DataFrame:
    if FitFile is None:
        raise RuntimeError("fitparse is required. Install with `pip install fitparse`.")

    fit = FitFile(str(path))
    rows: List[Dict[str, Any]] = []

    for record in fit.get_messages('record'):
        row: Dict[str, Any] = {}
        for field in record:
            name = field.name
            value = field.value
            # convert some known fields
            if name in ('position_lat', 'position_long') and value is not None:
                row[name] = semicircles_to_degrees(value)
            else:
                row[name] = value
        rows.append(row)

    df = pd.DataFrame(rows)

    # Normalize timestamp if present
    if 'timestamp' in df.columns:
        df['timestamp'] = pd.to_datetime(df['timestamp'])
        df = df.sort_values('timestamp').reset_index(drop=True)

    return df


def summarize_df(df: pd.DataFrame) -> Dict[str, Any]:
    summary: Dict[str, Any] = {}
    if 'timestamp' in df.columns:
        start = df['timestamp'].iloc[0]
        end = df['timestamp'].iloc[-1]
        duration = (end - start).total_seconds()
        summary['start'] = start
        summary['end'] = end
        summary['duration_s'] = duration
    if 'distance' in df.columns:
        try:
            summary['distance_m'] = float(df['distance'].max())
        except Exception:
            pass
    if 'heart_rate' in df.columns:
        summary['hr_mean'] = float(df['heart_rate'].mean())
        summary['hr_max'] = float(df['heart_rate'].max())
    if 'speed' in df.columns:
        summary['speed_mean'] = float(df['speed'].mean())
        summary['speed_max'] = float(df['speed'].max())
    return summary


def quick_plot(df: pd.DataFrame, out_path: Path | None = None, show: bool = True) -> None:
    sns.set(style='whitegrid')
    fig, ax = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

    if 'timestamp' in df.columns and 'heart_rate' in df.columns:
        sns.lineplot(x='timestamp', y='heart_rate', data=df, ax=ax[0])
        ax[0].set_ylabel('Heart Rate (bpm)')

    if 'timestamp' in df.columns and 'speed' in df.columns:
        sns.lineplot(x='timestamp', y='speed', data=df, ax=ax[1])
        ax[1].set_ylabel('Speed (m/s)')

    plt.tight_layout()
    if out_path is not None:
        fig.savefig(str(out_path))
        logging.info('Saved plot to %s', out_path)
    if show:
        plt.show()
    plt.close(fig)


def process_folder(directory: Path, out_dir: Path, save_csv: bool = True, plot: bool = False, show: bool = False) -> None:
    directory = Path(directory)
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    fit_files = sorted(directory.glob('*.fit'))
    if not fit_files:
        logging.warning('No .fit files found in %s', directory)
        return

    summaries = []
    for f in fit_files:
        logging.info('Parsing %s', f.name)
        try:
            df = parse_fit_to_df(f)
        except Exception as e:
            logging.error('Failed to parse %s: %s', f.name, e)
            continue

        summary = summarize_df(df)
        summary['file'] = f.name
        summaries.append(summary)

        if save_csv:
            csv_path = out_dir / (f.stem + '.csv')
            df.to_csv(csv_path, index=False)
            logging.info('Wrote %s', csv_path)

        if plot:
            plot_path = out_dir / (f.stem + '_plot.png')
            quick_plot(df, out_path=plot_path, show=show)

    # Save summary
    if summaries:
        summary_df = pd.DataFrame(summaries)
        summary_csv = out_dir / 'summary.csv'
        summary_df.to_csv(summary_csv, index=False)
        logging.info('Wrote summary to %s', summary_csv)


def main():
    parser = argparse.ArgumentParser(description='Explore .fit files in a folder')
    parser.add_argument('--dir', '-d', type=str, default='data/coros/13_Jan_2025_extract', help='Directory with .fit files')
    parser.add_argument('--out', '-o', type=str, default=None, help='Output directory for CSVs and plots (default: <dir>/parsed)')
    parser.add_argument('--no-save-csv', dest='save_csv', action='store_false', help='Do not save per-file CSVs')
    parser.add_argument('--plot', action='store_true', help='Create quick plots (saved as PNG)')
    parser.add_argument('--show', action='store_true', help='Show plots interactively')
    args = parser.parse_args()

    dir_path = Path(args.dir)
    if args.out is None:
        out_path = dir_path / 'parsed'
    else:
        out_path = Path(args.out)

    process_folder(dir_path, out_path, save_csv=args.save_csv, plot=args.plot, show=args.show)


if __name__ == '__main__':
    main()
