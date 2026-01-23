"""Minimal script: load one .fit file and write a CSV.

Usage examples:
  python code/explore_coros.py --file data/coros/13_Jan_2025_extract/457402814182752257.fit
  python code/explore_coros.py --dir data/coros/13_Jan_2025_extract

If `--file` is not provided, the first `*.fit` in `--dir` will be used.
"""

from pathlib import Path
import argparse
import logging
import sys
from typing import Any

import pandas as pd

try:
    from fitparse import FitFile
except Exception:
    FitFile = None


logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")


def semicircles_to_degrees(value: Any) -> Any:
    try:
        return float(value) * (180.0 / (2 ** 31))
    except Exception:
        return value


def parse_fit_to_df(path: Path) -> pd.DataFrame:
    if FitFile is None:
        logging.error("fitparse is not installed. Run: python -m pip install fitparse")
        sys.exit(1)

    fit = FitFile(str(path))
    rows = []
    for record in fit.get_messages('record'):
        row = {}
        for field in record:
            name = field.name
            val = field.value
            if name in ("position_lat", "position_long") and val is not None:
                row[name] = semicircles_to_degrees(val)
            else:
                row[name] = val
        rows.append(row)

    df = pd.DataFrame(rows)
    if 'timestamp' in df.columns:
        df['timestamp'] = pd.to_datetime(df['timestamp'])
        df = df.sort_values('timestamp').reset_index(drop=True)
    return df


def main():
    parser = argparse.ArgumentParser(description='Load one .fit file and save CSV')
    parser.add_argument('--file', '-f', type=str, help='Path to .fit file')
    parser.add_argument('--dir', '-d', type=str, default='data/coros/13_Jan_2025_extract', help='Directory to search for .fit files if --file not given')
    parser.add_argument('--out', '-o', type=str, help='Output CSV path (default: same folder, same name .csv)')
    args = parser.parse_args()

    file_path = None
    if args.file:
        file_path = Path(args.file)
    else:
        search_dir = Path(args.dir)
        fits = sorted(search_dir.glob('*.fit'))
        if not fits:
            logging.error('No .fit files found in %s', search_dir)
            sys.exit(1)
        file_path = fits[0]

    if not file_path.exists():
        logging.error('File not found: %s', file_path)
        sys.exit(1)

    logging.info('Parsing %s', file_path)
    df = parse_fit_to_df(file_path)

    if args.out:
        out_path = Path(args.out)
    else:
        out_path = file_path.with_suffix('.csv')

    df.to_csv(out_path, index=False)
    logging.info('Wrote CSV to %s', out_path)

    # Print a tiny preview
    print('\nPreview:')
    print(df.head().to_string(index=False))


if __name__ == '__main__':
    main()
