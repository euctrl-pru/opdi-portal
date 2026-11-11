#!/usr/bin/env python3
"""Write the table of the Validation summary page from validation_measures.csv.

The table has one line per published item: every event type the pipeline
publishes (the list in check_site.EVENT_TYPES) and, with --opdi, every column
of the flight list and every measurement column. For each item it shows the
headline accuracy, the reference data it was measured against, the number of
cases (n) and links to the Validation method and Limitations sections of the
methods page. An item with no headline row in the measures file is shown as
"Not validated".

A row of assets/validation/validation_measures.csv is a headline row when its
`headline` column is 1. Its `item` column names the items it applies to,
separated by semicolons, written exactly as `check_site.py --list-types`
prints them (for example holding-start;holding-end).

Output: content/methods/_generated/validation_summary.md, which the page
content/methods/validation-summary.qmd includes with
{{< include _generated/validation_summary.md >}}. Do not edit it by hand.

Usage:

  python tools/gen_validation_summary.py
  python tools/gen_validation_summary.py --opdi ../opdi/src   # also list every column

Plain Python 3 with no extra packages.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import check_site  # noqa: E402

ROOT = check_site.ROOT
OUT = Path("content/methods/_generated/validation_summary.md")

NOTE = ("<!-- Written by tools/gen_validation_summary.py from "
        "assets/validation/validation_measures.csv. Do not edit by hand. -->\n\n")


def read_rows(path: Path) -> list[dict]:
    import csv
    if not path.is_file():
        return []
    with path.open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def items(opdi_src: Path | None) -> list[tuple[str, str]]:
    """Every published item as (name, kind)."""
    out = [(name, "event") for name in check_site.EVENT_TYPES]
    if opdi_src is not None:
        schema = check_site.load_schema(opdi_src)
        out += [(c.name, "flight list column") for c in schema.FLIGHT_LIST]
        out += [(c.name, "measurement") for c in schema.MEASUREMENT_COLUMNS]
    return out


def cell(text: str) -> str:
    return " ".join(str(text).split()).replace("|", "\\|")


def summary(rows: list[dict], all_items: list[tuple[str, str]]) -> str:
    headline = {}
    for row in rows:
        if str(row.get("headline", "")).strip() != "1":
            continue
        for name in (i.strip() for i in row.get("item", "").split(";")):
            if name:
                headline.setdefault(name, row)
    known = {name for name, _ in all_items}
    # Items that have a headline row but are not in the list are shown too.
    extra = [(name, row.get("kind", "")) for name, row in headline.items() if name not in known]

    lines = ["| Item | Kind | Headline accuracy | Reference | n | Details |",
             "|---|---|---|---|---:|---|"]
    for name, kind in all_items + extra:
        row = headline.get(name)
        if row is None:
            lines.append(f"| `{name}` | {kind} | Not validated |  |  |  |")
            continue
        unit = f" {row['unit']}" if row.get("unit") else ""
        accuracy = f"{cell(row['label'])}: {row['value']}{unit}"
        page = f"{row['page']}.qmd"
        details = f"[Validation]({page}#validation-method), [Limitations]({page}#limitations)"
        lines.append(f"| `{name}` | {kind or cell(row.get('kind', ''))} | {accuracy} | "
                     f"{cell(row.get('reference', ''))} | {row.get('n', '')} | {details} |")
    return NOTE + "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Write the Validation summary table.")
    parser.add_argument("--measures", type=Path, default=ROOT / check_site.MEASURES_CSV,
                        help="measures file (default: assets/validation/validation_measures.csv)")
    parser.add_argument("--out", type=Path, default=ROOT / OUT, help=f"output file (default: {OUT})")
    parser.add_argument("--opdi", type=Path, default=None,
                        help="src folder of the opdi repository; adds every flight list and measurement column")
    args = parser.parse_args(argv)

    text = summary(read_rows(args.measures), items(args.opdi))
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(text, encoding="utf-8")
    print(f"wrote {args.out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
