#!/usr/bin/env python3
"""Write the column tables of the data dictionary from the pipeline's schema.

The OPDI pipeline declares every published column once, in the file
src/opdi/output/schema.py of the opdi repository: its name, type, unit and
description. This script reads that declaration and writes:

  content/data/_generated/flight_list_columns.md    the flight list
  content/data/_generated/flight_events_columns.md  the event columns of the flight events file
  content/data/_generated/measurement_columns.md    the measurement columns of that file
  content/data/_generated/flight_index_columns.md   the monthly flight index (when declared)
  content/data/_generated/catalogue_columns.md      the dataset catalogue (when declared)
  assets/data/dictionary.csv                         every column of every table, one row each

The Markdown files are tables with the headings Column, Type, Unit and
Description. Pages include them with {{< include _generated/<file>.md >}}.
Do not edit them by hand: run this script again after the schema changes.

Usage:

  python tools/gen_data_dictionary.py --opdi ../opdi/src          # write the files
  python tools/gen_data_dictionary.py --opdi ../opdi/src --check  # exit 1 if they are out of date

The schema file is loaded by its path, so the opdi package and Spark do not
need to be installed. Plain Python 3 with no extra packages.
"""
from __future__ import annotations

import argparse
import csv
import importlib.util
import io
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MD_DIR = Path("content/data/_generated")
CSV_PATH = Path("assets/data/dictionary.csv")

NOTE = ("<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py "
        "in the opdi repository. Do not edit by hand. -->\n\n")


def load_schema(opdi_src: Path):
    """Load opdi/output/schema.py from the pipeline's src folder by file path."""
    path = Path(opdi_src) / "opdi" / "output" / "schema.py"
    if not path.is_file():
        raise SystemExit(f"{path} does not exist. Give the src folder of the opdi repository, "
                         "for example --opdi ../opdi/src")
    spec = importlib.util.spec_from_file_location("opdi_published_schema", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    try:
        spec.loader.exec_module(module)
    except ImportError as error:
        raise SystemExit(f"{path} could not be loaded on its own: it imports another module "
                         f"({error}). The schema must load without the rest of the pipeline.")
    return module


def markdown_table(columns) -> str:
    lines = ["| Column | Type | Unit | Description |", "|---|---|---|---|"]
    for c in columns:
        description = " ".join(c.description.split()).replace("|", "\\|")
        unit = c.unit.replace("|", "\\|")
        lines.append(f"| `{c.name}` | {c.dtype} | {unit} | {description} |")
    return NOTE + "\n".join(lines) + "\n"


def tables(schema) -> dict[str, tuple]:
    """The published tables, as name -> columns."""
    measurement_names = {c.name for c in schema.MEASUREMENT_COLUMNS}
    # The flight events file repeats the flight list columns after the event
    # and its measurements. Its own columns are the event columns (some of
    # which, such as flight_id and version, share a name with a flight list
    # column but are listed here all the same) and the columns that follow the
    # repeated ones: flight_version and release.
    event_columns = tuple(schema.EVENT_COLUMNS)
    event_file_own = event_columns + tuple(
        c for c in schema.FLIGHT_EVENTS
        if c not in event_columns
        and c.name not in measurement_names
        and (c not in schema.FLIGHT_LIST or c.name == "release")
    )
    out = {
        "flight_list": tuple(schema.FLIGHT_LIST),
        "flight_events": event_file_own,
        "measurement": tuple(schema.MEASUREMENT_COLUMNS),
    }
    for name, attribute in (("flight_index", "FLIGHT_INDEX"), ("catalogue", "CATALOGUE")):
        if hasattr(schema, attribute):
            out[name] = tuple(getattr(schema, attribute))
    return out


def dictionary_csv(schema) -> str:
    """Every column of every published file, with the file it belongs to."""
    out = io.StringIO()
    writer = csv.writer(out, lineterminator="\n")
    writer.writerow(["table", "name", "type", "unit", "group", "description"])
    files = {"flight_list": schema.FLIGHT_LIST, "flight_events": schema.FLIGHT_EVENTS}
    for attribute, name in (("FLIGHT_INDEX", "flight_index"), ("CATALOGUE", "catalogue")):
        if hasattr(schema, attribute):
            files[name] = getattr(schema, attribute)
    for table, columns in files.items():
        for c in columns:
            writer.writerow([table, c.name, c.dtype, c.unit, c.group, " ".join(c.description.split())])
    return out.getvalue()


def outputs(schema) -> dict[Path, str]:
    files = {MD_DIR / f"{name}_columns.md": markdown_table(cols) for name, cols in tables(schema).items()}
    files[CSV_PATH] = dictionary_csv(schema)
    return files


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Write the data dictionary tables from the pipeline schema.")
    parser.add_argument("--opdi", type=Path, required=True, help="src folder of the opdi repository")
    parser.add_argument("--root", type=Path, default=ROOT, help="site folder (default: this repository)")
    parser.add_argument("--check", action="store_true",
                        help="write nothing; exit 1 when a file differs from a fresh run")
    args = parser.parse_args(argv)

    schema = load_schema(args.opdi)
    stale = []
    for path, text in outputs(schema).items():
        target = args.root / path
        current = target.read_text(encoding="utf-8") if target.is_file() else None
        if args.check:
            if current != text:
                stale.append(str(path))
            continue
        target.parent.mkdir(parents=True, exist_ok=True)
        if current != text:
            target.write_text(text, encoding="utf-8")
            print(f"wrote {path}")
        else:
            print(f"unchanged {path}")
    if args.check:
        for path in stale:
            print(f"out of date: {path}")
        if stale:
            print("Run: python tools/gen_data_dictionary.py --opdi <opdi src folder>")
            return 1
        print("OK: the data dictionary matches the schema")
    return 0


if __name__ == "__main__":
    sys.exit(main())
