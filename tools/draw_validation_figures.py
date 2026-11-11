#!/usr/bin/env python3
"""Draw the validation figures of the methods pages and record their numbers.

Each figure on a methods page is drawn by one function in this file. The
function reads committed study results (CSV files under papers/<study>/data/),
draws one chart, and returns the numbers the page quotes about it. Running the
script:

  1. writes each chart to assets/validation/<page>_<figure>.svg, and
  2. adds or replaces that figure's rows in assets/validation/validation_measures.csv.

A row is identified by its page and measure_id: running the script again
replaces the row instead of adding a copy. Rows of other figures are kept.

Every chart uses the same style: 7 x 3.5 inches, a light grey grid, one
accent colour, axis labels with units, and the number of cases (n) in the title.

Adding a figure (for the writer of a methods page):

  @figure("holding", "episode_duration")
  def holding_episode_duration(root: Path) -> list[dict]:
      data = read_csv(root, "papers/<study>/data/<file>.csv")
      fig, ax = new_figure(f"Title (n = {len(data):,})")
      ...draw on ax...
      return [measure(page="holding", measure_id="...", label="...", value=..., ...)]

The first argument of @figure is the methods page file name without .qmd; the
second names the figure. The page shows the chart with
![Caption](../../assets/validation/<page>_<figure>.svg).

Usage:

  python tools/draw_validation_figures.py               # draw every figure
  python tools/draw_validation_figures.py --list        # list the registered figures
  python tools/draw_validation_figures.py --only holding_episode_duration

Needs pandas and matplotlib.
"""
from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path
from typing import Callable

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))

from check_site import MEASURE_COLUMNS, MEASURES_CSV  # noqa: E402

OUT_DIR = Path("assets/validation")

# One style for every chart.
ACCENT = "#003399"      # EUROCONTROL blue, as used on the home page
MUTED = "#8c8c8c"       # reference lines and secondary marks
GRID = "#e0e0e0"
FIGSIZE = (7.0, 3.5)    # inches

#: Registered figures: "<page>_<figure>" -> (page, figure, function).
FIGURES: dict[str, tuple[str, str, Callable[[Path], list[dict]]]] = {}


def figure(page: str, name: str):
    """Register a figure function for a methods page."""
    def register(function):
        key = f"{page}_{name}"
        if key in FIGURES:
            raise ValueError(f"figure {key} is registered twice")
        FIGURES[key] = (page, name, function)
        return function
    return register


def measure(page: str, measure_id: str, label: str, value, unit: str = "", n="",
            reference: str = "", source_file: str = "", source_field: str = "",
            headline: bool = False, item: str = "", kind: str = "") -> dict:
    """One row of validation_measures.csv. See check_site.MEASURE_COLUMNS for the fields."""
    return {
        "page": page, "measure_id": measure_id, "label": label, "value": str(value),
        "unit": unit, "n": str(n), "reference": reference, "source_file": source_file,
        "source_field": source_field, "headline": "1" if headline else "0",
        "item": item, "kind": kind,
    }


def read_csv(root: Path, relative: str):
    import pandas as pd
    path = root / relative
    if not path.is_file():
        raise FileNotFoundError(f"{relative} does not exist: the figure needs this committed study file")
    return pd.read_csv(path)


def new_figure(title: str):
    """A figure and axes in the shared style. Put the number of cases in the title."""
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(figsize=FIGSIZE)
    ax.set_title(title, loc="left", fontsize=11)
    ax.grid(True, color=GRID, linewidth=0.8)
    ax.set_axisbelow(True)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(MUTED)
    ax.tick_params(colors="#333333", labelsize=9)
    return fig, ax


def save(fig, path: Path) -> None:
    import matplotlib.pyplot as plt
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    # No date in the file, so an unchanged chart gives an unchanged file.
    fig.savefig(path, format="svg", metadata={"Date": None})
    plt.close(fig)


def upsert_measures(rows: list[dict], path: Path) -> None:
    """Add rows to the measures file, replacing rows with the same page and measure_id."""
    existing = []
    if path.is_file():
        with path.open(encoding="utf-8", newline="") as handle:
            existing = list(csv.DictReader(handle))
    new = {(r["page"], r["measure_id"]): r for r in rows}
    merged = []
    for row in existing:
        key = (row.get("page"), row.get("measure_id"))
        merged.append(new.pop(key) if key in new else row)
    merged.extend(r for r in rows if (r["page"], r["measure_id"]) in new)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(MEASURE_COLUMNS), lineterminator="\n",
                                extrasaction="ignore")
        writer.writeheader()
        for row in merged:
            writer.writerow({c: row.get(c, "") for c in MEASURE_COLUMNS})


# ---------------------------------------------------------------------------
# Figures. Add new ones below, one function per figure.
# ---------------------------------------------------------------------------

HOLDING_DIR = "papers/airspace-events-v1/data/holding"
HOLDING_REFERENCE = ("Hand-corrected holding labels of Olive and others, "
                     "London Heathrow (EGLL) and Dublin (EIDW), 1 October 2019")


@figure("holding", "episode_duration")
def holding_episode_duration(root: Path) -> list[dict]:
    """Length of the holding episodes found on the validation day, and the scores."""
    episodes = read_csv(root, f"{HOLDING_DIR}/holding_episodes_2019-10-01.csv")
    score = read_csv(root, f"{HOLDING_DIR}/holding_score_2019-10-01.csv")
    both = score[score["airport"] == "ALL"].iloc[0]

    minutes = episodes["duration_s"] / 60.0
    median = float(minutes.median())
    n = len(minutes)

    fig, ax = new_figure(f"Holding episodes found at EGLL and EIDW, 1 October 2019 (n = {n:,})")
    ax.hist(minutes, bins=range(0, int(minutes.max()) + 2), color=ACCENT, edgecolor="white", linewidth=0.5)
    ax.axvline(median, color=MUTED, linestyle="--", linewidth=1)
    ax.annotate(f"median {median:.1f} min", xy=(median, ax.get_ylim()[1] * 0.9),
                xytext=(6, 0), textcoords="offset points", fontsize=9, color="#333333")
    ax.set_xlabel("Episode length (minutes)")
    ax.set_ylabel("Episodes (count)")
    save(fig, root / OUT_DIR / "holding_episode_duration.svg")

    score_file = f"{HOLDING_DIR}/holding_score_2019-10-01.csv"
    common = dict(page="holding", n=int(both["n_flights"]), reference=HOLDING_REFERENCE,
                  source_file=score_file, item="holding-start;holding-end", kind="event")
    return [
        measure(measure_id="precision", value=f"{both['precision']:.2f}",
                label="Precision: share of the time detected as holding that the labels also mark as holding",
                source_field="precision, row airport = ALL", **common),
        measure(measure_id="recall", value=f"{both['recall']:.2f}",
                label="Recall: share of the time labelled as holding that the rule detects",
                source_field="recall, row airport = ALL", **common),
        measure(measure_id="f1", value=f"{both['f1']:.2f}", headline=True,
                label="F1 score: the balance of precision and recall (1 is perfect)",
                source_field="f1, row airport = ALL", **common),
        measure(page="holding", measure_id="episode_median_min", value=f"{median:.1f}", unit="min",
                n=n, label="Median length of a detected holding episode",
                reference=HOLDING_REFERENCE,
                source_file=f"{HOLDING_DIR}/holding_episodes_2019-10-01.csv",
                source_field="median of duration_s / 60", item="holding-start;holding-end", kind="event"),
    ]


# ---------------------------------------------------------------------------
# Command line
# ---------------------------------------------------------------------------

def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Draw the validation figures and record their numbers.")
    parser.add_argument("--root", type=Path, default=ROOT, help="site folder (default: this repository)")
    parser.add_argument("--only", action="append", help="draw only this figure (<page>_<figure>); repeatable")
    parser.add_argument("--list", action="store_true", help="list the registered figures and stop")
    args = parser.parse_args(argv)

    if args.list:
        for key, (page, _, function) in FIGURES.items():
            doc = (function.__doc__ or "").strip().split("\n")[0]
            print(f"{key}\t{page}\t{doc}")
        return 0

    unknown = [k for k in (args.only or []) if k not in FIGURES]
    if unknown:
        print(f"unknown figure(s): {', '.join(unknown)}; see --list")
        return 1

    import matplotlib
    matplotlib.use("Agg")
    matplotlib.rcParams["svg.hashsalt"] = "opdi"   # stable element ids in the SVG
    matplotlib.rcParams["svg.fonttype"] = "none"   # text stays text: smaller, searchable files

    rows = []
    for key, (_, _, function) in FIGURES.items():
        if args.only and key not in args.only:
            continue
        found = function(args.root)
        rows.extend(found)
        print(f"drew {OUT_DIR / (key + '.svg')} and {len(found)} measure(s)")
    upsert_measures(rows, args.root / MEASURES_CSV)
    print(f"updated {MEASURES_CSV}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
