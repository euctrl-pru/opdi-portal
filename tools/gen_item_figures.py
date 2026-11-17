#!/usr/bin/env python3
"""Write the availability and accuracy figures of the Algorithms pages.

Every number shown under "Availability and accuracy" on a page in
content/algorithms/ comes from this script. It reads committed study files,
records each number with its source in assets/algorithms/item_figures.csv,
and writes one small Markdown table per page,
content/algorithms/_generated/figures_<page>.md, which the page includes with

    {{< include _generated/figures_<page>.md >}}

tools/check_site.py checks that every number written in that section of a
page is a row of item_figures.csv for the same page.

The figures are listed in FIGURES below, one entry per number: the page, the
study file, the row (section, item, measure) and how to show it. To replace
the figures of one day by those of another study (for example the three-day
accuracy tables under assets/accuracy/), change the entries here and run the
script again; the pages do not need to be edited.

Two kinds of figure are kept apart and never merged into one number:

  availability  how often the item exists: among OPDI's own flights, or among
                the movements that airport or Network Manager records hold;
  accuracy      how close the item is to the records when it exists.

Each share gets a label beside it (never instead of it):
  availability  high (at least 80 %), partial (50 to 80 %), low (under 50 %);
  accuracy      good (at least 80 % within 60 s), fair (50 to 80 %),
                limited (under 50 %); "no reference" under 30 movements.

Usage:

  python tools/gen_item_figures.py           # write the files
  python tools/gen_item_figures.py --check   # exit 1 when they are out of date

Plain Python 3, no extra packages.
"""
from __future__ import annotations

import argparse
import csv
import io
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT_CSV = Path("assets/algorithms/item_figures.csv")
OUT_DIR = Path("content/algorithms/_generated")

#: The three-day accuracy tables (10, 14 and 17 June 2026, worldwide, current
#: settings); see assets/accuracy/README.md. Rows with days = "all three days".
SOURCE = "assets/accuracy/item_accuracy.csv"
POOLED = "all three days"
DAYS_TEXT = "10, 14 and 17 June 2026"

AIRPORT = "airport operator records (Europe)"
NM = "Network Manager flight records (Europe)"
OWN = "OPDI flight list"
OWN_EVENTS = "OPDI flight events"
REF_KEY = {AIRPORT: "airport records", NM: "Network Manager"}

CSV_COLUMNS = ("page", "measure_id", "label", "value", "unit", "n", "kind",
               "badge", "reference", "source_file", "source_field")

#: Which count each column is computed over, and which label goes with it.
COUNT_OF = {
    "availability_own_pct": "flights_eligible",
    "availability_ref_pct": "reference_movements",
    "within_60s_pct": "compared",
    "within_10pct_pct": "compared",
    "share_equal_pct": "compared",
    "median_abs_diff_s": "compared",
    "p90_abs_diff_s": "compared",
    "median_ratio": "compared",
    "values_per_day": "",
}
LABEL_OF = {
    "availability_own_pct": "availability_own_label",
    "availability_ref_pct": "availability_ref_label",
    "within_60s_pct": "accuracy_label",
    "within_10pct_pct": "accuracy_label",
    "share_equal_pct": "accuracy_label",
}


@dataclass(frozen=True)
class Fig:
    """One number of one page: a column of one row of SOURCE."""
    page: str
    measure_id: str
    label: str          # what the number is, in words
    item: str           # item of SOURCE
    column: str         # column of SOURCE
    kind: str           # availability, accuracy, timing or count
    reference: str      # AIRPORT, NM, OWN or OWN_EVENTS
    unit: str = "%"     # %, s, per day, or "" for a ratio
    labelled: bool = True   # show the label of the staged table beside it


def own(page, mid, label, item, events=False):
    """Share of OPDI's flights that could carry the item that do."""
    return Fig(page, mid, label, item, "availability_own_pct", "availability", OWN_EVENTS if events else OWN)


def found(page, mid, label, item, ref=AIRPORT):
    """Share of the reference movements for which OPDI has the item."""
    return Fig(page, mid, label, item, "availability_ref_pct", "availability", ref)


def w60(page, mid, label, item, ref=AIRPORT):
    return Fig(page, mid, label, item, "within_60s_pct", "accuracy", ref)


def equal(page, mid, label, item, ref=AIRPORT):
    return Fig(page, mid, label, item, "share_equal_pct", "accuracy", ref)


def median(page, mid, label, item, ref=AIRPORT, kind="accuracy"):
    return Fig(page, mid, label, item, "median_abs_diff_s", kind, ref, "s", False)


def p90(page, mid, label, item, ref=AIRPORT):
    return Fig(page, mid, label, item, "p90_abs_diff_s", "accuracy", ref, "s", False)


def per_day(page, mid, label, item, list_=False):
    return Fig(page, mid, label, item, "values_per_day", "count", OWN if list_ else OWN_EVENTS, "per day", False)


FIGURES: tuple[Fig, ...] = (
    # --- flight list ----------------------------------------------------
    per_day("tracks", "rows", "Flights in the flight list, per day", "flight list: FIRST_SEEN", True),
    own("airports", "adep_filled", "Flights with a departure airport (adep)", "flight list: ADEP"),
    own("airports", "ades_filled", "Flights with a destination airport (ades)", "flight list: ADES"),
    found("airports", "adep_found", "Recorded flights with a departure airport", "flight list: ADEP", NM),
    equal("airports", "adep_right", "Departure airport equal to the record", "flight list: ADEP", NM),
    found("airports", "ades_found", "Recorded flights with a destination airport", "flight list: ADES", NM),
    equal("airports", "ades_right", "Destination airport equal to the record", "flight list: ADES", NM),
    own("aircraft", "typecode_filled", "Flights with an aircraft type (typecode)", "flight list: TYPECODE"),
    found("aircraft", "type_found", "Recorded flights with an aircraft type", "flight list: TYPECODE", NM),
    equal("aircraft", "type_agrees", "Aircraft type equal to the record", "flight list: TYPECODE", NM),
    per_day("first-last-seen", "events", "first_seen events (one per track), per day", "first_seen"),
    own("flight-list-milestones", "atot_filled", "Departures with atot", "flight list: ATOT"),
    own("flight-list-milestones", "aldt_filled", "Arrivals with aldt", "flight list: ALDT"),
    own("flight-list-milestones", "aobt_filled", "Departures with aobt", "flight list: AOBT"),
    own("flight-list-milestones", "aibt_filled", "Arrivals with aibt", "flight list: AIBT"),
    own("flight-list-milestones", "stnd_dep_filled", "Departures with stnd_dep", "flight list: STND_DEP"),
    own("flight-list-milestones", "c40_arr_filled", "Arrivals with c40_arr", "flight list: C40_ARR"),
    w60("flight-list-milestones", "atot_w60", "atot within 60 s of the record", "flight list: ATOT"),
    w60("flight-list-milestones", "aobt_w60", "aobt within 60 s of the record", "flight list: AOBT"),
    equal("flight-list-milestones", "rwy_dep_exact", "rwy_dep equal to the recorded runway", "flight list: RWY_DEP"),
    equal("flight-list-milestones", "rwy_arr_exact", "rwy_arr equal to the recorded runway", "flight list: RWY_ARR"),
    equal("flight-list-milestones", "stnd_dep_equal", "stnd_dep equal to the recorded stand",
          "flight list: STND_DEP"),
    equal("flight-list-milestones", "stnd_arr_equal", "stnd_arr equal to the recorded stand",
          "flight list: STND_ARR"),
    # --- take-off and landing -----------------------------------------
    own("take-off-landing", "atot_own", "Departures with an ATOT", "ATOT", True),
    found("take-off-landing", "atot_found", "Recorded take-offs with an ATOT", "ATOT"),
    w60("take-off-landing", "atot_w60", "ATOT within 60 s of the record", "ATOT"),
    median("take-off-landing", "atot_median", "ATOT: median difference from the record", "ATOT"),
    p90("take-off-landing", "atot_p90", "ATOT: 90th percentile of the difference", "ATOT"),
    own("take-off-landing", "aldt_own", "Arrivals with an ALDT", "ALDT", True),
    found("take-off-landing", "aldt_found", "Recorded landings with an ALDT", "ALDT"),
    w60("take-off-landing", "aldt_w60", "ALDT within 60 s of the record", "ALDT"),
    median("take-off-landing", "aldt_median", "ALDT: median difference from the record", "ALDT"),
    p90("take-off-landing", "aldt_p90", "ALDT: 90th percentile of the difference", "ALDT"),
    w60("take-off-landing", "atot_acdm_w60", "ATOT from the runway pass (method acdm) within 60 s", "ATOT (acdm)"),
    w60("take-off-landing", "atot_legacy_w60", "ATOT from the runway window (method legacy) within 60 s",
        "ATOT (legacy)"),
    w60("take-off-landing", "aldt_acdm_w60", "ALDT from the runway pass (method acdm) within 60 s", "ALDT (acdm)"),
    w60("take-off-landing", "aldt_legacy_w60", "ALDT from the runway window (method legacy) within 60 s",
        "ALDT (legacy)"),
    w60("take-off-landing", "atot_nm_w60", "ATOT within 60 s of Network Manager, at airports without records",
        "ATOT", NM),
    w60("take-off-landing", "aldt_nm_w60", "ALDT within 60 s of Network Manager, at airports without records",
        "ALDT", NM),
    equal("take-off-landing", "atot_runway", "Take-off runway equal to the record", "ATOT runway (info.runway)"),
    equal("take-off-landing", "aldt_runway", "Landing runway equal to the record", "ALDT runway (info.runway)"),
    # --- other runway events -------------------------------------------
    own("line-up-take-off-roll", "lineup_own", "Departures with a line-up", "line-up", True),
    own("line-up-take-off-roll", "roll_own", "Departures with a take-off-roll", "take-off-roll", True),
    found("line-up-take-off-roll", "found", "Recorded take-offs with a take-off-roll", "take-off-roll"),
    median("line-up-take-off-roll", "lineup_median", "line-up: median time to the recorded take-off",
           "line-up", kind="timing"),
    median("line-up-take-off-roll", "roll_median", "take-off-roll: median time to the recorded take-off",
           "take-off-roll", kind="timing"),
    own("xing-runway-threshold", "own", "Arrivals with an xing-runway-threshold", "xing-runway-threshold", True),
    found("xing-runway-threshold", "found", "Recorded landings with an xing-runway-threshold",
          "xing-runway-threshold"),
    w60("xing-runway-threshold", "w60", "xing-runway-threshold within 60 s of the recorded landing",
        "xing-runway-threshold"),
    median("xing-runway-threshold", "median", "Median difference from the recorded landing",
           "xing-runway-threshold"),
    own("runway-vacated", "own", "Arrivals with a runway-vacated", "runway-vacated", True),
    found("runway-vacated", "found", "Recorded landings with a runway-vacated", "runway-vacated"),
    median("runway-vacated", "median", "Median time from the recorded landing", "runway-vacated", kind="timing"),
    per_day("go-around", "events", "go-around events, per day", "go-around"),
    per_day("runway-crossings", "events", "runway-crossing-entry events, per day", "runway-crossing-entry"),
    # --- block times ---------------------------------------------------
    own("block-times", "aobt_own", "Departures with an AOBT", "AOBT", True),
    found("block-times", "aobt_found", "Recorded off-blocks with an AOBT", "AOBT"),
    w60("block-times", "aobt_w60", "AOBT within 60 s of the record", "AOBT"),
    median("block-times", "aobt_median", "AOBT: median difference from the record", "AOBT"),
    own("block-times", "aibt_own", "Arrivals with an AIBT", "AIBT", True),
    found("block-times", "aibt_found", "Recorded in-blocks with an AIBT", "AIBT"),
    w60("block-times", "aibt_w60", "AIBT within 60 s of the record", "AIBT"),
    median("block-times", "aibt_median", "AIBT: median difference from the record", "AIBT"),
    equal("block-times", "aobt_stand", "AOBT naming the recorded stand", "AOBT stand (info.stand)"),
    equal("block-times", "aibt_stand", "AIBT naming the recorded stand", "AIBT stand (info.stand)"),
    # --- vertical profile ----------------------------------------------
    own("tops", "toc_own", "Flights with a top-of-climb", "top-of-climb", True),
    per_day("tops", "toc", "top-of-climb events, per day", "top-of-climb"),
    per_day("tops", "tod", "top-of-descent events, per day", "top-of-descent"),
    own("level-segments", "own", "Flights with a level-start", "level-start", True),
    per_day("level-segments", "events", "level-start events, per day", "level-start"),
    per_day("level-offs", "climb", "level-off-climb-start events, per day", "level-off-climb-start"),
    per_day("level-offs", "descent", "level-off-descent-start events, per day", "level-off-descent-start"),
    own("flight-level-crossings", "fl100_own", "Flights with an xing-fl100", "xing-fl100", True),
    per_day("flight-level-crossings", "fl100", "xing-fl100 events, per day", "xing-fl100"),
    per_day("flight-level-crossings", "fl245", "xing-fl245 events, per day", "xing-fl245"),
    # --- distance rings ------------------------------------------------
    found("distance-rings", "c40_arr_found", "Recorded 40 NM inbound crossings found", "xing-40nm inbound"),
    w60("distance-rings", "c40_arr_w60", "40 NM inbound crossing within 60 s of the record", "xing-40nm inbound"),
    median("distance-rings", "c40_arr_median", "40 NM inbound: median difference", "xing-40nm inbound"),
    found("distance-rings", "c40_dep_found", "Recorded 40 NM outbound crossings found", "xing-40nm outbound"),
    w60("distance-rings", "c40_dep_w60", "40 NM outbound crossing within 60 s of the record", "xing-40nm outbound"),
    found("distance-rings", "c100_arr_found", "Recorded 100 NM inbound crossings found", "xing-100nm inbound"),
    w60("distance-rings", "c100_arr_w60", "100 NM inbound crossing within 60 s of the record", "xing-100nm inbound"),
    found("distance-rings", "c100_dep_found", "Recorded 100 NM outbound crossings found", "xing-100nm outbound"),
    w60("distance-rings", "c100_dep_w60", "100 NM outbound crossing within 60 s of the record",
        "xing-100nm outbound"),
    # --- airspace ------------------------------------------------------
    own("fir-crossings", "own", "Flights with an xing-fir", "xing-fir", True),
    per_day("fir-crossings", "events", "xing-fir events, per day", "xing-fir"),
    per_day("holding", "events", "holding-start events, per day", "holding-start"),
    per_day("point-merge", "legs", "pms-leg-entry events, per day", "pms-leg-entry"),
    per_day("point-merge", "merge", "pms-merge-point events, per day", "pms-merge-point"),
    # --- airport surface -----------------------------------------------
    per_day("surface-events", "taxiway", "entry-taxiway events, per day", "entry-taxiway"),
    per_day("surface-events", "stand", "entry-parking_position events, per day", "entry-parking_position"),
    w60("surface-events", "stand_entry_w60",
        "entry-parking_position within 60 s of the recorded in-block (nearest per record)",
        "entry-parking_position"),
    w60("surface-events", "stand_exit_w60",
        "exit-parking_position within 60 s of the recorded off-block (nearest per record)",
        "exit-parking_position"),
    # --- measurements --------------------------------------------------
    found("distance-flown", "found", "Recorded flights with a distance flown", "measurement: Distance flown (NM)",
          NM),
    Fig("distance-flown", "ratio_p50", "Distance flown over the recorded route length: median",
        "measurement: Distance flown (NM)", "median_ratio", "accuracy", NM, "", False),
    Fig("distance-flown", "within_10pct", "Flights whose distance flown is within 10 % of the route length",
        "measurement: Distance flown (NM)", "within_10pct_pct", "accuracy", NM),
    own("time-passed", "own", "Flights with a time_passed_s", "measurement: Time Passed (s)", True),
)

#: Algorithm page -> anchors of its items on the airport pages.
AIRPORT_ANCHORS = {
    "take-off-landing": (("take-off times", "take-off"), ("landing times", "landing")),
    "block-times": (("off-block times", "off-block"), ("in-block times", "in-block")),
    "flight-list-milestones": (("rwy_dep", "runway-dep"), ("rwy_arr", "runway-arr"),
                               ("stnd_dep", "stand-dep"), ("stnd_arr", "stand-arr")),
    "airports": (("adep", "adep"), ("ades", "ades")),
}


def read_rows(root: Path, source: str) -> list[dict]:
    with (root / source).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def lookup(rows: list[dict], fig: Fig) -> dict:
    ref = REF_KEY.get(fig.reference)
    found_ = [r for r in rows if r["days"] == POOLED and r["item"] == fig.item
              and (ref is None or r["reference"] == ref)]
    if ref is None and len(found_) > 1:
        found_ = found_[:1]  # availability among OPDI's flights is the same on every reference row
    if len(found_) != 1 or not found_[0].get(fig.column):
        raise KeyError(f"{fig.page}/{fig.measure_id}: no single value of {fig.column!r} for "
                       f"({fig.item!r}, {ref!r}) in {SOURCE}")
    return found_[0]


def shown(value: float, unit: str) -> str:
    if unit == "%":
        return f"{value:.1f} %"
    if unit == "s":
        return f"{value:.0f} s"
    if unit == "per day":
        return f"{value:,.0f}"
    return f"{value:.2f}"


def recorded(value: float, unit: str) -> str:
    """The value as written in the CSV, in the unit shown on the page."""
    if unit == "%":
        return f"{value:.1f}"
    if unit in ("s", "per day"):
        return f"{value:.0f}"
    return f"{value:.2f}"


def badge(fig: Fig, row: dict) -> str:
    """The label of the staged table for this figure, or ""."""
    if not fig.labelled or fig.column not in LABEL_OF:
        return ""
    return row.get(LABEL_OF[fig.column], "")


def airport_line(page: str) -> str:
    """The sentence that links a page to its rows on the airport pages."""
    anchors = AIRPORT_ANCHORS.get(page)
    if not anchors:
        return ("Per airport: every item at the [100 busiest European airports]"
                "(../accuracy/europe.qmd#all-items) and the [100 busiest airports worldwide]"
                "(../accuracy/world.qmd#all-items), as files to download.")
    europe = ", ".join(f"[{w}](../accuracy/europe.qmd#{a})" for w, a in anchors)
    world = ", ".join(f"[{w}](../accuracy/world.qmd#{a})" for w, a in anchors)
    return (f"Per airport: at the 100 busiest European airports ({europe}) and at the "
            f"100 busiest airports worldwide ({world}).")


def build(root: Path) -> tuple[str, dict[str, str]]:
    rows = read_rows(root, SOURCE)
    out = io.StringIO()
    writer = csv.writer(out, lineterminator="\n")
    writer.writerow(CSV_COLUMNS)
    tables: dict[str, list[str]] = {}
    for fig in FIGURES:
        row = lookup(rows, fig)
        value = float(row[fig.column])
        n_col = COUNT_OF[fig.column]
        n = int(round(float(row[n_col]))) if n_col and row.get(n_col) else 0
        label = badge(fig, row)
        writer.writerow([fig.page, fig.measure_id, fig.label, recorded(value, fig.unit), fig.unit, n,
                         fig.kind, label, fig.reference, SOURCE,
                         f"days '{POOLED}', item '{fig.item}', reference '{row['reference']}', "
                         f"column '{fig.column}'"])
        lines = tables.setdefault(fig.page, [
            "| What | Kind | Value | Out of | Label | Compared with |",
            "|---|---|---|---|---|---|",
        ])
        count = f"{n:,}" if n else ""
        lines.append(f"| {fig.label} | {fig.kind} | {shown(value, fig.unit)} | {count} | "
                     f"{label} | {fig.reference} |")
    for page in tables:
        # The numbers of the caption and of the per-airport sentence.
        writer.writerow([page, "context", f"Three days together, {DAYS_TEXT}; the 100 busiest airports",
                         "", "", "", "context", "", "", SOURCE, ""])
    pages = {}
    for page, lines in tables.items():
        pages[page] = (
            "<!-- Written by tools/gen_item_figures.py from assets/accuracy/item_accuracy.csv; "
            "do not edit. -->\n\n" + "\n".join(lines) + "\n\n"
            ": Three days together ({d}). \"Out of\" is the number of flights, records or compared "
            "values the share is computed over, to four significant figures. {{.striped}}\n\n"
            "{a}\n".format(d=DAYS_TEXT, a=airport_line(page))
        )
    return out.getvalue(), pages


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--check", action="store_true", help="only report whether files are out of date")
    args = parser.parse_args(argv)
    text, pages = build(args.root)
    wanted = {args.root / OUT_CSV: text}
    wanted.update({args.root / OUT_DIR / f"figures_{p}.md": t for p, t in pages.items()})
    stale = [p for p, t in wanted.items() if not p.is_file() or p.read_text(encoding="utf-8") != t]
    if args.check:
        for p in stale:
            print(f"out of date: {p.relative_to(args.root)}")
        return 1 if stale else 0
    for path, content in wanted.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
    print(f"wrote {OUT_CSV} and {len(pages)} tables under {OUT_DIR}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
