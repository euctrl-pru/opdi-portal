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

#: The study of 14 June 2026 (one worldwide day, after the corrections of the
#: spot checks). Scorecard rows: section, item, measure, value, count.
DAY = "papers/spot-check-corrections-v1/data/scorecard_day_final.csv"
DAY_TEXT = "2026-06-14"

AIRPORT = "airport operator records (Europe), {d}".format(d=DAY_TEXT)
NM = "Network Manager flight records (Europe), {d}".format(d=DAY_TEXT)
OWN = "OPDI flight list, {d}".format(d=DAY_TEXT)
OWN_EVENTS = "OPDI flight events, {d}".format(d=DAY_TEXT)

CSV_COLUMNS = ("page", "measure_id", "label", "value", "unit", "n", "kind",
               "badge", "reference", "source_file", "source_field")


@dataclass(frozen=True)
class Fig:
    """One number of one page."""
    page: str
    measure_id: str
    label: str          # what the number is, in words
    item: str           # scorecard item
    measure: str        # scorecard measure
    kind: str           # availability, accuracy or count
    reference: str
    unit: str = "%"     # %, s, events, flights, or "" for a ratio
    section: str = ""   # scorecard section; "" matches any
    badge: str = ""     # availability, accuracy, or "" for no label
    source: str = DAY


def share(page, mid, label, item, measure, kind, reference, badge="", section=""):
    return Fig(page, mid, label, item, measure, kind, reference, "%", section, badge)


FIGURES: tuple[Fig, ...] = (
    # --- flight list ----------------------------------------------------
    Fig("tracks", "rows", "Flights in the flight list", "flight list", "flights", "count", OWN, "flights"),
    share("tracks", "flt_id", "Flights with a call sign (flt_id)", "flight list FLT_ID",
          "share filled", "availability", OWN, "availability"),
    share("airports", "adep_filled", "Flights with a departure airport (adep)", "flight list ADEP",
          "share filled", "availability", OWN, "availability"),
    share("airports", "ades_filled", "Flights with a destination airport (ades)", "flight list ADES",
          "share filled", "availability", OWN, "availability"),
    share("airports", "adep_right", "Departure airport equal to the record", "ADEP source all",
          "share right", "accuracy", NM),
    share("airports", "adep_wrong", "Departure airport different from the record", "ADEP source all",
          "share wrong", "accuracy", NM),
    share("airports", "adep_none", "No departure airport for a recorded flight", "ADEP source all",
          "share none", "availability", NM),
    share("airports", "ades_right", "Destination airport equal to the record", "ADES source all",
          "share right", "accuracy", NM),
    share("airports", "ades_wrong", "Destination airport different from the record", "ADES source all",
          "share wrong", "accuracy", NM),
    share("airports", "ades_none", "No destination airport for a recorded flight", "ADES source all",
          "share none", "availability", NM),
    share("airports", "adep_runway_right", "Departure airport equal to the record, when taken from a runway event",
          "ADEP source runway_event", "share right", "accuracy", NM),
    share("airports", "ades_runway_right", "Destination airport equal to the record, when taken from a runway event",
          "ADES source runway_event", "share right", "accuracy", NM),
    share("aircraft", "typecode_filled", "Flights with an aircraft type (typecode)", "flight list TYPECODE",
          "share filled", "availability", OWN, "availability"),
    share("aircraft", "registration_filled", "Flights with a registration", "flight list REGISTRATION",
          "share filled", "availability", OWN, "availability"),
    share("aircraft", "operator_filled", "Flights with an operator code (icao_operator)", "flight list ICAO_OPERATOR",
          "share filled", "availability", OWN, "availability"),
    share("aircraft", "type_agrees", "Aircraft type equal to the record", "aircraft type",
          "share agrees", "accuracy", NM),
    share("aircraft", "type_differs", "Aircraft type different from the record", "aircraft type",
          "share differs", "accuracy", NM),
    Fig("first-last-seen", "events", "first_seen events (one per track)", "first_seen", "events",
        "count", OWN_EVENTS, "events", "iv"),
    share("flight-list-milestones", "atot_filled", "Flights with atot", "flight list ATOT",
          "share filled", "availability", OWN, "availability"),
    share("flight-list-milestones", "aldt_filled", "Flights with aldt", "flight list ALDT",
          "share filled", "availability", OWN, "availability"),
    share("flight-list-milestones", "aobt_filled", "Flights with aobt", "flight list AOBT",
          "share filled", "availability", OWN, "availability"),
    share("flight-list-milestones", "aibt_filled", "Flights with aibt", "flight list AIBT",
          "share filled", "availability", OWN, "availability"),
    share("flight-list-milestones", "stnd_dep_filled", "Flights with stnd_dep", "flight list STND_DEP",
          "share filled", "availability", OWN, "availability"),
    share("flight-list-milestones", "c40_arr_filled", "Flights with c40_arr", "flight list C40_ARR",
          "share filled", "availability", OWN, "availability"),
    share("flight-list-milestones", "rwy_dep_exact", "rwy_dep equal to the recorded runway", "RWY_DEP (flight list)",
          "runway exact", "accuracy", AIRPORT),
    share("flight-list-milestones", "rwy_arr_exact", "rwy_arr equal to the recorded runway", "RWY_ARR (flight list)",
          "runway exact", "accuracy", AIRPORT),
    share("flight-list-milestones", "stnd_dep_equal", "stnd_dep equal to the recorded stand", "STND_DEP (flight list)",
          "stand equal", "accuracy", AIRPORT),
    share("flight-list-milestones", "stnd_arr_equal", "stnd_arr equal to the recorded stand", "STND_ARR (flight list)",
          "stand equal", "accuracy", AIRPORT),
    # --- take-off and landing -----------------------------------------
    share("take-off-landing", "atot_found", "Recorded take-offs with an ATOT", "ATOT", "share of records found",
          "availability", AIRPORT, "availability"),
    share("take-off-landing", "atot_w60", "ATOT within 60 s of the record", "ATOT", "share within 60 s",
          "accuracy", AIRPORT, "accuracy"),
    Fig("take-off-landing", "atot_median", "ATOT: median difference from the record", "ATOT",
        "median absolute difference s", "accuracy", AIRPORT, "s"),
    Fig("take-off-landing", "atot_p90", "ATOT: 90th percentile of the difference", "ATOT",
        "p90 absolute difference s", "accuracy", AIRPORT, "s"),
    share("take-off-landing", "aldt_found", "Recorded landings with an ALDT", "ALDT", "share of records found",
          "availability", AIRPORT, "availability"),
    share("take-off-landing", "aldt_w60", "ALDT within 60 s of the record", "ALDT", "share within 60 s",
          "accuracy", AIRPORT, "accuracy"),
    Fig("take-off-landing", "aldt_median", "ALDT: median difference from the record", "ALDT",
        "median absolute difference s", "accuracy", AIRPORT, "s"),
    Fig("take-off-landing", "aldt_p90", "ALDT: 90th percentile of the difference", "ALDT",
        "p90 absolute difference s", "accuracy", AIRPORT, "s"),
    share("take-off-landing", "atot_acdm_w60", "ATOT from the runway pass (method acdm) within 60 s", "ATOT (acdm)",
          "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    share("take-off-landing", "atot_legacy_w60", "ATOT from the runway window (method legacy) within 60 s",
          "ATOT (legacy)", "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    share("take-off-landing", "aldt_acdm_w60", "ALDT from the runway pass (method acdm) within 60 s", "ALDT (acdm)",
          "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    share("take-off-landing", "aldt_legacy_w60", "ALDT from the runway window (method legacy) within 60 s",
          "ALDT (legacy)", "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    share("take-off-landing", "atot_runway", "Take-off runway equal to the record", "ATOT event runway (departures)",
          "runway exact", "accuracy", AIRPORT),
    share("take-off-landing", "aldt_runway", "Landing runway equal to the record", "ALDT event runway (arrivals)",
          "runway exact", "accuracy", AIRPORT),
    # --- other runway events -------------------------------------------
    share("line-up-take-off-roll", "found", "Recorded take-offs with a line-up and a take-off-roll",
          "take-off-roll (vs take-off record)", "share of records found", "availability", AIRPORT, "availability"),
    Fig("line-up-take-off-roll", "lineup_median", "line-up: median time to the recorded take-off",
        "line-up (vs take-off record)", "median absolute difference s", "count", AIRPORT, "s"),
    Fig("line-up-take-off-roll", "roll_median", "take-off-roll: median time to the recorded take-off",
        "take-off-roll (vs take-off record)", "median absolute difference s", "count", AIRPORT, "s"),
    share("xing-runway-threshold", "found", "Recorded landings with an xing-runway-threshold",
          "xing-runway-threshold (vs landing record)", "share of records found", "availability", AIRPORT,
          "availability"),
    share("xing-runway-threshold", "w60", "xing-runway-threshold within 60 s of the recorded landing",
          "xing-runway-threshold (vs landing record)", "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    Fig("xing-runway-threshold", "median", "Median difference from the recorded landing",
        "xing-runway-threshold (vs landing record)", "median absolute difference s", "accuracy", AIRPORT, "s"),
    share("runway-vacated", "found", "Recorded landings with a runway-vacated",
          "runway-vacated (vs landing record)", "share of records found", "availability", AIRPORT, "availability"),
    Fig("runway-vacated", "median", "Median time from the recorded landing",
        "runway-vacated (vs landing record)", "median absolute difference s", "count", AIRPORT, "s"),
    Fig("go-around", "events", "go-around events", "go-around", "events", "count", OWN_EVENTS, "events", "iv"),
    Fig("runway-crossings", "events", "runway-crossing-entry events", "runway-crossing-entry", "events",
        "count", OWN_EVENTS, "events", "iv"),
    # --- block times ---------------------------------------------------
    share("block-times", "aobt_found", "Recorded off-blocks with an AOBT", "AOBT", "share of records found",
          "availability", AIRPORT, "availability"),
    share("block-times", "aobt_w60", "AOBT within 60 s of the record", "AOBT", "share within 60 s",
          "accuracy", AIRPORT, "accuracy"),
    Fig("block-times", "aobt_median", "AOBT: median difference from the record", "AOBT",
        "median absolute difference s", "accuracy", AIRPORT, "s"),
    share("block-times", "aibt_found", "Recorded in-blocks with an AIBT", "AIBT", "share of records found",
          "availability", AIRPORT, "availability"),
    share("block-times", "aibt_w60", "AIBT within 60 s of the record", "AIBT", "share within 60 s",
          "accuracy", AIRPORT, "accuracy"),
    Fig("block-times", "aibt_median", "AIBT: median difference from the record", "AIBT",
        "median absolute difference s", "accuracy", AIRPORT, "s"),
    share("block-times", "aobt_stand", "AOBT naming the recorded stand", "AOBT event stand", "stand equal",
          "accuracy", AIRPORT),
    share("block-times", "aibt_stand", "AIBT naming the recorded stand", "AIBT event stand", "stand equal",
          "accuracy", AIRPORT),
    # --- vertical profile ----------------------------------------------
    Fig("tops", "toc", "top-of-climb events", "top-of-climb", "events", "count", OWN_EVENTS, "events", "iv"),
    Fig("tops", "tod", "top-of-descent events", "top-of-descent", "events", "count", OWN_EVENTS, "events", "iv"),
    Fig("level-segments", "events", "level-start events", "level-start", "events", "count", OWN_EVENTS,
        "events", "iv"),
    Fig("level-offs", "climb", "level-off-climb-start events", "level-off-climb-start", "events", "count",
        OWN_EVENTS, "events", "iv"),
    Fig("level-offs", "descent", "level-off-descent-start events", "level-off-descent-start", "events", "count",
        OWN_EVENTS, "events", "iv"),
    Fig("flight-level-crossings", "fl100", "xing-fl100 events", "xing-fl100", "events", "count", OWN_EVENTS,
        "events", "iv"),
    Fig("flight-level-crossings", "fl245", "xing-fl245 events", "xing-fl245", "events", "count", OWN_EVENTS,
        "events", "iv"),
    # --- distance rings ------------------------------------------------
    share("distance-rings", "c40_arr_found", "Recorded 40 NM inbound crossings found", "xing-40nm inbound",
          "share of records found", "availability", AIRPORT, "availability"),
    share("distance-rings", "c40_arr_w60", "40 NM inbound crossing within 60 s of the record", "xing-40nm inbound",
          "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    Fig("distance-rings", "c40_arr_median", "40 NM inbound: median difference", "xing-40nm inbound",
        "median absolute difference s", "accuracy", AIRPORT, "s"),
    share("distance-rings", "c40_dep_found", "Recorded 40 NM outbound crossings found", "xing-40nm outbound",
          "share of records found", "availability", AIRPORT, "availability"),
    share("distance-rings", "c40_dep_w60", "40 NM outbound crossing within 60 s of the record",
          "xing-40nm outbound", "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    share("distance-rings", "c100_arr_found", "Recorded 100 NM inbound crossings found", "xing-100nm inbound",
          "share of records found", "availability", AIRPORT, "availability"),
    share("distance-rings", "c100_arr_w60", "100 NM inbound crossing within 60 s of the record",
          "xing-100nm inbound", "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    share("distance-rings", "c100_dep_found", "Recorded 100 NM outbound crossings found", "xing-100nm outbound",
          "share of records found", "availability", AIRPORT, "availability"),
    share("distance-rings", "c100_dep_w60", "100 NM outbound crossing within 60 s of the record",
          "xing-100nm outbound", "share within 60 s", "accuracy", AIRPORT, "accuracy"),
    # --- airspace ------------------------------------------------------
    Fig("fir-crossings", "events", "xing-fir events", "xing-fir", "events", "count", OWN_EVENTS, "events", "iv"),
    Fig("holding", "events", "holding-start events", "holding-start", "events", "count", OWN_EVENTS, "events",
        "iv"),
    Fig("point-merge", "legs", "pms-leg-entry events", "pms-leg-entry", "events", "count", OWN_EVENTS, "events",
        "iv"),
    Fig("point-merge", "merge", "pms-merge-point events", "pms-merge-point", "events", "count", OWN_EVENTS,
        "events", "iv"),
    # --- airport surface -----------------------------------------------
    Fig("surface-events", "taxiway", "entry-taxiway events", "entry-taxiway", "events", "count", OWN_EVENTS,
        "events", "iv"),
    Fig("surface-events", "stand", "entry-parking_position events", "entry-parking_position", "events", "count",
        OWN_EVENTS, "events", "iv"),
    share("surface-events", "stand_entry_w60",
          "entry-parking_position within 60 s of the recorded in-block (nearest per record)",
          "entry-parking_position (vs in-block record, nearest per record)", "share within 60 s", "accuracy",
          AIRPORT, "accuracy"),
    share("surface-events", "stand_exit_w60",
          "exit-parking_position within 60 s of the recorded off-block (nearest per record)",
          "exit-parking_position (vs off-block record, nearest per record)", "share within 60 s", "accuracy",
          AIRPORT, "accuracy"),
    # --- measurements --------------------------------------------------
    Fig("distance-flown", "ratio_p50", "Distance flown over the recorded route length: median",
        "distance flown / route length", "p50", "accuracy", NM, ""),
    Fig("distance-flown", "ratio_p25", "Distance flown over the recorded route length: 25th percentile",
        "distance flown / route length", "p25", "accuracy", NM, ""),
    share("distance-flown", "ratio_below_half", "Flights whose distance flown is under half the route length",
          "distance flown / route length", "share below 0.5", "availability", NM),
    share("distance-flown", "ratio_above", "Flights whose distance flown is over 1.5 times the route length",
          "distance flown / route length", "share above 1.5", "accuracy", NM),
)


def read_rows(root: Path, source: str) -> list[dict]:
    with (root / source).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def lookup(rows: list[dict], fig: Fig) -> dict:
    found = [r for r in rows if r["item"] == fig.item and r["measure"] == fig.measure
             and (not fig.section or r["section"] == fig.section)]
    if len(found) != 1:
        raise KeyError(f"{fig.page}/{fig.measure_id}: {len(found)} rows for "
                       f"({fig.section!r}, {fig.item!r}, {fig.measure!r}) in {fig.source}")
    return found[0]


def shown(value: float, unit: str) -> str:
    if unit == "%":
        return f"{value * 100:.1f} %"
    if unit == "s":
        return f"{value:.0f} s"
    if unit in ("events", "flights"):
        return f"{value:,.0f}"
    return f"{value:.2f}"


def recorded(value: float, unit: str) -> str:
    """The value as written in the CSV, in the unit shown on the page."""
    if unit == "%":
        return f"{value * 100:.1f}"
    if unit == "s":
        return f"{value:.0f}"
    if unit in ("events", "flights"):
        return f"{value:.0f}"
    return f"{value:.2f}"


def badge(fig: Fig, value: float, n: int) -> str:
    if fig.badge == "availability":
        return "high" if value >= 0.8 else "partial" if value >= 0.5 else "low"
    if fig.badge == "accuracy":
        if n < 30:
            return "no reference"
        return "good" if value >= 0.8 else "fair" if value >= 0.5 else "limited"
    return ""


def build(root: Path) -> tuple[str, dict[str, str]]:
    cache: dict[str, list[dict]] = {}
    out = io.StringIO()
    writer = csv.writer(out, lineterminator="\n")
    writer.writerow(CSV_COLUMNS)
    tables: dict[str, list[str]] = {}
    for fig in FIGURES:
        rows = cache.setdefault(fig.source, read_rows(root, fig.source))
        row = lookup(rows, fig)
        value = float(row["value"])
        n = int(float(row["count"] or 0))
        label = badge(fig, value, n)
        writer.writerow([fig.page, fig.measure_id, fig.label, recorded(value, fig.unit), fig.unit, n,
                         fig.kind, label, fig.reference, fig.source,
                         f"section {row['section']}, item '{fig.item}', measure '{fig.measure}'"])
        lines = tables.setdefault(fig.page, [
            "| What | Kind | Value | Out of | Label | Compared with |",
            "|---|---|---|---|---|---|",
        ])
        count = "" if fig.unit in ("events", "flights") else f"{n:,}"
        lines.append(f"| {fig.label} | {fig.kind} | {shown(value, fig.unit)} | {count} | "
                     f"{label} | {fig.reference} |")
    pages = {}
    for page, lines in tables.items():
        pages[page] = (
            "<!-- Written by tools/gen_item_figures.py from committed study files; "
            "do not edit. -->\n\n" + "\n".join(lines) + "\n\n"
            ": Figures of one worldwide day ({d}); \"Out of\" is the number of records or "
            "flights the share is computed over. {{.striped}}\n".format(d=DAY_TEXT)
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
