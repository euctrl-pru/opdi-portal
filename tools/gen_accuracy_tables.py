#!/usr/bin/env python3
"""Write the tables of the Accuracy and availability pages.

Every table on content/accuracy/index.qmd, europe.qmd and world.qmd comes
from this script. It reads the staged tables under assets/accuracy/ (see
assets/accuracy/README.md for every file and column) and writes small
Markdown files under content/accuracy/_generated/, which the pages include
with

    {{< include _generated/<name>.md >}}

It also writes assets/pages/accuracy_numbers.csv: the numbers the pages may
write in their own prose, each with what it is. tools/check_site.py checks
that every number written on an accuracy page outside the included tables,
and every number inside a [...]{.measured} span on any page, is a row of
that file for the same page. A sentence such as "72 of the 100 airports send
airport records" is therefore checked against the staged tables.

Two measures are kept apart and never merged into one figure:

  availability  how often an item exists: among OPDI's own flights, or among
                the movements of a reference;
  accuracy      how close the item is to the reference when both exist.

The labels are those of the staged tables (assets/accuracy/README.md):
availability high / partial / low; accuracy good / fair / limited /
too few compared / no reference. They are written as words.

Usage:

  python tools/gen_accuracy_tables.py           # write the files
  python tools/gen_accuracy_tables.py --check   # exit 1 when they are out of date

Plain Python 3, no extra packages.
"""
from __future__ import annotations

import argparse
import csv
import io
import json
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ACC = Path("assets/accuracy")
OUT_DIR = Path("content/accuracy/_generated")
NUMBERS_CSV = Path("assets/pages/accuracy_numbers.csv")
#: The labels of every item, for the Algorithms index.
ALGORITHMS_LABELS = Path("content/algorithms/_generated/labels.md")
NUMBER_COLUMNS = ("page", "measure_id", "label", "value", "n", "source_file")

POOLED = "all three days"
DAYS = ("2026-06-10", "2026-06-14", "2026-06-17")
DAY_NAMES = {"2026-06-10": "10 June", "2026-06-14": "14 June", "2026-06-17": "17 June"}

#: Share of departures with an off-block time and of arrivals with an
#: in-block time (in %) under which an airport counts as having almost no
#: ground reception.
GROUND_NEARLY_ABSENT_PCT = 2.0

HEADER = "<!-- Written by tools/gen_accuracy_tables.py from assets/accuracy/; do not edit. -->\n\n"

# ---------------------------------------------------------------------------
# Items: order, names and algorithm pages
# ---------------------------------------------------------------------------

RINGS = (40, 50, 60, 100, 110, 120)

#: Every item of item_accuracy.csv, in the order of the Algorithms index:
#: (item, name shown, algorithm page).
ITEMS: tuple[tuple[str, str, str], ...] = (
    ("flight list: FIRST_SEEN", "`first_seen` (flight list)", "first-last-seen"),
    ("flight list: LAST_SEEN", "`last_seen` (flight list)", "first-last-seen"),
    ("first_seen", "`first_seen`", "first-last-seen"),
    ("last_seen", "`last_seen`", "first-last-seen"),
    ("flight list: ADEP", "`adep` (flight list)", "airports"),
    ("flight list: ADES", "`ades` (flight list)", "airports"),
    ("flight list: TYPECODE", "`typecode` (flight list)", "aircraft"),
    ("ATOT", "`ATOT`", "take-off-landing"),
    ("ATOT (acdm)", "`ATOT` with `info.method` = `acdm`", "take-off-landing"),
    ("ATOT (legacy)", "`ATOT` with `info.method` = `legacy`", "take-off-landing"),
    ("ATOT runway (info.runway)", "`ATOT` runway (`info.runway`)", "take-off-landing"),
    ("ALDT", "`ALDT`", "take-off-landing"),
    ("ALDT (acdm)", "`ALDT` with `info.method` = `acdm`", "take-off-landing"),
    ("ALDT (legacy)", "`ALDT` with `info.method` = `legacy`", "take-off-landing"),
    ("ALDT runway (info.runway)", "`ALDT` runway (`info.runway`)", "take-off-landing"),
    ("flight list: ATOT", "`atot` (flight list)", "flight-list-milestones"),
    ("flight list: ALDT", "`aldt` (flight list)", "flight-list-milestones"),
    ("flight list: RWY_DEP", "`rwy_dep` (flight list)", "flight-list-milestones"),
    ("flight list: RWY_ARR", "`rwy_arr` (flight list)", "flight-list-milestones"),
    ("line-up", "`line-up`", "line-up-take-off-roll"),
    ("take-off-roll", "`take-off-roll`", "line-up-take-off-roll"),
    ("xing-runway-threshold", "`xing-runway-threshold`", "xing-runway-threshold"),
    ("runway-vacated", "`runway-vacated`", "runway-vacated"),
    ("go-around", "`go-around`", "go-around"),
    ("runway-crossing-entry", "`runway-crossing-entry`", "runway-crossings"),
    ("runway-crossing-vacated", "`runway-crossing-vacated`", "runway-crossings"),
    ("AOBT", "`AOBT`", "block-times"),
    ("AOBT stand (info.stand)", "`AOBT` stand (`info.stand`)", "block-times"),
    ("AIBT", "`AIBT`", "block-times"),
    ("AIBT stand (info.stand)", "`AIBT` stand (`info.stand`)", "block-times"),
    ("flight list: AOBT", "`aobt` (flight list)", "flight-list-milestones"),
    ("flight list: AIBT", "`aibt` (flight list)", "flight-list-milestones"),
    ("flight list: STND_DEP", "`stnd_dep` (flight list)", "flight-list-milestones"),
    ("flight list: STND_ARR", "`stnd_arr` (flight list)", "flight-list-milestones"),
) + tuple(
    (f"{end}-{element}", f"`{end}-{element}`", "surface-events")
    for element in ("parking_position", "apron", "taxiway", "threshold", "deicing_pad", "hangar")
    for end in ("entry", "exit")
) + (
    ("top-of-climb", "`top-of-climb`", "tops"),
    ("top-of-descent", "`top-of-descent`", "tops"),
    ("level-start", "`level-start`", "level-segments"),
    ("level-end", "`level-end`", "level-segments"),
    ("level-off-climb-start", "`level-off-climb-start`", "level-offs"),
    ("level-off-climb-end", "`level-off-climb-end`", "level-offs"),
    ("level-off-descent-start", "`level-off-descent-start`", "level-offs"),
    ("level-off-descent-end", "`level-off-descent-end`", "level-offs"),
) + tuple(
    (f"xing-fl{fl}", f"`xing-fl{fl}`", "flight-level-crossings") for fl in (50, 70, 100, 245)
) + tuple(
    (f"xing-{r}nm {d}", f"`xing-{r}nm`, {d}", "distance-rings")
    for r in RINGS for d in ("inbound", "outbound")
) + tuple(
    (f"flight list: C{r}_{s}", f"`c{r}_{s.lower()}` (flight list)", "distance-rings")
    for r in RINGS for s in ("ARR", "DEP")
) + (
    ("xing-fir", "`xing-fir`", "fir-crossings"),
    ("holding-start", "`holding-start`", "holding"),
    ("holding-end", "`holding-end`", "holding"),
    ("measurement: Holding duration (s)", "`holding_duration_s`", "holding"),
    ("measurement: Holding distance (NM)", "`holding_distance_nm`", "holding"),
    ("measurement: Holding circuits", "`holding_circuits`", "holding"),
    ("pms-leg-entry", "`pms-leg-entry`", "point-merge"),
    ("pms-leg-exit", "`pms-leg-exit`", "point-merge"),
    ("pms-merge-point", "`pms-merge-point`", "point-merge"),
    ("measurement: Point merge leg time (s)", "`pms_leg_time_s`", "point-merge"),
    ("measurement: Point merge leg distance (NM)", "`pms_leg_distance_nm`", "point-merge"),
    ("measurement: Point merge direct-to distance (NM)", "`pms_direct_to_distance_nm`", "point-merge"),
    ("measurement: Distance flown (NM)", "`distance_flown_nm`", "distance-flown"),
    ("measurement: Time Passed (s)", "`time_passed_s`", "time-passed"),
)
ITEM_NAME = {item: name for item, name, _ in ITEMS}
ITEM_PAGE = {item: page for item, _, page in ITEMS}

#: What a reference would need to be, for the items that have none.
NEEDED = {
    "first-last-seen": "none needed: they are the times of the first and last report received",
    "surface-events": "records of the taxi route or of surface movement radar",
    "go-around": "records of go-arounds from the airport or air traffic control",
    "runway-crossings": "records of runway crossings from the airport or air traffic control",
    "tops": "a measured vertical profile of the flight",
    "level-segments": "a measured vertical profile of the flight",
    "level-offs": "a measured vertical profile of the flight",
    "flight-level-crossings": "a measured vertical profile of the flight",
    "distance-rings": "crossing times of this circle; the airport records give only the 40 and 100 NM circles",
    "fir-crossings": "records of the times each flight entered each FIR",
    "holding": "records of holds from air traffic control",
    "point-merge": "records of the instructions given to each arrival",
    "time-passed": "none needed: it is the time between two report times",
}

#: The main items of the airport pages: (anchor, column title, item, name in text).
MAIN = (
    ("take-off", "Take-off", "ATOT", "Take-off time (`ATOT`)"),
    ("landing", "Landing", "ALDT", "Landing time (`ALDT`)"),
    ("off-block", "Off-block", "AOBT", "Off-block time (`AOBT`)"),
    ("in-block", "In-block", "AIBT", "In-block time (`AIBT`)"),
    ("runway-dep", "Runway dep.", "flight list: RWY_DEP", "Departure runway (`rwy_dep`)"),
    ("runway-arr", "Runway arr.", "flight list: RWY_ARR", "Arrival runway (`rwy_arr`)"),
    ("stand-dep", "Stand dep.", "flight list: STND_DEP", "Departure stand (`stnd_dep`)"),
    ("stand-arr", "Stand arr.", "flight list: STND_ARR", "Arrival stand (`stnd_arr`)"),
    ("adep", "Dep. airport", "flight list: ADEP", "Departure airport (`adep`)"),
    ("ades", "Dest. airport", "flight list: ADES", "Destination airport (`ades`)"),
)

#: Algorithm page -> anchors of its items on the airport pages.
PAGE_ANCHORS = {
    "take-off-landing": ("take-off", "landing"),
    "block-times": ("off-block", "in-block"),
    "flight-list-milestones": ("runway-dep", "runway-arr", "stand-dep", "stand-arr"),
    "airports": ("adep", "ades"),
}

# ---------------------------------------------------------------------------
# Reading and formatting
# ---------------------------------------------------------------------------


def read(root: Path, name: str) -> list[dict]:
    with (root / ACC / name).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def num(text: str) -> float | None:
    try:
        return float(text)
    except (TypeError, ValueError):
        return None


def pct(text: str) -> str:
    v = num(text)
    return "" if v is None else f"{v:.1f} %"


def count(text: str) -> str:
    v = num(text)
    return "" if v is None else f"{round(v):,}"


def cell(value: str, label: str) -> str:
    """A share and its label, as in '80.8 % high'."""
    if not value:
        return label or ""
    return f"{value} {label}".strip()


def table(head: list[str], rows: list[list[str]], caption: str = "") -> str:
    lines = ["| " + " | ".join(head) + " |", "|" + "|".join("---" for _ in head) + "|"]
    lines += ["| " + " | ".join(c.replace("|", "\\|") for c in row) + " |" for row in rows]
    text = "\n".join(lines) + "\n"
    if caption:
        text += f"\n: {caption} {{.striped .hover}}\n"
    return text


def accuracy_text(r: dict) -> str:
    """The accuracy share of a row, with what it measures."""
    measure = r.get("accuracy_measure", "")
    value = pct(r.get("accuracy_value_pct", ""))
    if not value:
        return ""
    if measure == "share within 60 s":
        return f"{value} within 60 s"
    if measure == "share equal":
        return f"{value} equal"
    if measure.startswith("share within 10%"):
        return f"{value} within 10 % of the route length"
    return value


def link(item: str, prefix: str = "../algorithms/") -> str:
    return f"[{ITEM_NAME[item]}]({prefix}{ITEM_PAGE[item]}.qmd)"


# ---------------------------------------------------------------------------
# Overall page
# ---------------------------------------------------------------------------


def overall_tables(items: list[dict], spread: list[dict]) -> dict[str, str]:
    pooled = [r for r in items if r["days"] == POOLED]
    unknown = sorted({r["item"] for r in pooled} - set(ITEM_NAME))
    if unknown:
        raise KeyError(f"items without a name in ITEMS: {unknown}")
    order = {item: i for i, (item, _, _) in enumerate(ITEMS)}
    ref_order = {"airport records": 0, "Network Manager": 1, "no reference": 2}
    pooled.sort(key=lambda r: (order[r["item"]], ref_order.get(r["reference"], 3)))

    per_day = {(r["item"], r["reference"], r["days"]): r for r in items if r["days"] != POOLED}
    rows = []
    for r in pooled:
        if r["reference"] != "no reference":
            continue
        rows.append([link(r["item"])]
                    + [count(per_day[(r["item"], r["reference"], d)]["values"]) for d in DAYS]
                    + [cell(pct(r["availability_own_pct"]), r["availability_own_label"]),
                       NEEDED[ITEM_PAGE[r["item"]]]])
    noref = table(["Item"] + [f"Values on {DAY_NAMES[d]}" for d in DAYS]
                  + ["Available among OPDI's flights (three days)", "What a reference would need to be"],
                  rows, "Items with no reference to compare with: how many there are each day.")

    sp = {(r["item"], r["reference"], r["measure"]): r for r in spread}
    rows = []
    for r in pooled:
        if r["reference"] == "no reference":
            continue
        for measure, words in (("availability_ref_pct", "available against the reference"),
                               ("within_60s_pct", "within 60 s"), ("share_equal_pct", "equal")):
            s = sp.get((r["item"], r["reference"], measure))
            if s is None:
                continue
            rows.append([link(r["item"]), r["reference"], words]
                        + [pct(s[d]) for d in DAYS]
                        + [f"{num(s['spread']):.1f} points", "yes" if s["stable"] == "True" else "no"])
    stability = table(["Item", "Reference", "Share"] + [DAY_NAMES[d] for d in DAYS]
                      + ["Largest minus smallest", "Stable"],
                      rows, "Day to day: the shares of the items with a reference on each day.")

    rows = [[link(s["item"]), s["reference"] if s["reference"] != "no reference" else "none",
             "available among OPDI's flights"] + [pct(s[d]) for d in DAYS]
            + [f"{num(s['spread']):.1f} points"]
            for s in spread if s["stable"] != "True"]
    unstable = table(["Item", "Reference", "Share"] + [DAY_NAMES[d] for d in DAYS] + ["Largest minus smallest"],
                     rows, "The shares that move by 5 points or more from one day to another.")
    rows = [[link(r["item"], ""), r["availability_own_label"],
             r["reference"] if r["reference"] != "no reference" else "none",
             r["availability_ref_label"], r["accuracy_label"]] for r in pooled]
    labels = table(["Item", "Availability among OPDI's flights", "Reference",
                    "Availability against the reference", "Accuracy"], rows,
                   "Labels of every item, the three days together. The numbers behind them are on "
                   "Accuracy and availability.")
    return {"overall_no_reference": noref, "algorithms_labels": labels,
            "overall_stability": stability, "overall_unstable": unstable}


# ---------------------------------------------------------------------------
# Airport pages
# ---------------------------------------------------------------------------


def by_airport(rows: list[dict]) -> tuple[list[dict], dict]:
    """The airports in rank order (first row of each), and rows by (airport, item)."""
    index = {(r["airport"], r["item"]): r for r in rows}
    airports, seen = [], set()
    for r in sorted(rows, key=lambda r: int(r["rank"])):
        if r["airport"] not in seen:
            seen.add(r["airport"])
            airports.append(r)
    return airports, index


def ground_airports(airports: list[dict], airports_csv: list[dict]) -> list[dict]:
    """Rows of airports.csv of the listed airports with almost no ground reception, in rank order."""
    ranks = {a["airport"]: a for a in airports}
    ground = [g for g in airports_csv if g["airport"] in ranks
              and (num(g["departures_with_off_block_pct"]) or 0) < GROUND_NEARLY_ABSENT_PCT
              and (num(g["arrivals_with_in_block_pct"]) or 0) < GROUND_NEARLY_ABSENT_PCT]
    ground.sort(key=lambda g: int(ranks[g["airport"]]["rank"]))
    return ground


def no_time_airports(airports: list[dict], index: dict) -> list[dict]:
    """Airports with no take-off time and no landing time."""
    return [a for a in airports
            if (num(index[(a["airport"], "ATOT")]["availability_own_pct"]) or 0) == 0
            and (num(index[(a["airport"], "ALDT")]["availability_own_pct"]) or 0) == 0]


def europe_tables(rows: list[dict], airports_csv: list[dict]) -> tuple[dict[str, str], dict]:
    airports, index = by_airport(rows)
    out = {}
    # Patterns: airports per label and item.
    labels = ("good", "fair", "limited", "too few compared", "no reference")
    prow = []
    stats = {}
    for anchor, title, item, name in MAIN:
        c = Counter(index[(a["airport"], item)]["accuracy_label"] for a in airports)
        stats[anchor] = c
        prow.append([name] + [str(c.get(lab, 0)) for lab in labels])
    out["europe_patterns"] = table(["Item"] + list(labels), prow,
                                   "Number of the 100 airports with each accuracy label.")

    limited = []
    for anchor, title, item, name in MAIN:
        names = [f"`{a['airport']}` {a['name']}" for a in airports
                 if index[(a["airport"], item)]["accuracy_label"] == "limited"]
        if names:
            limited.append([name, "; ".join(names)])
    out["europe_limited"] = table(["Item", "Airports labelled limited"], limited,
                                  "Airports where an item is compared with the records at least 30 times and "
                                  "fewer than half of the values agree.")

    ranks = {a["airport"]: a for a in airports}
    ground = ground_airports(airports, airports_csv)
    out["europe_ground"] = table(
        ["Rank", "ICAO", "Airport", "Departures with off-block", "Departures with line-up",
         "Departures with take-off", "Arrivals with landing", "Arrivals with in-block"],
        [[ranks[g["airport"]]["rank"], f"`{g['airport']}`", g["name"],
          pct(g["departures_with_off_block_pct"]), pct(g["departures_with_line_up_pct"]),
          pct(g["departures_with_take_off_pct"]), pct(g["arrivals_with_landing_pct"]),
          pct(g["arrivals_with_in_block_pct"])] for g in ground],
        "Airports where fewer than 2 % of OPDI's departures have an off-block time and fewer than 2 % "
        "of its arrivals an in-block time. Empty: no arrival in OPDI's flight list.")

    none = no_time_airports(airports, index)
    out["europe_no_times"] = table(
        ["Rank", "ICAO", "Airport", "OPDI departures", "OPDI arrivals", "Arrivals in the records with an `ades`"],
        [[a["rank"], f"`{a['airport']}`", a["name"],
          count(index[(a["airport"], "ATOT")]["flights_eligible"]) or "0",
          count(index[(a["airport"], "ALDT")]["flights_eligible"]) or "0",
          cell(pct(index[(a["airport"], "flight list: ADES")]["availability_ref_pct"]),
               index[(a["airport"], "flight list: ADES")]["availability_ref_label"])]
         for a in none],
        "Airports with no take-off time and no landing time on any of the three days. Last column: "
        "share of the arrivals in the Network Manager records that OPDI gives this destination.")

    facts = {
        "airports": len(airports),
        "with_records": sum(a["has_airport_records"] == "True" for a in airports),
        "without_records": sum(a["has_airport_records"] != "True" for a in airports),
        "ground": len(ground),
        "no_times": len(none),
        "stats": stats,
    }
    return out, facts


def world_tables(rows: list[dict]) -> tuple[dict[str, str], dict]:
    airports, index = by_airport(rows)
    out = {}
    facts = {
        "airports": len(airports),
        "outside": sum(a["in_europe"] != "True" for a in airports),
        "in_europe": sum(a["in_europe"] == "True" for a in airports),
        "with_records": sum(a["has_airport_records"] == "True" for a in airports),
    }
    return out, facts


# ---------------------------------------------------------------------------
# Lookup payloads (read by assets/js/accuracy-lookup.js)
# ---------------------------------------------------------------------------

LABELS = {"availability": ["high", "partial", "low"],
          "accuracy": ["good", "fair", "limited", "too few compared", "no reference"]}
REFERENCES = ["airport records", "Network Manager", "no reference"]
LOOKUP_SCOPES = ("europe", "world", "overall")
LOOKUP_JSON = "assets/pages/accuracy_lookup_{scope}.json"

#: Sections of the Algorithms sidebar in _quarto.yml: page -> section title.
ITEM_GROUPS = {
    "Flight list": ("tracks", "cleaning", "airports", "aircraft", "first-last-seen",
                    "flight-list-milestones", "movements"),
    "Runway and stands": ("take-off-landing", "line-up-take-off-roll", "xing-runway-threshold",
                          "runway-vacated", "go-around", "runway-crossings", "block-times",
                          "surface-events"),
    "Vertical profile": ("tops", "level-segments", "level-offs", "flight-level-crossings"),
    "Airspace and terminal area": ("distance-rings", "fir-crossings", "holding", "point-merge"),
    "Measurements": ("distance-flown", "time-passed"),
}


def item_group(page: str) -> str:
    """The Algorithms sidebar section of an algorithm page ("Other" when it has none)."""
    for group, pages in ITEM_GROUPS.items():
        if page in pages:
            return group
    return "Other"


def _n(text, digits: int | None = None):
    v = num(text)
    if v is None:
        return None
    return round(v) if digits is None else round(v, digits)


def _item_entries(names) -> dict:
    return {item: {"name": ITEM_NAME[item], "page": f"../algorithms/{ITEM_PAGE[item]}.qmd",
                   "group": item_group(ITEM_PAGE[item])} for item in names}


def _lookup_base(scope: str, items) -> dict:
    return {"scope": scope, "labels": LABELS, "references": REFERENCES,
            "main": [{"anchor": a, "title": t, "item": i, "name": n} for a, t, i, n in MAIN],
            "items": _item_entries(items)}


def airport_payload(rows: list[dict], airports_csv: list[dict], scope: str) -> dict:
    """The lookup payload of the europe or world airport page."""
    airports, index = by_airport(rows)
    ground = {g["airport"] for g in ground_airports(airports, airports_csv)}
    no_times = {a["airport"] for a in no_time_airports(airports, index)}
    out_airports = []
    for a in airports:
        code = a["airport"]
        if a["has_airport_records"] == "True":
            where = "airport records"
        elif scope == "europe":
            where = "Network Manager"
        else:
            where = "Europe, no airport records" if a["in_europe"] == "True" else "outside Europe"
        out_airports.append({
            "icao": code, "name": a["name"], "country": a["country"], "rank": int(a["rank"]),
            "movements": _n(a["nm_movements" if scope == "europe" else "opdi_movements"]),
            "where": where,
            "flags": [f for f, s in (("ground", ground), ("no_times", no_times)) if code in s],
            "limited": [anchor for anchor, _, item, _ in MAIN
                        if index[(code, item)]["accuracy_label"] == "limited"]})
    codes = {a["airport"] for a in airports}
    rows_by = {}
    seen_items = set()
    for r in rows:
        if r["airport"] not in codes:
            continue
        seen_items.add(r["item"])
        rows_by.setdefault(r["airport"], {})[r["item"]] = [
            _n(r["availability_own_pct"], 1), r["availability_own_label"], _n(r["flights_eligible"]),
            REFERENCES.index(r["reference"]), _n(r["reference_movements"]),
            _n(r["availability_ref_pct"], 1), r["availability_ref_label"], _n(r["compared"]),
            accuracy_text(r), _n(r.get("median_abs_diff_s", "")), r["accuracy_label"]]
    payload = _lookup_base(scope, [i for i, _, _ in ITEMS if i in seen_items])
    payload["airports"] = out_airports
    payload["rows"] = rows_by
    return payload


def overall_payload(items: list[dict]) -> dict:
    """The lookup payload of the overall page: the pooled rows of every item, one per reference."""
    pooled = [r for r in items if r["days"] == POOLED]
    overall = {}
    for item, _, _ in ITEMS:
        rows = sorted((r for r in pooled if r["item"] == item), key=lambda r: REFERENCES.index(r["reference"]))
        if not rows:
            continue
        overall[item] = [[
            REFERENCES.index(r["reference"]), _n(r["values_per_day"]), _n(r["flights_eligible"]),
            _n(r["availability_own_pct"], 1), r["availability_own_label"], r["compared_with"],
            _n(r["reference_movements"]), _n(r["availability_ref_pct"], 1), r["availability_ref_label"],
            _n(r["compared"]), accuracy_text(r), _n(r["median_abs_diff_s"]), _n(r["p90_abs_diff_s"]),
            r["accuracy_label"]] for r in rows]
    payload = _lookup_base("overall", overall)
    payload["overall"] = overall
    return payload


def lookup_include(scope: str) -> str:
    """The Markdown the accuracy pages include for the search widget."""
    anchors = "".join(f"[]{{#{a}}}" for a, _, _, _ in MAIN) + "\n\n" if scope != "overall" else ""
    return (HEADER + anchors
            + f'::: {{#acc-lookup-{scope} .acc-lookup data-scope="{scope}" '
              f'data-src="../../assets/pages/accuracy_lookup_{scope}.json"}}\n'
              "The search below needs JavaScript. Every figure is also in the files under "
              f"[{'Download' if scope == 'overall' else 'Every other item'}](#all-items).\n"
              ":::\n\n"
              '<link rel="stylesheet" href="../../assets/css/accuracy-lookup.css">\n'
              '<script src="../../assets/js/accuracy-lookup.js" defer></script>\n')


def dump_json(payload: dict) -> str:
    return json.dumps(payload, ensure_ascii=False, separators=(",", ":")) + "\n"


# ---------------------------------------------------------------------------
# Registered numbers
# ---------------------------------------------------------------------------

RULES = (
    ("day_a", "first day (10 June 2026)", 10), ("day_b", "second day (14 June 2026)", 14),
    ("day_c", "third day (17 June 2026)", 17), ("year", "year of the three days", 2026),
    ("w60", "seconds of the accuracy label", 60), ("w30", "seconds, share within 30 s", 30),
    ("w300", "seconds, share within 300 s", 300), ("high", "threshold of high and good, %", 80),
    ("half", "threshold of partial and fair, %", 50), ("min_n", "fewest movements for a label", 30),
    ("top", "airports per list", 100), ("ten_pct", "share of the route length, %", 10),
    ("stable_points", "points a stable share moves less than", 5),
    ("stable_s", "seconds a stable time moves less than", 10),
    ("ground_pct", "ground reception threshold, %", GROUND_NEARLY_ABSENT_PCT),
    ("match_h", "hours within which a record is matched", 6), ("bits", "bits of the aircraft address", 24),
    ("ring40", "radius, NM", 40), ("ring100", "radius, NM", 100),
)


def numbers(items: list[dict], spread: list[dict], eu: dict, world: dict) -> list[list]:
    out = []
    for page in ("accuracy/index", "accuracy/europe", "accuracy/world", "limitations"):
        out += [[page, mid, label, value, "", "rule"] for mid, label, value in RULES]
    pooled = {(r["item"], r["reference"]): r for r in items if r["days"] == POOLED}
    first = pooled[("flight list: FIRST_SEEN", "no reference")]
    per_day = [r for r in items if r["item"] == "flight list: FIRST_SEEN" and r["days"] != POOLED]
    src_items = str(ACC / "item_accuracy.csv")
    out.append(["accuracy/index", "items", "items in the overall table", len({r["item"] for r in items}), "",
                src_items])
    out.append(["accuracy/index", "flights", "flight-list rows, three days", round(num(first["flights_eligible"])),
                "", src_items])
    for r in per_day:
        out.append(["accuracy/index", f"flights_{r['days']}", f"flight-list rows on {r['days']}",
                    round(num(r["flights_eligible"])), "", src_items])
    out.append(["accuracy/index", "stable", "day-to-day measures that are stable",
                sum(s["stable"] == "True" for s in spread), len(spread), str(ACC / "day_spread.csv")])

    def add(page, mid, label, item, ref, column, transform=None):
        v = num(pooled[(item, ref)][column])
        if transform:
            v = transform(v)
        out.append([page, mid, label, round(v, 1), "", src_items])

    for page in ("accuracy/index", "limitations"):
        add(page, "atot_ref", "recorded take-offs with an ATOT, %", "ATOT", "airport records", "availability_ref_pct")
        add(page, "atot_w60", "ATOT within 60 s, %", "ATOT", "airport records", "within_60s_pct")
        add(page, "aldt_ref", "recorded landings with an ALDT, %", "ALDT", "airport records", "availability_ref_pct")
        add(page, "aldt_w60", "ALDT within 60 s, %", "ALDT", "airport records", "within_60s_pct")
        add(page, "aobt_ref", "recorded off-blocks with an AOBT, %", "AOBT", "airport records",
            "availability_ref_pct")
        add(page, "aobt_w60", "AOBT within 60 s, %", "AOBT", "airport records", "within_60s_pct")
        add(page, "aibt_ref", "recorded in-blocks with an AIBT, %", "AIBT", "airport records",
            "availability_ref_pct")
        add(page, "aibt_w60", "AIBT within 60 s, %", "AIBT", "airport records", "within_60s_pct")
    lim = "limitations"
    add(lim, "adep_missing", "flights without adep, %", "flight list: ADEP", "Network Manager",
        "availability_own_pct", lambda v: 100 - v)
    add(lim, "ades_missing", "flights without ades, %", "flight list: ADES", "Network Manager",
        "availability_own_pct", lambda v: 100 - v)
    add(lim, "type_missing", "NM flights without a type, %", "flight list: TYPECODE", "Network Manager",
        "availability_ref_pct", lambda v: 100 - v)
    for m in ("acdm", "legacy"):
        add(lim, f"atot_{m}_w60", f"ATOT ({m}) within 60 s, %", f"ATOT ({m})", "airport records", "within_60s_pct")
        add(lim, f"aldt_{m}_w60", f"ALDT ({m}) within 60 s, %", f"ALDT ({m})", "airport records", "within_60s_pct")
    add(lim, "aibt_median", "AIBT median difference, s", "AIBT", "airport records", "median_abs_diff_s")
    add(lim, "aibt_mean_earlier", "AIBT: how much earlier than the record on average, within 300 s, s", "AIBT",
        "airport records", "mean_signed_diff_within_300s_s", lambda v: -v)
    add(lim, "aobt_median", "AOBT median difference, s", "AOBT", "airport records", "median_abs_diff_s")
    add(lim, "aobt_mean_signed", "AOBT mean signed difference within 300 s, s", "AOBT", "airport records",
        "mean_signed_diff_within_300s_s")

    src_eu = str(ACC / "airport_accuracy_europe.csv")
    for key in ("airports", "with_records", "without_records", "ground", "no_times"):
        for page in ("accuracy/europe", "limitations", "accuracy/index"):
            out.append([page, f"eu_{key}", f"European airports: {key}", eu[key], "", src_eu])
    for anchor, c in eu["stats"].items():
        for lab, n in c.items():
            out.append(["accuracy/europe", f"eu_{anchor}_{lab}", f"airports labelled {lab} for {anchor}", n, "",
                        src_eu])
    src_w = str(ACC / "airport_accuracy_world.csv")
    for key in ("airports", "outside", "in_europe", "with_records"):
        out.append(["accuracy/world", f"world_{key}", f"world airports: {key}", world[key], "", src_w])
    return out


# ---------------------------------------------------------------------------
# Command line
# ---------------------------------------------------------------------------


def build(root: Path) -> dict[Path, str]:
    items = read(root, "item_accuracy.csv")
    spread = read(root, "day_spread.csv")
    eu_rows = read(root, "airport_accuracy_europe.csv")
    world_rows = read(root, "airport_accuracy_world.csv")
    airports = read(root, "airports.csv")
    pages = overall_tables(items, spread)
    eu_pages, eu = europe_tables(eu_rows, airports)
    world_pages, world = world_tables(world_rows)
    pages.update(eu_pages)
    pages.update(world_pages)
    labels = pages.pop("algorithms_labels")
    wanted = {root / OUT_DIR / f"{name}.md": HEADER + text for name, text in pages.items()}
    wanted[root / ALGORITHMS_LABELS] = HEADER + labels
    out = io.StringIO()
    writer = csv.writer(out, lineterminator="\n")
    writer.writerow(NUMBER_COLUMNS)
    writer.writerows(numbers(items, spread, eu, world))
    wanted[root / NUMBERS_CSV] = out.getvalue()
    payloads = {"europe": airport_payload(eu_rows, airports, "europe"),
                "world": airport_payload(world_rows, airports, "world"),
                "overall": overall_payload(items)}
    for scope, payload in payloads.items():
        wanted[root / LOOKUP_JSON.format(scope=scope)] = dump_json(payload)
        wanted[root / OUT_DIR / f"{scope}_lookup.md"] = lookup_include(scope)
    return wanted


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--check", action="store_true", help="only report whether files are out of date")
    args = parser.parse_args(argv)
    wanted = build(args.root)
    stale = [p for p, t in wanted.items() if not p.is_file() or p.read_text(encoding="utf-8") != t]
    # Generated Markdown that is no longer wanted.
    unwanted = sorted(p for p in (args.root / OUT_DIR).glob("*.md")
                      if p not in wanted and p.read_text(encoding="utf-8").startswith(HEADER))
    if args.check:
        for p in stale + unwanted:
            print(f"out of date: {p.relative_to(args.root)}")
        return 1 if stale or unwanted else 0
    for path, content in wanted.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
    for path in unwanted:
        path.unlink()
    print(f"wrote {len(wanted)} files, removed {len(unwanted)} stale ones")
    return 0


if __name__ == "__main__":
    sys.exit(main())
