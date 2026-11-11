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


#: Registered functions that record numbers without drawing a chart.
NUMBERS_ONLY: set[str] = set()


def measure_set(page: str, name: str = "numbers"):
    """Register a function that returns a page's numbers without drawing a chart.

    For a number quoted on a methods page that has no figure. The function
    reads committed study files and returns ``measure(...)`` rows, exactly
    like a figure function, but saves no SVG.
    """
    def register(function):
        figure(page, name)(function)
        NUMBERS_ONLY.add(f"{page}_{name}")
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


@figure("holding", "score_by_airport")
def holding_score_by_airport(root: Path) -> list[dict]:
    """Precision, recall and F1 of the holding rule at each labelled airport."""
    score_file = f"{HOLDING_DIR}/holding_score_2019-10-01.csv"
    sweep_file = f"{HOLDING_DIR}/holding_sweep_2019-10-01.csv"
    episodes_file = f"{HOLDING_DIR}/holding_episodes_2019-10-01.csv"
    score = read_csv(root, score_file).set_index("airport")
    sweep = read_csv(root, sweep_file)
    episodes = read_csv(root, episodes_file)

    names = {"EGLL": "London Heathrow (EGLL)", "EIDW": "Dublin (EIDW)", "ALL": "Both airports"}
    order = ["EGLL", "EIDW", "ALL"]
    n_all = int(score.loc["ALL", "n_flights"])
    fig, ax = new_figure(f"Holding detection against hand-corrected labels (n = {n_all:,} flights)")
    width = 0.26
    colours = {"precision": ACCENT, "recall": "#6f8fd0", "f1": "#333333"}
    labels = {"precision": "Precision", "recall": "Recall", "f1": "F1 score"}
    for i, key in enumerate(("precision", "recall", "f1")):
        xs = [j + (i - 1) * width for j in range(len(order))]
        values = [float(score.loc[a, key]) for a in order]
        ax.bar(xs, values, width=width, color=colours[key], label=labels[key])
        for x, v in zip(xs, values):
            ax.annotate(f"{v:.2f}", xy=(x, v), xytext=(0, 2), textcoords="offset points",
                        ha="center", fontsize=8, color="#333333")
    ax.set_xticks(range(len(order)))
    ax.set_xticklabels([f"{names[a]}\n{int(score.loc[a, 'n_flights'])} flights" for a in order], fontsize=9)
    ax.set_ylim(0, 1.05)
    ax.set_ylabel("Score (0 to 1, 1 is perfect)")
    ax.legend(frameon=False, fontsize=8, loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=3)
    save(fig, root / OUT_DIR / "holding_score_by_airport.svg")

    common = dict(page="holding", reference=HOLDING_REFERENCE, source_file=score_file,
                  item="holding-start;holding-end", kind="event")
    rows = []
    for a in ("EGLL", "EIDW"):
        n = int(score.loc[a, "n_flights"])
        for key, word in (("precision", "Precision"), ("recall", "Recall"), ("f1", "F1 score")):
            rows.append(measure(measure_id=f"{key}_{a.lower()}", value=f"{float(score.loc[a, key]):.2f}",
                                n=n, label=f"{word} at {names[a]}",
                                source_field=f"{key}, row airport = {a}", **common))
        rows.append(measure(measure_id=f"episodes_{a.lower()}", value=int(score.loc[a, "n_episodes"]), n=n,
                            label=f"Holding episodes detected at {names[a]}",
                            source_field=f"n_episodes, row airport = {a}", **common))
    rows.append(measure(measure_id="positive_windows", value=int(score.loc["ALL", "n_positive"]),
                        n=int(score.loc["ALL", "n_windows"]),
                        label="Six-minute windows labelled as holding, out of all windows scored",
                        source_field="n_positive and n_windows, row airport = ALL", **common))
    rows.append(measure(measure_id="f1_any_touch", value=f"{float(score.loc['ALL', 'f1_any_overlap']):.2f}",
                        n=n_all, label="F1 score when a window counts as detected as soon as it touches an episode",
                        source_field="f1_any_overlap, row airport = ALL", **common))
    # Best F1 of the sweep at each airport, against the score of the default setting.
    gains = []
    for a in ("EGLL", "EIDW"):
        best = float(sweep[sweep["airport"] == a]["f1"].max())
        gains.append(best - float(score.loc[a, "f1"]))
    n_points = int(sweep.groupby("airport").size().max())
    rows.append(measure(page="holding", measure_id="sweep_points", value=n_points, n=n_points,
                        label="Settings of the three main thresholds tried at each airport",
                        reference=HOLDING_REFERENCE, source_file=sweep_file,
                        source_field="rows per airport", item="holding-start;holding-end", kind="event"))
    rows.append(measure(page="holding", measure_id="sweep_best_gain", value=f"{max(gains):.2f}",
                        n=n_points, label="Largest gain in F1 score of any tried setting over the default, at either airport",
                        reference=HOLDING_REFERENCE, source_file=sweep_file,
                        source_field="max(f1) per airport minus f1 of holding_score, row of that airport",
                        item="holding-start;holding-end", kind="event"))
    minutes = episodes["duration_s"] / 60.0
    for q, word in ((0.25, "Lower quartile"), (0.75, "Upper quartile")):
        rows.append(measure(page="holding", measure_id=f"episode_q{int(q * 100)}_min",
                            value=f"{float(minutes.quantile(q)):.1f}", unit="min", n=len(episodes),
                            label=f"{word} of the length of a detected holding episode",
                            reference=HOLDING_REFERENCE, source_file=episodes_file,
                            source_field=f"quantile {q} of duration_s / 60",
                            item="holding-start;holding-end", kind="event"))
    racetracks = int((episodes["pattern"] == "racetrack").sum())
    rows.append(measure(page="holding", measure_id="racetracks", value=racetracks, n=len(episodes),
                        label="Detected episodes classed as racetrack patterns (the rest are single orbits)",
                        reference=HOLDING_REFERENCE, source_file=episodes_file,
                        source_field="count of pattern = racetrack", item="holding-start;holding-end", kind="event"))
    rows.append(measure(page="holding", measure_id="orbits", value=len(episodes) - racetracks, n=len(episodes),
                        label="Detected episodes classed as single orbits",
                        reference=HOLDING_REFERENCE, source_file=episodes_file,
                        source_field="count of pattern = orbit", item="holding-start;holding-end", kind="event"))
    return rows


# --- Track identity ----------------------------------------------------------

TRACK_DIR = "papers/track-construction-v5/data"
TRACK_REFERENCE = ("EUROCONTROL Network Manager flight data for Europe, "
                   "2025-06-14 and 2026-06-14")


@figure("track-identity", "one_track")
def track_identity_one_track(root: Path) -> list[dict]:
    """Share of reference flights that come out as exactly one track."""
    src = f"{TRACK_DIR}/two_days/link_recommended.csv"
    r = read_csv(root, src).iloc[0]

    groups = [
        ("Flights crossing the edge\nof the European area", "crossing_share_one_track",
         "base_crossing_share_one_track", "crossing_flights"),
        ("All other flights", "other_share_one_track", "base_other_share_one_track", "other_flights"),
        ("All flights", "share_one_track", "base_share_one_track", "flights"),
    ]
    n = int(r["flights"])
    fig, ax = new_figure(f"Flights that come out as one track (n = {n:,} flights)")
    width = 0.36
    for j, (name, new, base, count) in enumerate(groups):
        for offset, key, colour in ((-width / 2, base, MUTED), (width / 2, new, ACCENT)):
            v = 100 * float(r[key])
            ax.bar(j + offset, v, width=width, color=colour)
            ax.annotate(f"{v:.1f}%", xy=(j + offset, v), xytext=(0, 2), textcoords="offset points",
                        ha="center", fontsize=8, color="#333333")
    ax.set_xticks(range(len(groups)))
    ax.set_xticklabels([f"{g[0]}\n({int(r[g[3]]):,})" for g in groups], fontsize=9)
    ax.set_ylim(0, 105)
    ax.set_ylabel("Flights in one track (%)")
    from matplotlib.patches import Patch
    ax.legend(handles=[Patch(color=ACCENT, label="Published rule (keeps a track across a gap that fits one flight)"),
                       Patch(color=MUTED, label="Same rule, but every gap over 30 min starts a new track")],
              frameon=False, fontsize=8, loc="upper center", bbox_to_anchor=(0.5, -0.28), ncol=1)
    save(fig, root / OUT_DIR / "track-identity_one_track.svg")

    common = dict(page="track-identity", reference=TRACK_REFERENCE, source_file=src,
                  item="flight_id", kind="flight list column")
    checkable = int(r["linked_true_link"] + r["linked_false_join"])
    rows = [
        measure(measure_id="one_track_all", value=f"{100 * float(r['share_one_track']):.1f}", unit="%",
                n=n, headline=True,
                label="Share of reference flights that come out as exactly one track",
                source_field="share_one_track", **common),
        measure(measure_id="one_track_crossing", value=f"{100 * float(r['crossing_share_one_track']):.1f}",
                unit="%", n=int(r["crossing_flights"]),
                label="Share of flights crossing the edge of the European area that come out as one track",
                source_field="crossing_share_one_track", **common),
        measure(measure_id="one_track_other", value=f"{100 * float(r['other_share_one_track']):.1f}",
                unit="%", n=int(r["other_flights"]),
                label="Share of all other flights that come out as one track",
                source_field="other_share_one_track", **common),
        measure(measure_id="base_one_track_all", value=f"{100 * float(r['base_share_one_track']):.1f}",
                unit="%", n=n,
                label="Share of all flights in one track when every gap over 30 min starts a new track",
                source_field="base_share_one_track", **common),
        measure(measure_id="base_one_track_crossing",
                value=f"{100 * float(r['base_crossing_share_one_track']):.1f}", unit="%",
                n=int(r["crossing_flights"]),
                label="Share of crossing flights in one track when every gap over 30 min starts a new track",
                source_field="base_crossing_share_one_track", **common),
        measure(measure_id="tracks_per_flight", value=f"{float(r['tracks_per_flight']):.2f}", n=n,
                label="Tracks per reference flight",
                source_field="tracks_per_flight", **common),
        measure(measure_id="no_track", value=int(r["flights_with_no_track"]), n=n,
                label="Reference flights with no track at all (counted as not in one track)",
                source_field="flights_with_no_track", **common),
        measure(measure_id="holes_linked", value=int(r["real_holes_linked"]), n=int(r["real_holes"]),
                label="Gaps of more than 30 min kept inside one track, out of all such gaps",
                source_field="real_holes_linked and real_holes", **common),
        measure(measure_id="holes_checkable", value=checkable, n=int(r["real_holes_linked"]),
                label="Kept gaps that the reference data can check",
                source_field="linked_true_link + linked_false_join", **common),
        measure(measure_id="holes_unchecked", value=f"{100 * float(r['linked_unknown_share']):.1f}", unit="%",
                n=int(r["real_holes_linked"]),
                label="Share of kept gaps that the reference data cannot check (outside Europe)",
                source_field="linked_unknown_share", **common),
        measure(measure_id="false_joins", value=int(r["linked_false_join"]), n=checkable,
                label="Kept gaps where the aircraft in fact landed and took off again (false joins)",
                source_field="linked_false_join", **common),
        measure(measure_id="false_join_rate", value=f"{100 * float(r['real_false_join_rate']):.3f}", unit="%",
                n=checkable, label="Share of checkable kept gaps that are false joins",
                source_field="real_false_join_rate", **common),
        measure(measure_id="hidden_landings_joined", value=int(r["negative_limit_false_joins"]),
                n=int(r["negative_limit_controls"]),
                label="Artificial gaps hiding a real landing that were wrongly kept in one track",
                source_field="negative_limit_false_joins and negative_limit_controls", **common),
        measure(measure_id="hidden_landings_upper", value=f"{100 * float(r['negative_false_join_upper_95']):.4f}",
                unit="%", n=int(r["negative_limit_controls"]),
                label="Upper 95% confidence bound on the share of hidden landings wrongly kept in one track",
                source_field="negative_false_join_upper_95", **common),
        measure(measure_id="cut_flights_rejoined",
                value=f"{100 * float(r['positive_linked']) / float(r['positive_controls']):.1f}", unit="%",
                n=int(r["positive_controls"]),
                label="Share of artificially cut flights (60 to 360 min removed) that are joined again",
                source_field="positive_linked / positive_controls", **common),
    ]
    return rows


# --- Trajectory cleaning -----------------------------------------------------

DECIMATION_DIR = "papers/statevector-decimation/data"
AIRPORTS_DIR = "papers/adep-ades-detection-v7/data"
CLEANING_REFERENCE = ("EUROCONTROL Network Manager movement data, "
                      "2025-06-05 to 2025-06-07 and 2024-06-05 to 2024-06-07")


@figure("trajectory-cleaning", "position_age")
def trajectory_cleaning_position_age(root: Path) -> list[dict]:
    """How often the position in a state vector is old, by altitude band."""
    src = f"{DECIMATION_DIR}/age_by_altitude.csv"
    age = read_csv(root, src)
    age = age[age["band"] != "(null)"].reset_index(drop=True)
    n = int(age["n"].sum())
    fig, ax = new_figure(f"Age of the position, by altitude (n = {n:,})")
    ys = range(len(age))
    ax.barh(list(ys), 100 * age["share_gt30s"], color=ACCENT, label="Position more than 30 s old")
    ax.barh(list(ys), 100 * (age["share_gt5s"] - age["share_gt30s"]), left=100 * age["share_gt30s"],
            color="#9fb3e0", label="Position 5 to 30 s old")
    ax.set_yticks(list(ys))
    ax.set_yticklabels([f"{b} ft" for b in age["band"]], fontsize=9)
    ax.invert_yaxis()
    ax.set_xlabel("State vectors (%)")
    ax.set_ylabel("Barometric altitude band")
    ax.legend(frameon=False, fontsize=8, loc="lower right")
    save(fig, root / OUT_DIR / "trajectory-cleaning_position_age.svg")

    ref_age = "OpenSky state vectors, 2025-06-05 06:00 to 09:00 UTC, European area"
    low = age.iloc[0]
    rows = [
        measure(page="trajectory-cleaning", measure_id="age_gt30_low",
                value=f"{100 * float(low['share_gt30s']):.1f}", unit="%", n=int(low["n"]),
                label=f"Share of state vectors between {low['band']} ft whose position is more than 30 s old",
                reference=ref_age, source_file=src, source_field=f"share_gt30s, row band = {low['band']}"),
    ]
    high = age.iloc[-1]
    rows.append(measure(page="trajectory-cleaning", measure_id="age_gt30_high",
                        value=f"{100 * float(high['share_gt30s']):.1f}", unit="%", n=int(high["n"]),
                        label=f"Share of state vectors in the highest band ({high['band']} ft) whose position is more than 30 s old",
                        reference=ref_age, source_file=src,
                        source_field=f"share_gt30s, row band = {high['band']}"))

    timing_file = f"{DECIMATION_DIR}/timing.csv"
    timing = read_csv(root, timing_file).set_index("rule")
    ratio = float(timing.loc["bucket", "rows"]) / float(timing.loc["modulo", "rows"])
    rows.append(measure(page="trajectory-cleaning", measure_id="thinning_ratio", value=f"{ratio:.3f}",
                        n=int(timing.loc["bbox_only", "rows"]),
                        label="Rows kept by the 5 s thinning used today, relative to keeping only seconds divisible by 5",
                        reference=ref_age, source_file=timing_file,
                        source_field="rows, rule = bucket / rows, rule = modulo"))

    for period in ("2025", "2024"):
        lad_file = f"{AIRPORTS_DIR}/ladder_{period}.csv"
        lad = read_csv(root, lad_file).set_index("run")
        raw, clean = lad.loc["L09_endpoint_radius"], lad.loc["L10_clean_tracks"]
        n_gt = int(clean["n_ground_truth"])
        common = dict(page="trajectory-cleaning", n=n_gt, reference=CLEANING_REFERENCE,
                      source_file=lad_file, item="", kind="")
        for side, word in (("raw", "without cleaning"), ("clean", "with cleaning")):
            row = raw if side == "raw" else clean
            run = "L09_endpoint_radius" if side == "raw" else "L10_clean_tracks"
            rows.append(measure(measure_id=f"ades_wrong_share_{side}_{period}",
                                value=f"{100 * (1 - float(row['ades_accuracy'])):.1f}", unit="%",
                                label=f"Share of named destination airports that are wrong, {word}, {period} sample",
                                source_field=f"1 - ades_accuracy, run = {run}", **common))
            rows.append(measure(measure_id=f"ades_wrong_{side}_{period}", value=int(row["ades_wrong"]),
                                label=f"Wrong destination airports, {word}, {period} sample",
                                source_field=f"ades_wrong, run = {run}", **common))
            rows.append(measure(measure_id=f"ades_coverage_{side}_{period}",
                                value=f"{100 * float(row['ades_coverage']):.1f}", unit="%",
                                label=f"Share of reference arrivals given a destination airport, {word}, {period} sample",
                                source_field=f"ades_coverage, run = {run}", **common))
    return rows


# --- Departure and destination airports -------------------------------------

AIRPORTS_REFERENCE = ("EUROCONTROL Network Manager movement data, 2025-06-05 to 2025-06-07, "
                      "matched on aircraft address, call sign and date")


@figure("airports", "by_type")
def airports_by_type(root: Path) -> list[dict]:
    """Right, wrong and unanswered airports by kind of airport, for departures and arrivals."""
    modes_file = f"{AIRPORTS_DIR}/modes_2025.csv"
    type_file = f"{AIRPORTS_DIR}/per_type_2025.csv"
    modes = read_csv(root, modes_file).set_index("run")
    types = read_csv(root, type_file)
    types = types[types["run"] == "shipped"]
    s = modes.loc["shipped"]
    n = int(s["n_ground_truth"])

    kinds = [("large_airport", "Large airports"), ("medium_airport", "Medium airports"),
             ("small_airport", "Small airfields"), ("heliport", "Heliports"),
             ("out of area", "Outside the area")]
    fig, ax = new_figure(f"Airports named, by kind of airport (n = {n:,} flights)")
    labels, right, wrong, none = [], [], [], []
    for role, role_name in (("departures", "departure"), ("arrivals", "arrival")):
        for key, name in kinds:
            row = types[(types["role"] == role) & (types["apt_type"] == key)].iloc[0]
            t = float(row["n_truth"])
            labels.append(f"{name}, {role_name} ({int(t):,})")
            right.append(100 * row["n_correct"] / t)
            wrong.append(100 * (row["n_answered"] - row["n_correct"]) / t)
            none.append(100 * (t - row["n_answered"]) / t)
    ys = list(range(len(labels)))
    ax.barh(ys, right, color=ACCENT, label="Right airport")
    ax.barh(ys, wrong, left=right, color="#d95f02", label="Wrong airport")
    ax.barh(ys, none, left=[a + b for a, b in zip(right, wrong)], color=GRID, label="No airport given")
    ax.set_yticks(ys)
    ax.set_yticklabels(labels, fontsize=8)
    ax.invert_yaxis()
    ax.set_xlim(0, 100)
    ax.set_xlabel("Reference flights of that kind (%)")
    ax.legend(frameon=False, fontsize=8, loc="lower right", bbox_to_anchor=(1.0, -0.38), ncol=3)
    save(fig, root / OUT_DIR / "airports_by_type.svg")

    common = dict(page="airports", n=n, reference=AIRPORTS_REFERENCE, source_file=modes_file,
                  kind="flight list column")
    rows = [
        measure(measure_id="adep_accuracy", value=f"{100 * float(s['adep_accuracy']):.1f}", unit="%",
                headline=True, item="adep",
                label="Departure airport: share of the airports named that are right (June 2025 sample)",
                source_field="adep_accuracy, run = shipped", **common),
        measure(measure_id="adep_coverage", value=f"{100 * float(s['adep_coverage']):.1f}", unit="%",
                item="adep", label="Departure airport: share of reference flights given an answer (June 2025 sample)",
                source_field="adep_coverage, run = shipped", **common),
        measure(measure_id="ades_accuracy", value=f"{100 * float(s['ades_accuracy']):.1f}", unit="%",
                headline=True, item="ades",
                label="Destination airport: share of the airports named that are right (June 2025 sample)",
                source_field="ades_accuracy, run = shipped", **common),
        measure(measure_id="ades_coverage", value=f"{100 * float(s['ades_coverage']):.1f}", unit="%",
                item="ades", label="Destination airport: share of reference flights given an answer (June 2025 sample)",
                source_field="ades_coverage, run = shipped", **common),
        measure(measure_id="adep_wrong", value=int(s["adep_wrong"]), item="adep",
                label="Departure airports named wrongly", source_field="adep_wrong, run = shipped", **common),
        measure(measure_id="ades_wrong", value=int(s["ades_wrong"]), item="ades",
                label="Destination airports named wrongly", source_field="ades_wrong, run = shipped", **common),
        measure(measure_id="ooa_share", value=f"{100 * float(s['adep_gt_ooa_share']):.1f}", unit="%",
                item="adep;adep_source",
                label="Share of reference flights that started outside the area covered",
                source_field="adep_gt_ooa_share, run = shipped", **common),
        measure(measure_id="ooa_precision", value=f"{100 * float(s['adep_ooa_precision']):.1f}", unit="%",
                item="adep;adep_source", **{**common, "n": round(float(s["adep_pred_ooa_share"]) * n)},
                label="Departures marked as starting outside the area: share that truly did",
                source_field="adep_ooa_precision, run = shipped"),
        measure(measure_id="ooa_recall", value=f"{100 * float(s['adep_ooa_recall']):.1f}", unit="%",
                item="adep;adep_source", **{**common, "n": round(float(s["adep_gt_ooa_share"]) * n)},
                label="Departures that truly started outside the area: share marked as such",
                source_field="adep_ooa_recall, run = shipped"),
    ]
    for role, col in (("departures", "adep"), ("arrivals", "ades")):
        for key, name in kinds[:2]:
            row = types[(types["role"] == role) & (types["apt_type"] == key)].iloc[0]
            rows.append(measure(page="airports", measure_id=f"{col}_accuracy_{key.split('_')[0]}",
                                value=f"{100 * float(row['accuracy']):.1f}", unit="%", n=int(row["n_truth"]),
                                label=f"{name}, {role}: share of the airports named that are right",
                                reference=AIRPORTS_REFERENCE, source_file=type_file,
                                source_field=f"accuracy, run = shipped, role = {role}, apt_type = {key}",
                                item=col, kind="flight list column"))
    small = types[types["apt_type"].isin(["small_airport", "heliport"])]
    rows.append(measure(page="airports", measure_id="small_correct", value=int(small["n_correct"].sum()),
                        n=int(small["n_truth"].sum()),
                        label="Flights from or to small airfields and heliports whose airport is named rightly",
                        reference=AIRPORTS_REFERENCE, source_file=type_file,
                        source_field="sum of n_correct and n_truth, run = shipped, apt_type in small_airport, heliport",
                        item="adep;ades", kind="flight list column"))

    m24_file = f"{AIRPORTS_DIR}/modes_2024.csv"
    s24 = read_csv(root, m24_file).set_index("run").loc["shipped"]
    ref24 = AIRPORTS_REFERENCE.replace("2025-06-05 to 2025-06-07", "2024-06-05 to 2024-06-07")
    for col, word in (("adep", "Departure"), ("ades", "Destination")):
        for q, qword in (("accuracy", "share of the airports named that are right"),
                         ("coverage", "share of reference flights given an answer")):
            rows.append(measure(page="airports", measure_id=f"{col}_{q}_2024",
                                value=f"{100 * float(s24[f'{col}_{q}']):.1f}", unit="%",
                                n=int(s24["n_ground_truth"]),
                                label=f"{word} airport, second sample (June 2024): {qword}",
                                reference=ref24, source_file=m24_file,
                                source_field=f"{col}_{q}, run = shipped", item=col, kind="flight list column"))

    datum_file = "papers/adep-ades-detection-v6.2/data/datum_swap_2025.csv"
    d = read_csv(root, datum_file).set_index("run").loc["datum_field"]
    for q, qword in (("accuracy", "share of the airports named that are right"),
                     ("coverage", "share of reference flights given an answer")):
        rows.append(measure(page="airports", measure_id=f"ades_{q}_field",
                            value=f"{100 * float(d[f'ades_{q}']):.1f}", unit="%", n=int(d["n_ground_truth"]),
                            label=f"Destination airport with the altitude limit measured above the airport ({float(d['trend_max_height_ft']):,.0f} ft): {qword}",
                            reference=AIRPORTS_REFERENCE, source_file=datum_file,
                            source_field=f"ades_{q}, run = datum_field", item="ades", kind="flight list column"))
    return rows


# --- Point merge -------------------------------------------------------------

PMS_DIR = "papers/airspace-events-v1/data/point_merge"
PMS_REFERENCE = "Visual inspection of 20 detected and 20 undetected arrivals at Dublin (EIDW), 2026-06-14"


@figure("point-merge", "visual_check")
def point_merge_visual_check(root: Path) -> list[dict]:
    """Detected and undetected Dublin arrivals, as judged by eye."""
    src = f"{PMS_DIR}/eidw_visual_check.csv"
    check = read_csv(root, src)
    check["detected"] = check["detected"].astype(str).str.lower() == "true"
    n = len(check)
    groups = [(True, "Leg detected"), (False, "No leg detected")]
    verdicts = [("yes", "A sequencing leg is visible", ACCENT), ("unsure", "Unclear", MUTED),
                ("no", "No sequencing leg is visible", GRID)]
    fig, ax = new_figure(f"Dublin arrivals plotted and judged by eye (n = {n} arrivals)")
    for j, (flag, name) in enumerate(groups):
        part = check[check["detected"] == flag]
        left = 0
        for key, label, colour in verdicts:
            k = int((part["judged_real_leg"] == key).sum())
            ax.barh(j, k, left=left, color=colour, label=label if j == 0 else None, edgecolor="white")
            if k:
                ax.annotate(str(k), xy=(left + k / 2, j), ha="center", va="center", fontsize=9,
                            color="white" if colour == ACCENT else "#333333")
            left += k
    ax.set_yticks([0, 1])
    ax.set_yticklabels([f"{g[1]} ({int((check['detected'] == g[0]).sum())})" for g in groups], fontsize=9)
    ax.invert_yaxis()
    ax.set_xlabel("Arrivals (count)")
    ax.xaxis.get_major_locator().set_params(integer=True)
    ax.legend(frameon=False, fontsize=8, loc="lower right", bbox_to_anchor=(1.0, -0.42), ncol=3)
    save(fig, root / OUT_DIR / "point-merge_visual_check.svg")

    det = check[check["detected"]]
    und = check[~check["detected"]]
    items = "pms-leg-entry;pms-leg-exit;pms-merge-point"
    common = dict(page="point-merge", reference=PMS_REFERENCE, source_file=src, item=items, kind="event")
    real = int((det["judged_real_leg"] == "yes").sum())
    rows = [
        measure(measure_id="detected_real_share", value=f"{100 * real / len(det):.1f}", unit="%", n=len(det),
                headline=True, label="Detected legs that show a real sequencing leg when plotted",
                source_field="share of judged_real_leg = yes among detected = True", **common),
        measure(measure_id="detected_real", value=real, n=len(det),
                label="Detected legs judged real", source_field="count judged_real_leg = yes, detected = True",
                **common),
        measure(measure_id="detected_unsure", value=int((det["judged_real_leg"] == "unsure").sum()), n=len(det),
                label="Detected legs judged unclear", source_field="count judged_real_leg = unsure, detected = True",
                **common),
        measure(measure_id="missed", value=int((und["judged_real_leg"] == "yes").sum()), n=len(und),
                label="Undetected arrivals that show a leg the rule missed",
                source_field="count judged_real_leg = yes, detected = False", **common),
        measure(measure_id="undetected_unsure", value=int((und["judged_real_leg"] == "unsure").sum()), n=len(und),
                label="Undetected arrivals judged unclear",
                source_field="count judged_real_leg = unsure, detected = False", **common),
    ]

    usage_file = f"{PMS_DIR}/point_merge_usage_2026-06-14.csv"
    usage = read_csv(root, usage_file)
    ref_day = "OPDI pipeline run over 2026-06-14, worldwide"
    used = usage[usage["arrivals"] > 0]
    common = dict(page="point-merge", reference=ref_day, source_file=usage_file, item=items, kind="event")
    rows += [
        measure(measure_id="airports_with_arrivals", value=len(used), n=len(usage),
                label="Airports in the merge point table that had arrivals that day",
                source_field="count of rows with arrivals > 0", **common),
        measure(measure_id="arrivals", value=int(usage["arrivals"].sum()),
                label="Arrivals at those airports", source_field="sum of arrivals", **common),
        measure(measure_id="arrivals_with_leg", value=int(usage["arrivals_with_leg"].sum()),
                n=int(usage["arrivals"].sum()),
                label="Arrivals with at least one sequencing leg", source_field="sum of arrivals_with_leg", **common),
        measure(measure_id="legs", value=int(usage["legs"].sum()), label="Sequencing legs detected",
                source_field="sum of legs", **common),
    ]
    for apt in ("EIDW", "LPPT", "ENGM"):
        row = usage[usage["apt_icao"] == apt].iloc[0]
        rows.append(measure(measure_id=f"share_{apt.lower()}", value=f"{float(row['leg_share_pct']):.1f}",
                            unit="%", n=int(row["arrivals"]),
                            label=f"Share of arrivals at {apt} with a sequencing leg",
                            source_field=f"leg_share_pct, row apt_icao = {apt}", **common))
        rows.append(measure(measure_id=f"legs_{apt.lower()}", value=int(row["arrivals_with_leg"]),
                            n=int(row["arrivals"]),
                            label=f"Arrivals at {apt} with a sequencing leg",
                            source_field=f"arrivals_with_leg, row apt_icao = {apt}", **common))
    return rows


# --- FIR crossings -----------------------------------------------------------

FIR_DIR = "papers/airspace-events-v1/data/fir"
FIR_REFERENCE = ("Exact point-in-polygon test against the same FIR boundaries "
                 "(AIRAC cycle 524), 2,000 tracks of 2026-06-14")


@figure("fir-crossings", "agreement")
def fir_crossings_agreement(root: Path) -> list[dict]:
    """Agreement of the grid lookup with the exact boundaries, for samples and for crossings."""
    src = f"{FIR_DIR}/fir_consistency_2026-06-14.csv"
    m = read_csv(root, src).set_index("metric")["value"]
    samples = int(m["samples"])
    h3x = int(m["h3_crossings"])
    matched = int(m["matched_crossings"])
    agree = float(m["agree_pct"])

    fig, ax = new_figure(f"Grid lookup against the exact FIR boundaries (n = {samples:,} positions)")
    bars = [(f"Positions\n({samples:,})", agree, "", 100 - agree),
            (f"Crossings\n({h3x:,})", 100 * matched / h3x, "", 100 - 100 * matched / h3x)]
    for j, (_, a, _, b) in enumerate(bars):
        ax.barh(j, a, color=ACCENT, label="Same region, or crossing of the same two regions" if j == 0 else None)
        ax.barh(j, b, left=a, color="#d95f02", label="Different" if j == 0 else None)
        ax.annotate(f"{a:.1f}%", xy=(a / 2, j), ha="center", va="center", fontsize=9, color="white")
    ax.set_yticks([0, 1])
    ax.set_yticklabels([b[0] for b in bars], fontsize=9)
    ax.invert_yaxis()
    ax.set_xlim(0, 100)
    ax.set_xlabel("Share (%)")
    ax.legend(frameon=False, fontsize=8, loc="lower right", bbox_to_anchor=(1.0, -0.42), ncol=2)
    save(fig, root / OUT_DIR / "fir-crossings_agreement.svg")

    common = dict(page="fir-crossings", reference=FIR_REFERENCE, source_file=src, item="xing-fir", kind="event")
    return [
        measure(measure_id="crossing_match", value=f"{100 * matched / h3x:.1f}", unit="%", n=h3x, headline=True,
                label="Detected crossings that match a crossing of the same two regions found with the exact boundaries",
                source_field="matched_crossings / h3_crossings", **common),
        measure(measure_id="matched", value=matched, n=h3x, label="Detected crossings with a match",
                source_field="matched_crossings and h3_crossings", **common),
        measure(measure_id="agree", value=f"{agree:.2f}", unit="%", n=samples,
                label="Positions placed in the same region by the grid lookup and the exact boundaries",
                source_field="agree_pct", **common),
        measure(measure_id="agree_both", value=f"{float(m['agree_where_both_pct']):.2f}", unit="%", n=samples,
                label="The same, counting only positions that both methods place in some region",
                source_field="agree_where_both_pct", **common),
        measure(measure_id="null_grid", value=f"{float(m['h3_null_pct']):.2f}", unit="%", n=samples,
                label="Positions in no region with the grid lookup", source_field="h3_null_pct", **common),
        measure(measure_id="null_exact", value=f"{float(m['exact_null_pct']):.2f}", unit="%", n=samples,
                label="Positions in no region with the exact boundaries", source_field="exact_null_pct", **common),
        measure(measure_id="timing_p50", value=f"{float(m['timing_error_p50_s']):.0f}", unit="s", n=matched,
                label="Median time difference between a matched crossing and its exact counterpart",
                source_field="timing_error_p50_s", **common),
        measure(measure_id="timing_p90", value=f"{float(m['timing_error_p90_s']):.0f}", unit="s", n=matched,
                label="90th percentile of that time difference", source_field="timing_error_p90_s", **common),
        measure(measure_id="timing_max", value=f"{float(m['timing_error_max_s']):.0f}", unit="s", n=matched,
                label="Largest time difference (a boundary crossed twice, matched to the wrong one)",
                source_field="timing_error_max_s", **common),
        measure(measure_id="tracks", value=int(m["tracks"]), n=int(m["tracks"]),
                label="Tracks in the check", source_field="tracks", **common),
    ]


# --- Measurements ------------------------------------------------------------

@measure_set("measurements")
def measurements_numbers(root: Path) -> list[dict]:
    """Checks of the cumulative distance on tracks that hold a reception gap."""
    src = f"{TRACK_DIR}/pipeline_day/link_distance_across_hole.csv"
    d = read_csv(root, src).iloc[0]
    ref = "Straight line between the two edges of each gap, OPDI pipeline run of 2026-06-14 (one aircraft in ten)"
    common = dict(page="measurements", reference=ref, source_file=src, item="distance_flown_nm",
                  kind="measurement")
    compared = int(d["holes_compared"])
    return [
        measure(measure_id="holes_compared", value=compared, n=int(d["holes_scored"]),
                label="Gaps inside a track where the distance across the gap was compared",
                source_field="holes_compared and holes_scored", **common),
        measure(measure_id="holes_off", value=int(d["holes_off_by_over_1_nm"]), n=compared,
                label="Gaps where the distance added differs from the straight line by more than 1 NM",
                source_field="holes_off_by_over_1_nm", **common),
        measure(measure_id="tracks_checked", value=int(d["tracks_checked_against_stored_distance"]),
                n=int(d["scored_tracks"]),
                label="Tracks with a kept gap whose cumulative distance was compared with an independent recomputation",
                source_field="tracks_checked_against_stored_distance", **common),
        measure(measure_id="tracks_off", value=int(d["tracks_off_stored_distance_by_over_1_nm"]),
                n=int(d["tracks_checked_against_stored_distance"]),
                label="Of those, tracks whose published distance differs by more than 1 NM",
                source_field="tracks_off_stored_distance_by_over_1_nm", **common),
    ]


# --- First and last seen -----------------------------------------------------

@measure_set("first-last-seen")
def first_last_seen_numbers(root: Path) -> list[dict]:
    """How old the position of the first and last state vector of an aircraft is."""
    src = f"{DECIMATION_DIR}/endpoint_age.csv"
    e = read_csv(root, src).set_index("endpoint")
    ref = "OpenSky state vectors, 2025-06-05 06:00 to 09:00 UTC, European area"
    first, last = e.loc["first row of aircraft"], e.loc["last row of aircraft"]
    common = dict(page="first-last-seen", reference=ref, source_file=src, item="first_seen;last_seen",
                  kind="event")
    return [
        measure(measure_id="first_age_p50", value=f"{float(first['p50_s']):.0f}", unit="s", n=int(first["n"]),
                label="Median age of the position in an aircraft's first state vector",
                source_field="p50_s, row endpoint = first row of aircraft", **common),
        measure(measure_id="last_age_p50", value=f"{float(last['p50_s']):.0f}", unit="s", n=int(last["n"]),
                label="Median age of the position in an aircraft's last state vector",
                source_field="p50_s, row endpoint = last row of aircraft", **common),
        measure(measure_id="last_age_gt30", value=f"{100 * float(last['share_gt30s']):.1f}", unit="%",
                n=int(last["n"]),
                label="Share of last state vectors whose position is more than 30 s old",
                source_field="share_gt30s, row endpoint = last row of aircraft", **common),
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
        if key in NUMBERS_ONLY:
            print(f"recorded {len(found)} measure(s) of {key} (no chart)")
        else:
            print(f"drew {OUT_DIR / (key + '.svg')} and {len(found)} measure(s)")
    upsert_measures(rows, args.root / MEASURES_CSV)
    print(f"updated {MEASURES_CSV}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
