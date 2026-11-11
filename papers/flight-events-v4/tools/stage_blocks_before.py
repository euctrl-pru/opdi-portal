#!/usr/bin/env python3
"""Stage the block family's pre-fix measurement, from this repo's own history.

The off-block/on-block chapter is a before/after argument, and the "before"
side cannot be regenerated: it is what the detectors produced when
`process_month` still projected `on_ground` away, and that fix is
unconditional -- no setting of the current tree brings the old behaviour back.
A rung cannot express it either, because the fix sits in the pipeline rather
than behind a rung flag.

What *is* available is the staging committed before the fix landed. `data/`
is tracked, so the previous run's `per_airport_2026.csv` and `ladder_2026.csv`
are in git along with the manifest recording which opdi commit produced them.
This reads those out of a named commit and writes them as a small, explicit
dataset, so the chapter's before-values come from a file like every other
number on the page.

Why that matters rather than hardcoding them: when this was written, the prose
claimed a pooled before-coverage of 7.30%/6.58% and an EDDS before-value of
1.63%, while the committed data for the run it named said 9.10%/7.85% and
42.51%. The literals had drifted a run behind the text around them, and
nothing could detect it because nothing read them from anywhere.

Usage (from the paper directory):

    python3 tools/stage_blocks_before.py --commit 0f6fe34
"""
import argparse
import csv
import io
import json
import subprocess
from pathlib import Path

PAPER = Path(__file__).resolve().parents[1]
REPO = PAPER.parents[1]
REL = PAPER.relative_to(REPO) / "data"
MILESTONES = ("AOBT", "AIBT")


def show(commit: str, path: str) -> str:
    return subprocess.run(
        ["git", "show", f"{commit}:{path}"],
        cwd=REPO, capture_output=True, text=True, check=True,
    ).stdout


def build(commit: str) -> list[dict]:
    man = json.loads(show(commit, str(REL / "_manifest.json")))
    per = list(csv.DictReader(io.StringIO(show(commit, str(REL / "per_airport_2026.csv")))))
    lad = list(csv.DictReader(io.StringIO(show(commit, str(REL / "ladder_2026.csv")))))

    def stamp(name):
        e = man.get(name, {})
        return str(e.get("git_sha"))[:8], e.get("produced_utc")

    per_sha, per_when = stamp("per_airport_2026.csv")
    lad_sha, lad_when = stamp("ladder_2026.csv")

    rows = [
        {"gt_airport": r["gt_airport"], "milestone": r["milestone"],
         "coverage_pct": r["coverage_pct"], "n_detected": r["n_detected"],
         "n_truth": r["n_truth"], "bias_s": r["bias_s"],
         "source_git_sha": per_sha, "source_produced_utc": per_when}
        for r in per if r["milestone"] in MILESTONES
    ]
    # The pooled row comes from the ladder, not from summing the per-aerodrome
    # rows: coverage is a ratio and the aerodromes differ in size by a factor of
    # twenty, so a mean of the twenty percentages is not the study-wide figure.
    rows += [
        {"gt_airport": "POOLED", "milestone": r["milestone"],
         "coverage_pct": r["coverage_pct"], "n_detected": r["n_detected"],
         "n_truth": r["n_truth"], "bias_s": r["bias_s"],
         "source_git_sha": lad_sha, "source_produced_utc": lad_when}
        for r in lad
        if r["rung"] == "V07_shipped" and r["milestone"] in MILESTONES
        and float(r["n_detected"]) > 0
    ]
    rows.sort(key=lambda r: (r["gt_airport"] == "POOLED", r["gt_airport"], r["milestone"]))
    return rows


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--commit", default="0f6fe34",
                    help="commit holding the pre-fix staging (default: 0f6fe34)")
    args = ap.parse_args()

    rows = build(args.commit)
    if not rows:
        raise SystemExit(f"{args.commit}: no AOBT/AIBT rows found -- wrong commit?")
    dest = PAPER / "data" / "blocks_before_2026.csv"
    with open(dest, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    pooled = {r["milestone"]: r["coverage_pct"] for r in rows if r["gt_airport"] == "POOLED"}
    print(f"wrote {dest.relative_to(REPO)}: {len(rows)} rows from {args.commit}")
    print(f"  pooled before: " + ", ".join(f"{k} {float(v):.2f}%" for k, v in sorted(pooled.items())))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
