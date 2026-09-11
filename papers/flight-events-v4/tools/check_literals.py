#!/usr/bin/env python3
"""Fail a render whose prose states a measurement as a literal.

Every measured number in this paper must reach the page through an inline R
expression that reads a staged CSV, so that re-running the analysis re-renders
the claim. A literal cannot do that: it says what was true on the day someone
typed it, and it keeps saying so after the number changes. This paper has
already been wrong that way -- an earlier draft reported `airborne` coverage
at a value the regenerated data no longer supported.

Not every number is a measurement. Algorithm parameters, thresholds, ICAO
milestone numbers and worked examples are properly literal, and marking them
is how the author says which is which:

    ... longer than 300 s <!-- literal-ok: PRU exclusion-box parameter -->

The marker takes a reason, on the line or the one above it. A bare marker with
no reason is itself an error -- the point is the justification, not the escape.
"""
import re
import sys
from pathlib import Path

#: Shapes that read as a measurement rather than a setting: a percentage with
#: a decimal, a signed time bias, a percentage-point delta. A bare integer
#: percent ("50%") is far more often a parameter, so it is not flagged.
MEASUREMENT = re.compile(r"[-+−]?\d+\.\d+\s*(?:%|pp|s\b)")
INLINE_R = re.compile(r"`r[ \t][^`]*`")
FENCE = re.compile(r"^\s*(```|:::)")
MARKER = re.compile(r"<!--\s*literal-ok:\s*(.+?)\s*-->")
BARE_MARKER = re.compile(r"<!--\s*literal-ok\s*-->")


def audit(path: Path):
    problems, in_fence, prev_marker = [], False, None
    for n, raw in enumerate(path.read_text().splitlines(), 1):
        if FENCE.match(raw):
            in_fence = not in_fence if raw.lstrip().startswith("```") else in_fence
            continue
        if in_fence or raw.lstrip().startswith("#|"):
            continue
        if BARE_MARKER.search(raw) and not MARKER.search(raw):
            problems.append((n, "literal-ok with no reason", raw.strip()))
            continue
        marker = MARKER.search(raw)
        # A number rendered by inline R is not a literal; blank those spans
        # before looking, so `r sprintf("%.2f%%", x)` cannot trip the check.
        prose = INLINE_R.sub("", raw)
        prose = MARKER.sub("", prose)
        hits = MEASUREMENT.findall(prose)
        if hits and not (marker or prev_marker):
            problems.append((n, f"literal measurement {hits}", raw.strip()[:110]))
        prev_marker = marker
    return problems


def main(argv):
    if len(argv) != 2:
        return "usage: check_literals.py PAPER.qmd"
    path = Path(argv[1])
    problems = audit(path)
    for n, why, text in problems:
        print(f"{path}:{n}: {why}\n    {text}")
    if problems:
        return (
            f"\n{len(problems)} literal measurement(s) in prose. Each must come "
            f"from a staged CSV via an inline R expression, or carry "
            f"<!-- literal-ok: reason --> saying why it is a parameter and not "
            f"a measurement."
        )
    print(f"{path}: no orphan measurements")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
