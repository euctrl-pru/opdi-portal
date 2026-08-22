# Task 1 Report: The segmentation engine, locked to the frozen algorithm

## Summary

Implemented the generic track-segmentation engine (`src/opdi/pipeline/segmentation/`)
and proved by test that it reproduces the frozen `TrackProcessor._add_track_id`
algorithm's partition exactly, under production parameters. This is the keystone
test for the whole track-construction-v1 study — all later arms build on this
engine, and A0 (the `legacy` arm) is the production baseline they are compared
against.

Work was done in the isolated worktree
`/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1`
on branch `track-construction-v1`. `src/opdi/pipeline/tracks.py` was never
touched — read-only, called only via `TrackProcessor._add_track_id` from the
test.

## Files changed

- `tests/conftest.py` — appended `callsign` field to `TRACK_SCHEMA` and to the
  tuple built in `make_track` (append, not insert, per the brief, to keep
  `test_cleaning_native.py`'s positional tuples valid).
- `src/opdi/pipeline/segmentation/__init__.py` (new) — package exports.
- `src/opdi/pipeline/segmentation/base.py` (new) — `BreakRule`,
  `SegmentationParams`, `assign_track_id`, and the `gap_minutes`/`altitude_ft`/
  `speed_kt` helpers available to break expressions. Transcribed from the brief
  verbatim, with one addition: a paragraph in the module docstring documenting
  the unit epsilon between the engine's `baro_altitude * FT_PER_M < 5000.0`
  (feet) comparison and the frozen algorithm's `baro_altitude < 1524.0`
  (metres) comparison — the two differ on a ~0.05 mm band and this is accepted
  deliberately per the project's aviation-units rule, not a bug to fix by
  comparing in metres.
- `src/opdi/pipeline/segmentation/methods.py` (new) — the `legacy()` arm (A0),
  transcribed verbatim from the brief.
- `tests/test_segmentation_base.py` (new) — transcribed verbatim from the
  brief: the equivalence test, the month-boundary equivalence test, and the
  temp-column-cleanup test.

## TDD evidence

**Step 2 — baseline suite before the new test existed (after the conftest.py
edit only), to prove the schema change didn't break anything:**

```
$ .venv310/bin/python -m pytest tests/ -q
196 passed in 77.72s (0:01:17)
```

**RED — Step 4, the new test module before the engine existed:**

```
$ .venv310/bin/python -m pytest tests/test_segmentation_base.py -q
ImportError while importing test module '.../tests/test_segmentation_base.py'.
...
E   ModuleNotFoundError: No module named 'opdi.pipeline.segmentation'
1 error in 0.46s
```

Matches the brief's predicted failure exactly.

**GREEN — Step 7, after writing `base.py`, `__init__.py`, `methods.py`:**

```
$ .venv310/bin/python -m pytest tests/test_segmentation_base.py -q
...                                                                     [100%]
3 passed in 9.78s
```

All three tests pass, including `test_engine_reproduces_frozen_algorithm` and
`test_engine_reproduces_frozen_algorithm_across_a_month_boundary` — the keystone
locks. No adjustment to the engine's logic was needed beyond a literal
transcription of the brief; the equivalence held on the first attempt.

**Full suite + lint — Step 8:**

```
$ .venv310/bin/python -m pytest tests/ -q
199 passed in 71.12s (0:01:11)
```

(196 pre-existing + 3 new, confirming nothing else regressed.)

Lint: `ruff` is not installed in the shared `.venv310` (`No module named ruff`).
Ran it via `uvx ruff` instead (downloads a pinned ruff on the fly, does not
touch the shared venv):

```
$ uvx ruff check src/opdi/pipeline/segmentation
All checks passed!
```

`uvx ruff check tests` reports 20 findings (all `I001` import-order / one
`F401`), but 19 of those pre-exist on the branch with none of my changes
applied (verified via `git stash` + rerun) — they're repo-wide lint debt in
`test_crossings.py`, `test_runways.py`, `test_events_phase.py`, etc. My new
`tests/test_segmentation_base.py` adds exactly one instance of the same
pre-existing `I001` pattern (`from conftest import make_track` grouped after
the `opdi.*` imports rather than merged with them), because its import block
is transcribed verbatim from the brief and matches the same house convention
every other test file already uses. I left it as specified rather than
deviating from the brief's exact code.

## Self-review

Read the full diff (`git show --stat HEAD`, `git diff HEAD~1 HEAD --
tests/conftest.py`) after committing:

- `tests/conftest.py`: exactly the two one-line additions specified, both
  appended at the end of their respective lists — confirmed positionally last,
  so `test_cleaning_native.py`'s positional tuples are untouched.
- `base.py`: `assign_track_id` builds `_grp` via the same
  `sha2(concat_ws("", *group_cols), 256)` shape the frozen algorithm uses;
  the running-offset window uses `.rowsBetween(Window.unboundedPreceding, 0)`
  — the detail the brief flagged as the most likely bug source — and it's
  present. All six temp columns (`_gap_minutes`, `_alt_ft`, `_spd_kt`, `_grp`,
  `_brk`, `_offset`, `_ts` — seven, actually) are dropped at the end, verified
  by `test_engine_drops_its_temporary_columns` passing.
  `SegmentationParams` defaults (`gap_minutes=30.0`, `low_alt_gap_minutes=15.0`,
  `low_alt_ft=5000.0`) match `OPDIConfig`'s `track_gap_threshold_minutes=30`,
  `track_gap_low_altitude_minutes=15`, `track_gap_low_altitude_meters=1524.0`
  (1524 m = 5000 ft) — checked against `src/opdi/config.py:260-266` directly.
- `methods.py`: `legacy()`'s break predicate matches the frozen algorithm's
  `track_split_condition` term-for-term (`gap > 30` OR `(gap > 15 AND alt <
  5000ft)`), and groups on `["icao24", "callsign"]`, matching the frozen
  algorithm's `concat_ws("", "icao24", "callsign")`.
- No edits, formatting changes, or even whitespace touches were made to
  `src/opdi/pipeline/tracks.py`. Confirmed via `git status` showing it absent
  from every diff and via `git log` — the frozen file has no new commit.
- No pandas, UDFs, or `applyInPandas` anywhere in the new code — pure Spark
  column expressions and window functions throughout.
- Line length and units: `base.py`'s conversions reuse `FT_PER_M = 3.28084`
  and `KT_PER_MPS = 1.94384`, matching the pipeline's own constants exactly
  (per project convention, "reuse the constants... so the two can never
  drift" — these are copied literals, since importing from `tracks.py` was
  not requested and `tracks.py` doesn't export them as named constants).

No issues found. The one deliberate addition beyond brief-verbatim content is
the unit-epsilon comment in `base.py`'s docstring, which the task instructions
explicitly asked for.

## Concerns

- `ruff` is absent from the shared `.venv310`. I worked around it with `uvx
  ruff` (ephemeral, no venv mutation), but flagging this in case CI or other
  sessions expect `.venv310/bin/ruff` to exist — it currently doesn't, on
  either this worktree or the base `opdi/.venv310`.
- The one `I001` finding in my new test file is pre-existing house style
  (matches every other test file), not something I introduced as a new
  pattern — noted above so it isn't mistaken for something I missed.

## Status

DONE. All three new tests pass, including both equivalence locks. Full suite
199/199. New source package lints clean. Single commit `46c6995`.
