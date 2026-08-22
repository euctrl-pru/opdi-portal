# Task 5 report: segmentation metrics

## Addendum: fixes from review (2026-08-22)

Review of the original submission came back Approved on quality and spec, and
confirmed the highest-risk item (homogeneity/completeness not transposed) was
correct. It found two Important gaps, both in test fixtures the brief itself
specified verbatim, plus one Minor reproducibility issue in the production
code. All three are fixed here; scope was held to exactly these three.

### 1. `boundary_error`'s real computation had no behavioural test

All seven original fixtures used `t_source="nm_inferred"`, which
`boundary_error`'s `t_source == "apdf"` filter always turns into an empty
frame — so the `percentile_approx(abs(unix_timestamp(...) - ...))` arithmetic
was never exercised.

Fix: gave the test file's `_matched` helper an optional `t_source` parameter
(default unchanged: `"nm_inferred"`) and let `samples` be either an int (the
original 10-s-spaced generator, untouched) or an explicit list of
second-offsets from `_T0`, so a test can pin exact `event_time` values. Added
`test_boundary_error_measures_the_gap_to_apdf_truth`: two flights with
`t_source="apdf"`, offsets chosen so one starts 30s after `t_off`/ends 45s
before `t_land`, the other 90s/100s. Verified empirically first that Spark's
`percentile_approx` is nearest-rank (not interpolated) — for a 2-element
frame it returns the smaller value at p50 and the larger at p90 — so the
assertions (`off_err_p50_s`≈30, `off_err_p90_s`≈90, `land_err_p50_s`≈45,
`land_err_p90_s`≈100) are hand-derivable from that rule, not guessed.

### 2. The mutual-exclusivity test never built a flight that is both merged and fragmented

The original fixture had every category disjoint by construction, so it could
not have caught the `~is_merged` guard being deleted from `is_fragmented`.

Fix: added `test_a_flight_both_merged_and_fragmented_counts_as_merged`. F5
spans two tracks (7 samples on T5a, 3 on T5b — fragmented in isolation), and
its dominant track T5a also carries F6 (merged). Asserts F5 is counted
merged, `fragmented_pct == 0.0`, and the three rates still sum to 100.
Verified this is a real kill test by temporarily deleting the guard
(`is_fragmented = F.col("n_tracks_for_flight") > 1`, no `~is_merged`) and
re-running just this test: it failed (`fragmented_pct` came back 33.33, not
0.0) exactly as hand-math predicted (double-counting pushes the sum to
400/3 ≈ 133.33%). Reverted immediately after confirming.

### 3. Nondeterministic dominant-track tie-break (Minor, fixed for reproducibility)

`match_rates` picked a flight's dominant track via
`.filter(n == best_n).dropDuplicates(["flight_key"])` with no explicit
ordering — arbitrary under a tie, and (worse) `per_track` was joined in
*after* the arbitrary pick, so the tie-break couldn't even see which track
was merged.

Fix: join `per_track` in first, then break ties deterministically with a
`row_number()` window ordered by `(n_flights_for_track desc, track_id asc)`
— a merged track always wins a tie, matching the documented "merge is the
worse failure" priority; `track_id` gives a total order for any remaining
tie between equally-(non-)merged tracks.

Added `test_tied_dominant_tracks_break_deterministically_toward_merged`: F7
splits 5/5 across `T9zulu` (also carries F8 → merged) and `T9alpha` (pure).
`T9alpha` sorts first alphabetically, so a naive track_id-only tie-break
would wrongly favour it. Confirmed this is a real, not theoretical, bug in
the *exact* test harness this suite uses (`local[1]`, `shuffle.partitions=1`):
with the old `dropDuplicates` code, the picked track depended on row
*insertion order*, not value — feeding `T9zulu`'s rows before `T9alpha`'s
happened to select correctly, but reordering to feed `T9alpha`'s row last
flipped the same code to the wrong (pure) track (`merged_pct` came back 50.0
instead of 100.0). The test fixture uses that failing insertion order, so it
is a genuine regression guard, not a coincidence of one code path.

### TDD evidence

**RED** (old `track_score.py`, i.e. before the tie-break fix, checked out via
`git stash`) — full new test file:

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q
.........F                                                              [100%]
FAILED tests/test_track_score.py::test_tied_dominant_tracks_break_deterministically_toward_merged
  assert 50.0 == 100.0 ± 1.0e-04
1 failed, 9 passed in 14.02s
```

(The other two new tests, `test_boundary_error_measures_the_gap_to_apdf_truth`
and `test_a_flight_both_merged_and_fragmented_counts_as_merged`, passed
against the old code in this run because the old code's `dropDuplicates`
happened to also pick correctly for those two fixtures under `local[1]` — the
priority-rule test's kill-test property was verified separately, below, by
directly removing the `~is_merged` guard, which is the actual bug it exists
to catch.)

**GREEN** (fixed `track_score.py` restored via `git stash pop`):

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q
..........                                                              [100%]
10 passed in 14.61s
```

**Priority-rule kill-test check** (guard temporarily removed from
`is_fragmented`, then restored):

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q -k both_merged_and_fragmented
FAILED ...::test_a_flight_both_merged_and_fragmented_counts_as_merged
  assert 33.333333333333336 == 0.0 ± 1.0e-12
1 failed, 9 deselected in 8.94s
```
(guard restored immediately after, full file re-run to confirm 10 passed)

**Full suite:**

```
$ uvx ruff check benchmarks/track_score.py tests/test_track_score.py
All checks passed!

$ .venv310/bin/python -m pytest tests/ -q
236 passed in 92.57s (0:01:32)
```

233 baseline (from the first submission) + 3 new = 236.

### Files changed (this addendum)

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_score.py`
  — `match_rates`: `Window` import, deterministic tie-break (join `per_track`
  before the tie-break, `row_number()` over `(n_flights_for_track desc,
  track_id asc)` replacing `dropDuplicates`). Nothing else touched —
  `vmeasure`, `boundary_error`'s own logic, `score_arm`, and `track_truth.py`
  are unchanged, per the reviewer's scope instruction.
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/test_track_score.py`
  — `_matched` gained optional `t_source`, `t_off`, `t_land` params and
  support for explicit per-row second-offsets (all backward compatible: the
  original seven tests are byte-identical in behaviour). Three new tests
  added, none of the original seven modified.

### Commit

`cd02e18` — `fix(bench): exercise boundary_error, enforce merge-over-fragment priority, fix nondeterministic tie-break`

### Concerns

None. All three review findings are fixed, each backed by an empirically
verified kill test (not just a plausible-looking assertion), the full 236-test
suite is green, and ruff is clean. The diff is scoped to exactly the three
items requested — no changes to `vmeasure`, `track_truth.py`, or any
traceability nits the reviewer flagged as out of scope.

## What was implemented

Created `benchmarks/track_score.py` (in the `opdi` worktree at
`/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1`),
transcribed verbatim from the task brief, exporting:

- `contingency(matched) -> DataFrame` — sample counts per `(track_id, flight_key)`.
- `vmeasure(matched) -> dict` — `homogeneity`, `completeness`, `v_measure`, computed
  by collecting the small contingency table to the driver and summing entropies in
  Python (Spark builds the table via groupBy; only the log arithmetic runs off-cluster).
- `match_rates(matched) -> dict` — `n_flights`, `clean_match_pct`, `fragmented_pct`,
  `merged_pct`. Three mutually exclusive outcomes per flight (clean / merged /
  fragmented) determined from each flight's dominant track, with merge taking
  priority over fragmentation when both conditions hold.
- `boundary_error(matched) -> dict` — `n_apdf_flights`, `off_err_p50_s`,
  `off_err_p90_s`, `land_err_p50_s`, `land_err_p90_s`, filtered to `t_source ==
  "apdf"` only, using `percentile_approx` over absolute second differences between
  track extent and `t_off`/`t_land`.
- `score_arm(matched) -> dict` — one flat row combining `n_tracks` plus all of the
  above, with the input frame cached/unpersisted around the three sub-calls.

Also created `tests/test_track_score.py`, transcribed verbatim from the brief (7
tests): perfect segmentation, total merge, total fragmentation, clean/fragmented/
merged classification (4-flight case), the 9-vs-1 stray-fragment case, the
mutual-exclusivity/sum-to-100 case, and `score_arm`'s flat-row shape.

## TDD evidence

**RED** — before `track_score.py` existed:

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q
ImportError while importing test module '.../tests/test_track_score.py'.
...
E   ModuleNotFoundError: No module named 'track_score'
=== 1 error in 0.16s ===
```

**GREEN** — after implementing `benchmarks/track_score.py`:

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q
.......                                                                 [100%]
7 passed in 11.94s
```

All 7 assertions matched the brief's stated values exactly, including the
load-bearing pair: total merge gave `completeness == 1.0`, `homogeneity ==
0.0` (not swapped), and total fragmentation gave the mirror image
(`homogeneity == 1.0`, `completeness == 0.0`) — confirming homogeneity and
completeness were not transposed.

## Full suite and lint

```
$ .venv310/bin/python -m pytest tests/ -q
233 passed in 90.27s (0:01:30)
```

226 baseline + 7 new = 233, as expected. Ran twice (once right after the new
tests passed, once again as the pre-commit full-suite check) — both green.

```
$ uvx ruff check benchmarks/track_score.py tests/test_track_score.py
```

Initial run flagged one `I001` (import-block sorting) in the test file: the
brief's snippet has a blank line before `from track_score import ...`, which
ruff's isort-style grouping (no first-party config in this repo) treats as
one unsorted block together with `pytest`/`pyspark`. Applied `ruff check
--fix`, which removed that blank line — the same style already used in
`tests/test_track_truth.py` (Task 4's test file, which groups its local
`from track_truth import ...` into the same block with no blank line).
Re-ran ruff after the fix: `All checks passed!`. Re-ran the test file after
the fix: still 7 passed. This is a formatting-only change; no logic or
expected values were touched.

No line exceeds 100 characters in either file (checked with `awk
'length>100'`).

## Files changed

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_score.py` (new)
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/test_track_score.py` (new)

## Commit

`3bf48a2` — `feat(bench): segmentation metrics -- V-measure, match rates, boundary error`
(2 files changed, 298 insertions(+))

## Self-review findings

- Diff is exactly the two new files, verbatim from the brief plus the one
  ruff-applied import reordering in the test file. No unintended edits to
  `conftest.py`, `tracks.py`, or any other existing file.
- `git status` after commit is clean; nothing left staged or untracked.
- Confirmed `contingency` and `boundary_error` are exercised indirectly via
  `score_arm` in the test suite even though not imported by name in the test
  file — brief only requires the module-level functions to exist and be
  correct, which they are.
- Confirmed the empty-`apdf`-rows edge case (every test fixture uses
  `t_source="nm_inferred"`) is handled: `percentile_approx` over an empty
  frame yields `null`, and the `or 0` fallbacks in `boundary_error`'s return
  dict convert that to `0`/`0.0` cleanly — this path is exercised by
  `test_score_arm_returns_a_flat_row_of_scalars`, which passed.
- No pandas, no UDF, no `pandas_udf` anywhere in the new module — the only
  Python-side loop is over the small collected contingency table in
  `vmeasure`, per the brief's explicit sanctioning of that pattern.

## Concerns

None. All 7 new tests pass with the brief's stated values, the full 233-test
suite is green, ruff is clean, and the diff contains no unauthorized changes.

## Addendum 2: sibling tie-break fix (round 2, 2026-08-22)

Round-1 re-review came back with all three findings addressed, and confirmed
independently (the reviewer re-ran `percentile_approx` live and re-implemented
the old `dropDuplicates` logic against the tie fixture). It surfaced one
deferred pre-existing observation, which the coordinator overrode to
in-scope: `boundary_error` has the identical unbroken-tie defect that
`match_rates` was just fixed for, in the same file.

### 1. Fix: `boundary_error`'s tie

Before:

```python
w = ends.groupBy("flight_key").agg(F.max("n").alias("best_n"))
best = ends.join(w, "flight_key").filter(F.col("n") == F.col("best_n"))
```

No dedup and no tie-break — if an APDF flight ties exactly across two tracks
on sample count `n`, both rows pass the `== best_n` filter and both survive
into `best`, double-counting the flight into `n_apdf_flights` and mixing both
tracks' errors into every percentile.

After — same treatment as `match_rates`, kept consistent so the two read as
one decision:

```python
tie_break = Window.partitionBy("flight_key").orderBy(
    F.col("n").desc(), F.col("track_id").asc()
)
best = (
    ends.withColumn("_rank", F.row_number().over(tie_break))
    .filter(F.col("_rank") == 1)
    .drop("_rank")
)
```

There's no merge/pure distinction here (no `n_flights_for_track` equivalent
in `boundary_error`), so the ordering is just dominant `n`, then `track_id`
for a total order — the same two-key shape as `match_rates`' tie-break, minus
the merge-priority key that doesn't apply in this function.

### 2. The kill test

`test_boundary_error_breaks_an_exact_track_tie_deterministically`: one flight
`F1` with 2 samples tied on each of two tracks (`Ta`, `Tb`), with deliberately
divergent errors (`Ta`: 200s/400s, `Tb`: 50s/50s) so a bug wouldn't just
double the count but would silently answer with the wrong track's numbers.
`Ta` sorts first alphabetically, so the correct tie-break picks it.

Verified as a genuine kill test the same way as round 1 — reverted
`boundary_error` to the old untie-broken code first:

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q -k boundary_error_breaks
FAILED ...::test_boundary_error_breaks_an_exact_track_tie_deterministically
  assert 2 == 1
1 failed, 10 deselected in 8.74s
```

(`n_apdf_flights` came back 2, the exact double-count the bug produces — the
percentile assertions never even ran, since the count assertion fails first.)
Then restored the fix and confirmed green:

```
$ .venv310/bin/python -m pytest tests/test_track_score.py -q
...........                                                             [100%]
11 passed in 14.92s
```

### 3. Sweep of `benchmarks/track_score.py`

Checked every `join`, `filter(... == best_...)`, `dropDuplicates`, and
`Window` in the file for the same class — does it pick deterministically
under a tie, and does it partition on something identifying exactly one
entity:

- `contingency()` — `groupBy("track_id", "flight_key").agg(count)`. Full
  aggregation over every distinct pair, not a pick-one. No tie-break needed.
- `vmeasure()` — `.collect()` over the whole contingency table, then Python
  dict accumulation keyed by `track_id`/`flight_key`. Order-independent by
  construction (dict `.get(...) + n` doesn't care what order rows arrive in).
  No issue.
- `match_rates()` — `per_flight`/`per_track` are full `groupBy(...).agg(sum/
  max/count)` aggregates, order-independent. The dominant-track pick (`best`)
  is the tie-break fixed in round 1: `Window.partitionBy("flight_key")
  .orderBy(n_flights_for_track desc, track_id asc)`, one row per `flight_key`.
  Already correct.
- `boundary_error()` — `ends = apdf.groupBy("flight_key", "track_id").agg(...)`
  uses `F.min`/`F.max`/`F.count` (true aggregates, order-independent) plus
  `F.first("t_off")` and `F.first("t_land")`. `F.first` **is** order-dependent
  in general Spark semantics — but checked against `track_truth.py`'s
  `overlap_join` (lines ~217–290): `t_off`/`t_land` are attached per
  ground-truth flight via a `Window.partitionBy("a.icao24", "a.event_time")
  .orderBy(t_off.asc())` disambiguation *before* the row reaches this module,
  so every sample row carrying the same `flight_key` already carries an
  identical `t_off`/`t_land` value. `F.first` therefore has no *different*
  candidates to tie-break between — it's non-deterministic in which row it
  reads, but deterministic in the value it returns. Not an instance of the
  bug class: there's no tie between distinguishable candidates. The dominant-
  track pick (`best`) is the tie-break fixed in this round.
  `score_arm()` — no joins or dedup; `.select("track_id").distinct().count()`
  is a straight distinct-count, not a pick-one, and is order-independent.

**No further instances found.** The two dominant-track picks (`match_rates`,
`boundary_error`) were the only two "pick one row per entity" operations in
the file, and both now use the same deterministic `row_number()`-over-window
pattern.

### Full suite and lint

```
$ uvx ruff check benchmarks/track_score.py tests/test_track_score.py
All checks passed!

$ .venv310/bin/python -m pytest tests/ -q
237 passed in 92.32s (0:01:32)
```

236 (post-round-1) + 1 new = 237. No line exceeds 100 characters in either
file (`awk 'length>100'` on both, no output).

### Files changed (round 2)

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_score.py`
  — `boundary_error`: replaced the unbroken `join`+`filter(n==best_n)` with a
  `row_number()` window tie-break, matching `match_rates`'s pattern. Nothing
  else touched.
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/test_track_score.py`
  — one new test, `test_boundary_error_breaks_an_exact_track_tie_deterministically`.
  No other test modified.

### Commit

`1622d9b` — `fix(bench): sweep the sibling tie-break bug in boundary_error`

### Concerns

None. The fix mirrors round 1's pattern exactly (same two-key window shape,
minus the merge-priority key that doesn't apply here), the kill test was
verified to fail against the pre-fix code for the precise reason described,
the full 237-test suite is green, and ruff is clean. The sweep covered every
construct in the file matching the flagged class and found no further
instances — `F.first(t_off)`/`F.first(t_land)` were checked and are safe
because `overlap_join` upstream already guarantees a single value per
`flight_key`, not because the pattern was overlooked.
