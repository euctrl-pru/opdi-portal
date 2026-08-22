# Task 4 report: Ground truth as flight intervals

## What was implemented

- `benchmarks/track_truth.py` (new): `load_apdf_times`, `load_flight_intervals`,
  `overlap_join`, exactly as the brief specifies except for one deliberate fix
  in `overlap_join` (see "Deviation from the brief" below).
- `tests/test_track_truth.py` (new): the brief's five tests verbatim, minus a
  ruff-flagged unused `pytest` import and an import-order fix (`ruff --fix`).
- `tests/conftest.py` (modified): added `from pathlib import Path` and
  `sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "benchmarks"))`
  right after the `PYSPARK_DRIVER_PYTHON` line, before the pyspark import — did
  not touch `TRACK_SCHEMA`, `make_track`, or anything else Tasks 1/3 added.
- `benchmarks/DATASETS.md` (modified): new "Ground truth semantics" heading
  with the Step 1 numbers.

## Step 1: what was measured, and what it means

Ran the brief's probe against `reference/flights_202506.parquet` (957,396
rows) and `reference/apdf_202506.parquet` (612,395 DEP rows), as a throwaway
pandas script (`benchmarks/_step1_probe.py`, deleted after use — not part of
the commit, per "pandas is fine here... Step 1 is a throwaway diagnostic").

**Hazard fix applied:** the brief's `dep.merge(nm, left_on="AP_C_FLTID",
right_on="AIRCRAFT_ID", how="inner")` merges on callsign alone across the
whole month — close to a cross join (612k x 957k). I added a day key to both
sides (`dep["_day"] = MVT_TIME_UTC.dt.date`, `nm["_day"] = AOBT_3.dt.date`)
before merging. Actual host memory here was 251 GB / 116 GB free (this run
was not on the 16 GB-capped pod), but the fix is applied regardless since it's
the correct join key, not just a memory workaround, and it's what the
committed module code needed anyway to avoid a many-to-many blowup.

**Numbers measured:**

- `TAXI_TIME_3`: mean 12.40 min, std 5.33, IQR [10, 15] min (n=957,396).
- `(ARVT_3 - AOBT_3) - FLT_DUR_3` in minutes: mean 12.396, IQR [9.7, 15] —
  essentially identical distribution to `TAXI_TIME_3` itself.
- Callsign+day join of APDF DEP to NM: 462,676 distinct keys, 499,497 merged
  rows (16,174 keys had >1 match, i.e. same-day callsign reuse/multi-leg).
- **`AOBT_3 + TAXI_TIME_3` vs real ATOT (APDF DEP `MVT_TIME_UTC`):** median
  error **0 s**, IQR **17 s** (25th pct -17 s, 75th pct 0 s), mean 174.5 s
  (skewed by a long tail — std 7360 s, min -85440 s, max 87780 s). 93.5% of
  matched rows within +-300 s, 93.8% within +-3600 s. Restricting the join to
  also key on `ADEP` (471,286 merged rows) tightens the IQR further to **14 s**
  and shrinks the mean skew (163.8 s), consistent with the tail being
  callsign-collision noise rather than inference error.
- **Corroboration on the arrival side:** `ARVT_3` vs real ALDT (APDF ARR
  `MVT_TIME_UTC`, joined on callsign+day+`ADES`, 467,868 merged rows): median
  error 0 s, IQR 25 s.

**Conclusion:** `TAXI_TIME_3` is **taxi-out time only** (`AOBT_3` to ATOT),
not total (out+in) taxi time — the near-identical match between `TAXI_TIME_3`
and `(ARVT_3-AOBT_3)-FLT_DUR_3` is explained by `ARVT_3` itself already being
a landing time (ALDT-like), not a gate/in-block arrival time, so there is no
separate taxi-in term to fold in. The IQR (17 s, tightening to 14 s) is well
under the brief's ~120 s threshold, so **the brief's fallback ("boundary
error only at APDF airports") was not triggered** — NM-inferred boundary
times (`AOBT_3 + TAXI_TIME_3` for take-off, `ARVT_3` for landing) are used for
both matching and boundary error in `load_flight_intervals`. `t_source`
(`"apdf"` vs `"nm_inferred"`) still travels with every row so a later study
can revisit this per airport if a subgroup turns out not to hold. Full
numbers are recorded in `benchmarks/DATASETS.md` under "Ground truth
semantics", and the conclusion is stated in `track_truth.py`'s module
docstring.

## Deviation from the brief: `overlap_join`'s output columns

The brief's Step 4 code for `overlap_join` hardcodes a `.select()` of
`g.flight_key`, `g.gt_adep`, `g.gt_ades`, `g.t_off`, `g.t_land`, `g.t_source`.
Running the brief's own Step 2 tests against that code failed all five with
`UNRESOLVED_COLUMN`, because the tests' `gt` fixtures (built via the `_gt`
helper from bare dicts) only ever populate `flight_key`, `icao24`, `t_off`,
`t_land`, and sometimes `callsign` — never `gt_adep`/`gt_ades`/`t_source`.
Spark's inferred schema for those frames genuinely lacks those columns, so
the hardcoded select can't resolve them; this isn't a null-value issue, it's
an absent-column issue.

I changed the final `.select()` to be generic: it now selects `assign`'s
`icao24`/`event_time`/`track_id`, plus every column `gt` carries other than
its own `icao24` (dropped as a duplicate key), each explicitly aliased via
`F.col(f"g.{c}").alias(c)`. This preserves the join predicate, the
`Window.partitionBy("a.icao24", "a.event_time").orderBy(F.col("g.t_off").asc())`
tie-break exactly as given, and the "no callsign in the join condition"
guarantee — it only changes which of `gt`'s existing columns get carried
into the output. Against production `gt` (from `load_flight_intervals`) this
still yields `flight_key`, `gt_adep`, `gt_ades`, `t_off`, `t_land`,
`t_source`, plus `callsign` and `day` (both harmless — never used in the
join). Documented in the function's docstring.

I did not change anything else about `overlap_join`, including the windowing
risk flagged in my task brief (`Window.partitionBy` with alias-qualified
strings) — that resolved correctly and the touching-interval tie-break test
passed as-is.

## TDD evidence

**RED** — `.venv310/bin/python -m pytest tests/test_track_truth.py -q` before
`track_truth.py` existed:

```
ImportError while importing test module '.../tests/test_track_truth.py'.
tests/test_track_truth.py:13: in <module>
    from track_truth import overlap_join
E   ModuleNotFoundError: No module named 'track_truth'
1 error in 0.15s
```

Also RED after creating `track_truth.py` verbatim per the brief (before the
`overlap_join` column fix), all 5 tests failing with:

```
UNRESOLVED_COLUMN.WITH_SUGGESTION: A column, variable, or function parameter
with name `g`.`gt_adep` cannot be resolved.
5 failed in 8.27s
```

**GREEN** — after the generic-select fix,
`.venv310/bin/python -m pytest tests/test_track_truth.py -q`:

```
.....                                                                   [100%]
5 passed in 10.21s
```

## Full suite

Baseline before this task: `.venv310/bin/python -m pytest tests/ -q` →
`215 passed in 75.94s`.

After this task: `.venv310/bin/python -m pytest tests/ -q` →
`220 passed in 77.67s` (215 + 5 new). Confirmed
`tests/test_segmentation_base.py::test_engine_reproduces_frozen_algorithm`
and `..._across_a_month_boundary` (Task 1's equivalence lock) are in that
count and pass.

## Lint

`uvx ruff check benchmarks/track_truth.py tests/test_track_truth.py
tests/conftest.py` → `All checks passed!` (one `--fix` applied to
`tests/test_track_truth.py` for import ordering/unused `pytest` import, both
pre-existing in the brief's snippet, unrelated to test semantics). Did not
run ruff against the rest of `benchmarks/`/`tests/` as a gate — that surfaces
171 pre-existing errors unrelated to this task; only the files this task
touched were checked clean.

Manually checked line length <=100 on `track_truth.py` and
`test_track_truth.py` (no violations; `DATASETS.md` prose lines run slightly
long in a couple of spots but it's Markdown, not linted).

## Files changed

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_truth.py`
  (new)
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/test_track_truth.py`
  (new)
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/conftest.py`
  (modified: `Path` import + one `sys.path.insert` line)
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/DATASETS.md`
  (modified: new "Ground truth semantics" section)

## Self-review findings

- Confirmed `overlap_join` never references `callsign` in its join predicate
  — only `icao24` and interval containment on `t_off`/`t_land` — matching the
  task's central design constraint. `test_overlap_join_ignores_callsign_entirely`
  exercises this directly.
- Confirmed `load_apdf_times` returns `(dep, arr)` un-joined, with the
  cross-join rationale kept in the docstring, per the brief.
- Confirmed the timestamp-arithmetic comment about `INTERVAL` literals not
  taking a column operand is preserved verbatim in `load_flight_intervals`.
- Confirmed `tracks.py` was not touched (no edits made anywhere near it).
- Confirmed no pandas/`applyInPandas`/`pandas_udf` inside `track_truth.py`
  itself — the Step 1 probe was a separate, deleted, un-committed script.
- Removed the throwaway `benchmarks/_step1_probe.py` before committing; it is
  not part of the diff.
- `git status` after commit is clean; nothing straggling.

## Concerns

- `load_flight_intervals` and `load_apdf_times` are not covered by unit tests
  (only `overlap_join` is, per the brief's Step 2/5 scope) — they read live
  S3 parquet via `REFERENCE_BASE = "s3a://..."` and were not exercised against
  the committed local `reference/*.parquet` files in this task. This matches
  the brief's stated test scope exactly, but a downstream task that calls
  `load_flight_intervals` for real should expect first contact with the S3
  path and Spark schema (column name casing, `TAXI_TIME_3` null handling on
  rows with no taxi time, etc.) to be the first time it's actually run
  end-to-end.
- The `overlap_join` output-column deviation (generic passthrough instead of
  a fixed list) is a substantive change to the brief's illustrative code, not
  a cosmetic one — flagging it clearly here and in the code's docstring so
  downstream tasks that read this file know the exact column set is
  data-driven from `gt.columns`, not a fixed schema.

---

# Fix report: review findings (round 2)

Review came back "Needs fixes" with five Important findings, all accepted by
the coordinator. All five addressed below, each with what changed, the
covering test(s), the exact command, and its output. Commit `ee35cfb` in the
`opdi` worktree (`track-construction-v1`).

## 1. Restore `overlap_join`'s explicit select; fix the test fixtures

**What changed.** `overlap_join`'s final `.select()` in
`benchmarks/track_truth.py` is back to the brief's explicit list —
`icao24`, `event_time`, `track_id` from `assign`, plus `flight_key`,
`gt_adep`, `gt_ades`, `t_off`, `t_land`, `t_source` from `gt`, each named.
The reviewer's point stands: against production `gt`
(`load_flight_intervals`'s output), the generic pass-through I'd shipped
leaked `callsign` and `day` into the result — exactly the leak this module
exists to prevent for arm A4. The docstring now records why the pass-through
was tried and reverted, so nobody re-introduces it.

Fixed the five `overlap_join` tests in `tests/test_track_truth.py` instead of
the contract: every `_gt(...)` fixture row now carries `gt_adep`, `gt_ades`,
`t_source` (arbitrary but present values — `"EBBR"`/`"BIKF"`/`"apdf"`, or the
reverse leg for the touching-interval test) alongside `flight_key`, `icao24`,
`t_off`, `t_land`. The fixtures were under-populated, not the contract wrong.

**Covering tests:** all 5 existing `overlap_join` tests in
`tests/test_track_truth.py`.

**Command and output:**

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q -k overlap_join
.....                                                                   [100%]
5 passed, 5 deselected in 10.33s
```

## 2. Make the arrival-side calibration reproducible

**What changed.** Added a "### Reproducing these numbers" subsection to
`benchmarks/DATASETS.md`, right after the "Ground truth semantics"
conclusion, with two runnable `.venv310/bin/python - <<'PY' ... PY` snippets:
the departure-side probe (`AOBT_3 + TAXI_TIME_3` vs real ATOT, with the
day-key hazard fix already applied) and a new arrival-side probe (`ARVT_3`
vs real ALDT, joined on `(callsign, day, ADES)`). Both read only the
committed `reference/*.parquet` files.

**Verification.** I did not just write the snippets — I extracted them
verbatim into two throwaway scripts and ran them, to confirm the recipe in
the doc actually reproduces the numbers already reported there (not
numbers from a different, undocumented run):

```
$ .venv310/bin/python benchmarks/_verify_dep_snippet.py
merged rows: 499497
ATOT inference error (s): count 499497.0 mean 174.53 std 7360.04 min -85440.0
  25% -17.0 50% 0.0 75% 0.0 max 87780.0
IQR (s): 17.0

$ .venv310/bin/python benchmarks/_verify_arr_snippet.py
merged rows: 467868
ARVT_3 vs real ALDT error (s): count 467868.0 mean 2.37 std 9964.25 min -86340.0
  25% 0.0 50% 0.0 75% 25.0 max 86340.0
IQR (s): 25.0
```

Both match `DATASETS.md`'s reported numbers exactly (17 s / 25 s IQR, same
merged-row counts). Both throwaway scripts were deleted after verification —
not part of the commit, consistent with Step 1's original probe.

## 3. Test `load_flight_intervals` and `load_apdf_times` against committed reference parquet

**What changed.** Added a `reference_base: str = REFERENCE_BASE` keyword to
both `load_apdf_times` and `load_flight_intervals` in
`benchmarks/track_truth.py`, threaded through the internal
`load_apdf_times(spark, months, reference_base=reference_base)` call inside
`load_flight_intervals`. Default behavior (the `s3a://` path) is unchanged;
tests pass `reference_base="reference"` to read the committed git-lfs
parquet directly. Documented in the module docstring.

Added 4 new tests to `tests/test_track_truth.py` against the real
`reference/flights_202506.parquet` / `reference/apdf_202506.parquet`:

- `test_load_apdf_times_resolves_real_columns_and_returns_dep_and_arr_unjoined`
  — checks `dep`/`arr` column sets and non-empty counts, exercising the real
  `AP_C_FLTID`/`ADEP_ICAO`/`ADES_ICAO`/`SRC_PHASE`/`MVT_TIME_UTC` column
  names and casing.
- `test_load_flight_intervals_marks_t_source_apdf_when_both_ends_are_measured`
  — icao24 `4cc577` / callsign `ICE73P`, EBBR→BIKF on 2025-06-05, a real leg
  with both APDF ATOT and ALDT present; asserts `t_source == "apdf"`.
- `test_load_flight_intervals_marks_t_source_nm_inferred_when_apdf_is_missing`
  — icao24 `4cc2aa` / callsign `FNA501`, BIAR→BGCO, an aerodrome pair APDF
  does not cover; asserts `t_source == "nm_inferred"`.
- `test_load_flight_intervals_attaches_apdf_dep_time_to_the_correct_leg` —
  icao24 `4cae87` / callsign `ABR1CE`, a genuine two-leg rotation that day
  (LFMN→LFLL in the morning, LFLL→LFPG at night); asserts each leg's
  `t_off` attaches to its *own* aerodrome's APDF milestone (morning leg
  hour < 12, night leg hour ≥ 18), not the other leg's.

I found the specific real-data test cases (not fabricated) by probing the
committed parquet directly with a throwaway pandas script before writing the
tests, then deleted the script — same pattern as Step 1's original probe.

**Two real bugs found and fixed while writing these tests** (neither
`load_flight_intervals` nor `load_apdf_times` had ever been run end-to-end
before — this is exactly the "Task 6 is first contact" risk the coordinator
flagged):

- **`AMBIGUOUS_REFERENCE` on `callsign`.** The original two-step join
  (`nm.join(dep, ...).drop(dep.callsign, ...).alias("d")`, then
  `.join(arr, ...).drop(arr.callsign, ...)`) failed with
  `[UNRESOLVED_COLUMN]`/`[AMBIGUOUS_REFERENCE]` the first time it was run
  against real data, because `nm`, `dep`, and `arr` all define a `callsign`
  column and Spark's resolver can reach the survivor through more than one
  qualifier path after the second join. Fixed by replacing both
  `drop(...).alias(...)` chains with an explicit `.select(...)` after each
  join that names every surviving column exactly once
  (`nm.icao24, nm.callsign, ..., dep.atot` then `j.icao24, ..., arr.aldt`).
- **Missing day key on the arrival-side join — the more serious one.** The
  arrival join only matched on `(callsign, ADES)`, with no day constraint
  (unlike the departure join, which already had `nm.day == dep.mvt_day`).
  Running it for real on 2025-06-05 showed the same `flight_key` appearing
  several times with wildly different `t_land` values, some 25 days away
  from the departure date — a recurring route was matching *every* ARR row
  APDF had for that callsign+ADES across the whole month. Row count for one
  day dropped from 303,852 (corrupted, fanned-out) to 31,871 (correct, close
  to the ~32,630 raw NM rows for that day) after adding
  `& (j.day == arr.mvt_day)` to the join condition. This is the same
  `flight_key`-collision failure mode as finding 4, but on the boundary
  timestamp itself rather than the key — considerably worse, since it
  doesn't just conflate two flights' identities, it assigns a flight the
  wrong landing time entirely.

**Covering tests:**

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q -k "load_apdf_times or load_flight_intervals"
....                                                                    [100%]
4 passed, 6 deselected in 14.32s
```

## 4. Add the off-block time to `flight_key`

**What changed.** `flight_key`'s hash input in `load_flight_intervals` now
includes `F.col("t_off").cast("string")` alongside `icao24`, `callsign`,
`day`, `gt_adep`, `gt_ades`. Comment added explaining why, with the measured
collision rate: 16,174 of 462,676 callsign+day keys in `flights_202506` had
more than one match (from the Step 1 diagnostic).

Added `test_flight_key_distinguishes_same_day_same_route_legs_by_off_block_time`
to `tests/test_track_truth.py`: builds two tiny synthetic parquet files (via
Spark itself, matching the real `flights_*`/`apdf_*` column names, casing,
and dtypes — `AOBT_3`/`ARVT_3`/`MVT_TIME_UTC` as `TimestampType`,
`TAXI_TIME_3` as `IntegerType`, confirmed against the real files with
`pyarrow.parquet.ParquetFile(...).schema_arrow`) into a `tmp_path`, with one
aircraft (`synth01`/`SYN1`) flying the identical route (EBBR→BIKF) twice on
the same day at different off-block times (08:00 and 14:00) and no APDF
coverage. Asserts `t_off` differs between the two rows and, load-bearingly,
that `flight_key` differs too.

**Command and output:**

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q -k flight_key
.                                                                       [100%]
1 passed, 9 deselected in 10.93s
```

## 5. Measure the `TAXI_TIME_3` null rate

**What changed.** Ran a throwaway pandas probe (deleted after use) against
`reference/flights_202506.parquet`:

```
total rows: 957396
TAXI_TIME_3 null count: 0   rate: 0.0
rows with icao24 present: 950659
TAXI_TIME_3 null count (icao24 present): 0  rate: 0.0
AOBT_3 null count: 0  rate: 0.0
```

**Measured rate: 0 of 957,396 rows (0.0%).** Negligible, per the
coordinator's stated fallback ("if negligible, say so and keep the coalesce
with a comment stating the measured rate"). Kept
`F.coalesce(F.col("taxi_min"), F.lit(0.0))` as-is; added a comment directly
above it in `benchmarks/track_truth.py` recording the 0.0% figure and noting
it should be re-measured if the module is ever pointed at a month where the
rate might not be zero. Did not add a distinct `t_source` value, since the
coordinator's instruction was conditional on a non-negligible rate.

No new test needed here — this is a data-property finding recorded in a
comment, not new branching logic. (The two `t_source` tests added for
finding 3 already cover both values the column takes.)

## `uvx ruff check benchmarks tests` (whole directories, not just touched files)

```
$ uvx ruff check benchmarks tests
... (170 pre-existing errors across the rest of benchmarks/ and tests/,
     unrelated to this task -- same population as the 171 seen before this
     round, minus the one already fixed in round 1)
Found 170 errors.
```

Isolated to just the files this task touches:

```
$ uvx ruff check benchmarks/track_truth.py tests/test_track_truth.py
```

First run flagged one `I001` (import-block ordering) in
`tests/test_track_truth.py`, caused by the new `load_apdf_times`,
`load_flight_intervals` import and the `pyspark.sql.types` import added for
finding 4's synthetic-parquet test. Applied `uvx ruff check --fix
tests/test_track_truth.py`. Re-ran:

```
$ uvx ruff check benchmarks/track_truth.py tests/test_track_truth.py
All checks passed!
```

`benchmarks/track_truth.py` was clean standalone on the first pass — no
findings surfaced only when linted alongside its siblings.

Manually re-checked line length ≤100 on both files after all changes:

```
$ awk '{ if (length($0) > 100) print FNR": "length($0) }' benchmarks/track_truth.py tests/test_track_truth.py
(no output)
```

## Full suite, once, after all five fixes

```
$ .venv310/bin/python -m pytest tests/ -q
......................................................................
...
225 passed in 97.59s
```

225 = 215 pre-Task-4 baseline + 10 in `test_track_truth.py` (5 original
`overlap_join` tests + 4 new `load_*` tests + 1 new `flight_key` test).

## Files changed (round 2)

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_truth.py`
  — `overlap_join` select restored to explicit list; `reference_base`
  override added to both loaders; both joins in `load_flight_intervals`
  rewritten as explicit selects (fixes the ambiguous-reference bug); arrival
  join gains a day key (fixes the fan-out bug); `flight_key` hash gains
  `t_off`; `TAXI_TIME_3` null-rate comment added.
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/test_track_truth.py`
  — 5 existing `overlap_join` fixtures fixed to populate `gt_adep`/`gt_ades`/
  `t_source`; 5 new tests added (`load_apdf_times` column resolution, 2×
  `t_source` branching, 1× correct-aerodrome-per-leg, 1× `flight_key`
  collision).
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/DATASETS.md`
  — "Reproducing these numbers" subsection added with both runnable probes.

Commit: `ee35cfb` — "fix(bench): address Task 4 review -- explicit
overlap_join contract, real-data coverage".

## Self-review (round 2)

- Confirmed `overlap_join`'s output no longer contains `callsign` or `day`
  under any circumstance — the select is a fixed list, not derived from
  `gt.columns`.
- Confirmed the two bugs found in `load_flight_intervals` (ambiguous
  reference, missing arrival day key) were real and not artifacts of my test
  setup, by reproducing the corrupted output first (303,852 fanned-out rows,
  multiple `t_land` values sharing one `flight_key` spanning the whole
  month) before applying the fix and confirming it dropped to 31,871 rows
  with each `flight_key` appearing once.
- Confirmed the `reference_base` default (`REFERENCE_BASE`, the `s3a://`
  path) is unchanged for callers that don't pass the new keyword — checked
  by re-reading both function signatures and the internal
  `load_apdf_times(spark, months, reference_base=reference_base)` call site.
- Confirmed no throwaway probe/smoke scripts were left in the working tree
  before committing (`_step1_probe.py` from round 1, and
  `_null_rate_probe.py`, `_explore_fixtures.py`, `_explore_fixtures2.py`,
  `_smoke_local_ref.py`, `_verify_dep_snippet.py`, `_verify_arr_snippet.py`
  from round 2, all deleted). `git status` after commit is clean.
- Re-ran the full suite once, after all five fixes, as the last verification
  step before committing (225 passed).

## Concerns

- The arrival-side day-match fix (`j.day == arr.mvt_day`, where `day` is
  derived from `AOBT_3`, the *departure* day) will not match a real APDF
  arrival for a flight that departs late enough to land after midnight UTC+
  local-day-boundary. Such a flight now falls back to `t_source ==
  "nm_inferred"` using `ARVT_3` instead of losing correctness silently (the
  old fan-out bug's failure mode) — a conservative trade I judged clearly
  better than the alternative, but it does mean APDF arrival coverage is
  very slightly undercounted for midnight-spanning flights. Flagging in case
  a later task wants to widen the arrival day match to `{day, day+1}`.
- The `flight_key` collision test is synthetic (built via Spark writing tiny
  parquet fixtures matching production column names/types), not a found
  real-world same-day-same-route-different-time pair. I did not find one by
  inspection in the time available; the synthetic version exercises the
  exact hash logic directly and deterministically, which I judged more
  valuable than a real example that would still need to be explained by
  hand, but a real-data example would be strictly stronger evidence if one
  turns up later.

---

# Fix report: round 3 (re-review findings)

Re-review verdict on round 2: all five original findings ADDRESSED. Two new
items raised for this round — one new breakage found in the round-2 fix
(arrival join keyed on the wrong day), one escalated from "deferred" (APDF
dedup defeating the `flight_key` fix). Commit `b81bb9c` in the `opdi`
worktree (`track-construction-v1`), on top of `ee35cfb`.

## 1. Key the arrival join on the arrival day, not the departure day

**What changed.** In `benchmarks/track_truth.py`, the arrival-side join
condition changed from `j.day == arr.mvt_day` (where `j.day` derives from
`AOBT_3`, the departure day) to `F.to_date(jdep.arvt) == arr.mvt_day` (where
`jdep.arvt` is `ARVT_3`, arrival-anchored, matching `arr.mvt_day`'s own
arrival-anchored `MVT_TIME_UTC`). The departure-side join was already correct
on this axis (`nm.day` from `AOBT_3` vs `dep.mvt_day` from APDF DEP's own
`MVT_TIME_UTC` — both departure-anchored) and was left unchanged.

This matches what `DATASETS.md`'s own arrival-side calibration snippet
already did (`nm["ARVT_3"].dt.date`) — production code and its own
calibration script now agree on which day field to use, where before they
did not.

**Consequence for the Concerns note from round 2:** the previous report's
concern about undercounting midnight-crossing flights' APDF arrival coverage
is now moot — keying on the arrival day recovers exactly those flights
instead of trading them away. Superseded by this round's fix; not carried
forward.

## 2. Replace APDF `dropDuplicates` with proximity disambiguation

**What changed.** In `load_apdf_times`, both `.dropDuplicates(["callsign",
"mvt_day", "apdf_adep"])` and the `_ades` equivalent are removed — every
APDF candidate row is now returned, not one arbitrary survivor per key.

In `load_flight_intervals`:

- `nm` gains a synthetic per-row id: `nm = nm.withColumn("_nm_id",
  F.monotonically_increasing_id())`. This exists because the natural key
  (callsign, day, aerodrome) is exactly what collides for the population
  Finding 4 protects, so it cannot be the disambiguation partition — a
  window partitioned by the natural key would still merge two distinct NM
  rows into one partition and only keep one.
- The departure join is followed by `row_number()` over
  `Window.partitionBy(nm._nm_id).orderBy(F.abs(F.unix_timestamp(dep.atot) -
  (F.unix_timestamp(nm.aobt) + F.coalesce(nm.taxi_min, F.lit(0.0)) *
  60)).asc_nulls_last())`, filtered to `_rdep == 1` — nearest candidate to
  each NM row's own `AOBT_3 + TAXI_TIME_3` estimate, independently per row.
- The arrival join (now day-corrected per finding 1) is followed by the same
  pattern: `Window.partitionBy(jdep._nm_id).orderBy(F.abs(F.unix_timestamp(
  arr.aldt) - F.unix_timestamp(jdep.arvt)).asc_nulls_last())`, filtered to
  `_rarr == 1` — nearest candidate to `ARVT_3`.

This is the same pattern `benchmarks/adep_ades.py:align_to_ground_truth`
already uses (`Window...orderBy(F.abs(F.unix_timestamp("t_start") -
F.unix_timestamp("gt_aobt")).asc_nulls_last())`, "ties are broken on
proximity ... rather than left to chance"), adapted to partition by a
synthetic per-row id instead of the natural key — `align_to_ground_truth`
partitions by the natural key because collapsing colliding rows there is an
accepted simplification for that study; `track_truth.py` cannot make that
trade, since preserving both colliding legs distinctly is the entire point
of Finding 4.

**On the real-data collision test the coordinator asked me to attempt:**
I wrote a probe (`benchmarks/_find_real_collision.py`) to search
`flights_202506.parquet`/`apdf_202506.parquet` for the precise pattern
needed — two NM rows sharing the exact same `(callsign, day, ADEP, ADES)`
*and* APDF holding more than one distinct candidate departure record for
that same `(callsign, day, ADEP)` key (the actual population the removed
`dropDuplicates` used to silently collapse). The naive per-key pandas loop
over the candidate set did not finish within a reasonable time budget. Per
the coordinator's explicit instruction that this was optional ("if you
genuinely cannot isolate a clean example, say so and keep the synthetic
test"), **I abandoned the search and did not isolate a real-data example.**
The probe script was deleted and is not part of the commit. The synthetic
test from round 2
(`test_flight_key_distinguishes_same_day_same_route_legs_by_off_block_time`)
is retained unchanged; it exercises the hash logic directly and
deterministically but, as the coordinator noted, cannot exercise the APDF
dedup/disambiguation path specifically, since its synthetic APDF frame is
empty (both legs resolve via the `nm_inferred` branch, where `t_off` already
differs by construction because `AOBT_3` differs). This is a real coverage
gap for the proximity-disambiguation code path specifically — flagging
explicitly rather than leaving it implied by the test passing.

## Sweep of every join and `dropDuplicates` in `track_truth.py` against its anchoring event

Requested explicitly by the coordinator, given this was the third
day-key/dedup bug found across two rounds. Ran after the round-2 fixes
landed:

```
$ grep -n '\.join(\|dropDuplicates\|Window.partitionBy' benchmarks/track_truth.py
147:    jdep = nm.join(
154:    w_dep = Window.partitionBy(nm._nm_id).orderBy(
184:    j = jdep.join(
191:    w_arr = Window.partitionBy(jdep._nm_id).orderBy(
269:    j = assign.alias("a").join(
276:    w = Window.partitionBy("a.icao24", "a.event_time").orderBy(F.col("g.t_off").asc())
```

Checked each:

- **Line 147, departure join** (`nm.callsign == dep.callsign & nm.day ==
  dep.mvt_day & nm.gt_adep == dep.apdf_adep`): `nm.day` is `to_date(AOBT_3)`
  (departure), `dep.mvt_day` is `to_date(APDF DEP's own MVT_TIME_UTC)`
  (departure). Both anchor to the same physical event. Correct, unchanged
  across both rounds.
- **Line 184, arrival join**: fixed this round — see finding 1 above. Now
  correct (both sides arrival-anchored).
- **Line 269, `overlap_join`**: keys on `icao24` equality plus timestamp
  *containment* (`t_off <= event_time <= t_land`), not a discrete day
  bucket at all. There is no day-key mismatch risk of this class here —
  containment against a continuous interval doesn't have a "which calendar
  day" question the way an equi-join on a `to_date(...)` column does.
- **`dropDuplicates`**: zero remaining in the file. Both instances (dep and
  arr sides of `load_apdf_times`) were removed this round per finding 2.
  There is no longer any key-based row collapsing anywhere in the module,
  so this specific bug class (a dedup key coarser than the row identity it's
  meant to preserve) has no remaining surface in `track_truth.py`.
- **`Window.partitionBy` at lines 154/191** (new this round): partition on
  `_nm_id` / `jdep._nm_id`, a synthetic id unique per physical NM row — not
  a natural key, so no collision-collapsing risk of the kind that motivated
  removing the dedup in the first place. **Line 276** (`overlap_join`'s
  existing tie-break, unchanged since round 1): partitions on
  `(a.icao24, a.event_time)`, the sample's own identity, ordered by
  `g.t_off` to resolve touching-interval ties — not a day key, already
  covered by `test_overlap_join_assigns_a_sample_to_only_one_flight_when_intervals_touch`.

**Sweep result: no further instances found.** Both joins now anchor
consistently on the physical event they represent (departure↔departure,
arrival↔arrival), and no dedup remains anywhere in the module that could
coarsen a key past the row identity a downstream computation depends on.

## Tests

No test files changed this round — the fix is entirely inside
`load_flight_intervals`/`load_apdf_times`, and the existing test suite
(10 tests in `tests/test_track_truth.py`, unchanged since round 2) already
covers the behavior surfaces these fixes touch: `t_source` apdf/nm_inferred
branching, per-leg correct-aerodrome attachment (which now also exercises
the corrected arrival-day join, since that test's legs are same-day), and
the `flight_key` distinctness test.

**My own run**, before committing:

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q
..........                                                              [100%]
10 passed in 23.17s
```

**Coordinator's independent full-suite run** (cited per the coordinator's
instruction that this stands in for a full re-run on my end):

> I ran the full suite myself: 225 passed in 94s, zero failures.

## Files changed (round 3)

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_truth.py`
  — arrival join re-keyed on `F.to_date(jdep.arvt)`; both `dropDuplicates`
  calls removed from `load_apdf_times`; `load_flight_intervals` gains a
  synthetic `_nm_id` column and two proximity-ordered `row_number()` windows
  (departure and arrival) replacing the plain left-join-then-select shape
  from round 2.

Commit: `b81bb9c` — "fix(bench): key arrival join on arrival day;
disambiguate APDF by proximity not dedup".

## Self-review (round 3)

- Confirmed both round-2 `dropDuplicates` calls are gone (`grep
  dropDuplicates benchmarks/track_truth.py` → no matches).
- Confirmed the arrival join condition no longer references `j.day` /
  `jdep.day` at all — only `F.to_date(jdep.arvt)`.
- Confirmed `_nm_id` is never selected into the final output of
  `load_flight_intervals` (it's dropped naturally — never listed in either
  explicit `.select(...)` after the two proximity windows) and does not leak
  into `flight_key` or any other downstream column.
- Confirmed `w_dep`'s and `w_arr`'s `.asc_nulls_last()` ordering means an
  NM row with zero APDF candidates (left-join null) still survives
  `row_number() == 1` on its own single (null) row — the `nm_inferred`
  fallback path is unaffected by adding the proximity windows.
- Re-checked line length (<=100) on the full modified file: no violations.
- Confirmed no stray files remain: `benchmarks/_find_real_collision.py`
  deleted, `git status` shows only the intended file before commit.
- Did not re-run the full 225-test suite myself in this round, per the
  coordinator's explicit instruction that their own run stands as the final
  confirmation; ran the targeted file (`test_track_truth.py`) myself as a
  cheap independent check before committing.

## Concerns (round 3)

- **The real-data `flight_key`-collision test was not delivered** — this
  was explicitly framed as optional by the coordinator, and I am reporting
  it plainly as not done rather than quietly settling for the synthetic
  test's weaker coverage. The synthetic test proves the hash formula is
  sensitive to `t_off`; it does not prove the proximity-disambiguation
  window (this round's core fix) actually assigns *different* real APDF
  candidates to two colliding legs — only that no APDF candidate exists is
  currently exercised. If this module gets more scrutiny before Task 6
  consumes it, a real or purpose-built-but-populated-APDF-fixture test of
  the disambiguation path specifically (not just the empty-APDF path) would
  close that gap.
- The naive per-key pandas search for a real collision example did not
  finish in the time I gave it; a vectorized version (e.g., a single groupby
  + nunique over the full frame rather than a per-key filter loop) would
  likely find one quickly if this is revisited — I did not build that
  version before stopping, per the instruction to abandon the hunt rather
  than keep iterating on it.

---

# Fix report: round 4 (one test, per re-review)

Re-review of round 3: both findings (arrival-day key, APDF proximity
disambiguation) ADDRESSED, and the join/dedup sweep independently verified
against a full file read (all six join/window sites checked, zero
`dropDuplicates`, no sixth instance). One Important gap remained, assessed
by the reviewer rather than self-graded: the proximity-disambiguation
window added in round 3 had no test exercising its actual decision (only
its pathway). Scope for this round was explicitly "one test, nothing else."
Commit `2e84017` in the `opdi` worktree (`track-construction-v1`), on top of
`b81bb9c`.

## What changed

Added `test_load_flight_intervals_disambiguates_competing_apdf_candidates_by_proximity`
to `tests/test_track_truth.py`, using the same synthetic-parquet fixture
pattern as `test_flight_key_distinguishes_same_day_same_route_legs_by_off_block_time`
(`_SYNTH_FLIGHTS_SCHEMA`/`_SYNTH_APDF_SCHEMA`, `reference_base=str(tmp_path)`):

- Two NM rows, same `(callsign, day, ADEP, ADES)` = `(SYN2, 2025-06-05,
  EBBR, BIKF)`, off-block at 08:00 and 14:00, `TAXI_TIME_3=10` both.
- Two APDF DEP candidates for that same `(callsign, day, ADEP)` key — both
  structurally eligible for both NM rows on the equality join alone, before
  disambiguation: 08:05 (close to the 08:00 leg's own `08:00 + 10min =
  08:10` estimate) and 14:07 (close to the 14:00 leg's `14:10` estimate).
- One unambiguous APDF ARR candidate per leg (10:05 and 16:05), so both legs
  actually reach `t_source == "apdf"` rather than the assertion testing a
  `t_source` that could never be anything else.

Asserts all four items specified: row count exactly 2; the 08:00 leg's
`t_off` is 08:05, not 14:07; the 14:00 leg's `t_off` is 14:07, not 08:05;
both legs have `t_source == "apdf"`.

Also added the one-line tie-break comment above `w_dep`'s definition in
`benchmarks/track_truth.py` (`w_arr` shares the same caveat implicitly, not
duplicated): "Ordered on proximity alone, no secondary key: exactly-
equidistant candidates tie non-deterministically (rare -- sub-second exact
ties)." No other change to `track_truth.py` — did not touch the second
deferred observation (`monotonically_increasing_id()` stability across a
stage recompute), per the explicit instruction to leave both deferred items
alone beyond this one comment. Did not write the mirrored arrival-side
version of the test — the coordinator marked it lower priority and
skippable, and it was not "nearly free" (it would need its own competing-
candidate ARR fixture, not reuse of this one), so it was skipped rather than
included.

## Verifying the test is not vacuous

Before committing, checked that the new test actually fails under the
pre-round-3 (round-2, `dropDuplicates`-based) implementation, not just that
it passes under the current one. Saved the current `track_truth.py`, swapped
in `git show ee35cfb:benchmarks/track_truth.py` (round 2, before the
proximity-window fix), and ran the new test alone:

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q -k proximity
[...]
>       assert afternoon["t_off"] == dt.datetime(2025, 6, 5, 14, 7, 0)  # ~14:07, not ~08:05
E       AssertionError: assert datetime.datetime(2025, 6, 5, 8, 5) == datetime.datetime(2025, 6, 5, 14, 7)
FAILED tests/test_track_truth.py::test_load_flight_intervals_disambiguates_competing_apdf_candidates_by_proximity
1 failed, 10 deselected in 10.98s
```

Confirms the pre-fix behavior collapses both legs onto the same arbitrary
`dropDuplicates` survivor (08:05, the alphabetically/physically first row),
exactly the bug round 3 fixed. Restored the current `track_truth.py` and
re-ran:

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q -k proximity
.                                                                       [100%]
1 passed, 10 deselected in 10.79s
```

## Full file

```
$ .venv310/bin/python -m pytest tests/test_track_truth.py -q
...........                                                            [100%]
11 passed in 23.76s
```

11 = 10 from round 3 + 1 new.

## Lint

```
$ uvx ruff check benchmarks/track_truth.py tests/test_track_truth.py
All checks passed!
```

## Files changed (round 4)

- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/tests/test_track_truth.py`
  — one new test.
- `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1/benchmarks/track_truth.py`
  — one two-line comment above `w_dep`, no logic change.

Commit: `2e84017` — "test(bench): cover the actual disambiguation decision,
not just the pathway".

## Self-review (round 4)

- Confirmed `git diff` against `b81bb9c` touches exactly the two intended
  spots: the new test function, and the two-line comment. No refactor, no
  incidental changes.
- Confirmed the test fails under the pre-round-3 implementation and passes
  under the current one (see above) — not just passing, actually
  discriminating.
- Confirmed the fixture's two APDF DEP candidates are genuinely both
  eligible for both NM rows before disambiguation (same callsign, same day,
  same ADEP on all four rows) — the join condition alone cannot separate
  them; only the proximity window does.
- Re-ran `uvx ruff check` and the full `test_track_truth.py` file after the
  cosmetic blank-line fix, to make sure that edit (fixing an accidental
  triple blank line from the append) didn't disturb anything: still 11
  passed, still clean.
- Did not touch the second deferred observation
  (`monotonically_increasing_id()` stability) or the mirrored arrival-side
  test, per explicit instruction.

## Concerns

None new this round. The two items carried forward from round 3 (no real-
data collision example found; the mirrored arrival-side disambiguation test
not written) stand as previously reported — both were explicitly scoped as
optional/deferred by the coordinator.
