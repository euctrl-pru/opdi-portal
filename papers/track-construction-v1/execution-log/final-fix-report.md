# Final fix wave — track-construction-v1

**Worktree:** `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1`
**Branch:** `track-construction-v1`
**Base:** `1622d9b` → **commit `335e300`** *fix(segmentation): close the whole-branch review -- lock, A6 dwell, legacy floor*
**Tests:** baseline 237 → **248 passing**, zero failures (+12 added, −1 deleted)
**Ruff:** no new findings; one pre-existing finding removed
**`src/opdi/pipeline/tracks.py`:** does not appear in the diff (verified — see §Verification)

Ten files changed:

```
benchmarks/track_score.py                  benchmarks/track_truth.py
src/opdi/config.py                         src/opdi/pipeline/segmentation/__init__.py
src/opdi/pipeline/segmentation/base.py     src/opdi/pipeline/segmentation/methods.py
tests/test_segmentation_base.py            tests/test_segmentation_methods.py
tests/test_track_score.py                  tests/test_track_truth.py
```

---

## The three required negative controls

All three were run and captured. Each was executed by reverting the fix in place,
running the new test, observing the failure, then restoring the fix.

### 1 — The lock could not see the wrong group key

Reverted the fixture to the original single-`(icao24, callsign)` `SAMPLES` and ran the
new negative-control assertion against it:

```
>       assert _partition(engine, "track_id") != _partition(frozen, "track_id")
E       assert {frozenset({d... 1, 13, 23)})} != {frozenset({d... 1, 13, 23)})}
E
E         Both sets are equal

tests/test_negctl_item1.py:31: AssertionError
1 failed in 8.78s
```

"**Both sets are equal**" is the finding in one line: with one group, a rule using
`group_cols=["icao24"]` — arm A4's key, not production's — produces a *byte-identical*
partition to the frozen algorithm's. Against the strengthened three-group fixture the
same assertion passes.

### 2 — `boundary_error` scored an empty sample as perfect

Reverted `boundary_error`'s return block to `float(e["off_p50"] or 0)`:

```
>           assert row[f] is None, f
E           AssertionError: off_err_p50_s
E           assert 0.0 is None
tests/test_track_score.py:123: AssertionError

>           assert e[f] is None, f
E           AssertionError: off_err_p50_s
E           assert 0.0 is None
tests/test_track_score.py:142: AssertionError

FAILED tests/test_track_score.py::test_score_arm_returns_a_flat_row_of_scalars
FAILED tests/test_track_score.py::test_boundary_error_on_an_empty_apdf_sample_is_none_not_zero
2 failed, 9 deselected
```

`0.0 is None` → the unfixed code answered **0.0 seconds of boundary error** for a sample
containing no APDF flight at all. Both tests go green against the fix.

### 3 — A6 fired only on reception gaps

Reverted A6's break expression to the gap-only form
(`prev_stationary & (gap_minutes() >= p.ground_dwell_minutes)`) and ran the new
continuous-parked-run test:

```
>       assert n_tracks(assign_track_id(df, airport_anchored(), P)) == 2  # A6 must too
E       AssertionError: assert 1 == 2
tests/test_segmentation_methods.py:307: AssertionError
1 failed, 19 deselected
```

The same test's two preceding assertions passed on that run — `legacy()` gave 1 track
(correct: it cannot see this) and `ground_anchored()` gave 2. So the observed failure is
exactly the reviewer's demonstration: **A5 = 2, A6 = 1** on an aircraft parked 30 minutes
at an aerodrome broadcasting every 60 s. Against the fix, A6 gives 2.

---

## What changed, per item

### 1. Strengthened the frozen-algorithm lock — `tests/test_segmentation_base.py`

`SAMPLES` now contains **three groups** instead of one:

| group | key | shape | legacy tracks |
|---|---|---|---|
| 1 | `abc123` / `TEST123` | the original gap cases | 3 |
| 2 | `abc123` / `TEST999` | **same airframe, second callsign** — 9 samples at 10-min spacing, interleaved in time with group 1 | 1 |
| 3 | `def456` / `TEST123` | **second airframe**, same callsign, 45-min gap | 2 |

Group 2's 10-minute spacing is deliberately under both legacy thresholds, so dropping the
callsign from the key merges groups 1 and 2 into **one unbroken track** where the correct
key yields four — the partitions then differ and the wrong key is detectable.

Rather than verifying the negative control once and throwing it away, I made it a
**permanent test**: `test_the_lock_rejects_the_wrong_group_key`. If someone later weakens
the fixture back to one group, that test goes green and the resulting silence is itself
caught by `EXPECTED_LOCK_TRACKS == 6` on the main lock. The fixture's docstring says
explicitly why it must keep three groups.

### 2. `boundary_error` returns `None` on an empty sample — `benchmarks/track_score.py`

The four `*_err_p*_s` fields return `None` when `n_apdf_flights == 0`. `n_apdf_flights`
itself stays an integer 0. `score_arm`'s docstring now states that every value is numeric
*except* those four.

`test_score_arm_returns_a_flat_row_of_scalars` previously walked this path and blessed the
bug with a blanket `isinstance(v, (int, float))`; it now asserts `None` for the boundary
fields and applies the numeric check only to the rest. Added
`test_boundary_error_on_an_empty_apdf_sample_is_none_not_zero` as the explicit case.

### 3. A6 accumulates the stationary run — `src/opdi/pipeline/segmentation/methods.py`

A6 now recognises a turnaround **two ways**, ORed, each firing exactly once:

* `gap_turnaround` — the previous sample was stationary at an aerodrome and at least
  `ground_dwell_minutes` elapsed before the next one arrived (the old behaviour);
* `parked_turnaround` — `~stationary & prev_stationary & (parked_run_min >= dwell)`, where
  `parked_run_min` is measured from `run_start`, the last sample at which the aircraft was
  *not* stationary-at-aerodrome. This is A5's `F.last(F.when(...), ignorenulls=True)`
  pattern adapted from the on-ground flag to the stationary-at-aerodrome predicate.

`run_start` is `coalesce`d with the group's own first timestamp, so a group that has never
moved still anchors its run rather than producing NULL.

The local `dwell_min` is gone; the two quantities are now named `gap_minutes()` (the
engine accessor, used directly) and `parked_run_min`. A5's equivalent local was renamed
`dwell_min` → `ground_run_min` for the same reason.

**Why the union rather than A5's exact shape.** A strict `~stationary & prev_stationary &
…` alone would have broken `test_airport_anchored_uses_height_above_field_not_barometric_altitude`,
whose fixture is two *stationary* samples across a reception gap — a real A6 property
(height above field) that I did not want to drop. The union keeps both and makes the
docstring's "whether that time was a gap in reception **or** a run of parked samples"
true, which is what item 3 asked for. Firing conditions are disjoint in effect: the
parked-run arm requires the current sample to be moving, so it cannot re-fire on every
sample of a long run.

New test: `test_airport_anchored_splits_a_continuous_parked_run` — 5 inbound airborne
samples, 30 parked samples at 60 s cadence at an aerodrome, 5 outbound. Asserts
legacy = 1, A5 = 2, A6 = 2. Negative control above.

### 4. Every domain arm ORs the legacy gap rule in as a floor

A6 and A7 now end `| legacy().break_expr(p)`, matching A5. The rationale is recorded once,
in **`methods.py`'s module docstring** (I chose `methods.py` over
`segmentation/__init__.py` because that is the file an arm author is editing when they
need it), covering both reasons: the one-change-at-a-time ladder, and degrading to legacy
rather than to one track a month when inputs are missing.

A6's NULL handling is now explicit rather than relying on propagation:

* `near_airport` → `coalesce(..., False)`. A sample with no aerodrome match can never
  trigger A6's own rule, so the arm degrades to the legacy floor there.
* `field_elev_ft` → `coalesce(..., 0.0)`. A sample that *is* near a known aerodrome but
  whose elevation is unknown is tested at sea level — i.e. on barometric altitude, exactly
  as legacy would. The docstring states this is the conservative direction: it can miss a
  high-field turnaround, it cannot invent a low-field one.

New test `test_airport_anchored_degrades_to_legacy_when_its_inputs_are_missing` runs A6
over all-NULL `near_airport`/`field_elev_ft` and asserts it equals legacy exactly.

**⚠️ Consequence of this ruling that the human should see — see §Things I decided
differently, item A.** The legacy floor means A6 can only ever *add* splits to legacy's,
so it can no longer suppress legacy's spurious low-altitude split away from any aerodrome.
That was half of A6's stated purpose and one of its two tests. I applied the ruling and
rewrote both the docstring and the test to be truthful about it.

### 5. Reconciled the NM-inferred-boundary prose — no behaviour change

The `t_source == "apdf"` filter is **untouched**. Both docstrings now tell the same story:

* `track_score.py:boundary_error` — the restriction is *deliberate conservatism*, cites
  the measured median 0 s / IQR 17 s (ATOT) and median 0 s / IQR 25 s (ALDT) from
  `benchmarks/DATASETS.md` "Ground truth semantics", and states that widening the filter
  is **an open claim-scope decision for the study's author, not a settled fact**. It ends
  "Do not 'reconcile' the two modules by deleting either statement: the measurement in
  `track_truth.py` and the restriction here are both true."
* `track_truth.py` — its "trusted for both matching *and* boundary error" now reads
  "precise enough *in principle*", followed by a new paragraph, **"What consumes them
  today is narrower, deliberately."**, pointing at `boundary_error` and saying the
  measurement here says widening would probably be justified but nothing here says it has
  been done.

A maintainer reading either file alone now reaches the same conclusion.

### 6. `SegmentationConfig` wired, not deleted — `src/opdi/config.py`, `base.py`

Added `SegmentationParams.from_config(config)`, accepting either an `OPDIConfig` or a
`SegmentationConfig` directly. It raises a clear `TypeError` naming the missing fields
rather than a `KeyError` if handed something else.

`SegmentationConfig`'s docstring no longer claims `pipeline/segmentation` reads it; it now
says `from_config` is the only reader, that a bare `SegmentationParams()` uses its own
defaults instead, and — parenthetically — records that the old claim was false and that
tuning `OPDIConfig().segmentation.low_alt_ft` produced no effect and no error.

**The point of the fix**, per the brief: `test_segmentation_config_and_params_agree_field_by_field`
compares `{name: default}` over `dataclasses.fields()` of both classes. Gain, lose or
re-default a field on either side and it fails. Plus
`test_params_from_config_reads_the_config_values` and
`test_params_from_config_rejects_an_object_missing_fields`.

### 7. The arm contract made public — `base.py`, `methods.py`

* Added **`segment_window()`** returning `Window.partitionBy(_GRP).orderBy(_TS)`. All four
  arms that need a window (A3, A5, A6, A7) now call it; none re-derives it.
* Added named constants `_GRP` / `_TS` and used them inside `assign_track_id` too, so the
  engine and the arms cannot drift on the literal.
* `methods.py` no longer imports anything private from `base` — the `_ALT_FT` / `_SPD_KT`
  imports are gone, replaced by `altitude_ft()` and `speed_kt()` throughout. `speed_kt()`
  was exported and used by nothing; A6 now uses it.
* `base.py`'s module docstring gained an **"arm contract"** section listing `_grp`, `_ts`,
  the three accessors and `segment_window()`, and stating that an arm needing a window
  *must* call `segment_window()` — with the reason: a hand-built
  `Window.partitionBy("icao24", "callsign")` silently disagrees with the engine's grouping
  for any arm whose `group_cols` are not those two.
* `segment_window` exported from both `base.__all__` and the package `__init__`.

### 8. Smaller items — all applied

**A7's `was_below` — removed, not replaced.** `climbing_away` requires the lag row to be
`<= descent_floor_ft`, and that lag row is inside `was_below`'s own
`rowsBetween(unboundedPreceding, -1)` window, so `climbing_away` implies `was_below == 1`
for every row. I removed it rather than replacing it because the condition the docstring
implies — "a descent reached the floor" — is *exactly* what the lag row being at or below
the floor already says; there was nothing left to express. The docstring records the
reasoning so it does not get re-added. It was also the arm's most expensive expression (a
running aggregate over an unbounded frame).

**A7's test now descends to the floor continuously.** The old fixture went 9000 → 900 m
(never reaching the 1,500 ft floor) then jumped discontinuously to 0 m. New fixture:
descent `[9000, 8000, …, 1000, 500, 200, 0]` m — 500 m is 1,640 ft (above the floor),
200 m is 656 ft (below it), so the crossing is continuous — then climb
`[0, 200, 500, 1000, 2000, 3000, 4000]` m. The test additionally asserts the track **sizes**
are `[5, 14]`, which pins *where* the split lands; that is what makes the descent leg
load-bearing, since deleting it changes those numbers.

**Deleted `test_the_three_outcomes_are_mutually_exclusive_and_sum_to_100`.** A comment in
its place records why: same fixture as `test_clean_match_counts_only_one_to_one_flights`,
which already pins 25/25/50 exactly, and the real case is covered by
`test_a_flight_both_merged_and_fragmented_counts_as_merged`.

**`overlap_join` — I chose the row-identity fix, not the documentation-only option.**
`assign` gains `_a_row = monotonically_increasing_id()` before the join and the window
partitions on `a._a_row` instead of `(a.icao24, a.event_time)`.

Why the fix over documenting a requirement: the requirement could not actually be met by
callers today — dedup on `(track_id, timestamp)` is listed in the project's evidence base
as *not yet implemented*, so a guard would have fired on legitimate production input and a
docstring warning would have left the metrics quietly wrong in the meantime. It is also
the cheaper option: `monotonically_increasing_id` is already the established device in this
same file (`load_flight_intervals`'s `_nm_id`) for precisely this problem — "the natural
key is not a row identity" — so it follows local precedent rather than introducing a new
pattern. Two tests: `test_overlap_join_keeps_duplicate_state_vectors` (two identical rows
in, two rows out) and `test_overlap_join_still_deduplicates_across_touching_intervals`
(the boundary-sample property the old window got right for the wrong reason must survive).

**`BreakRule.break_expr` is now required.** Moved to second position, ahead of the two
fields with defaults, so the dataclass enforces it; `__post_init__` additionally raises
`TypeError: BreakRule('x'): break_expr must be callable, got str` for a non-callable. Every
construction in the package is by keyword, so the reorder is not a breaking change — I
verified all eight call sites. Covered by `test_break_rule_requires_a_break_expression`.

**`TRAFFIC_DEFAULT_GAP_MINUTES`** moved above `traffic_style` (it was defined after its
only use) and added to `__all__`, with a comment noting it is the one number an A3 sweep
would want to vary.

**`recommended()` now has coverage.** `test_every_registered_arm_runs` iterates `ARMS` —
which is what a Task-6 runner does — and asserts each arm returns the same row count, no
NULL `track_id`, and at least one track over a trajectory containing a gap, a ground run
and a climb. Plus `test_recommended_is_at_least_as_aggressive_as_the_arms_it_combines`
(A8 is A5 OR A6, so it can never split less than either).

**`boundary_error`'s `F.first` assumption, downgraded.** There was in fact *no*
"provably safe" comment at that call site to edit — I grepped the whole tree for
"provably" and "structurally guaranteed" and found neither in `track_score.py`. I added
the honest comment the item asks for: `t_off` is **structural** (`flight_key` is a hash
*over* `t_off`, so two rows with the same key cannot disagree about it); `t_land` is **an
assumption**, resting on `(aircraft, callsign, day, ADEP, ADES, second-precision t_off)`
identifying at most one physical leg — "a statement about the world rather than about the
hash" — believed safe, not enforced, and `F.first` would pick arbitrarily rather than fail
if it were ever violated.

**The one `I001` this branch added** (`tests/test_segmentation_base.py`) is fixed. The ~19
pre-existing `tests/` findings are untouched, as instructed.

**`test_airframe_only_still_splits_a_null_callsign_airframe_on_gaps`** now runs
`legacy()` on its own fixture and asserts 2 tracks, proving the "A4 does not do worse"
claim its docstring makes instead of asserting it in prose.

---

## Things I decided differently, and why

### A. A6 lost half of its stated purpose — a direct consequence of ruling 4

**This is the one item a human should look at before merging.**

Ruling 4 ("every domain arm ORs the legacy gap rule in as a floor") and A6's existing
docstring and test are in genuine conflict. A6's docstring claimed two fixes, and they
point in opposite directions:

1. *add* a split legacy misses — an aircraft parked at a 6,000 ft aerodrome never
   satisfies `baro_altitude < 1524 m`, so legacy merges two flights;
2. *suppress* a split legacy invents — "a long, slow, low dwell in the middle of nowhere
   satisfies legacy's low-altitude rule and is split, when it is not a turnaround at all."

Once legacy is ORed in as a floor, **A6 ≥ legacy in splits, always**, so (2) is
unreachable by construction. Its test —
`test_airport_anchored_requires_the_break_to_be_at_an_airport`, which asserted A6 = 1 where
legacy = 2 — became unsatisfiable, not merely wrong.

I applied the ruling as written rather than carving out an exception, because its stated
rationale (each arm's delta from A0 must measure one idea) is precisely an argument that
an arm must not *remove* legacy splits either. But I did not quietly drop the property:

* The **test was rewritten, not deleted**, to assert the direction that survives:
  proximity as the sole discriminator. Two frames differing *only* in `near_airport`, over
  a fixture legacy splits neither way (20-min gap, 1900 m — under the 30-min rule, over
  the 1524 m rule). A6 gives 2 at the aerodrome and 1 away from it, so the difference is
  attributable to proximity alone. Its docstring records that the suppression case "is
  unreachable under the floor ruling and was removed rather than weakened."
* `test_airport_anchored_uses_height_above_field_not_barometric_altitude` was also
  sharpened while I was there: it now holds `baro_altitude` and `near_airport` fixed and
  moves only `field_elev_ft` (6,200 ft vs 0), which isolates the height-above-field
  arithmetic instead of confounding it with proximity.
* A6's docstring states the cost in its own section, **"The legacy floor's cost, stated"**,
  naming the unfixed case and calling it "a deliberate consequence of the
  one-change-at-a-time ladder, not an oversight; an arm that suppressed legacy splits
  would need to be a separate rung."
* `methods.py`'s module docstring cross-references it.

**If the human disagrees**, the smallest reversal is dropping the floor from A6 only and
restoring the original test — but note that A6 then needs a different answer to the
NULL-input problem, since `coalesce(near_airport, False)` alone leaves it with no break at
all for uncovered airframes.

### B. A6's parked-run rule is a union, not A5's exact shape

Item 3 said "A5's accumulation is the model; adapt it". A literal adaptation
(`~stationary & prev_stationary & run >= dwell`, mirroring A5's
`~on_ground & prev_ground & …`) would have dropped the reception-gap path and broken the
height-above-field test, whose fixture is two stationary samples across a gap. I kept both
paths as an explicit OR. This makes the docstring's "gap in reception **or** a run of
parked samples" literally true, which was the item's actual complaint. Detailed in §3.

### C. The item-1 negative control became a permanent test

The brief asked me to verify the wrong key fails and report it. I did both that *and*
committed the check as `test_the_lock_rejects_the_wrong_group_key`, because the failure
mode is regression-prone: the whole bug was that a future fixture edit can silently
disarm the lock, and a one-off verification does not protect against that.

### D. `overlap_join`: fix rather than document

Item 8 offered either. I took the fix. Reasoning in §8 above — the documented-requirement
option would have asked callers for a guarantee (deduplicated input) that the pipeline
does not currently provide.

---

## Verification

**Test suite.** 248 passing, zero failures (baseline 237). Confirmed independently by the
coordinator at 105 s after my own run of 102.54 s. Net +11: twelve tests added, one
deleted.

Added: `test_the_lock_rejects_the_wrong_group_key`,
`test_break_rule_requires_a_break_expression`,
`test_segmentation_config_and_params_agree_field_by_field`,
`test_params_from_config_reads_the_config_values`,
`test_params_from_config_rejects_an_object_missing_fields`,
`test_boundary_error_on_an_empty_apdf_sample_is_none_not_zero`,
`test_airport_anchored_degrades_to_legacy_when_its_inputs_are_missing`,
`test_airport_anchored_splits_a_continuous_parked_run`,
`test_every_registered_arm_runs`,
`test_recommended_is_at_least_as_aggressive_as_the_arms_it_combines`,
`test_overlap_join_keeps_duplicate_state_vectors`,
`test_overlap_join_still_deduplicates_across_touching_intervals`.

Deleted: `test_the_three_outcomes_are_mutually_exclusive_and_sum_to_100`.

**Ruff.** `uvx ruff check src/opdi benchmarks tests`, diffed line-by-line against the
baseline captured before any edit:

```
< Found 248 errors.                                          # baseline
> Found 247 errors.                                          # after
< src/opdi/config.py:1181:101: E501 Line too long (102 > 100)
> src/opdi/config.py:1193:101: E501 Line too long (102 > 100)   # same finding, line shifted
< tests/test_segmentation_base.py:11:1: I001 [*] Import block is un-sorted   # REMOVED
```

**No new findings.** The only substantive change is the removal of the `I001` this branch
had added. The `config.py` E501 is the same pre-existing finding displaced by 12 lines
because my docstring edit above it is longer. The ~19 pre-existing `tests/` findings are
untouched, as instructed.

**`tracks.py` untouched.** `git diff --name-only 1622d9b..HEAD | grep -c "pipeline/tracks.py"`
returns `0`. Ten files in the diff, none of them `tracks.py`.

**Other constraints honoured:** `boundary_error`'s `t_source == "apdf"` filter behaviour is
unchanged (item 5 was prose only); no secondary sort keys were added to `track_truth.py`'s
proximity windows; the pre-existing `tests/` ruff findings were left alone; no refactoring
beyond the listed items.

---
---

# Addendum — Ruling amended: A6's floor is the general gap rule only

**Follow-up commit:** `d7a40a5` — *fix(segmentation): A6 floors on legacy's general gap
rule, not the rule it replaces*
**Tests:** 248 → **249 passing**. **Ruff:** byte-identical to the previous run (`diff` of
the two concise outputs is empty).

The coordinator amended the floor ruling in response to §A above. Recording the change,
because the amendment is the more interesting artefact than the original ruling.

## The amendment

A6 ORs in **only legacy's general gap rule** — `gap_minutes() > p.gap_minutes`, the
30-minute any-altitude rule — and **not** the low-altitude rule. A5 and A7 keep the full
legacy floor, unchanged.

The reasoning resolves the conflict rather than trading one horn for the other: A6's
thesis is that **legacy's `altitude < 5,000 ft` test is a crude proxy for "at an
aerodrome", and real airport geometry is better.** So the floor should be the part of
legacy that A6 is *not* replacing. Flooring an arm on the very rule it exists to replace
makes the replacement unmeasurable by construction.

All three properties now hold simultaneously:

| property | mechanism |
|---|---|
| NULL-input safety — the reason a floor was imposed at all | `near_airport` NULL → coalesced `False` → no A6 break, but a >30 min gap still splits. No month-long tracks. |
| Direction 2 (suppression) reachable again | A 20-minute low, slow dwell away from any aerodrome: legacy's low-altitude rule splits it; A6 does not, since 20 < 30 and the floor stays silent. |
| Direction 1 (the missed high-field turnaround) | Unaffected. |

A5 and A7 keep the full floor because neither has a suppression thesis — A5 adds the
continuous-turnaround split, A7 adds the descent-climb split, both purely additive, so a
full floor costs them nothing. A6 is the only arm claiming to *remove* a legacy split,
which is exactly why it was the only one that conflicted.

## What changed

**1. Legacy's two halves are now named** — `src/opdi/pipeline/segmentation/methods.py`.
Rather than inline a bare threshold at A6's fallback, `legacy()`'s break expression is
split into two module-level, exported, individually-documented functions:

* `legacy_general_gap(p)` → `gap_minutes() > p.gap_minutes`. Its docstring: it "asserts
  only 'a long enough silence is a new track', which no arm in the study disputes, so it
  is safe for every arm to inherit."
* `legacy_low_altitude_gap(p)` → the `low_alt_gap_minutes` / `low_alt_ft` conjunction. Its
  docstring: "production's proxy for 'the aircraft is on the ground somewhere' — it uses
  barometric altitude because that is all it has. It is exactly the rule
  `airport_anchored` exists to replace with real airport geometry, which is why A6 is the
  one arm that does not inherit it as a floor."

`legacy()` is now `legacy_general_gap(p) | legacy_low_altitude_gap(p)` — **behaviour
unchanged**, and the frozen-algorithm lock re-confirms it. A reader at A6's return
statement sees a named concept and a cross-reference, not an unexplained `30.0`.

**2. A6's fallback** is `gap_turnaround | parked_turnaround | legacy_general_gap(p)`, with
an inline comment naming what is deliberately absent and why.

**3. A6's docstring** now leads with **both** directions as a numbered list — "a split
legacy misses" and "a split legacy invents" — replacing the "legacy floor's cost, stated"
section that conceded direction 2 was unreachable. A new section, "Its floor is
deliberately only half of legacy", states the exclusion and its reason: "An arm cannot
measure the replacement of a rule while also inheriting that rule as its own floor."

**4. The package floor policy** in `methods.py`'s module docstring gained a third section,
"**Which floor: the part of legacy the arm is not replacing**", stating the A5/A7 rule,
the A6 exception, and why the exception does not weaken the NULL-safety reason. It no
longer contradicts the code.

**5. The suppression test is restored** to its original direction —
`test_airport_anchored_requires_the_break_to_be_at_an_airport`, asserting A6 = 1 where
legacy = 2 over a 20-minute low, slow dwell with `near_airport=False`. Its docstring
records why the assertion is now satisfiable: "Under a full legacy floor the low-altitude
rule would fire through the floor and A6 would report 2 — the assertion would be
unsatisfiable, since an arm ORing in a rule can never split less than that rule does."

The proximity-isolation test written in its place is **kept**, as
`test_airport_anchored_isolates_proximity_from_every_other_input`. Not redundant: the
restored test varies proximity *and* altitude regime together, while this one holds
`baro_altitude`, speed and the floor fixed and moves only `near_airport`. Keeping both is
the +1 in the test count.

**6. `test_airport_anchored_degrades_to_legacy_when_its_inputs_are_missing`** was renamed
`..._still_splits_on_a_long_gap_when_its_inputs_are_missing`, because A6 no longer
degrades to *legacy* — it degrades to the general gap rule. Same 45-minute fixture, same
assertion; the docstring now says the general rule alone suffices for the property the
floor exists to provide. The old name would have been a false claim about the code.

## Negative control — required, and captured

Reverted A6's fallback to the full `legacy().break_expr(p)` (the version committed in
`335e300`) and ran the restored suppression test:

```
>       assert n_tracks(assign_track_id(df, airport_anchored(), P)) == 1
E       AssertionError: assert 2 == 1
1 failed, 20 deselected in 8.75s
```

**`assert 2 == 1`** — under the full floor, legacy's low-altitude rule fires straight
through and A6 splits a dwell nowhere near an aerodrome. That is exactly the
unsatisfiability described in §A, now demonstrated rather than argued. Against the amended
floor the test passes.

## Verification

* **249 passing**, zero failures (was 248; +1 for the retained proximity-isolation test).
* **Ruff:** `diff` of the concise output against the pre-amendment run is **empty** — no
  findings added, none removed.
* `src/opdi/pipeline/tracks.py` still absent from the diff.
* **A5 and A7 verified unchanged:** `grep -n "legacy().break_expr(p)"` returns exactly two
  hits — `ground_anchored` and `vertical_profile`. Nothing else was touched.
