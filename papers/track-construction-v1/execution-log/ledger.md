# SDD ledger — plan: /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v1/prompt.txt

Spec: the plan file itself (§Background, §Design). Self-contained; no separate spec doc.

## Environment

- Plan lives in the **opdi-portal** worktree: `.claude/worktrees/track-construction-v1-plan`
  (branch `worktree-track-construction-v1-plan`).
- Code lands in the **opdi** submodule, so a second worktree was created for it:
  `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1`
  (branch `track-construction-v1`, from `feature/flight-events-v3` @ c0e8a3e).
  **Implementers work there**, not in the plan's worktree.
- `.venv310` symlinked into the opdi worktree (uv venv is not inherited by linked
  worktrees); added to `.git/modules/opdi/info/exclude` so status stays clean.
  Verified: pyspark 4.1.1, correct per Global Constraints.

## Pre-flight conflict scan

### Cross-task rows (tasks sharing a file or interface)

| Tasks | Produces → consumes | Finding |
|---|---|---|
| T1 → T2 | `methods.py` `ARMS` dict | Clean. Plan states "this ARMS replaces the Task 1 version" explicitly. |
| T2 → T3 | `methods.py` `ARMS` dict | Clean. Same explicit replacement instruction. |
| T1 → T3 | `tests/conftest.py` `TRACK_SCHEMA` / `make_track` | Clean. Both append fields; T1 adds `callsign`, T3 adds `near_airport`/`field_elev_ft`. Positional tuples stay valid. |
| T1 → T2,3 | `assign_track_id`, `BreakRule`, `SegmentationParams` | Clean. Signatures identical at every call site. |
| T3 → T6 | `airport_anchored` needs `near_airport`,`field_elev_ft` ← `attach_airport_context` | Clean. Interface stated in both task headers. |
| T3 → T6 | `SegmentationConfig` fields → `SegmentationParams` fields | Clean. Field names match one-for-one. |
| T4 → T5 | `overlap_join` cols → `score_arm` | Clean. `track_id`,`flight_key`,`t_off`,`t_land`,`t_source`,`event_time` all produced and all consumed. |
| T4 → T6 | `load_flight_intervals(spark, months, days)` | Clean. |
| T5 → T6 | `score_arm(matched)` | Clean. |
| T6 → T7 | `PERIODS`, `attach_airport_context` | Clean. |
| T6 → T8 | `ASSIGN_BASE`, `PERIODS` | Clean. |
| T8 → T9 | payoff CSVs → paper | Clean. |
| T1 → T4 | `tests/conftest.py` sys.path | Clean. T4 adds `benchmarks` to path + `from pathlib import Path`; T1 does not touch imports. |

### Per-task self-consistency rows

| Task | Its tests vs its code | Finding |
|---|---|---|
| T1 | equivalence test vs engine | **Epsilon.** Engine compares `baro_altitude*3.28084 < 5000`; frozen compares `baro_altitude < 1524.0`. These differ on [1524.0, 1524.00005) m. No test sample is near it. See Ruling 1. |
| T2 | `test_traffic_style_splits_on_its_shorter_default_gap` | **DEFECT.** Asserts 2 tracks, but at 10000 m (32808 ft) with default `low_alt_ft=5000`, `both_airborne` is TRUE so the predicate suppresses the split → 1 track. Test contradicts the arm it tests. See Ruling 2. |
| T2 | other traffic/airframe tests | Clean. Verified by hand. |
| T2 | `TRAFFIC_DEFAULT_GAP_MINUTES` defined after use | Legal (module-level, resolved at call time). Minor style only, not a defect. |
| T3 | `test_vertical_profile_splits_on_a_descent_climb_cycle...` | **DEFECT.** Descent floor is 1500 ft; the test's lowest altitude is 900 m = 2953 ft, so `below_floor` never fires and no break occurs → 1 track, but the test asserts 2. See Ruling 3. |
| T3 | ground_anchored tests | Clean. Dwell arithmetic verified by hand: turnaround 8 min ≥ 5 → break; touch-and-go 1 min < 5 → no break. |
| T3 | airport_anchored tests | Clean. Height-above-field 34 ft < 1000, speed 0 kt < 40, near_airport true → break; legacy misses it (1900 m > 1524 m). Contrast is real. |
| T4 | `overlap_join` window | **RISK.** `Window.partitionBy("a.icao24", "a.event_time")` uses alias-qualified strings; Spark may not resolve these. Flagged to the implementer. See Ruling 4. |
| T5 | metric tests | Clean after the pre-commit fixes (mutual exclusivity now holds; verified by hand on the 4-flight case). |
| T6 | `attach_airport_context` column names | **RISK.** Assumes `aerodrome` on the zones table and `ident`/`elevation_ft` on `oa_airports`. Unverifiable without cluster access. Deferred to T6. |
| T7,T8,T9 | — | Require cluster. Not scanned in depth; see Ruling 5. |

## Rulings

**Ruling 1 (T1 unit epsilon) — keep aviation units in the engine; document the epsilon.**
Why: the plan's Global Constraints and workspace CLAUDE.md both mandate aviation units
with the unit in the field name for new thresholds, and scaling the comparison rather
than the stored value. Reverting to a metres comparison would satisfy bit-equality but
violate the standing rule. Cost if wrong: a sample whose barometric altitude falls in
[1524.0, 1524.00005) m could be partitioned differently by A0 than by the frozen
algorithm — a 0.05 mm band, not reachable by real ADS-B quantisation.

**Ruling 2 (T2 traffic test contradicts its arm) — fix the test, not the arm.**
The arm is right: a 12-minute reception hole at FL350 is a coverage gap, not a landing,
and suppressing that split is the whole point of porting traffic's predicate. The test
was written against the wrong altitude. Rewrite it at low altitude (300 m), where the
predicate does not suppress and the contrast with legacy still holds (12 min < legacy's
15-minute low-altitude threshold, so legacy keeps it whole and traffic_style splits).
Cost if wrong: the test would assert traffic-style behaviour that the arm does not have,
and Task 2 could not go green without breaking the arm.

**Ruling 2 — VERIFIED BY MEASUREMENT** (`probe_traffic_style.py`, throwaway, standalone).
Plan as written, two samples 12 min apart at 10000 m: `traffic_style` → **1 track** while
the test asserts 2 (legacy → 1, as the test asserts). The predicate does suppress the
split, exactly as the ruling claimed. Replacement at 300 m: `traffic_style` → **2**,
`legacy` → **1**. Both assertions hold. The correction carried into the Task 2 dispatch
is right.

**Ruling 3 — WITHDRAWN. I was wrong; the plan's test is correct as written.**
Original ruling claimed `test_vertical_profile_splits_on_a_descent_climb_cycle...` could
never fire, because the *descent* bottoms out at 900 m = 2953 ft against a 1500 ft floor.
That reading missed the second half of the fixture: the **climb** is `i*900` for
`i in range(10)`, so it *starts at 0 m*. The trajectory therefore does cross the floor,
`was_below` becomes 1, and the break fires on the climb's second sample.

Confirmed by running the arm's logic standalone (`probe_vertical_profile.py`, throwaway,
no repo imports since Task 2's agent was editing that worktree): the plan's original
altitudes give **2 tracks**, the proposed "correction" also gives 2, and the cruise
step-descent control still gives 1. Both work; the change was unnecessary.

**Do NOT carry a correction into the Task 3 dispatch.** Had I not probed, I would have
instructed an implementer to rewrite a correct test — the ruling would have been
self-fulfilling and invisible, since the rewritten test would also have passed.

Lesson recorded for the remaining tasks: a hand-traced claim about a Spark expression is
a hypothesis, not a finding. Ruling 2 (Task 2's traffic test) was hand-traced the same
way and IS being carried into a dispatch — see the verification note below.

**Ruling 4 (T4 alias-qualified window) — let the implementer resolve at the test gate.**
Rather than pre-specifying the fix, the implementer is told the risk and told the tests
are the contract. `F.col("a.icao24")` in `partitionBy` may need to become an unqualified
column after the select, or the join sides need renaming instead of aliasing. Cost if
wrong: one extra fix round on Task 4.

**Ruling 5 (scope) — execute Tasks 1-5 now; stop and ask before Tasks 6-9.**
Tasks 1-5 are pure local TDD: no credentials, no cluster, no network, fully gated by
tests that run in seconds. Tasks 6-9 write ~5 GB to a **shared** S3 bucket already at
~67 of ~100 GB (43 GB of it another project's, still growing), and occupy the shared
single-job Spark slot on a 30-CPU namespace for what the plan itself calls its longest
step. Both are side effects outside this worktree that norms say to ask about first, and
they are the skill's named stop condition. Cost if wrong: the human wanted the cluster
runs launched unattended and must now say so — one round trip, against the risk of
filling a shared bucket or blocking a colleague's Spark job for hours.

**Ruling 4 — SUPERSEDED by measurement (resolved before Task 4 dispatched).**
Ran a local-Spark probe (`probe_window_alias.py`, throwaway) against pyspark 4.1.1:
`Window.partitionBy("a.icao24", "a.event_time")` with alias-qualified *strings* resolves
correctly, as does the `F.col()` variant. The touching-interval tie-break returns the
earlier flight (`F1`), which is what
`test_overlap_join_assigns_a_sample_to_only_one_flight_when_intervals_touch` asserts.
The plan's code as written is correct; no change needed and no fix round spent.

---

## Progress

- BASE (opdi worktree) = `c0e8a3e`
- Briefs generated for Tasks 1-5.
- Task 1: dispatched (sonnet). Model choice: plan text carries complete code, but the
  frozen-equivalence lock is the one place real Spark debugging is likely, so mid-tier
  rather than cheapest.
- Task 1: implemented, commit `46c6995`. 199/199 tests pass (196 pre-existing + 3 new).
  **The equivalence lock passed on the first attempt** — the engine reproduces the
  frozen `_add_track_id` partition, including the month-boundary split. Ruling 1's
  epsilon did not bite, as predicted. Task reviewer dispatched (sonnet).

**Ruling 6 (environment) — `ruff` is not installed in `.venv310`; use `uvx ruff`.**
Surfaced by the Task 1 implementer. Every task in the plan ends with a lint step written
as `.venv310/bin/python -m ruff check`, which cannot work. `uvx ruff` runs it without
mutating the shared venv, which matters because `.venv310` is symlinked from the main
opdi checkout and other sessions use it. Carry this into every later dispatch. Cost if
wrong: nothing — worst case a lint step is skipped, which the reviewer would catch.

- Task 1 review: ✅ spec compliant, quality **Approved**. Zero Critical, zero Important.
  Reviewer independently traced the equivalence-test arithmetic by hand and confirmed it
  exercises all three branches of the frozen split condition plus the month-suffix quirk;
  confirmed `tracks.py` untouched by reading the frozen function directly.
- Task 1 ⚠️ item (reviewer could not run ruff) **resolved by controller**: ran
  `uvx ruff check` myself — `src/opdi/pipeline/segmentation` reports "All checks passed!",
  `tests/` reports exactly 20 errors. That matches the implementer's claim of 19
  pre-existing + 1 new. Claim verified; nothing to fix in this task.
- `Task 1: complete (commits c0e8a3e..46c6995, review clean)`
- Task 2: dispatched (haiku). Model choice: brief carries complete code and I supplied the
  one correction verbatim, so this is transcription plus testing — cheapest tier is right.
  Carried into the dispatch: Ruling 2 (the wrong traffic test, with exact replacement) and
  Ruling 6 (`uvx ruff`, not `.venv310/bin/python -m ruff`).

- Task 2: implemented, commit `0013266`. 208/208 pass (9 new). Task 1's equivalence lock
  still passes, so arm A0 did not drift while A2-A4 were added. Reviewer dispatched.
- **Task 3 fixtures pre-verified** (`probe_task3_arms.py`, throwaway, standalone). All 7
  assertions measured against inline replicas of the engine and both arms:
  turnaround legacy→1 / ground_anchored→2; touch-and-go ground_anchored→1;
  away-from-airport airport_anchored→1 / legacy→2; high-field-elevation legacy→1 /
  airport_anchored→2. Every fixture reaches the threshold it claims to exercise, and both
  contrast cases genuinely contrast. No correction needed for Task 3.

- Task 2 review: ✅ spec compliant, quality **Approved**. Zero Critical, zero Important.
  Reviewer independently recomputed the rewritten test's arithmetic (300 m x 3.28084 =
  984 ft, under the 5000 ft threshold) and confirmed both assertions hold against the
  real thresholds rather than plausible-looking numbers. Confirmed a single `ARMS`
  definition survives, and that `traffic_style`'s predicate reuses the engine's own
  `_grp`/`_ts` window rather than inventing a different grouping.
- Task 2 ⚠️ item (reviewer had no ruff binary) **resolved by controller**: `uvx ruff check`
  finds exactly 1 error — `I001` import-ordering in `tests/test_segmentation_methods.py`.
  The reviewer's suspicion was right. Cosmetic, auto-fixable, deferred (below) rather than
  fixed here: controller-side fixes skip review.
- `Task 2: complete (commits 46c6995..0013266, review clean)`
- Task 3: dispatched (sonnet). Model choice: largest brief, and it edits `config.py` where
  the insertion point needs judgment. Carried into the dispatch: Ruling 6 (`uvx ruff`), the
  append-don't-insert conftest rule, the single-`ARMS` rule, and — importantly — that I
  have **measured** every fixture, so a red test means a transcription bug in the arm and
  the fixture numbers must not be touched.

- **Task 3: implementer killed mid-task by a session limit** (reset 19:50 UTC; noticed
  20:06 UTC). NOT a failure of the work. Controller-side state check before deciding
  anything: HEAD still `0013266`, four files modified and uncommitted
  (`config.py`, `methods.py`, `tests/conftest.py`, `tests/test_segmentation_methods.py`).
  Ran the full suite myself: **215 passed**, zero failures — exactly +7 over Task 2's 208,
  matching this task's expected new-test count, with Task 1's equivalence lock among them.
  So the implementation is complete and green; only self-review, commit and report were cut off.

**Ruling 7 (recovery) — resume the original agent rather than dispatching a fresh one.**
Why: its context knows what it wrote and why, and the limit had already reset, so the
constraint that killed it no longer applies. A fresh implementer would re-derive the whole
task to finish three mechanical steps. I did NOT commit the work myself: doing so would
have made me the author of code I then send to review, and the review seat is the
guarantee worth protecting here. I only ran the tests and reported the state back.
Also instructed it to cite its own captured RED/GREEN evidence and to declare an honest
gap rather than reconstruct a RED run it never made. Cost if wrong: if the resumed agent
has lost its transcript, it re-reads the brief and diff and finishes anyway — a few
thousand tokens, no risk to the work, which is already on disk and passing.

- Task 3: resumed agent completed the cut-off tail. Commit `4900128`. Working tree clean.
  215 passing; it also re-ran a focused 42-test subset post-resume (the arms, Task 1's
  equivalence lock, and the positional-tuple test that the conftest append could break).
  Self-review confirmed: append-only conftest, single 8-entry `ARMS`, unit-named
  `SegmentationConfig` fields, `tracks.py` untouched. Reviewer dispatched (sonnet).

- Task 3 review: ✅ spec compliant, quality **Approved**. Zero Critical, zero Important.
  The reviewer independently traced all seven fixtures against their thresholds and found
  none vacuous — and independently rediscovered the second-half floor crossing at t=600,
  which is the trap I fell into on Ruling 3. Independent confirmation the withdrawal was
  right. It also confirmed the highest-risk unit case: A6's `height_ft = _ALT_FT -
  field_elev_ft` is feet-minus-feet, so the silent 3.28x class of bug is not present.
- `Task 3: complete (commits 0013266..4900128, review clean)`
- Task 4: dispatched (sonnet). Model choice: Step 1 is an open-ended empirical measurement
  whose result the implementer must interpret and write into `DATASETS.md` — judgment, not
  transcription. Carried in: Ruling 6 (`uvx ruff`), Ruling 4's superseding measurement (so
  it does not second-guess the alias-qualified window), the conftest history from Tasks 1
  and 3, and a hazard warning (below).

**Ruling 8 (Task 4 Step 1 probe hazard) — instruct a day key on the probe's merge.**
The plan's own Step 1 diagnostic does `dep.merge(nm, left_on="AP_C_FLTID",
right_on="AIRCRAFT_ID")` — callsign alone, across a whole month, ~600k x ~957k rows. A
callsign recurs daily, so this is close to a cross join and can exhaust the pod's 16 GB
cap before printing anything. This is the *same* defect I fixed in `load_apdf_times`
during the plan's self-review and missed in the probe script beside it. Told the
implementer to add a day key or restrict to a few days, and to say which it did.
Cost if wrong: a probe that OOMs, costing one round trip — no risk to committed work.
Verified separately that the git-lfs reference parquets are materialised in the worktree
(real `PAR1` files), so the probe can actually read its inputs.

- Task 4: implemented, commit `07a7841`, status DONE_WITH_CONCERNS. 220 passing
  (215 + 5 new). Reviewer dispatched (sonnet) with both open questions named for its
  independent judgment.

### FINDING — the plan's open question #1 is answered, and better than hoped

`TAXI_TIME_3` **is taxi-out only**. Measured on `flights_202506` (957,396 rows) against
`apdf_202506` (612,395 DEP rows), joined on callsign + day:

- `AOBT_3 + TAXI_TIME_3` vs real APDF ATOT: **median error 0 s, IQR 17 s**. Adding `ADEP`
  to the join key tightens the IQR to **14 s** and shrinks the mean skew, showing the long
  tail is callsign-collision noise rather than inference error.
- `TAXI_TIME_3` mean 12.40 min; `(ARVT_3 - AOBT_3) - FLT_DUR_3` has an essentially
  identical distribution (mean 12.396), which is what makes "taxi-out only" the reading.

**The implementer went beyond the brief and measured the arrival side too** — which is the
part I had flagged as the real risk, since inferring a *landing* time is a different claim
from inferring a take-off time. `ARVT_3` vs real APDF ALDT, joined on callsign+day+`ADES`:
**median error 0 s, IQR 25 s**. So `ARVT_3` already *is* the landing time, not an in-block
time, and there is no systematic taxi-in bias to correct for.

Consequence: NM-inferred boundaries are good enough for **boundary error**, not merely for
matching. The plan's fallback ("restrict boundary error to APDF airports") is not needed,
and the study's boundary-accuracy figures can use the whole ECAC sample rather than the
PRU-covered subset. This materially widens what the paper can claim.

**Action for Task 9:** the plan's "Open questions for the human" §1 is now resolved and
must be rewritten as a measured result, and `track_truth.py`'s docstring + `DATASETS.md`
already carry the numbers.

### Task 4 review: ❌ spec, **Needs fixes**, five Important findings — all accepted

**Ruling 9 (`overlap_join` contract) — reviewer is right; restore the explicit select.**
I was holding this pending an independent read, and the reviewer found the consequence
neither the implementer nor I saw: against production `gt`, the generic pass-through leaks
**`callsign` and `day`** into `overlap_join`'s output — in the one module whose stated
purpose is keeping callsign out of the scoring path for arm A4. The five tests were failing
because the fixtures under-populate `gt`, not because the contract was wrong. Fix the
fixtures, restore the select. Cost if wrong: an explicit select is stricter than needed and
a future caller with a narrower `gt` gets an error at the join instead of silently wrong
columns — the failure I would rather have.

**Ruling 10 (arrival-side calibration) — accept; make it reproducible.**
The `ARVT_3` vs ALDT number is what unlocks whole-ECAC boundary error, and its script was
deleted, so no future reader can reproduce it. That is precisely the situation
`benchmarks/provenance.py` exists to prevent ("an output with no manifest entry is reported
as unverified rather than shown as fact"). Ordered the probe committed to `DATASETS.md` as
a runnable snippet beside the departure-side one. Cost if wrong: a few lines of dead
documentation.

**Ruling 11 (untested loaders) — accept, scoped. My brief's scope was wrong.**
The brief limited tests to `overlap_join`. For a module the plan itself calls "the metric
foundation," leaving the two-sided APDF join, the `t_source` branching and `flight_key`
untested was my error, not the implementer's. Ordered `REFERENCE_BASE` made overridable and
focused tests added against the **committed local** reference parquet (confirmed present in
the worktree), covering real column casing, `t_source` branching, and dep/arr aerodrome
keying. Deliberately not exhaustive — enough that Task 6 is not first contact. Cost if
wrong: modest extra test surface.

**Ruling 12 (`flight_key` collisions) — accept. This one threatens the headline metric.**
Plan-mandated: my brief specified a hash over `(icao24, callsign, day, gt_adep, gt_ades)`
with **no time component**. The implementer's own Step 1 data shows 16,174 of 462,676
callsign+day keys had >1 match. Two legs by the same aircraft, same callsign, same city
pair, same day collapse into one ground-truth flight — and because this study *measures
merging*, a collision makes a segmentation look **better** than it is, in exactly the
statistic the paper reports. Silent and directional, the worst combination. Ordered `t_off`
added to the hash plus a test that two same-day same-route legs get distinct keys.
Cost if wrong: none material; the key is internal to scoring.

**Ruling 13 (`TAXI_TIME_3` nulls) — accept, measure first.**
Plan-mandated `coalesce(taxi_min, 0.0)` turns a missing taxi time into "departed instantly"
while `t_source` still reads `nm_inferred`, indistinguishable from a measured value. Ordered
the null rate counted and reported: if negligible, keep the coalesce with the measured rate
in a comment; if not, give those rows a distinct `t_source` so boundary error can exclude
them. Cost if wrong: a small documented bias instead of an undocumented one.

- Task 4 fix round 1/5 dispatched by resuming the original implementer (rounds 1-3 resume,
  per the skill — its context holds the Step 1 work and the module).

### Task 4 fix round 1: all five claimed fixed — and it found two bugs nobody was looking for

Commit `ee35cfb`. 225 passing (215 baseline + 10 in `test_track_truth.py`). Measured:
`TAXI_TIME_3` null rate **0 of 957,396 (0.0%)** — negligible, so Ruling 13 resolves to
"keep the coalesce, document the measured rate". `flight_key` collision test passes.

**Writing the Ruling 11 tests surfaced two bugs in `load_flight_intervals` that had never
been run end-to-end:**

1. `AMBIGUOUS_REFERENCE` on `callsign` — would have crashed at first real use in Task 6.
2. **A missing day key on the arrival-side join**, fanning a recurring route's ARR match
   across the whole month and corrupting `t_land`. One day's rows: **303,852 → 31,871**
   after the fix. Roughly a 10x fan-out silently inflating and corrupting every landing
   boundary.

**This is my bug, and it is the fourth instance of the same defect in this plan.** When I
rewrote `load_flight_intervals` during the plan's self-review — *specifically to remove a
callsign-only cross join* — I keyed the departure join on `(callsign, day, ADEP)` and the
arrival join on `(callsign, ADES)` only. I dropped the day key on the second side while
fixing the first. The running tally of callsign-join-missing-a-day-key in this one plan:
`load_apdf_times` dep↔arr (caught in self-review), the Step 1 probe (caught at dispatch,
Ruling 8), and this one (caught only by real-data tests).

Three of the four were introduced by me, and the most damaging was introduced *in the act
of fixing another instance of itself*. Recorded because it is the single most transferable
lesson of this run: when a defect class is found once, sweep every sibling occurrence
before declaring it fixed — and note that my own fix is a prime place for the next one.

It was caught only because the reviewer pushed back on my test scope (Ruling 11) and I
accepted. Had I defended the brief's scope as written, the corruption would have reached
Task 6 and every boundary-error figure in the paper.

**Deferred minor / trade-off to watch:** the arrival day-key fix means a flight departing
23:50 and landing 00:30 has `mvt_day` on the following date and so misses its APDF ARR
match, falling back to NM `ARVT_3`. Given the measured `ARVT_3`≈ALDT agreement (median 0 s,
IQR 25 s) that fallback is cheap and correct, but it should be labelled honestly via
`t_source` — flagged to the re-reviewer to confirm.

- Task 4 fix round 1/5: scoped re-review dispatched (sonnet), `07a7841..ee35cfb`.

### Task 4 fix round 1/5 re-review: 5 addressed, 1 new Important, 1 escalated → round 2

`Task 4: fix round 1/5 (5 addressed, 0 open of the original; 1 new Important + 1 escalated
observation open — arrival join keyed on departure day; APDF dedup defeats the flight_key
fix; commits 07a7841..ee35cfb)`

Re-reviewer verified each fix was non-vacuous: the real-data `t_source` tests would fail
under the pre-fix fan-out, and the `flight_key` collision test would fail under the old
time-less hash. Both checks are the right ones to have made.

**Ruling 14 (new breakage) — key the arrival join on arrival day, not departure day.**
The fix keyed the arrival join `j.day == arr.mvt_day`, but `j.day` comes from `AOBT_3` — a
*departure*-anchored day compared against an *arrival*-anchored one. The departure join is
safe under the same pattern only because both its sides anchor to the same physical event.
So the condition silently means "did not cross midnight". The unsafe case: for a same-day
same-route collision where one leg crosses midnight, one leg's departure day can coincide
with the other's real arrival day, attaching the **wrong leg's ALDT** while `t_source` still
reads `"apdf"` — silently wrong instead of safely absent.
Decisive evidence, found by the re-reviewer: the arrival calibration snippet committed to
`DATASETS.md` uses `nm["ARVT_3"].dt.date` — the correct key. **The production code and its
own calibration script disagree.** Ordered `F.to_date(F.col("arvt"))`; this also recovers
the midnight-crossers the fix was trading away. Cost if wrong: none — it is strictly more
correct than both the original and the round-1 fix.

**Ruling 15 (escalated from "deferred") — replace the APDF `dropDuplicates` with proximity
disambiguation, because it defeats Ruling 12.**
The re-reviewer graded this a deferred pre-existing observation. I disagree and pulled it
into round 2. `load_apdf_times` dedups to one arbitrary APDF row per
`(callsign, mvt_day, aerodrome)`. For two same-day same-route legs — **exactly the 16,174-key
population Ruling 12 existed to protect** — both NM rows join to the same surviving APDF
row, get the same `atot`, hence the same `t_off`, hence the same `flight_key` again. Adding
`t_off` to the hash cannot separate them because the differing value was deduplicated away
upstream. Ordered: join all candidates, disambiguate by proximity to the NM row's own
estimate via `row_number()` on absolute time difference — the pattern
`adep_ades.py:align_to_ground_truth` already uses in this repo, with the same stated
rationale, so the two agree. Also ordered a real-data collision test if a clean example can
be isolated, with an explicit statement if not. Cost if wrong: Ruling 12's fix is cosmetic
for the only population it was written for, and the merge statistic stays flattered.

**Pattern, now unmistakable.** Five instances in this plan of "a join on callsign missing or
mis-keying its day component"; three introduced while fixing a previous one; three of five
mine. Instructed the implementer to sweep every join and every `dropDuplicates` in
`track_truth.py` against the physical event each is meant to anchor to, and report what the
sweep found, rather than waiting for a sixth to be caught downstream.

- Task 4 fix round 2/5 dispatched (resumed original implementer; rounds 1-3 resume).

### Task 4 fix round 2: implemented correctly, but the agent stalled on an optional extra

The implementer returned "I'll wait for the monitor notification before continuing" — not
the report contract. Controller-side state check rather than assumption: HEAD still
`ee35cfb` (uncommitted), `track_truth.py` modified +79/-27, plus an untracked
`benchmarks/_find_real_collision.py`. Ran the suite myself: **225 passed**, zero failures.
Read the diff: **both Ruling 14 and Ruling 15 are correctly implemented** — `dropDuplicates`
gone from both sides in favour of `row_number()` over a window partitioned by an NM row id
ordered on absolute time difference, and the arrival join re-keyed to
`F.to_date(jdep.arvt) == arr.mvt_day`.

**Ruling 16 (budget) — abandon the real-data collision hunt; keep the synthetic test.**
The agent stalled hunting a real same-day same-route collision example in the reference
data. That was explicitly optional in my round-2 instruction. Round 2 alone has cost ~274k
subagent tokens on top of round 1's ~248k, and the substantive fixes were already done and
green before the hunt started. Ordered it dropped, the throwaway script deleted, and the
attempt reported honestly as "attempted, not isolated within budget, synthetic test
retained". Cost if wrong: the collision test stays synthetic — it still exercises the exact
hash logic and would fail under the pre-fix formula, which the re-reviewer confirmed. What
it cannot exercise is the APDF dedup path, so that gap is stated rather than hidden.

Also instructed: do not lose the join/dedup sweep result to the stall — that sweep is the
most valuable output of this round and must be reported even if the answer is "no further
instances".

### Task 4 fix round 2 committed: `b81bb9c`

`Task 4: fix round 2/5 (2 addressed per implementer, 1 self-disclosed gap open — untested
proximity window; commits ee35cfb..b81bb9c)`

Working tree clean, throwaway probe deleted (verified). Implementer's sweep claim, to be
checked independently by the re-review: departure join anchors both sides to the departure
event; arrival join now anchors both sides to the arrival event; `overlap_join` uses
continuous interval containment rather than a day bucket, so it cannot exhibit this defect
class; **zero `dropDuplicates` remain in the file**. No sixth instance found.

**Open, self-disclosed:** the new `row_number()` proximity-disambiguation window has no
positive test. The synthetic collision test only exercises the empty-APDF (`nm_inferred`)
branch, so the path added to fix Finding B is covered only incidentally by the real-data
tests, which do not assert on it. Handed to the re-reviewer for a severity judgement rather
than ruled on by me, because I have an incentive to call it Minor: Task 4 has now consumed
roughly 800k subagent tokens across three implementer turns, two reviews and two
re-reviews, and Task 5 is still to come. Deciding it myself under that pressure is exactly
when a self-serving call gets made. The re-reviewer is told the cost consideration and
asked to state whether one focused test is warranted and what it should assert.

**Sequencing note:** deliberately NOT dispatching Task 5's implementer concurrently with
this re-review, despite the wall-clock cost and non-overlapping files. If the re-review
opens fix round 3, two implementers would be committing to the same branch at once — an
index conflict, which is the hazard the skill's no-parallel-implementers rule exists for.

### Task 4 fix round 2 re-review: both ADDRESSED, sweep VERIFIED, coverage gap → round 3

Re-reviewer read the full 292-line file and independently located every join / window /
`dropDuplicates` site (lines 147, 154, 184, 191, 269, 276), confirming the sweep claim
rather than taking it on trust. It also cleared two risks I had named:
`monotonically_increasing_id()` does partition per NM row, so two colliding legs
disambiguate independently; and `asc_nulls_last()` + left join does **not** degrade into an
inner join — an NM row with no APDF candidate survives with `row_number() == 1`.

**Ruling 17 (untested disambiguation window) — honour the reviewer's Important grade.**
I referred this to the re-reviewer specifically so I would not grade it myself: with Task 4
at ~800k subagent tokens and Task 5 still pending, my incentive to call it Minor was
obvious and disqualifying. It came back **Important**, with the reasoning that the existing
tests exercise the disambiguation *pathway* but never its *decision* — every current
partition holds either one null row or one real candidate, so "given two competing
candidates, does each NM row get the nearer one?" is never asked. That is the exact
question the code was added to answer. Ordered one focused test with the reviewer's own
four assertions; explicitly scoped OUT the mirrored arrival-side test and both deferred
observations, to keep the round small. Cost if wrong: one test's worth of budget on a path
that is load-bearing for every APDF-sourced boundary time in the study.

**Deferred minor (Task 4):** the proximity windows order only on `F.abs(...)` with no
secondary key, so exactly-equidistant candidates tie non-deterministically. Sub-second
ties, rare. A `.orderBy(..., F.col("atot"))` secondary key would settle it if this module
is touched again.

**Deferred minor (Task 4):** `monotonically_increasing_id()` is not guaranteed stable across
a Spark stage recompute (e.g. after executor loss). Used within a single lineage here, so
latent and low-probability rather than an active bug — recorded for awareness.

- Task 4 fix round 3/5 dispatched (resumed original implementer), scoped to one test.

### Task 4 fix round 3: `2e84017` — one test, with a negative control

`Task 4: fix round 3/5 (1 addressed; commits b81bb9c..2e84017)`

The implementer confirmed the new test **fails against the pre-round-3 `dropDuplicates`
implementation** before confirming it passes against current code. That negative control is
what makes it evidence rather than decoration — a test that passes on both implementations
would prove nothing about the fix. Scope held: one test plus the optional one-line
tie-break comment, nothing else. Round-3 re-review dispatched on **haiku** — the diff is
5 KB and the judgement is mechanical (do the four assertions exist, does the fixture create
a genuine two-candidate contest), so the cheapest tier is right.

**Ruling 18 (parallelism) — dispatch Task 5's implementer alongside the round-3 re-review.**
The skill's bar is no two *implementation* subagents at once, because they would race on the
git index. A re-reviewer is read-only, so exactly one implementer is active. Earlier I held
Task 5 back while a fix round was live, which was correct then and is not now: round 3 is
committed and the re-review can only produce a round 4, which I will serialise if it
happens. Cost if wrong: if the re-review does open round 4, I wait for Task 5 to commit
before dispatching the fix — a delay, not a conflict.

- Task 5: dispatched (sonnet). Baseline 226 passing; brief adds 7, expect 233. Carried in:
  the 14 pre-verified metric values, `match_rates` taking no `jaccard_min`, the
  driver-side entropy sum being deliberate rather than a native-Spark violation, and a
  warning that homogeneity/completeness are trivially transposable — with the mirror-image
  merge and fragmentation tests named as the load-bearing pair that catches it.

### `Task 4: complete (commits 4900128..2e84017, review clean)`

Round-3 re-review (haiku): **ADDRESSED**. It independently worked the fixture's arithmetic —
the 08:00 leg's taxi estimate is 300 s from the 08:05 candidate but ~5.7 hours from the
14:07 one, and the 14:00 leg reverses that — so the window genuinely has to choose, and
chooses per row. Negative control confirmed valid: under `dropDuplicates` one leg would
lose its candidate entirely or both would collapse onto the same wrong row. Scope held.

Model-choice note: haiku did this in 24k tokens, one tool call, 33 seconds. Right tier for a
5 KB diff whose judgement is mechanical. Worth repeating for small scoped re-reviews.

**Task 4 cost in full: four implementer turns, two reviews, three re-reviews, ~1.1M subagent
tokens.** Expensive, and worth it — it is the task that surfaced the arrival-join fan-out
(303,852 → 31,871 rows for one day), the `flight_key` collision that would have flattered
the study's own merge statistic, and the APDF dedup that silently defeated the fix for it.
Every one of those was found by review or by tests that review insisted on, not by me.

- Task 5: implemented, commit `3bf48a2`. **233/233 passing** (226 baseline + 7 new). RED
  confirmed `ModuleNotFoundError`; GREEN confirmed all 7 including the merge/fragmentation
  mirror pair. Reviewer dispatched (sonnet) with the transposition risk named as the single
  thing most worth its attention, and instructed to trace the conditional-entropy
  expressions **by hand** rather than confirm the tests pass — a transposed pair would
  produce plausible numbers and pass a superficial read.

**Branch state:** 8 commits on `track-construction-v1` in the `opdi` worktree, off
`c0e8a3e`. Remote is `euctrl-pru/opdi`. To push after the final whole-branch review.

**Ruling 19 (final review scope) — run the whole-branch review over Tasks 1-5 now, at the
cluster boundary, rather than deferring it to plan completion.**
The skill places the broad review after all tasks, but this plan's Tasks 6-9 need cluster
credentials and shared-infrastructure side effects I am deliberately not triggering
unattended (Ruling 5). Tasks 1-5 are a coherent, committed, self-contained deliverable —
the entire tested foundation — and the human will decide whether 6-9 proceed. Reviewing now
also catches cross-task issues while the work is fresh, and gives the accumulated deferred
minors (the ruff import-order debt, the tie-break and `monotonically_increasing_id` notes)
a triage seat. Cost if wrong: if 6-9 later run, they get their own reviews and a second
whole-branch pass — duplicated effort, not risk.

### Task 5 review: ✅ spec, **Approved**, but two Important plan-mandated gaps → fix round 1

**The headline check passed.** The reviewer hand-derived the orientation from the code
rather than trusting green tests: `h_f_given_t` sums `P(f|t)` → feeds homogeneity (merging
drives it down); `h_t_given_f` sums `P(t|f)` → feeds completeness (fragmentation drives it
down). It then re-derived both mirror cases from raw entropy arithmetic. **Not transposed.**
That was the one error that would have swapped fragmentation and merging for every arm and
inverted the paper's conclusions while remaining superficially credible.

**Ruling 20 (`boundary_error` untested) — fix, despite being plan-mandated and Approved.**
All seven of my brief's tests hardcode `t_source="nm_inferred"`, so `boundary_error`'s
`t_source == "apdf"` filter always yields empty and every value collapses through the
`or 0` fallback. The `percentile_approx` arithmetic — one of the three metric families this
study reports — has zero behavioural coverage. Task 4 has just demonstrated twice what
untested logic in this codebase costs. Ordered one test with hand-computable offsets.
Cost if wrong: a small amount of test surface on a metric that would otherwise first be
validated by the paper's own figures.

**Ruling 21 (priority rule untested) — fix.** My mutual-exclusivity fixture contains only
cleanly-one-or-the-other flights, so it proves the sum invariant but never the rule the
docstring exists to state: merged beats fragmented when a flight is both. The reviewer's
test of the test: it would still pass at 100% with the `~is_merged` guard deleted. Ordered
a genuine both-failures fixture. Cost if wrong: negligible.

**Ruling 22 (tie-break determinism) — graded Minor by the reviewer; fixing anyway.**
`dropDuplicates(["flight_key"])` picks arbitrarily when a flight splits evenly across two
tracks, so if one tied track is pure and the other merged, the flight's label flips between
runs. This project stamps provenance on every figure so numbers are traceable to the code
that made them; a metric that can change without the code changing defeats that outright.
Ordered a deterministic order — `n_flights_for_track` descending (so a merged track wins the
tie, matching the documented "a merge invents an endpoint" intent), then `track_id` for a
total order. Cost if wrong: a tie-break rule that is defensible but arguable, now explicit
and tested rather than implicit and random.

**Deferred to Task 9 (paper design decision, NOT a code change):** `boundary_error` filters
to `t_source == "apdf"` on the rationale that NM-inferred endpoints carry their own
inference error. Task 4 then measured that error at **median 0 s, IQR 17-25 s**. The filter
therefore now discards most of the ECAC sample for very little accuracy gain, and the paper
should either widen it or state explicitly why it does not. Explicitly told the implementer
NOT to change it — this is a claim-scope decision for the human, not a fix.

- Task 5 fix round 1/5 dispatched (resumed original implementer), scoped to three items.

### Task 5 fix round 1: `cd02e18` — 236/236, with kill-test verification

`Task 5: fix round 1/5 (3 addressed pending re-review; commits 3bf48a2..cd02e18)`

The implementer did the thing that makes a test worth having: it verified both new logic
tests are genuine **kill tests** by deleting the guard / restoring the old ordering and
watching them fail, and confirmed the tie-break really was nondeterministic before patching
it. A claimed kill test that would pass against the unfixed code is worse than no test —
it manufactures confidence. Re-review dispatched (sonnet) and told to check exactly that,
plus whether the new tie-break ordering silently changed why any of the seven original
tests pass.

New `boundary_error` test pins two APDF-sourced flights at 30 s/45 s and 90 s/100 s offsets
and asserts p50 ≈ 30/45 and p90 ≈ 90/100 — the first time that arithmetic has run at all.

### Task 5 fix round 1 re-review: all three ADDRESSED — and a sixth sibling found

Re-reviewer verified harder than the implementer did: ran `percentile_approx` live on the
exact `{30,90}` / `{45,100}` sets to confirm the asserted percentiles, and **re-implemented
the old `dropDuplicates` logic** against the tie fixture to prove the kill test genuinely
fails (old code gave `merged_pct=50.0`, the test asserts `100.0`). Regression check argued
from the code, not assumed: `range(0, n*10, 10)` reduces to the prior offsets, and no
original fixture ever reaches a tie, so the rewrite is a no-op for all seven.

**Ruling 23 (sibling tie bug in `boundary_error`) — override the reviewer's "deferred"
grade and fix it now.**
`boundary_error` does `best = ends.join(w, "flight_key").filter(n == best_n)` with **no
dedup and no tie-break at all**. An APDF-sourced flight tied across two tracks is retained
twice — double-counted into `n_apdf_flights` and skewing every percentile it feeds.

This is the **sixth** instance in this codebase of "a key or tie-break that silently picks
arbitrarily or fans out", and it sits in the file I just fixed for the same class. The
established pattern this run: fixing one instance while leaving its sibling is precisely how
the next one survives — Task 4 lost a whole extra review cycle to a join keyed correctly on
one side and not the other, inside a fix written to correct that very defect. I recorded
the lesson explicitly ("when a defect class is found once, sweep every sibling before
declaring it fixed"). Deferring a known sibling in the file I just fixed would contradict
that on the same day I wrote it. Cost if wrong: one small round on a rare (exact-tie) path.

Also ordered: a kill-test-verified test, and a sweep of every join / `best_` filter /
`dropDuplicates` / `Window` in `track_score.py`, with the result reported even if it is
"no further instances".

- Task 5 fix round 2/5 dispatched (resumed original implementer), scoped to one fix, one
  test, one sweep.

### Task 5 fix round 2: `1622d9b` — sibling swept, 237/237

`Task 5: fix round 2/5 (1 addressed pending re-review; commits cd02e18..1622d9b)`

Kill test verified: `n_apdf_flights` returned 2 rather than 1 against the untie-broken code.
Sweep result reported as asked — the two dominant-track picks (`match_rates`,
`boundary_error`) were the only "pick one row per entity" operations, and both now share the
same deterministic `row_number()`-over-window tie-break. The implementer also volunteered a
third site it judged safe rather than staying silent: `boundary_error`'s
`F.first(t_off/t_land)` is order-dependent in general but claimed provably safe because
`overlap_join` guarantees one identical value per `flight_key` upstream.

That `F.first` argument is the one thing I asked the re-reviewer to attack, because it rests
on a property of a *different file* and a wrong "provably safe" is more dangerous than an
acknowledged risk. Re-review dispatched (sonnet) with both files pre-authorised.

### `Task 5: complete (commits 2e84017..1622d9b, review clean)`

Round-2 re-review: ADDRESSED, sweep **verified** independently. Both dominant-track picks
now share one idiom; the reviewer confirmed `boundary_error` correctly drops the
merge-priority key (that concept has no `per_track` frame there), so it is a strict subset
of `match_rates`' ordering rather than a divergent decision. Kill test hand-traced: under
the old code `filter(n == best_n)` keeps both tied rows and `n_apdf_flights` returns 2.

**On the `F.first` claim — the reviewer improved it rather than just passing it.** The
argument substantially holds: `flight_key` is hashed before the join and `overlap_join`
attaches the same `t_off`/`t_land` to every matched sample, so all rows sharing a key carry
identical values. But **`t_land` is not part of the hash**, so safety rests on the physical
uniqueness of (aircraft, route, day, second-precision departure) rather than on anything the
code enforces. "Provably safe" overstates it. Added as deferred item 8 — a comment-accuracy
fix, not a bug.

### ALL FIVE TASKS COMPLETE

| Task | Commits | Rounds | Outcome |
|---|---|---|---|
| 1 Engine + A0 | `c0e8a3e..46c6995` | 0 | clean first review |
| 2 Arms A2-A4 | `..0013266` | 0 | clean first review |
| 3 Arms A5-A7 | `..4900128` | 0 | clean first review |
| 4 Ground truth | `..2e84017` | 3 | 5 Important + 1 new + 1 escalated, all fixed |
| 5 Metrics | `..1622d9b` | 2 | 2 Important + 1 escalated sibling, all fixed |

237 tests passing. 10 commits. Final whole-branch review dispatched on **opus** — the most
capable tier, per the skill, because this is the last gate before a human acts on the work.
Handed it all 7 deferred items with an explicit instruction that triaging each is a required
output, since a roll-up nobody acts on is a silent discard.

### FINAL WHOLE-BRANCH REVIEW (opus): **Needs fixes before merge**, no Critical

No Critical findings. `tracks.py` genuinely untouched (reviewer read both algorithms side by
side). Every unit conversion in the branch correct in direction and magnitude — including
`airport_anchored`'s feet-minus-feet, the one place two sources meet. 237 tests pass.

But it found three things **nine prior review cycles missed**, and demonstrated each by
running code rather than asserting it:

**F1. The frozen-algorithm lock passes with the group key wrong.** Both lock fixtures use a
single `(icao24, callsign)` pair, so there is only ever one group — and `_partition()`
compares sets of event times, which is invariant under *any* group-key change when there is
one group. The reviewer built a `group_cols=["icao24"]` variant (arm A4's key, not
production's) and showed it also matches the frozen algorithm. **The half of the property
that says "group on icao24 AND callsign" is entirely unverified.** The property holds today;
the test that exists to protect it cannot see it. This is the study's foundation.

**F2. `boundary_error` returns a perfect score on an empty sample.** `float(x or 0)` turns a
NULL percentile into `0.0` — the *best possible value* — so an arm with no APDF coverage
outscores every arm actually measured. `vmeasure` and `match_rates` degrade to 0.0 meaning
bad; this one inverts. And `test_score_arm_returns_a_flat_row_of_scalars` walks the empty
path and blesses it with `isinstance(v, (int, float))`. A test escorting the bug past review.

**F3. Arm A6 does not do what its docstring says.** `dwell_min = gap_minutes()` is the
interval from the *immediately preceding sample*, so across a run of parked samples it is
the sampling period — seconds — and never reaches `ground_dwell_minutes`. A6 fires only on
reception gaps; "or a run of parked samples" is false. Demonstrated: parked 30 min at 60 s
cadence gives `airport_anchored: 1 track`, `ground_anchored: 2`. Both A6 tests use
two-sample gap fixtures and cannot catch it.

**Ruling 24 (F3) — fix A6's behaviour, not its docstring.** Make it accumulate the
stationary run as A5 does. Not because the docstring says so, but because an A6 that can
only see gaps means comparing A5 to A6 conflates "on-ground flag vs airport geometry" with
"continuous coverage vs gap-only" — confounding the exact comparison this study exists to
make. Cost if wrong: A6 becomes a slightly different arm from the one first drafted, and the
paper must describe what it actually does.

**Ruling 25 (F4 fallback policy) — every domain arm ORs the legacy gap rule in as a floor.**
A5 did; A6 and A7 did not, and the reviewer showed why that is not neutral: A6 depends on
`near_airport`/`field_elev_ft`, which **nothing in this repo produces**. Task 6 supplies them
by left join, so off-aerodrome samples go NULL → no break → with no fallback, an entire month
in one track. A6 would score as a catastrophic merger for reasons unrelated to its idea, and
the paper would report that as a measurement. Uniform floor means each arm is "legacy plus
one idea" (what a one-change-at-a-time ladder requires) and a missing input degrades to
legacy instead of to a month-long track. Cost if wrong: no arm can score worse than legacy
on fragmentation — a real constraint, stated in the docstring rather than hidden.

**Ruling 26 (F5 contradiction) — reconcile the prose, do not touch the filter.**
`track_truth.py` says NM-inferred times are trusted for boundary error; `track_score.py`
says the opposite. Both in this branch. Whether to widen the filter is the human's
claim-scope call (deferred item 7), so the code stays and the docstrings are made to agree
on what is settled fact (the measurement) versus open decision (the filter's width).

**Ruling 27 (F6 dead config) — wire it, do not delete it.** `SegmentationConfig` is read by
nothing and its docstring's claim that `pipeline/segmentation` reads it is false; it is a
third parallel copy of the thresholds. Task 6 needs config-driven parameters, so add
`SegmentationParams.from_config()` **plus a test asserting the two default sets match field
by field** — that test is the actual fix, since it is what stops them drifting apart again.

Fix wave dispatched as ONE opus subagent with the complete list (per the skill: one wave,
not one fixer per finding), including three required negative controls.

### Fix wave: killed by a second session limit, work intact, resumed

Terminated mid-task (limit reset 06:00 UTC; noticed 07:14, already cleared). Controller-side
state check rather than assumption: HEAD still `1622d9b`, ten files modified and
uncommitted, **248 passing** (baseline 237, so +11). Spot-checked the two substantive fixes
and both landed:

- **F1** — lock fixture now carries three groups (same airframe on two callsigns; a second
  airframe on the first callsign), and the implementer went beyond the brief by adding
  `test_the_lock_rejects_the_wrong_group_key` as a **permanent** test. I asked for a one-off
  negative control; it made the control regression-protected. Better than specified.
- **F3/F4** — A6 accumulates the stationary run via
  `F.last(F.when(~stationary, _ts), ignorenulls=True)` and returns
  `gap_turnaround | parked_turnaround | legacy()`; A7 returns `climbing_away | legacy()`;
  `near_airport` coalesces to `False`, `field_elev_ft` to `0.0`; A7's dead `was_below` term
  is gone.

Resumed to finish the last item (`overlap_join` duplicate-sample guard), commit and report.
Explicitly instructed: report the negative controls actually observed, and **declare an
honest gap rather than reconstruct one** — a fabricated negative control is worse than none,
since it is the only evidence the fix works at all.

**Recurring environment note:** two of the four long-running implementer turns in this run
were killed by session limits, both mid-task with uncommitted work. Neither lost anything,
because the pattern held each time: check `git status` and `git log` before assuming
anything, run the suite to establish whether the work is sound, then resume the original
agent rather than starting cold. Worth keeping for anyone continuing this plan.

### Fix wave complete: `335e300`, 248 passing, all three negative controls captured

All eight items applied. Ruff clean of new findings; the one `I001` this branch added is
gone; `tracks.py` absent from the 10-file diff.

**Negative controls — all observed, none reconstructed** (this is the evidence the fixes do
anything at all, so it was the required output):
1. Old single-group fixture + `group_cols=["icao24"]` → **"Both sets are equal"**. The wrong
   group key produced a **byte-identical partition**. The lock genuinely could not see half
   its own property.
2. Unfixed `boundary_error` → `assert 0.0 is None` on both new tests: a perfect score from
   zero APDF rows.
3. Gap-only A6 → `assert 1 == 2` on a 30-minute parked run, with `legacy()==1` and
   `ground_anchored()==2` on the same fixture — reproducing the reviewer's demonstration.

`overlap_join`: implementer chose row-identity (`monotonically_increasing_id()`, following
the `_nm_id` precedent in the same file) over documenting a requirement callers cannot meet
today, since dedup is not implemented. Agreed — the alternative would have been a fiction.

**Ruling 25 AMENDED — the implementer found a consequence I did not foresee, and was right.**
A6's docstring claimed two fixes in *opposite* directions: **add** a split legacy misses (the
high-field-elevation turnaround) and **suppress** a split legacy invents (a long low slow
dwell nowhere near an aerodrome). With legacy ORed in as a floor, A6 ≥ legacy in splits
always, so direction 2 is unreachable **by construction** — its test became unsatisfiable,
not merely failing. The agent applied my ruling, rewrote the test to the surviving direction,
documented the cost, and escalated rather than silently absorbing the loss. Exactly right.

Neither of its two options was what I wanted. The conflict points at the real answer: A6's
thesis is that **legacy's `altitude < 5000 ft` is a crude proxy for "at an aerodrome"**, so
the floor should be the part of legacy A6 is *not* replacing, not the part it exists to
replace. **Amended: A6 ORs in only legacy's general 30-minute gap rule, not the
low-altitude rule.** This keeps NULL-input safety (a >30 min gap still splits, so no
month-long tracks), restores direction 2 as measurable (a 20-minute low dwell away from an
aerodrome: legacy splits, A6 does not), and leaves direction 1 untouched.

**A6 only.** Checked A5 and A7: both are purely additive (A5 adds the continuous-turnaround
split, A7 the descent-climb split), neither claims to remove a legacy split, so a full floor
costs them nothing. A6 is the only arm with a suppression thesis, which is exactly why it is
the only one that conflicted. Cost if wrong: A6 under-splits relative to legacy in some
population the floor would have caught — visible in its fragmentation/merge scores, and the
paper reports both.

### Amendment applied: `d7a40a5`, 249 passing

New negative control captured for the amendment itself: reverting A6's fallback to the full
`legacy().break_expr(p)` gives `AssertionError: assert 2 == 1` — the unsatisfiability the
implementer argued in prose, now demonstrated. Passes against the amended floor.

**The implementer improved on the instruction twice, both times correctly.** I said "do not
inline a bare 30-minute threshold"; it went further and split `legacy()`'s expression into
two named exported halves, `legacy_general_gap` and `legacy_low_altitude_gap`, with
`legacy()` as their OR — so A6's fallback reads as a named concept with a cross-reference
rather than a magic number. And it **renamed** the NULL-input test from
`..._degrades_to_legacy_...` to `..._still_splits_on_a_long_gap_...`, because after the
amendment A6 no longer degrades to legacy but to the general gap rule — the old name would
have been a false claim about the code. Catching that a *test name* had become a lie is the
kind of thing that survives review by not being looked at.

Ruff byte-identical to the pre-amendment run; `tracks.py` still absent; A5/A7 verified
untouched (`grep "legacy().break_expr(p)"` returns exactly two hits).

Single scoped re-review of the whole fix wave dispatched (`1622d9b..d7a40a5`, sonnet) — the
skill allows exactly one, with residuals adjudicated rather than fixed. It is told to check
that splitting `legacy()` did not change the subject of the frozen-algorithm lock, since
that refactor touches the very expression the lock protects.

### Deferred-findings triage returned by the final review

Items 1-5 **acceptable to carry**; item 6 (`F.first` "provably safe" wording) **must fix**;
item 7 (APDF filter width) **flagged for the human**, and upgraded — it is not merely a
scope decision but an internal contradiction (F5). The reviewer also corrected my note on
item 5: the arrival day-key fix *recovers* midnight-crossers rather than trading them away.
My ledger had it backwards.

### Deferred items originally logged (7, +1 below)

8. `boundary_error`'s in-code "provably safe" claim about `F.first(t_off/t_land)` should be
   downgraded to state the assumption it actually rests on (physical flight uniqueness),
   since `t_land` is not in the `flight_key` hash.



1. Task 1 — 19 pre-existing `I001`/`F401` ruff findings in `tests/`, plus 1 new of the same
   pattern from a brief-mandated import block.
2. Task 2 — one `I001` import-ordering finding in `tests/test_segmentation_methods.py`.
   A single `uvx ruff check --fix tests/` clears this and item 1 together.
3. Task 2 — `test_airframe_only_still_splits_a_null_callsign_airframe_on_gaps`'s docstring
   promises more than its fixture proves (the arm never reads callsign, so it would pass
   with any value).
4. Task 4 — proximity windows order only on `F.abs(...)`; exactly-equidistant candidates
   tie non-deterministically. Rare (sub-second), but the same class as Ruling 22.
5. Task 4 — `monotonically_increasing_id()` not guaranteed stable across a Spark stage
   recompute. Latent, single-lineage use here.
6. Task 4 — arrival day-key fix trades APDF coverage for midnight-spanning flights; cheap
   given `ARVT_3` ≈ ALDT, but should be stated in the paper.
7. Task 5 / Task 9 — `boundary_error`'s `t_source == "apdf"` filter predates Task 4's
   measurement that NM-inferred boundaries are accurate to median 0 s / IQR 17-25 s. The
   filter now discards most of the ECAC sample for little gain. **Claim-scope decision for
   the human, not a code fix.**

### SUPERSEDED — pending ruling on Task 4's `overlap_join` deviation (now Ruling 9)

The brief's `overlap_join` ends with an explicit select naming `g.gt_adep`, `g.gt_ades`,
`g.t_source`, but the brief's own Step 2 fixtures never populate them, so all five tests
failed with `UNRESOLVED_COLUMN` against the plan's code as written. **Third plan defect of
the same family**: module and tests written without cross-checking their column sets.
The implementer changed the select to a generic pass-through of whatever `gt` carries.
Holding my ruling until the reviewer gives an independent read — the trade-off is fail-fast
(explicit select, catches a missing `t_source` at the join) versus flexibility (generic,
defers that failure into Task 5's `boundary_error`). Recorded now so it cannot be lost.

**Risk noted for Task 6:** `load_flight_intervals` and `load_apdf_times` have no unit tests
— only `overlap_join` does, which matches the brief's scope but means Task 6 will be their
first end-to-end execution against real S3 parquet (column casing, `TAXI_TIME_3` nulls).

- **Task 5 metric expectations pre-verified** (`probe_metrics.py`, throwaway, standalone
  reimplementation of `track_score.py`). All 14 assertions measured and correct:
  perfect segmentation → homogeneity/completeness/V all 1.0; total merge → completeness
  1.0, homogeneity 0.0; total fragmentation → the exact reverse (so the two are **not**
  transposed — the failure mode that looks plausible either way); the 4-flight case →
  25% clean / 25% fragmented / 50% merged, summing to exactly 100; and the 9-vs-1 stray
  fragment → 0% clean, 100% fragmented, confirming the strict definition bites.
  My controller-side rewrite of `match_rates` during the plan's self-review is correct.

**Deferred minor (Task 2):** `tests/test_segmentation_methods.py` has one `I001`
import-ordering finding (`from conftest import ...` placed before the `opdi.*` block,
whereas Task 1's test file puts it after). Same class as the Task 1 debt below. A single
`uvx ruff check --fix tests/` pass would clear both; left for the final review to triage
so the fix goes through a review seat.

**Deferred minor (Task 2):** `test_airframe_only_still_splits_a_null_callsign_airframe_on_gaps`
has a docstring implying it specifically exercises null-callsign handling, but
`airframe_only` never reads callsign, so it would pass with any value. The test is still
valid (it verifies gap-splitting survives the group-key change) and the plan's own text
already says "the point is that it does not do *worse*" — but the prose promises more
than the fixture proves. Worth tightening when the paper is written.

**Deferred minor (Task 1):** `tests/` carries 19 pre-existing `I001`/`F401` ruff findings
unrelated to this work; the new test file adds a 20th of the same pattern because its
imports were transcribed verbatim from the brief. Not fixed: the brief mandated the
import block, and reformatting pre-existing lint debt is outside this task. Flag to the
final whole-branch review for triage.

