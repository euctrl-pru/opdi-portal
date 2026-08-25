# Track Construction: ship the segmentation, revise V1, publish V2

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the recommended segmentation and the flight-list labelling fix in
the `opdi/` pipeline behind a new version string; revise
`papers/track-construction-v1/` against nine reviewer comments; and publish
`papers/track-construction-v2/` as a release note measured by running the real
pipeline.

**Architecture:** Three workstreams in strict order. Production code changes
first, because both papers' regeneration jobs fingerprint the files they touch —
changing `flights.py` after a run would silently invalidate it. Then the
measurements. Then the papers.

**Tech Stack:** PySpark 4.1.1 on Python 3.10 (`.venv310`), Kubernetes namespace
`eurocontrol`, parquet over S3A at `s3a://eurocontrol/opdi`, Quarto + knitr (R).

**Spec:** none written. This plan is the spec. Its requirements come from the
reviewer's nine comments on V1 (2026-08-25) and four decisions taken with the
user the same day, recorded verbatim in Global Constraints.

## Global Constraints

### Decisions taken with the user

- **`standard` becomes the default. No new version column.**
  `SegmentationConfig.method` flips from `"legacy"` to `"standard"`.
  A version gate was considered and **dropped**: `osn_tracks` carries no version
  column today, and the user's ruling was not to add one. The discontinuity is
  therefore documented in the config docstring, the release note and
  `CLAUDE.md`, not in the data. `FLIGHT_LIST_VERSION` is a separate, existing
  string already emitted as a column, and bumping it stays in scope.
- **Changes are committed locally and not pushed.** No PR, no push, no merge.
  Report the branch and worktree path at the end.
  *Risk, stated once:* a worktree can be deleted with its session. This is the
  user's explicit instruction and is followed, not worked around.
- **Interval containment: measure before changing anything.** Count what the
  wholly-inside-window restriction discards and report it. Do not alter the
  metric definition in this plan.
- **V1 is revised in place.** No v1.1. V1 was committed but never pushed or
  wired into the site, so there is no published version to preserve.

### Audience and prose

- **The reader knows OPDI; they do not know this methodology.** Assume state
  vectors, ADEP/ADES, `icao24` and the pipeline's existence are familiar. Do not
  assume homogeneity, completeness, V-measure, interval containment, boundary
  error, or any arm name means anything to them.
- **Every term is defined at first use, and every section opens by saying what
  it is for.** A section that starts with a table is a section that has skipped
  its introduction.
- Both papers must read smoothly front to back. A reader should never have to
  jump forward to understand a sentence.

### Engineering

- **Nothing production is touched by a benchmark.** Every table redirects under
  `research/tcv2/`; the write guard checks the *resolved destination*, never the
  logical table name.
- **One Spark job at a time.** Driver port pinned to 7078; cluster quota 30 CPU
  / 192 GiB. A second concurrent job kills both.
- **S3 is a shared 100 GB bucket.** Batch `DeleteObjects` is broken on this
  endpoint — single-object deletes only. Never delete a prefix this study did not
  create. `opdi/osn_symposium_paper_2026/` belongs to another project.
- **Units: SI in storage, aviation in anything human-facing**, with the unit in
  the field name. Reuse `FT_PER_M = 3.28084`, `KT_PER_MPS = 1.94384`.
- **Never mutate a published `version` string.** New behaviour gets a new value.
- **Never push to `main`/`master`, never force-push, never merge.**
- **No number is typed into a paper's prose.** Every figure is an inline R
  expression over a CSV in `data/`, produced by exactly one job in the paper's
  `regenerate_*.py` and stamped by `benchmarks/provenance.py`.
- **Do not install into `.venv310`.** Lint with `uvx ruff`.

## Repository layout

Two git worktrees:

| Role | Path | Referred to as |
|---|---|---|
| Pipeline + benchmarks | `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1` | `$OPDI` |
| Papers (portal) | `/home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan` | `$PORTAL` |

## File Structure

**Create:**

- `$OPDI/benchmarks/track_continuity.py` — how much `track_id` changes between
  two methods over the same state vectors.
- `$OPDI/benchmarks/track_diagnostics.py` — NULL-altitude rate; the ground-truth
  containment census; one worked divergence example.
- `$OPDI/benchmarks/regenerate_track_v2.py` — executable definition of V2.
- `$OPDI/tests/test_flights_labelling.py`, `$OPDI/tests/test_track_continuity.py`,
  `$OPDI/tests/test_track_pipeline_v2.py`, `$OPDI/tests/test_segmentation_default.py`.
- `$PORTAL/papers/track-construction-v2/index.qmd` and `data/`.

**Modify:**

- `$OPDI/src/opdi/pipeline/flights.py` — the labelling aggregate; version bump.
- `$OPDI/src/opdi/config.py` — default segmentation method.
- `$OPDI/benchmarks/track_pipeline_v2.py` — redirect fix; paired scoring; exports.
- `$OPDI/benchmarks/track_sweep.py` — per-axis profile output (Task 6).
- `$OPDI/benchmarks/regenerate_track_v1.py` — new jobs for the V1 revision.
- `$PORTAL/papers/track-construction-v1/index.qmd` — the nine comments.
- `$PORTAL/papers/_quarto.yml`, `$PORTAL/papers/index.qmd`.

---

## Workstream A — production code

### Task 1: Redirect every table step 03 writes

The V2 smoke run died here, and the guard was right to kill it:

```
RuntimeError: refusing to write 'opdi_endpoint_candidates'
(resolves to 's3a://eurocontrol/opdi/opdi_endpoint_candidates'):
this benchmark writes only under 'research/'
```

`redirect_tables()` maps the three tables in `TABLES`. Step 03 writes **two**:
`opdi_flight_list` and `opdi_endpoint_candidates`. The candidates table was not
in the map, so it resolved to the production path — and it is written
`mode="overwrite"`, so an unguarded run would have destroyed the live cache and
cross-contaminated every subsequent arm. This is the same class of bug V1 hit;
the guard exists because of it.

**Files:**
- Modify: `$OPDI/benchmarks/track_pipeline_v2.py:63` (`TABLES`)
- Test: `$OPDI/tests/test_track_pipeline_v2.py`

**Interfaces:**
- Produces: `TABLES` gains `"opdi_endpoint_candidates"`; `table_for(method,
  name)` therefore resolves it under `research/tcv2/<method>/`.

- [ ] **Step 1: Write the failing test**

```python
# $OPDI/tests/test_track_pipeline_v2.py
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "benchmarks"))

import track_pipeline_v2 as tp  # noqa: E402


def test_every_table_step_03_writes_is_redirected():
    """The guard catches an unredirected write; the map must make it unnecessary.

    `opdi_endpoint_candidates` is written mode="overwrite". Unredirected it
    resolves to the production cache, so an unguarded run destroys it and every
    later arm reads another arm's candidates. The smoke run proved the guard
    fires; this proves it should not have to.
    """
    assert "opdi_endpoint_candidates" in tp.TABLES
    assert "opdi_flight_list" in tp.TABLES
    resolved = tp.table_for("legacy", "opdi_endpoint_candidates")
    assert resolved.startswith("research/tcv2/legacy/")
```

- [ ] **Step 2: Run it to confirm it fails**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_track_pipeline_v2.py -v
```

Expected: FAIL on the first assert.

- [ ] **Step 3: Add the table**

In `$OPDI/benchmarks/track_pipeline_v2.py`, replace the `TABLES` definition:

```python
#: Every table the run materialises, in the order the steps write them.
#:
#: `opdi_endpoint_candidates` is here because step 03 writes it, not because
#: anything reads it afterwards -- and it is written mode="overwrite". Left out
#: of the redirect it resolves to the production cache, so a run would delete
#: real data and then feed each arm the previous arm's candidates. The write
#: guard caught exactly that on the first end-to-end attempt. A table this list
#: forgets is a table the guard has to stop, and the guard stops the whole run.
TABLES = ("osn_tracks", "osn_tracks_clean", "opdi_flight_list",
          "opdi_endpoint_candidates")
```

- [ ] **Step 4: Run the test to confirm it passes**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_track_pipeline_v2.py -v
```

Expected: 1 passed.

- [ ] **Step 5: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check benchmarks/track_pipeline_v2.py tests/test_track_pipeline_v2.py
git add benchmarks/track_pipeline_v2.py tests/test_track_pipeline_v2.py
git commit -m "fix(bench): redirect the candidates table step 03 also writes

The first end-to-end run died on the write guard: opdi_endpoint_candidates was
not in the redirect map, so it resolved to the production cache -- which step 03
writes mode=overwrite. The guard was right. The map was incomplete."
```

---

### Task 2: Fix the flight list's labelling aggregate

Reviewer comments 1 and 8: *"fix one aggregate in the flight list, then ship the
segmentation — Yes let's do this!"* and *"7.2 you detail something that needs to
be fixed. This should just be fixed."*

`flights.py:441` labels a flight `F.min("flight_id")` — the lexicographically
smallest callsign in the track. Line 422 fills nulls with `""`, and a blank
sorts before every real callsign. While callsign is part of the track's group
key this is harmless: every track is callsign-homogeneous by construction, so
the minimum is the only value there is. It stops being harmless the moment a
segmentation drops callsign from that key. Measured on 2025, **64% of
`airframe_only` tracks carry a real callsign alongside blanks**, and `F.min`
returns the blank — so the flight matches no ground truth at all, and ADEP/ADES
coverage falls from 80% to 54% for reasons that have nothing to do with
segmentation.

The replacement is the track's **most frequent non-blank** callsign. Most
frequent rather than smallest-real: picking alphabetically would fix the blank
by reinstating the arbitrary tie-break that caused it. Ties break on the
callsign itself, so the result does not depend on partitioning.

**Files:**
- Modify: `$OPDI/src/opdi/pipeline/flights.py:88` (`FLIGHT_LIST_VERSION`)
- Modify: `$OPDI/src/opdi/pipeline/flights.py:435-445` (the aggregate)
- Test: `$OPDI/tests/test_flights_labelling.py`

**Interfaces:**
- Produces: the `_FLT_ID` column is the dominant non-blank callsign of the
  track, or `""` when the track never broadcast one.
- `FLIGHT_LIST_VERSION` becomes `"v5.0.0"`.

- [ ] **Step 1: Read the current aggregate before changing it**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
sed -n '415,455p' src/opdi/pipeline/flights.py
```

The exact surrounding `groupBy`/`agg` shape must be preserved; only the one
aggregate expression changes.

- [ ] **Step 2: Write the failing tests**

```python
# $OPDI/tests/test_flights_labelling.py
"""The flight list must label a track with the callsign it actually flew."""
from opdi.pipeline.flights import dominant_flight_id


def test_blank_samples_do_not_win_the_label(spark):
    """F.min returns "" here. That is the bug, in one test."""
    df = spark.createDataFrame(
        [("t1", ""), ("t1", ""), ("t1", "SAS123"), ("t1", "SAS123")],
        "track_id string, flight_id string",
    )
    out = {r["track_id"]: r["_FLT_ID"]
           for r in df.groupBy("track_id").agg(dominant_flight_id()).collect()}
    assert out == {"t1": "SAS123"}


def test_the_most_frequent_callsign_wins_not_the_smallest(spark):
    """Two real callsigns in one track: frequency decides, not the alphabet."""
    df = spark.createDataFrame(
        [("t1", "ZZZ999"), ("t1", "ZZZ999"), ("t1", "ZZZ999"), ("t1", "AAA111")],
        "track_id string, flight_id string",
    )
    out = {r["track_id"]: r["_FLT_ID"]
           for r in df.groupBy("track_id").agg(dominant_flight_id()).collect()}
    assert out == {"t1": "ZZZ999"}


def test_ties_break_deterministically_on_the_callsign(spark):
    """Equal counts must not depend on partitioning, or two runs disagree."""
    df = spark.createDataFrame(
        [("t1", "BBB222"), ("t1", "AAA111")],
        "track_id string, flight_id string",
    )
    out = {r["track_id"]: r["_FLT_ID"]
           for r in df.groupBy("track_id").agg(dominant_flight_id()).collect()}
    assert out == {"t1": "AAA111"}


def test_a_track_that_never_broadcast_a_callsign_keeps_a_blank(spark):
    """Not NULL, and not dropped: the flight exists, it is just unlabelled."""
    df = spark.createDataFrame(
        [("t1", ""), ("t1", "")], "track_id string, flight_id string")
    out = {r["track_id"]: r["_FLT_ID"]
           for r in df.groupBy("track_id").agg(dominant_flight_id()).collect()}
    assert out == {"t1": ""}


def test_a_callsign_homogeneous_track_is_unchanged(spark):
    """Legacy tracks are homogeneous by construction; the fix must be a no-op.

    If this fails, the change is not backward compatible for legacy data and the
    version bump is hiding a regression rather than describing an improvement.
    """
    df = spark.createDataFrame(
        [("t1", "SAS123"), ("t1", "SAS123")],
        "track_id string, flight_id string",
    )
    out = {r["track_id"]: r["_FLT_ID"]
           for r in df.groupBy("track_id").agg(dominant_flight_id()).collect()}
    assert out == {"t1": "SAS123"}
```

- [ ] **Step 3: Run them to confirm they fail**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_flights_labelling.py -v
```

Expected: `ImportError: cannot import name 'dominant_flight_id'`.

- [ ] **Step 4: Implement it**

Add to `$OPDI/src/opdi/pipeline/flights.py`, near the other module-level
helpers:

```python
def dominant_flight_id():
    """The callsign a track actually flew, as a single aggregate expression.

    Replaces ``F.min("flight_id")``. The minimum is the lexicographically
    smallest callsign in the track, and line 422 fills nulls with ``""``, which
    sorts before everything -- so any track carrying one blank sample is labelled
    blank.

    That was correct while ``callsign`` was part of the track's group key: every
    track was then callsign-homogeneous and the minimum was the only value
    present. It stops being correct the moment a segmentation groups on the
    airframe alone. Measured on 2025, 64% of such tracks carry a real callsign
    alongside blanks, and the resulting blank labels drop ADEP/ADES coverage from
    80% to 54% -- a collapse with no segmentation cause at all.

    **Most frequent, not smallest-real.** Choosing the alphabetically smallest
    real callsign would fix the blank by restoring the arbitrary tie-break that
    caused the problem. Ties break on the callsign itself, so the result is
    deterministic rather than dependent on how Spark partitioned the input.

    A track that never broadcast a callsign keeps ``""``. It is an unlabelled
    flight, not an absent one, and dropping it would shrink the denominator of
    every downstream rate.
    """
    real = F.when(F.trim(F.col("flight_id")) != "", F.trim(F.col("flight_id")))
    # sort_array on (-count, callsign) structs: the head is the most frequent,
    # ties resolved by the callsign. Done inside the aggregate so this stays one
    # expression and the caller's groupBy shape is untouched.
    counted = F.map_entries(
        F.aggregate(
            F.collect_list(real),
            F.create_map().cast("map<string,int>"),
            lambda acc, x: F.map_concat(
                F.map_filter(acc, lambda k, _v: k != x),
                F.create_map(x, F.coalesce(acc[x], F.lit(0)) + F.lit(1)),
            ),
        )
    )
    ranked = F.sort_array(
        F.transform(counted, lambda e: F.struct(
            (-e["value"]).alias("neg_n"), e["key"].alias("callsign")))
    )
    return F.coalesce(ranked[0]["callsign"], F.lit("")).alias("_FLT_ID")
```

> **Implementer note:** if the `aggregate`/`map_concat` form proves awkward on
> this Spark version, the equivalent two-stage form is acceptable — count per
> `(track_id, flight_id)` in one `groupBy`, rank with a window on
> `(count desc, callsign asc)`, take rank 1, then join back. It must produce
> identical results on all five tests. Do not change the tests to fit the
> implementation.

Then replace line 441's `F.min("flight_id").alias("_FLT_ID"),` with:

```python
                dominant_flight_id(),
```

- [ ] **Step 5: Bump the version**

At `$OPDI/src/opdi/pipeline/flights.py:88`:

```python
#: v5.0.0: a flight is labelled with its track's dominant non-blank callsign
#: rather than F.min("flight_id"). Under legacy segmentation the two agree on
#: every track, so the bump describes a capability rather than a correction to
#: published data -- but the value changes for any segmentation that does not
#: carry callsign in its group key, and published data must say which rule
#: produced it. Never mutate a released value.
FLIGHT_LIST_VERSION = "v5.0.0"
```

- [ ] **Step 6: Run the tests and the full suite**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_flights_labelling.py -v
.venv310/bin/python -m pytest tests/ -q
```

Expected: 5 passed, then the whole suite green (260+ as of 2026-08-23).

- [ ] **Step 7: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check src/opdi/pipeline/flights.py tests/test_flights_labelling.py
git add src/opdi/pipeline/flights.py tests/test_flights_labelling.py
git commit -m "fix(flights): label a flight with the callsign it actually flew

F.min(\"flight_id\") returns the blank, because nulls are filled with \"\" and a
blank sorts first. Harmless while callsign is in the track's group key -- every
track is then homogeneous -- and a 26-point ADEP/ADES coverage loss the moment it
is not. Replaced with the dominant non-blank callsign, ties broken on the
callsign so the result does not depend on partitioning.

A no-op on legacy tracks, verified by test. FLIGHT_LIST_VERSION -> v5.0.0."
```

---

### Task 3: Ship `standard` as the default

Reviewer comment 9: *"implement the whole recommended algorithm in the opdi/
main branch"*, with the user's decision that it becomes the default.

**No version column.** A gate stamping each row with the segmentation that
produced it was considered and dropped: `osn_tracks` has no version column
today, and the user ruled against adding one. The consequence is that published
data does not self-identify, so **the discontinuity has to be documented where
people will actually meet it** — the config docstring, the release note, and
`CLAUDE.md` — because nothing in the data will say it.

`recommended()` already exists in `src/opdi/pipeline/segmentation/methods.py`,
including the bounded-lookback fix to `prev_real` — the subtlety comment 9 calls
"the prev". An unbounded `F.last` reaches back past a gap break into the previous
flight, so a track whose callsign starts blank and resolves later gets compared
against the callsign from before the gap and splits in its own middle. Measured,
that cost 31 points of fragmentation. The fix requires the previous real
callsign to be no older than `gap_minutes`; a longer silence is itself a break,
so there is nothing to carry across. **Verify this bound is present before
flipping the default** — shipping the unbounded form would be worse than
shipping nothing.

**Files:**
- Modify: `$OPDI/src/opdi/config.py` (`SegmentationConfig.method`)
- Test: `$OPDI/tests/test_segmentation_default.py`

**Interfaces:**
- Produces: `SegmentationConfig.method` default `"standard"`. No schema change,
  no new field.

- [ ] **Step 1: Verify the bounded lookback is in place**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
grep -n "recent" -B 4 -A 6 src/opdi/pipeline/segmentation/methods.py | sed -n '1,40p'
.venv310/bin/python -m pytest tests/test_tracks_method.py -v
```

Expected: the `recent = (... ) / 60.0 < p.gap_minutes` guard is present in
`recommended()`, and the five method-dispatch tests pass. **If the guard is
absent, stop and fix it before any other step in this task.**

- [ ] **Step 2: Write the failing tests**

```python
# $OPDI/tests/test_segmentation_default.py
"""The shipped segmentation, asserted where it cannot be quietly reverted."""
from opdi.config import OPDIConfig
from opdi.pipeline.segmentation.methods import ARMS


def test_standard_is_the_default_segmentation():
    cfg = OPDIConfig.for_environment("opensky")
    assert cfg.segmentation.method == "standard"


def test_every_environment_ships_the_same_segmentation():
    """A default that varies by environment is a default nobody can reason about.

    dev and live would otherwise be able to publish track_ids that local runs
    cannot reproduce, and nothing in the data would say why.
    """
    for env in ("opensky", "local", "dev", "live"):
        assert OPDIConfig.for_environment(env).segmentation.method == "standard"


def test_legacy_is_still_reachable():
    """Reproducing a pre-release track_id must remain possible.

    Without this the old ids become unreproducible by any configuration, which
    is a stronger break than the release intends.
    """
    assert "legacy" in ARMS


def test_standard_resolves_to_the_recommended_rule():
    """`standard` is an alias. If it ever stops resolving to the arm the study
    measured, the shipped algorithm and the published evidence part company."""
    rule = ARMS["recommended"]()
    assert rule.group_cols == ["icao24"]
    assert rule.month_suffix is False
```

- [ ] **Step 3: Run them to confirm they fail**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_segmentation_default.py -v
```

Expected: the first two fail (default is still `"legacy"`); the last two pass
already, and are there to stay passing.

- [ ] **Step 4: Flip the default**

In `$OPDI/src/opdi/config.py`, on `SegmentationConfig`:

```python
    method: str = "standard"
    """Which segmentation builds ``track_id``.

    ``"legacy"`` is the rule that produced every release up to and including
    OPDI v0.0.2: group on ``SHA2(icao24 || callsign)``, split on a gap over
    ``gap_minutes`` or a shorter gap below ``low_alt_gap_ft``, and suffix the id
    with ``_{year}_{month}``.

    ``"standard"`` is the rule this release ships. It groups on the airframe
    alone and splits when the last *non-blank* callsign genuinely changes. Two
    independent failures of the legacy key motivate it, and they are separable:
    blank callsigns formed tracks of their own (42.4% of legacy tracks are
    blank-labelled, which is where its fragmentation comes from), and a callsign
    change mid-airframe was invisible once callsign was in the key.

    .. warning::

       **This changes ``track_id`` for all data produced from this release
       forward**, in shape as well as value -- there is no ``_{year}_{month}``
       suffix. A consumer joining on ``track_id`` across the boundary gets an
       empty join rather than an error, which reads as missing data.

       **Nothing in the published data says which rule produced a row.**
       ``osn_tracks`` carries no version column, so the only way to tell is to
       know when the row was published. Anyone comparing data across the release
       has to be told; the table will not tell them.

       Set this back to ``"legacy"`` to reproduce pre-release ``track_id``
       values. Reproducing them also requires the *raw* altitudes, which
       ``osn_tracks_clean`` no longer carries -- see the V2 paper.
    """
```

- [ ] **Step 5: Run the tests and the full suite**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_segmentation_default.py tests/test_tracks_method.py -v
.venv310/bin/python -m pytest tests/ -q
```

Expected: all pass. **Any test that asserted `method == "legacy"` as the default
must be updated to assert the new default explicitly, not deleted** — a deleted
test is a lost guarantee.

- [ ] **Step 6: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check src/opdi/config.py tests/test_segmentation_default.py
git add src/opdi/config.py tests/test_segmentation_default.py
git commit -m "feat(tracks): ship the recommended segmentation as the default

segmentation.method now defaults to \"standard\": group on the airframe, split
when the last non-blank callsign genuinely changes. Legacy remains selectable
and reproduces pre-release ids.

track_id changes for all data from here forward, in shape as well as value, and
a consumer joining across the boundary gets an empty join rather than an error.
osn_tracks carries no version column and none is added, so the data cannot say
which rule produced it -- the discontinuity is documented in this docstring, the
release note and CLAUDE.md instead, and consumers have to be told."
```

---

## Workstream B — measurements the V1 revision needs

### Task 4: The containment census, and the boundary-error distribution

Two reviewer comments, one job each, both cheap and both read by the revised V1.

**Comment 3 — containment.** V1 restricts ground truth to flights whose whole
airborne interval lies inside the sample window, and the reviewer asks whether
that discards something useful. The restriction exists because a flight only
half-observed *must* score as fragmented or truncated no matter how good the
segmentation is — the samples simply are not there — so including it measures
the window, not the algorithm. That is the defence; it is not the same as
showing the cost. This job counts what is excluded so the paper can state it.

**Comment 4 — boundary error.** V1 reports p10/p50/p90 of the signed offset
between a track's ends and the flight's ATOT/ALDT. The reviewer wants the
distribution. Percentiles hide shape: a symmetric spread and a bimodal one with
the same p50 are different failures, and only one of them is a tuning problem.

**Files:**
- Modify: `$OPDI/benchmarks/track_diagnostics.py` (created in Task 5 of
  Workstream C order — if not yet present, create it here)
- Modify: `$OPDI/benchmarks/regenerate_track_v1.py` (two new jobs)

**Interfaces:**
- Produces `containment_<period>.csv`: `period`, `n_gt_flights`,
  `n_wholly_inside`, `n_clipped_start`, `n_clipped_end`, `pct_kept`,
  `median_observed_fraction_clipped`.
- Produces `boundary_hist_<period>.csv`: `arm`, `edge` (`off`/`land`),
  `bin_lower_s`, `bin_upper_s`, `n`.

- [ ] **Step 1: Write the containment census**

Add to `$OPDI/benchmarks/track_diagnostics.py`:

```python
def containment_census(gt, window_start, window_end) -> dict:
    """What the wholly-inside-the-window restriction excludes, and how much.

    V1 scores only ground-truth flights whose entire airborne interval lies
    inside the sample window. The reason is not tidiness: a flight observed for
    its last twenty minutes has no samples for the rest of itself, so it scores
    as truncated however well the segmentation performed. Including it would
    measure where the window fell, not what the algorithm did.

    That argument justifies the restriction; it does not measure its price.
    This does -- how many flights go, and how much of each one was actually
    visible -- so the paper can state the cost rather than assert there is none.
    """
    inside = (F.col("t_start") >= F.lit(window_start)) & (
        F.col("t_end") <= F.lit(window_end))
    clipped_start = F.col("t_start") < F.lit(window_start)
    clipped_end = F.col("t_end") > F.lit(window_end)

    observed = (
        F.least(F.col("t_end").cast("long"), F.lit(window_end).cast("long"))
        - F.greatest(F.col("t_start").cast("long"),
                     F.lit(window_start).cast("long"))
    )
    total = F.col("t_end").cast("long") - F.col("t_start").cast("long")

    agg = gt.select(
        F.count(F.lit(1)).alias("n"),
        F.sum(F.when(inside, 1).otherwise(0)).alias("n_in"),
        F.sum(F.when(clipped_start, 1).otherwise(0)).alias("n_cs"),
        F.sum(F.when(clipped_end, 1).otherwise(0)).alias("n_ce"),
    ).collect()[0]

    frac = gt.filter(~inside).select(
        (observed / F.when(total > 0, total)).alias("f")
    ).approxQuantile("f", [0.5], 0.01)

    return {
        "n_gt_flights": agg["n"],
        "n_wholly_inside": agg["n_in"],
        "n_clipped_start": agg["n_cs"],
        "n_clipped_end": agg["n_ce"],
        "pct_kept": round(100.0 * agg["n_in"] / agg["n"], 2) if agg["n"] else 0.0,
        "median_observed_fraction_clipped": round(frac[0], 3) if frac else None,
    }
```

- [ ] **Step 2: Write the boundary-error histogram**

```python
def boundary_histogram(matched, extents, bin_seconds: int = 30,
                       span_seconds: int = 900):
    """The signed boundary offsets as a distribution, not three percentiles.

    p10/p50/p90 cannot distinguish a symmetric spread from a bimodal one, and
    the two mean different things: a spread is noise to be tuned against, two
    modes are two populations, one of which is probably a different failure
    wearing the same number.

    Sign convention follows :func:`track_score.boundary_error` -- a negative
    ``off`` means the track starts *before* take-off. Bins are clamped to
    +/- ``span_seconds`` so the tails do not stretch the axis into
    uselessness; the clamped counts stay in the end bins rather than being
    dropped, so the histogram still sums to the sample.
    """
    ...  # implementer: reuse boundary_error's join, then bucket the signed
         # offsets with F.floor(off / bin_seconds) * bin_seconds, clamped.
```

> **Implementer note:** `track_score.boundary_error` at
> `benchmarks/track_score.py:173` already computes the signed offsets and the
> `extents` join. Factor that join into a helper both functions call rather than
> duplicating it — a second copy will drift from the first, and the sign
> convention is exactly the thing that must not drift.

- [ ] **Step 3: Declare both jobs in the V1 regeneration spec**

Add to `jobs()` in `$OPDI/benchmarks/regenerate_track_v1.py`, one pair per
period, with `code_paths` including `benchmarks/track_diagnostics.py`.

- [ ] **Step 4: Run them**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -u benchmarks/regenerate_track_v1.py --only containment_2025 containment_2024 boundary_hist_2025 boundary_hist_2024
```

- [ ] **Step 5: Report the containment number before writing prose about it**

State `pct_kept` and `median_observed_fraction_clipped` for both periods. **If
more than ~15% of ground-truth flights are excluded, say so explicitly in this
task's report** — the user asked to measure before deciding, and a large number
is a decision they wanted to make, not one to absorb into a paragraph.

- [ ] **Step 6: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check benchmarks/track_diagnostics.py benchmarks/regenerate_track_v1.py
git add benchmarks/track_diagnostics.py benchmarks/regenerate_track_v1.py
git commit -m "feat(bench): measure what the containment rule excludes, and the boundary distribution"

cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git add papers/track-construction-v1/data/
git commit -m "data(track-v1): containment census and boundary-error histograms"
```

---

### Task 5: Re-run V1's payoff jobs against the fixed flight list

Task 2 changed `flights.py`, which `regenerate_track_v1.py` declares as a
dependency of the payoff jobs. They are therefore **stale by fingerprint**, and
correctly so: their numbers describe a flight list that no longer exists.

This also collapses V1's §7.2/§7.3 structure. Those two sections exist to show
what `F.min` costs by measuring with and without a repair applied in the
benchmark. Once the repair is in production, the "repaired" number *is* the
number, and the pair becomes history rather than a finding. Section 7 is
rewritten accordingly in Task 6.

**Files:** none created; outputs replace existing CSVs in
`$PORTAL/papers/track-construction-v1/data/`.

- [ ] **Step 1: Confirm what is stale, and that it is only what should be**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python benchmarks/regenerate_track_v1.py --check
```

Expected: the `payoff_*` jobs are stale; the `arms_*` and `sweep_*` jobs are
**not** — they do not depend on `flights.py`. If the arms jobs are also stale,
Task 2 or 3 touched something it should not have; find out what before running
anything.

- [ ] **Step 2: Re-run the stale payoff jobs**

One at a time. ~2h total.

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -u benchmarks/regenerate_track_v1.py
```

- [ ] **Step 3: Verify the fixed run reproduces the previously-repaired numbers**

The old `payoff_fixcallsign_*` CSVs measured the repair applied in the
benchmark; the new `payoff_*` CSVs measure it applied in production. They should
agree closely. **A material disagreement means the production fix and the
benchmark repair are not the same operation** — find out which is right before
publishing either.

- [ ] **Step 4: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git add papers/track-construction-v1/data/
git commit -m "data(track-v1): payoff re-measured against the fixed flight list"
```

---

## Workstream C — the papers

### Task 6: Revise V1 against the nine comments

Revised in place. The reader knows OPDI and does not know this methodology, so
every metric, arm and join rule is introduced before it is used.

**Files:**
- Modify: `$PORTAL/papers/track-construction-v1/index.qmd`

Comment-by-comment, with V1's section numbering (Summary is unnumbered, so
"Chapter 2" is `## What a track is`):

- [ ] **Step 1: §2 — drop the identity-churn framing and the freeze claim (comment 2)**

Two things go. **"Identity churn is a problem"** is not the argument any more:
`track_id` changing between algorithms is a *consequence* of shipping a better
one, to be documented and versioned, not a cost weighed against the improvement.
Rewrite the section so churn appears as something the release manages —
pointing forward to the version column added in Task 3 — rather than as an
objection. **The `CRITICAL - DO NOT MODIFY` claim** is gone from the code and
must go from the paper; replace it with the versioning position, which is what
actually governs.

- [ ] **Step 2: §3.3 — explain interval containment properly (comment 3)**

The current text names the technique and assumes it. Rebuild it as: what a
ground-truth flight's *interval* is (ATOT→ALDT from APDF, or first/last-seen
where APDF has no movement record); why the join cannot use callsign (the arms
under test disagree about callsign, so joining on it would prejudge the
comparison); what containment means concretely — an assignment's samples fall
inside a flight's interval, so a track and a flight are matched when their
intervals overlap on the same `icao24`; and a worked micro-example with two
flights of one airframe.

Then answer the reviewer's question with Task 4's numbers: state how many
flights the wholly-inside rule excludes, why a partly-observed flight cannot
fairly be scored, and — using `median_observed_fraction_clipped` — how much of
an excluded flight was actually visible.

- [ ] **Step 3: §3.4 — plot the boundary-error distribution (comment 4)**

Add a figure from `boundary_hist_<period>.csv`: signed offset on the x-axis, one
panel for take-off and one for landing, arms overlaid or facetted. Keep the
percentile table — it is what the text quotes — and let the figure carry the
shape. State the sign convention in the caption, not only in the prose: a
negative `off` means the track began before the aircraft did.

- [ ] **Step 4: §5 — expand the ladder, especially A8 (comment 5)**

Every arm gets: what it changes relative to legacy, in one sentence; what it is
trying to fix; the rule in prose precise enough to reimplement (**no
pseudocode**); and what the measurement said. **A8 gets the most room** — it is
the recommendation and it is currently the least clear. Its two independent
fixes must be separable in the reader's mind before any number appears: drop
callsign from the group key so blanks stop starting tracks, *and* break when the
last non-blank callsign genuinely changes so two real flights still separate.
Then the bounded lookback, and why an unbounded one silently splits a track in
its own middle.

Also add, as **PDF side information**, a diagram and description of the whole
`/opdi` pipeline that runs before segmentation: step 01 ingestion (bounding box
`(-25.86653, 26.74617, 49.65699, 70.25976)`, 5-second bucket decimation keeping
the last row per aircraft-bin), step 02 tracks, step 02a cleaning, step 03
flight list, step 04 events. The reader needs to know what has already happened
to a state vector before a segmentation ever sees it. Use the knitr-emitted
mermaid pattern with a table alongside — **never
`::: {.content-visible when-format="html"}`**, which lets Quarto try to
rasterise mermaid via headless Chromium that does not start here, hanging the
PDF render with no error.

- [ ] **Step 5: §6 — justify the emphasis on legacy, name the axes, show the table (comment 6)**

The reviewer's objection is fair: a whole chapter tunes an algorithm the paper
does not recommend. Open the section by saying why it is there — the sweep
establishes that legacy's shortfall is **structural, not a tuning failure**,
which is the premise the rest of the paper rests on. 262 cells across every axis
extended until it turned over span 0.95 points; A8 gains ~40. Without the sweep
a reader may reasonably suspect the baseline was simply left untuned.

Then fix the unsupported claim. Name the three axes explicitly — `gap_minutes`,
`low_alt_gap_minutes`, `low_alt_ft` — and show the per-axis profile table from
`sweep_2025_stage1.csv` / `_ext.csv` so "interior optimum" is visible rather
than asserted. Where `low_alt_ft`'s stage-1 optimum sat on the grid edge, say so
and show the extension: an optimum at an edge is where you stopped looking.

- [ ] **Step 6: §7 — label the arm, and document the ADEP/ADES parameters (comment 7)**

Add an explicit arm column to every table in §7 so no reader has to infer
whether a row is A1, A8 or the shipped configuration.

Then add a subsection documenting the ADEP/ADES detection parameters actually
used, cross-checked against both `adep-ades-detection-v6.2` and
`DetectionConfig()` in the code — the departure and arrival paths, radii, caps,
penalties and the ranking rule. **Cross-check, do not copy:** v6.2's shipped
configuration was verified field-for-field against `DetectionConfig()` when it
was written, and if the two now disagree that disagreement is a finding to
report, not a discrepancy to smooth over.

Finally, restructure §7.2/§7.3 (comment 8): the labelling bug is fixed in
production as of Task 2, so it stops being a live caveat. Keep a short account
of what it was and what it cost — it explains why the ranking inverts, and a
reader who meets an old flight list needs to recognise it — but the headline
numbers are now the fixed ones, stated plainly and without a repaired/unrepaired
pairing.

- [ ] **Step 7: Whole-paper readability pass**

Against the audience constraint: knows OPDI, does not know this methodology.

- Every section opens by saying what it is for and what the reader will know at
  the end of it.
- Every term is defined at first use — homogeneity, completeness, V-measure,
  clean match, fragmentation, merging, boundary error, interval containment,
  arm, ladder, sweep, stage-1 grid.
- Summary states the finding and its consequence before any method detail.
- No forward references that a reader must resolve to follow a sentence.
- Update the **Limitations** section: "The baseline is not the shipped
  `track_id`" now needs rewriting, since as of Task 3 the shipped `track_id` is
  A8's.

- [ ] **Step 8: Render and check**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v1
OPDI_RENDER=check quarto render index.qmd --to html
```

Render `index.qmd` **by name**. `quarto render <dir>` exits 0 having done
nothing when there is no `_quarto.yml` at that level, which looks like success.
Then confirm the provenance table shows **zero unverified** files.

- [ ] **Step 9: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git add papers/track-construction-v1/
git commit -m "papers(track-v1): revise against review

Nine comments: identity churn reframed as a managed consequence and the freeze
claim removed; interval containment explained and its exclusions measured;
boundary error shown as a distribution; the ladder expanded, A8 most of all;
the legacy sweep given the justification it needs and its three axes named and
tabulated; arms labelled in section 7 and the ADEP/ADES parameters documented;
the labelling bug reported as fixed rather than pending.

Rewritten throughout for a reader who knows OPDI and not this methodology."
```

---

### Task 7: Run the V2 study

Three arms — `legacy`, `airframe_only`, `standard` — through the real pipeline,
steps 01→02→02a→03, one day per period. `airframe_only` is the ablation
midpoint: `standard` is `airframe_only` plus the callsign-change break, so the
three arms separate the two fixes.

**Preconditions, all three checked before starting:**

1. `kubectl -n eurocontrol get pods | grep -c Running` is 0.
2. S3 headroom ≥ 12 GB (an arm peaks ~6.8 GB; the runner refuses below 8).
3. Tasks 1–3 are committed — the runs fingerprint that code.

- [ ] **Step 1: Build `regenerate_track_v2.py`**

Same shape as `regenerate_track_v1.py` — its `Job` class, `main()` and CLI are
unchanged. Replace the docstring, `PAPER`, and `jobs()`:

```python
METHODS = ["legacy", "airframe_only", "standard"]
DAYS = {"2025": "2025-06-05", "2024": "2024-06-05"}

SEG = ["src/opdi/pipeline/segmentation/base.py",
       "src/opdi/pipeline/segmentation/methods.py", "src/opdi/config.py"]
#: V2 runs the real steps, so the steps are dependencies. V1 did not need these:
#: it read a track table someone else had built. That difference is the study.
STEPS = ["src/opdi/pipeline/tracks.py", "src/opdi/pipeline/cleaning/native.py",
         "src/opdi/pipeline/flights.py", "src/opdi/ingestion/osn_statevectors.py"]
SCORE = ["benchmarks/track_truth.py", "benchmarks/track_score.py",
         "benchmarks/osn_sample.py", "benchmarks/adep_ades.py",
         "benchmarks/flight_list_v7.py", "benchmarks/track_diagnostics.py"]
```

One `pipeline_<period>` job per period (outputs `pipeline_<period>.csv` plus
`extents_<method>_<period>.csv` for each arm) and one `continuity_<period>` job
reading those extents.

- [ ] **Step 2: Add the runner's remaining exports**

`track_pipeline_v2.py` needs, before its per-method cleanup deletes the tables:

- `export_track_extents(spark, method, period, days, results_dir)` — one row per
  track (`track_id, icao24, t_start, t_end, n_points`). Two partitions never
  coexist on S3, so this summary is the only way the continuity comparison can
  happen at all.
- `track_diagnostics.null_rates(...)` merged into the per-method row.

`osn_tracks_clean` survives until the per-method `finally`; `osn_tracks` and the
state vectors do not. Both calls must come **after** `build_flight_list` and
**before** that `finally`.

- [ ] **Step 3: Write `track_continuity.py` with its tests**

`compare(before, after) -> dict` with `n_before`, `n_after`, `identical_ids`,
`identical_pct`, `mean_tracks_per_airframe_before`,
`mean_tracks_per_airframe_after`. Measured on the **id string**, not the
partition: two methods can agree perfectly about where flights begin and end and
still share no id, because legacy suffixes `_{year}_{month}` and standard does
not — and for a downstream join that is a total break.

Tests: identical partitions report 100%; a pure rename reports 0% with equal
counts; merging two tracks halves tracks-per-airframe.

- [ ] **Step 4: Verify `--check` needs no cluster**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
OPDI_PAPER_DIR=/home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v2 \
  .venv310/bin/python benchmarks/regenerate_track_v2.py --check
```

Expected: non-zero, all outputs missing, **no Spark session and no S3 call**.

- [ ] **Step 5: Run both periods, serially**

~23 min ingest per period plus ~40 min per arm; about 4h total.

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
export OPDI_PAPER_DIR=/home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v2
.venv310/bin/python -u benchmarks/regenerate_track_v2.py --only pipeline_2025
.venv310/bin/python -u benchmarks/regenerate_track_v2.py --only pipeline_2024
.venv310/bin/python -u benchmarks/regenerate_track_v2.py --only continuity_2025 continuity_2024
```

- [ ] **Step 6: Verify, and sanity-check the numbers**

```bash
.venv310/bin/python benchmarks/regenerate_track_v2.py --check
.venv310/bin/python /home/jupyter/.claude/jobs/e2181584/tmp/v2_progress.py
```

`--check` exits 0; nothing remains under `research/tcv2/`. Then confirm:

- `legacy`'s `clean_match_pct` is near V1's harness figure (49.64% / 52.33%). **A
  large gap is a finding, not a bug** — it is the harness-versus-pipeline
  difference this study exists to measure — but understand it before publishing
  it.
- `standard` > `airframe_only` > `legacy` on `clean_match_pct`. If `standard`
  scores *below* `airframe_only`, the callsign-change break costs more than it
  buys on this sample, and **that changes what Task 3 shipped** — report it
  immediately rather than writing it up.
- `null_baro_pct` is near the 21.11% the smoke run measured.

- [ ] **Step 7: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check benchmarks/
git add benchmarks/
git commit -m "feat(bench): the track-construction V2 study, end to end"

cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git add papers/track-construction-v2/data/
git commit -m "data(track-v2): three arms through the real pipeline, both periods"
```

---

### Task 8: Write V2

`$PORTAL/papers/track-construction-v2/index.qmd` — a **release note**. Same
audience rule: knows OPDI, does not know this methodology. V2 must stand alone;
where it uses a metric V1 defines, it defines it again briefly rather than
sending the reader away.

- [ ] **Step 1: Executable-render preamble**

Copy the pattern from V1's `index.qmd` lines 1–130: a setup chunk calling
`regenerate_track_v2.py` before drawing anything, honouring `OPDI_RENDER=check`
and `allow-stale`, plus the `cache()` reader and the knitr-emitted
`mermaid_or_table()` helper.

- [ ] **Step 2: Write the sections, in this order**

1. **Summary** `{.unnumbered}` — what shipping `standard` improves, what it
   costs on ADEP/ADES, and the `track_id` break. Every number an inline
   expression.
2. **Reading this page** — `callout-note`: what is regenerated, what a
   provenance row means, and that V1's figures are a *harness* measurement while
   these come from the pipeline.
3. **What changed, in one table** — `segmentation.method` `"legacy"` →
   `"standard"` and `FLIGHT_LIST_VERSION` → `v5.0.0`, with what each expands to.
   State plainly that `osn_tracks` gains no version column, so a row does not
   say which segmentation produced it and the release date is the only
   discriminator.
4. **What the pipeline does to a state vector before segmentation sees it** —
   the same steps 01→04 diagram as V1 §5, so V2 stands alone.
5. **How a segmentation is scored** `{#sec-metrics}` — notation and the four
   quantities. Write $N$ for ground-truth flights; classify each as **clean**
   (one track, and that track only this flight), **merged**, or **fragmented** —
   mutually exclusive, summing to 100%. Then homogeneity ↔ merging,
   completeness ↔ fragmentation, V-measure as their harmonic mean.
6. **The two rules, step by step** `{#sec-rules}` — mermaid + table for each,
   then the worked divergence from `divergence_example_2025.csv`.
7. **What it improves** `{#sec-results}` — three arms, both periods.
8. **Which change is doing the work** `{#sec-ablation}` — `legacy` →
   `airframe_only` isolates dropping callsign from the group key;
   `airframe_only` → `standard` isolates the callsign-change break. Split the
   total gain in percentage points.
9. **What it costs downstream** `{#sec-payoff}` — ADEP/ADES.
10. **Why that effect is small** `{#sec-dilution}` — the flight list already
    resolves most airframes; segmentation only moves the ambiguous ones.
    Quantify the denominator so a small number is not read as a null result.
11. **What breaks** `{#sec-continuity}` — from `continuity_*.csv`, with the
    `callout-important` **"A `track_id` is not a flight"**: the discontinuity is
    invisible to anyone who thinks it is, because the join simply returns
    nothing. Since no version column marks the change, this section is where a
    consumer either learns it or does not.
12. **The cleaned table cannot reproduce its own ids** `{#sec-hazard}` —
    `callout-warning`. `track_id` is assigned in step 02 from *raw* altitudes;
    step 02a then NULLs the altitudes that fail its filters, 21.1% of
    `baro_altitude` among them. Anyone re-deriving the partition from the cleaned
    table gets a different answer and concludes the algorithm is
    non-deterministic. It is not; the input is gone.
13. **What ships** `{#sec-defaults}` — the config, and the warning that
    re-running a past month will not reproduce it.
14. **Provenance** `{#sec-provenance}` — the manifest table from v6.2:2376-2412.
15. **Limitations** — one day per period; five V1 arms not re-run through the
    pipeline; step 04 events not measured; ground truth is Network Manager
    flights only; V1's figures are a different measurement and are not
    differences from these; and **published data carries no marker of which
    segmentation produced it**, so distinguishing the two regimes depends on
    knowing the release date.

- [ ] **Step 3: Render and verify provenance**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v2
OPDI_RENDER=check quarto render index.qmd --to html
```

Zero unverified rows in the provenance table. A file listed **unverified** was
staged outside the regeneration chain — V1 shipped 16 such rows once, all runner
scratch, and the fix was deleting them plus a `.gitignore`.

- [ ] **Step 4: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git add papers/track-construction-v2/
git commit -m "papers: track construction V2 -- a release note for the shipped segmentation"
```

---

### Task 9: Publish, and report

- [ ] **Step 1: List both papers on the site**

Add `track-construction-v1/index.qmd` and `track-construction-v2/index.qmd` to
`$PORTAL/papers/_quarto.yml`'s render list and rows to
`$PORTAL/papers/index.qmd`. V1 was committed but never wired in — check before
assuming either is listed.

- [ ] **Step 2: Render the papers site**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers
OPDI_RENDER=check quarto render
```

- [ ] **Step 3: Document the research prefix**

Add `opdi/research/tcv2/` to `$OPDI/benchmarks/DATASETS.md`: what it holds, that
it is transient, and that the runner deletes it. An undocumented prefix is a
prefix nobody dares delete.

- [ ] **Step 4: Update the stale notes in `CLAUDE.md`**

The workspace `CLAUDE.md` states that `tracks.py:_add_track_id` is frozen and
marked `CRITICAL - DO NOT MODIFY`. As of Task 3 that is false in both the code
and the release. Update the "Conventions that matter" and "Known
inconsistencies" sections to describe the versioned position instead.

- [ ] **Step 5: Commit locally, do not push**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
git add benchmarks/DATASETS.md && git commit -m "docs: record the tcv2 research prefix"

cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git add papers/ && git commit -m "papers: publish track construction V1 and V2"
```

**No push, no PR, no merge** — the user's explicit instruction.

- [ ] **Step 6: Report**

State: both worktree paths and branch names; the commit SHAs; which papers
rendered; the containment number from Task 4; whether V2's `legacy` arm agreed
with V1's harness figure; and — restated because the work lives only in
worktrees — that a worktree can be deleted with its session, so anything to be
kept should be pushed or cherry-picked by the user.

---

## Self-review

**Comment coverage.** 1 → Task 2 + Task 6 Step 6. 2 → Task 6 Step 1. 3 → Task 4
Step 1 + Task 6 Step 2. 4 → Task 4 Step 2 + Task 6 Step 3. 5 → Task 6 Step 4. 6
→ Task 6 Step 5. 7 → Task 6 Step 6. 8 → Task 2 + Task 6 Step 6. 9 → Task 3.
Readability and the OPDI-literate audience → Global Constraints plus Task 6 Step
7 and Task 8 Step 2.

**Decision coverage.** Default flips, no version column → Task 3, with the
consequence carried into Task 8 sections 3, 11 and 15 and into `CLAUDE.md` at
Task 9 Step 4. Commit locally, no push → Task 9 Step 5 and the closing report.
Measure containment first → Task 4, which reports rather than changes the
metric. Revise V1 in place → Task 6.

**Placeholders.** One deliberate: `boundary_histogram`'s body in Task 4 Step 2,
left as an implementer note because the correct implementation is to factor
`track_score.boundary_error`'s existing join rather than write a second copy, and
prescribing that refactor blind would be worse than describing it. Every other
code step carries its code.

**Type consistency.** `dominant_flight_id()` (Task 2) is an aggregate expression
already aliased `_FLT_ID`, so the caller's `agg(...)` list is unchanged. Task 3
adds no new symbol at all — it changes one default and one docstring, which is
why it is a single isolated commit. `containment_census(gt, start, end)` consumes the frame
`track_truth.load_flight_intervals` returns (`t_start`, `t_end`).
`export_track_extents` writes the five columns `track_continuity.compare` reads.
`null_rates` returns the five keys Task 7 Step 2 merges and Task 8 section 12
reads.

**Ordering.** Workstream A must complete before B and C: both papers' jobs
fingerprint `flights.py`, `config.py` and `tracks.py`, so running anything first
would produce results invalidated by the next commit. Task 5 exists only because
Task 2 makes V1's payoff numbers stale, and that is the fingerprint working, not
failing.

**Known risks.**
1. Task 7's precondition is that the V2 runner works end to end. The smoke run
   reached step 03 before the guard stopped it, so everything upstream is proven;
   Task 1 removes the one known blocker, but step 03 has not yet run to
   completion under redirect.
2. Task 3 flips a production default on the strength of V1's harness
   measurements. Task 7 Step 6 is the check that the pipeline agrees. If it does
   not, Task 3 is the task to revisit — which is why it is a single isolated
   commit touching one default and one docstring.
3. **The release is silent in the data.** With no version column, a consumer who
   joins `track_id` across the release boundary gets an empty result and no
   signal as to why. Every mitigation available is documentary — the config
   docstring, V2's `{#sec-continuity}`, and `CLAUDE.md` — and documentary
   mitigations only work on people who read them. This was the user's explicit
   ruling and is implemented as given; it is recorded here because it is the
   plan's largest unhedged risk.
