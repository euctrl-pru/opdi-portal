# Track Construction: ship the segmentation, revise V1, publish V2

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the recommended segmentation and the flight-list labelling fix in
the `opdi/` pipeline as the shipped default; revise
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
- **S3 is a shared 200 GB bucket** (`BUCKET_QUOTA_GB = 200.0`). Batch `DeleteObjects` is broken on this
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
| Pipeline + benchmarks | `/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2` | `$OPDI` |

**`$OPDI` moved (2026-08-31).** The original `track-construction-v1`
worktree was deleted mid-session with the agent that held it. Its work was
recovered onto branch `track-construction-v1-recovered`, merged to `opdi` main,
and Task 7 then created `track-construction-v2` (branch of the same name, based
on `origin/main` at `8077c4a`, carrying commit `c380344`). Every path in Tasks
1-9 written as `.../track-construction-v1` means `.../track-construction-v2`
from here on. The venv is at `$OPDI/.venv310` and is untracked -- if it is
missing, pass an interpreter through `OPDI_PYTHON` rather than rebuilding it.
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
- Produces: `dominant_flight_id(df, track_col)` -> one-row-per-track label frame;
  `resolve_flight_id(sv, track_col)` -> the same frame with `flight_id` replaced
  by the track dominant value. The `_FLT_ID` column is the dominant non-blank callsign of the
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


def _label(df):
    """The one-row-per-track label frame, as a dict, for readable assertions."""
    return {r["track_id"]: r["_dominant_flight_id"]
            for r in dominant_flight_id(df).collect()}


def test_blank_samples_do_not_win_the_label(spark):
    """F.min returns "" here. That is the bug, in one test."""
    df = spark.createDataFrame(
        [("t1", ""), ("t1", ""), ("t1", "SAS123"), ("t1", "SAS123")],
        "track_id string, flight_id string",
    )
    assert _label(df) == {"t1": "SAS123"}


def test_the_most_frequent_callsign_wins_not_the_smallest(spark):
    """Two real callsigns in one track: frequency decides, not the alphabet.

    AAA111 sorts first and would win under any smallest-real rule. It must lose.
    """
    df = spark.createDataFrame(
        [("t1", "ZZZ999"), ("t1", "ZZZ999"), ("t1", "ZZZ999"), ("t1", "AAA111")],
        "track_id string, flight_id string",
    )
    assert _label(df) == {"t1": "ZZZ999"}


def test_ties_break_deterministically_on_the_callsign(spark):
    """Equal counts must not depend on partitioning, or two runs disagree."""
    df = spark.createDataFrame(
        [("t1", "BBB222"), ("t1", "AAA111")],
        "track_id string, flight_id string",
    )
    assert _label(df) == {"t1": "AAA111"}


def test_a_track_that_never_broadcast_a_callsign_has_no_row(spark):
    """It drops out of the label frame, and the caller's left join restores "".

    Asserted here so the contract is explicit: this function does not invent a
    blank, the join does. A caller using an inner join would silently lose the
    flight, which is why Step 4 specifies a left join and a coalesce.
    """
    df = spark.createDataFrame(
        [("t1", ""), ("t1", "")], "track_id string, flight_id string")
    assert _label(df) == {}


def test_a_callsign_homogeneous_track_is_unchanged(spark):
    """Legacy tracks are homogeneous by construction; the fix must be a no-op.

    If this fails, the change is not backward compatible for legacy data and the
    version bump is hiding a regression rather than describing an improvement.
    """
    df = spark.createDataFrame(
        [("t1", "SAS123"), ("t1", "SAS123")],
        "track_id string, flight_id string",
    )
    assert _label(df) == {"t1": "SAS123"}


def test_two_tracks_are_labelled_independently(spark):
    """The window partitions by track. A leak across tracks is the same class of
    bug as the unbounded lookback that cost 31 points of fragmentation in V1."""
    df = spark.createDataFrame(
        [("t1", "SAS123"), ("t1", ""), ("t2", "KLM456"), ("t2", "")],
        "track_id string, flight_id string",
    )
    assert _label(df) == {"t1": "SAS123", "t2": "KLM456"}


# --- resolve_flight_id: the invariant the rest of the module depends on ------

def test_resolution_gives_every_sample_of_a_track_one_callsign(spark):
    """flight_id is a grouping key at ten sites and is never aggregated.

    One value per track is the invariant those sites were written on. This is
    the test that says so.
    """
    from opdi.pipeline.flights import resolve_flight_id

    df = spark.createDataFrame(
        [("t1", "SAS123"), ("t1", ""), ("t1", "SAS123"), ("t1", "")],
        "track_id string, flight_id string",
    )
    out = resolve_flight_id(df)
    assert {r["flight_id"] for r in out.collect()} == {"SAS123"}


def test_resolution_does_not_add_or_drop_samples(spark):
    """A left join that fans out is the bug wearing the fix's clothes.

    The failure this whole task addresses is a track becoming several rows at a
    grouping key. A resolution step that duplicates rows would cause exactly
    that, one stage earlier, and every downstream count would be wrong in a way
    no label assertion catches.
    """
    from opdi.pipeline.flights import resolve_flight_id

    df = spark.createDataFrame(
        [("t1", "SAS123"), ("t1", ""), ("t2", "KLM456"), ("t2", "KLM456"),
         ("t3", ""), ("t3", "")],
        "track_id string, flight_id string",
    )
    assert resolve_flight_id(df).count() == df.count() == 6


def test_resolution_leaves_an_unlabelled_track_blank_not_null(spark):
    """Downstream code fillna's to "" and compares against it. NULL would slip
    past those comparisons and reappear as a different bug."""
    from opdi.pipeline.flights import resolve_flight_id

    df = spark.createDataFrame(
        [("t1", ""), ("t1", "")], "track_id string, flight_id string")
    assert [r["flight_id"] for r in resolve_flight_id(df).collect()] == ["", ""]


def test_resolution_is_a_no_op_on_a_legacy_style_track(spark):
    """Legacy tracks are callsign-homogeneous by construction.

    If this fails, the change is not backward compatible and the version bump
    describes a regression rather than a capability.
    """
    from opdi.pipeline.flights import resolve_flight_id

    df = spark.createDataFrame(
        [("t1", "SAS123"), ("t1", "SAS123")],
        "track_id string, flight_id string",
    )
    out = [r["flight_id"] for r in resolve_flight_id(df).collect()]
    assert out == ["SAS123", "SAS123"]
```

- [ ] **Step 3: Run them to confirm they fail**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_flights_labelling.py -v
```

Expected: `ImportError: cannot import name 'dominant_flight_id'`.

- [ ] **Step 4: Implement it**

> **Controller rulings R1/R2/R6 — read before writing any code. These change
> the shape of the fix from what the section above describes.**
>
> The problem is not one aggregate. `flights.py` reads the track table at
> **three** places, each doing the identical rename:
>
> | Method | Line | Reads |
> |---|---|---|
> | `_track_border_flags` | ~422 | trend out-of-area flags |
> | `build_endpoint_candidates` | ~534 | **the shipped departure path** |
> | the airport-proximity / trend path | ~621 | arrival trend |
>
> Downstream, `flight_id` is used as a **grouping or join key** — never
> aggregated — at lines 828, 839, 857, 873, 982, 1003, 1029, 1043, 1077 and
> 1168. The module is written on the invariant **one `flight_id` per
> `track_id`**, which legacy guarantees by construction because callsign is in
> the track's group key.
>
> `standard` breaks that invariant. A track carrying several callsigns *fans out
> into multiple rows* at every one of those keys — a larger failure than the
> blank label, and the real reason coverage collapses.
>
> **Therefore: resolve `flight_id` to the track's dominant non-blank value at all
> three entry points, immediately after the rename.** Every downstream grouping
> then sees the invariant it was written for, and `F.min("flight_id")` at line
> 441 becomes a minimum over identical values (replace it anyway, for clarity).
>
> Resolve on the **unfiltered** frame — `_track_border_flags` and
> `build_endpoint_candidates` both filter to `(_rn == 1) | (_rr == 1)` a few
> lines later, and a mode over two rows ties back to the alphabet, reproducing
> the original bug in a subtler form.
>
> **Report which sites you changed and what you verified about each.** If you
> conclude a site does not need it, say why.

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
    blank = F.trim(F.coalesce(F.col("flight_id"), F.lit(""))) == ""
    counts = (
        df.filter(~blank)
        .groupBy(track_col, "flight_id")
        .agg(F.count(F.lit(1)).alias("_n"))
    )
    rank = Window.partitionBy(track_col).orderBy(
        F.col("_n").desc(), F.col("flight_id").asc()
    )
    return (
        counts.withColumn("_r", F.row_number().over(rank))
        .filter(F.col("_r") == 1)
        .select(track_col, F.col("flight_id").alias("_dominant_flight_id"))
    )
```

with the signature `def dominant_flight_id(df, track_col="track_id"):` — it
returns a **frame**, one row per track, not an aggregate expression. `Window` is
already imported in this module.

Then add a second helper that applies it, and call *that* at all three entry
points:

```python
def resolve_flight_id(sv, track_col: str = "track_id"):
    """Give every sample of a track the one callsign that track flew.

    The rest of this module uses ``flight_id`` as a grouping and join key -- at
    ten separate sites -- and never aggregates it. That is only safe while every
    track carries exactly one value, which legacy segmentation guarantees by
    construction because callsign is part of the track's group key.

    A segmentation that groups on the airframe alone breaks the guarantee, and
    then a track carrying two callsigns fans out into two rows at every one of
    those keys. Resolving the column here, at the point the track table is read,
    restores the invariant the module was written on instead of patching each
    site that depends on it.

    A track that never broadcast a callsign keeps ``""``: an unlabelled flight,
    not an absent one. Dropping it would shrink the denominator of every
    downstream rate.
    """
    labels = dominant_flight_id(sv, track_col)
    return (
        sv.join(labels, on=track_col, how="left")
        .withColumn(
            "flight_id",
            F.coalesce(F.col("_dominant_flight_id"), F.lit("")),
        )
        .drop("_dominant_flight_id")
    )
```

Call it immediately after the rename at each of the three sites, e.g.:

```python
        sv = sv.withColumnRenamed("callsign", "flight_id").fillna({"flight_id": ""})
        sv = resolve_flight_id(sv)
```

and replace `F.min("flight_id").alias("_FLT_ID")` at line 441 with
`F.first("flight_id").alias("_FLT_ID")` — after resolution every value in the
group is identical, so the choice of aggregate no longer carries meaning, and
`first` says that where `min` implied a decision.

> **Implementer note:** this mirrors `benchmarks/flight_list_v7.py:167`
> (`dominant_callsign`), which is the same operation on the benchmark side and
> was validated in V1. Prefer converging on its logic exactly — Task 5 Step 3
> checks that the production fix and the benchmark repair produce the same
> numbers, and that check is only meaningful if they were written to be the same
> operation.

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

Expected: 10 passed, then the whole suite green (260+ as of 2026-08-23).

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

### Task 2b: The same invariant in `events.py`

**Added mid-execution (ruling R8).** Task 2's implementer found the identical
latent fan-out in step 04 and correctly left it alone. It cannot stay left
alone, because Task 3 flips the production **default** — and step 04 being out
of scope for the *measurement* does not put it out of scope for the *release*.

`src/opdi/pipeline/events.py:912-913` does the same rename this plan has now
fixed three times:

```python
    sv_f = sv_f.withColumnRenamed("callsign", "flight_id")
    sv_f = sv_f.fillna({"flight_id": ""})
```

and line 968 groups on it without aggregating:

```python
    result = df_labelled.groupBy(
        "track_id", "icao24", "flight_id",
        "hexaero_apt_icao", "hexaero_osm_id", "hexaero_aeroway", "hexaero_ref", "trace_id",
    ).agg(...)
```

Under `standard`, one track that traverses one airport zone while broadcasting
two callsigns becomes **two event groups** — two `entry-runway` events where one
aircraft entered one runway once. Line 1003 then publishes `flight_id` as
`osn_flight_id`, so the duplication reaches the published milestone table, which
is OPDI's actual deliverable.

**Files:**
- Modify: `$OPDI/src/opdi/pipeline/events.py:912-913`
- Test: `$OPDI/tests/test_events_labelling.py`

**Interfaces:**
- Consumes: `resolve_flight_id(sv, track_col="track_id")` from wherever Task 2
  placed it. **Import it; do not reimplement it.** Two copies of this logic is
  how production and the benchmark drifted apart in the first place — the
  finding that started this whole study.

- [ ] **Step 1: Locate the helper Task 2 wrote**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
grep -rn "def resolve_flight_id" src/opdi/
```

Import from wherever it is. If it sits in `flights.py` and importing
`pipeline.flights` from `pipeline.events` would create a cycle, move it to a
module both can import and update `flights.py`'s import — moving it is correct,
copying it is not.

- [ ] **Step 2: Write the failing test**

```python
# $OPDI/tests/test_events_labelling.py
"""Step 04 must not emit one event per callsign a track happened to broadcast."""
from pyspark.sql import functions as F


def test_a_track_with_two_callsigns_yields_one_group_per_zone(spark):
    """The fan-out, stated as the thing a reader would notice in the data.

    One aircraft entering one runway once must be one event. Grouping on an
    unresolved `flight_id` makes it two, and both look entirely plausible in
    isolation -- which is why this needs a test rather than an inspection.
    """
    from opdi.pipeline.events import resolve_flight_id  # or its shared home

    df = spark.createDataFrame(
        [("t1", "abc123", "SAS123", "EKCH", "rwy04L"),
         ("t1", "abc123", "", "EKCH", "rwy04L"),
         ("t1", "abc123", "SAS123", "EKCH", "rwy04L")],
        "track_id string, icao24 string, flight_id string, "
        "hexaero_apt_icao string, hexaero_ref string",
    )
    grouped = (
        resolve_flight_id(df)
        .groupBy("track_id", "icao24", "flight_id",
                 "hexaero_apt_icao", "hexaero_ref")
        .agg(F.count(F.lit(1)).alias("n"))
    )
    rows = grouped.collect()
    assert len(rows) == 1, f"expected one group, got {len(rows)}"
    assert rows[0]["flight_id"] == "SAS123"
    assert rows[0]["n"] == 3
```

- [ ] **Step 3: Run it to confirm it fails**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_events_labelling.py -v
```

Expected: FAIL with two groups, one of them labelled `""`.

- [ ] **Step 4: Apply the helper — *before* the `dropna`, not after**

> **Corrected mid-execution (ruling R21). The original instruction here said
> "after line 913", which was wrong**, because line 921 is
> `sv.dropna(subset=["lat", "lon", "baro_altitude_c"])` and resolving after it
> breaks the contract the helper's own docstring states: *resolve on the
> unfiltered frame*.
>
> Step 03 votes over the whole month's rows. Resolving after that `dropna` would
> make step 04 vote only over samples carrying position **and** barometric
> altitude — and velocity-only broadcasts carry a callsign with no position,
> while cleaning NULLs bad `baro_altitude_c`. A track whose real callsign
> appears mostly in non-positional samples would then be `SAS123` in
> `opdi_flight_list` and `""` in the same track's `info.osn_flight_id`.

Order: rename, `fillna`, guarded resolve, **then** `dropna`.

```python
    sv_f = sv_f.withColumnRenamed("callsign", "flight_id")
    sv_f = sv_f.fillna({"flight_id": ""})
    # One callsign per track, before flight_id becomes a grouping key below.
    # Without this, a track that broadcast two callsigns while crossing one
    # runway emits two entry-runway events for one crossing, and the result is
    # published as osn_flight_id -- so the duplication reaches the milestone
    # table rather than staying an internal artefact.
    #
    # Before the dropna, not after: step 03 resolves over the whole month, and
    # a narrower population here would let the flight list and the event table
    # name the same track differently.
    sv_f = resolve_flight_id(sv_f)   # guarded; see the version note in Step 1
    sv_f = sv_f.dropna(subset=["lat", "lon", "baro_altitude_c"])
```

- [ ] **Step 5: Verify, including the whole suite**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_events_labelling.py -v
.venv310/bin/python -m pytest tests/ -q
```

The suite stood at **277 passed** after Task 2. Report the new count. If an
existing events test fails, report it rather than weakening it — under the old
default nothing exercised a multi-callsign track, so a failure here is
information about the change.

- [ ] **Step 6: Check whether any other event path groups on `flight_id`**

```bash
grep -n "flight_id" src/opdi/pipeline/events.py
```

Known at time of writing: 912/913 (rename), 918 (column list), 968 (**the
grouping**), 1003 (published as `osn_flight_id`), 1314
(`col("track_id").alias("flight_id")` — a different thing, leave it), 1456 (DDL
comment). Confirm nothing else groups or joins on it, and say so in the report.

- [ ] **Step 7: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check src/opdi/pipeline/events.py tests/test_events_labelling.py
git add src/opdi/pipeline/events.py tests/test_events_labelling.py
git commit -m "fix(events): one event per zone crossing, not one per callsign

events.py groups on flight_id at line 968 without aggregating it, which is only
sound while a track carries one callsign -- true by construction under legacy
segmentation, false under standard. A track broadcasting two callsigns across
one runway emitted two entry-runway events, and line 1003 publishes flight_id as
osn_flight_id, so the duplication reached the milestone table.

Same resolution helper as flights.py, imported rather than copied."
```

---

### Task 3: Ship `standard` as the default

**Prerequisite (ruling R8): Task 2b must be complete.** Flipping the default
before `events.py` is fixed ships duplicate published events.

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
    # t_off / t_land, NOT t_start / t_end. `load_flight_intervals` returns the
    # ground-truth *flight* interval as (t_off, t_land, t_source, day);
    # t_start/t_end are the *track* extents from `track_score.track_extents`,
    # a different frame entirely. Mixing the two silently compares a track
    # against a window it was never measured against.
    inside = (F.col("t_off") >= F.lit(window_start)) & (
        F.col("t_land") <= F.lit(window_end))
    clipped_start = F.col("t_off") < F.lit(window_start)
    clipped_end = F.col("t_land") > F.lit(window_end)

    observed = (
        F.least(F.col("t_land").cast("long"), F.lit(window_end).cast("long"))
        - F.greatest(F.col("t_off").cast("long"),
                     F.lit(window_start).cast("long"))
    )
    total = F.col("t_land").cast("long") - F.col("t_off").cast("long")

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
                       span_seconds: int = 1800):
    """The signed boundary offsets as a distribution, not three percentiles.

    p10/p50/p90 cannot distinguish a symmetric spread from a bimodal one, and
    the two mean different things: a spread is noise to tune against, two modes
    are two populations, one of which is probably a different failure wearing
    the same number. ``boundary_error``'s own docstring makes exactly this
    argument about ``abs()``; this is the same argument one level further out.

    Sign convention is ``boundary_error``'s, unchanged: ``off = trk_start -
    t_off``, so **negative ``off`` means the track starts before take-off**, and
    ``land = trk_end - t_land``, so **positive ``land`` means it ends after
    landing**. Both of those are the normal case -- an OPDI track includes
    ground movement by design, while ground truth's interval is airborne only.
    A histogram that loses this convention inverts the reader's diagnosis.

    Restricted to ``t_source == "apdf"``, as ``boundary_error`` is.

    Bins are clamped to +/- ``span_seconds`` so the tails do not stretch the
    axis into uselessness. Clamped counts stay in the end bins rather than
    being dropped, so the histogram sums to the sample and an end bin reads
    honestly as "this many, at least this far out".
    """
    ...  # implementer: see the note below.
```

> **Implementer note — do this as a refactor, not a copy.**
> `benchmarks/track_score.py:173` `boundary_error` already computes both signed
> offsets and the `extents` join. Extract that shared part into a helper
> returning a per-flight frame with the signed `off` and `land` in seconds, have
> `boundary_error` call it, and build the histogram from the same helper.
>
> **`boundary_error`'s existing return values must not change** — V1's published
> tables quote `off_err_p50_s = 109` and `land_err_p50_s = 374`, and the four
> absolute fields are documented as kept unchanged so already-published runs
> stay comparable. Run the existing `track_score` tests to prove the refactor is
> behaviour-preserving before adding the histogram.
>
> Span is 1800 s rather than 900: `land_err_p50_s` is 374 s and that is a
> *median*, so a 900 s span would clamp a large share of the arrival side into
> the end bin and hide the very shape the figure exists to show.

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

### Task 4b: The ground-truth midnight defect, and the manifest's empty `inputs`

**Added mid-execution (rulings R26, R27).** Task 4's containment census found a
real ground-truth defect while cross-checking its own numbers.

`benchmarks/track_truth.py:162-164`:

```python
    nm = nm.filter(F.col("icao24").isNotNull()).withColumn("day", F.to_date("aobt"))
    if days:
        nm = nm.filter(F.col("day").isin([str(d) for d in days]))
```

`day` is the **off-block** day. The `t_off`/`t_land` window is applied *after*
this filter, so a flight that pushed back at 23:5x and got airborne after
midnight is dropped by the day key even though its interval lies wholly inside
the sampled window. Measured: **53 flights (2025), 55 (2024)** — 0.06%, and
one-directional, so it is a small systematic bias rather than noise.

The module's own docstring at lines 125-143 already argues that the window must
be expressed on `t_off`/`t_land` *rather than* on `day`, because `day` is the
departure day and the two windows do not close at the same instant. Line 164
contradicts the design the docstring states. This is the third midnight-boundary
defect found in this module — the arrival-side join key was fixed earlier for a
closely related reason, and that fix is documented in the same file.

**Why now:** in isolation 0.06% would not justify invalidating every published
V1 figure. But ruling R25 already re-runs all ten stale jobs, so the marginal
cost of this fix is a code review rather than a re-run. Deferring it means
paying the re-run bill twice.

**Files:**
- Modify: `$OPDI/benchmarks/track_truth.py:162-164`
- Modify: `$OPDI/benchmarks/regenerate_track_v1.py` (`Job.run`)
- Test: `$OPDI/tests/test_track_truth_window.py`

**Interfaces:**
- `load_flight_intervals` keeps its signature and its returned columns
  (`t_off`, `t_land`, `t_source`, `day`, plus identity). Only which rows survive
  changes.

- [ ] **Step 1: Write the failing test**

```python
# $OPDI/tests/test_track_truth_window.py
"""Ground truth is windowed on the flight, not on the day it pushed back."""


def test_a_flight_off_block_before_midnight_is_kept(spark):
    """The defect, in one case.

    Off-block 23:52 on the day before the sample; airborne 00:14 and landed
    04:30 inside it. The interval lies wholly within the window, so the flight
    belongs in the sample -- but a filter keyed on the off-block day drops it.
    """
    ...  # implementer: build the minimal NM frame load_flight_intervals reads,
         # or exercise the day-filter expression directly if constructing the
         # full frame needs the reference parquet. Say which you chose and why.


def test_a_flight_genuinely_outside_the_window_is_still_dropped(spark):
    """Widening the pre-filter must not widen the window itself.

    The point of the fix is that the pre-filter stops deciding membership, not
    that membership gets looser. A flight airborne before the window opens stays
    out.
    """
    ...
```

> **Implementer note.** The right fix is almost certainly to widen the `day`
> pre-filter by one day on each side and let the existing `t_off`/`t_land`
> window make the exact cut — the pre-filter exists for partition pruning, not
> for correctness, and the docstring already says the window is the authority.
> **Do not simply delete the pre-filter**: it is what keeps this from scanning
> more than it needs.
>
> One edge to handle explicitly: `CLAUDE.md` warns that `apdf_tidy()` covers one
> month at a time and that a wide window silently drops rows. Widening by a day
> at a month boundary can reach outside the loaded months. Both study samples
> are mid-June so neither hits it, but say in your report what your fix does at
> a month edge rather than leaving it to be discovered.

- [ ] **Step 2: Run the tests to confirm they fail**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
.venv310/bin/python -m pytest tests/test_track_truth_window.py -v
```

- [ ] **Step 3: Fix the pre-filter, and say so where the design is documented**

Amend the docstring at `track_truth.py:125-143` to record that the `day`
pre-filter is a pruning aid widened past the window, and that the
`t_off`/`t_land` comparison is what decides membership. The docstring already
argues the principle; it should now describe the code that implements it.

- [ ] **Step 4: Fix the manifest's empty `inputs` (ruling R27)**

`Job.run` in `regenerate_track_v1.py` re-records provenance after the script
exits, without `inputs`, overwriting the entry the script itself wrote. Every
one of the ten existing outputs carries `inputs: {}` as a result. Two lines.
The re-run that follows is the one chance to have the manifest come out right
without a third pass.

- [ ] **Step 5: Full suite**

```bash
.venv310/bin/python -m pytest tests/ -q
```

It stood at **312 passed**. Report the new count. **Do not run any regeneration
job in this task** — Task 5 owns the re-run, and a run launched here would be
invalidated by any later text edit anyway.

- [ ] **Step 6: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v1
uvx ruff check benchmarks/track_truth.py benchmarks/regenerate_track_v1.py tests/test_track_truth_window.py
git add benchmarks/track_truth.py benchmarks/regenerate_track_v1.py tests/test_track_truth_window.py
git commit -m "fix(bench): window ground truth on the flight, not on its off-block day

load_flight_intervals pre-filtered on to_date(aobt) and only then applied the
t_off/t_land window, so a flight that pushed back at 23:5x and got airborne
after midnight was dropped although its interval lay wholly inside the sample.
53 flights in 2025, 55 in 2024 -- small, but one-directional.

The docstring already argued the window must be expressed on t_off/t_land
rather than on day. The code did not do it. The pre-filter is now widened past
the window and kept only for pruning.

Also stops Job.run overwriting each output's provenance entry with one that has
no inputs."
```

---

### Task 5: Re-run every stale job against the fixed code

> **Scope widened mid-execution (ruling R25). This task was written as "re-run
> the payoff jobs"; it is now "re-run all ten".**
>
> **Expect fourteen, not ten (ruling R28).** The count below was taken before
> Task 4b. `track_truth.py` is a declared dependency of the containment jobs
> *and* the boundary histograms, so 4b's fix makes those four stale as well.
> Establish the real list by running `--check` yourself at the moment you start;
> do not work from a list fixed in advance. The four extras are cheap — roughly
> 75 s each for the census — and the sweeps still dominate the bill.
>
> `--check` reported **10 stale, 4 current** before Task 4b. `payoff_*` changed genuinely —
> Task 2 changed `flights.py` behaviour. `arms_*` and `sweep_*` are stale
> because `config.py` and `track_score.py` are declared dependencies that this
> plan edited, and their numbers provably should not move: the arms select
> their rule explicitly rather than through the default, and the `track_score`
> refactor is behaviour-preserving by 14 tests.
>
> **Re-run them anyway.** Narrowing a declared dependency to dodge a re-run is
> the move that lets a real change through later, and "I am confident the
> numbers did not move" is not verification — that distinction is the whole
> premise of the manifest. Expect roughly **8–14 hours** of contended cluster
> time; the sweeps dominate.
>
> Run `--check` with `OPDI_PAPER_DIR` set. Without it, `REPO.parent` resolves
> inside `.claude/worktrees` and every output reads as "missing" — a confident
> lie the module documents and which I walked into once already.

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

- [ ] **Step 2b: Report the measured `match_rates` delta (ruling R32)**

The ground-truth fixes removed inflated intervals — up to 26.9 hours, where a
nightly service took its `t_off` from yesterday's rotation. Ten such intervals
leave the 2025 sample and seven leave 2024.

**Report what actually moved, and do not describe it as "fewer merges".** Traced
through `match_rates`, the contamination worked three ways at once and none of
them is inflated merging:

- a neighbouring leg that lost *all* its samples to an inflated interval
  vanished from `matched` entirely, so it left the **`n_flights` denominator**
  rather than counting as merged;
- the inflated flight itself spanned several tracks and scored **fragmented**;
- a track that genuinely merged two real legs carried only one `flight_key`, so
  a **real merge was masked**.

Nobody has measured the size of this. Both earlier reports counted inflated
*intervals*, never flights absorbed. So: quote `n_flights`, `clean_match_pct`,
`fragmented_pct` and `merged_pct` before and after for both periods, and let the
numbers say which way it went. This is a figure the paper will carry, so it must
come from the re-run rather than from anyone's reasoning about the mechanism.

- [ ] **Step 3: The two payoff arms should now agree — and that agreement is the test**

**Read this before interpreting the numbers (ruling R18).** This step was
written when production still had the bug: `payoff_*` measured the pipeline
as-shipped, `payoff_fixcallsign_*` measured it with the benchmark repairing the
labelling, and the *gap between them* was the finding.

Task 2 removed the bug from production, so the gap should now be gone.
`flight_list_v7.py`'s `--fix-callsign` applies a repair to a frame production has
already resolved, and the operation is idempotent — so the two arms should
produce the same numbers.

That inverts what this step checks, and makes it a stronger test than it was:

- **They agree** → the production fix and the benchmark repair are the same
  operation on the same rows. That is the convergence requirement, confirmed
  from the other direction.
- **They disagree** → they are *not* the same operation, and one of them is
  wrong. That is now a finding, not a measurement. Do not publish either number
  until you know which.

Both jobs still run. Keep them both: two jobs that agree are the evidence, and a
future change to either side breaks the agreement visibly. Task 6 must say why
both are run, or a reader meets two identical columns and reads it as
redundancy.

One known and accepted difference, which does **not** apply to this sample:
production resolves after the month filter while the benchmark resolves over its
whole redirected table, so the two can differ for a track straddling a month
boundary. V1's payoff samples three June days; no track in it straddles one.

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

Two exact passages, both in `## What a track is, and why the question is open
{#sec-problem}` (around lines 214-235 of `index.qmd`).

**First, this bullet goes**, leaving three failure modes rather than four:

```
- **Identity churn** — the same physical flight gets a different `track_id`
  depending on when the pipeline ran.
```

The reviewer's position, which is now the project's: churn is not a failure of a
segmentation. A `track_id` changing when the rule changes is a *consequence* of
shipping a better rule — something a release documents and consumers are told
about — not a cost to be weighed against the improvement. Nothing in the study
measures it, either, so listing it beside three measured failures overstated it.
Check the sentence after the list ("Merging is the more damaging failure...")
still reads correctly against three items.

**Second, this closing paragraph is now false and must be rewritten:**

```
The rule has never been benchmarked. It is also frozen: `_add_track_id` is
marked *CRITICAL — DO NOT MODIFY*, because changing it breaks `track_id`
continuity with every dataset OPDI has published. This study therefore
re-implements it inside a generic engine and measures the re-implementation,
rather than editing production code.
```

Every clause after the first is out of date. The marker is gone from the code,
the rule is versioned rather than frozen, and as of this release `standard` is
what production runs. Replace it with the position that actually governs:
segmentation is a versioned choice; changing it is a release decision that
consumers must be told about, and one that has now been taken. Keep "the rule
has never been benchmarked" as the study's motivation — that part is still true
and is why V1 exists.

Then say plainly why V1 still measures a re-implementation: it was a study of
*algorithms*, run in a harness so that eight arms could be compared without
eight pipeline runs. That is a legitimate design and a real limitation, and V2
is the paper that measures the pipeline itself. A reader arriving at V1 first
must leave §2 knowing which of the two they are reading.

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

> **AMENDED 2026-08-31.** Runs after Task 13, not before it. Three changes below:
> the flight list is built for two arms rather than three (comment 7), every arm
> is scored gate-to-gate as well as airborne (comment 4), and the study restarts
> from code that already exists.
>
> **The code is already written and committed** at `c380344` on branch
> `track-construction-v2` — `benchmarks/regenerate_track_v2.py` and
> `benchmarks/track_continuity.py`, 439 tests passing. Steps 1-4 below are
> therefore *verification* of existing code plus the amendments, not fresh
> implementation. The first attempt died mid-run with no results: it reached
> step 02a cleaning at 104/200 on the `legacy` arm of `pipeline_2025` and left
> nothing on S3, so its `finally` blocks worked. Nothing needs cleaning up
> before restarting.

Three arms — `legacy`, `airframe_only`, `standard` — through the real pipeline,
steps 01→02→02a→03, one day per period. `airframe_only` is the ablation
midpoint: `standard` is `airframe_only` plus the callsign-change break, so the
three arms separate the two fixes.

**Comment 7 — the flight list is built for two arms only.** ADEP/ADES is run for
`legacy` and `standard`. `airframe_only` gets segmentation scoring and continuity
but **no step 03**, which is the expensive step. The ablation survives intact:
it was only ever the midpoint of the *segmentation* comparison, and the
downstream question is "does shipping this change ADEP/ADES", which needs the
before and the after, not the midpoint. Expect this to cut roughly a third off
the run.

**Comment 4 — every arm is scored twice.** Pass a scorer closure to `run_arm`
that calls `track_score.score_arm_gated(matched, extents, matched_gate)`, where
`matched_gate` is `overlap_join(assign, gt, bounds=("t_off_block", "t_in_block"))`
over the same `assign` and `gt`. The row gains the `gate_*` columns. Ground truth
must come from a `load_flight_intervals` that has been through Task 11 — if
`t_off_block` is missing, the run fails at the join rather than silently scoring
airborne twice, which is the behaviour we want.

**Preconditions, all three checked before starting:**

1. `kubectl -n eurocontrol get pods | grep -c Running` is 0.
2. S3 headroom ≥ 12 GB (an arm peaks ~6.8 GB; the runner refuses below 8).
3. Tasks 1–3 **and 10–12** are committed — the runs fingerprint that code.
   Starting before Task 12 lands means re-running everything a second time.
4. Task 13 is complete and `regenerate_track_v1.py --check` exits 0.

- [ ] **Step 1: Build `regenerate_track_v2.py`**

Same shape as `regenerate_track_v1.py` — its `Job` class, `main()` and CLI are
unchanged. Replace the docstring, `PAPER`, and `jobs()`:

```python
METHODS = ["legacy", "airframe_only", "standard"]
#: Comment 7: ADEP/ADES for the before and the after only. `airframe_only` is
#: the segmentation ablation's midpoint and has no downstream question of its
#: own, so it skips step 03 -- the expensive step.
FLIGHT_LIST_METHODS = ["legacy", "standard"]
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
- **`gate_clean_match_pct <= clean_match_pct` is not guaranteed and its
  direction is the finding.** The gate interval is a superset, so it matches
  strictly more samples — which can *lower* the clean rate by exposing taxi
  samples that landed in the wrong track, or *raise* it by nothing at all. A
  gate rate identical to the airborne rate across all three arms means the gate
  columns are not being computed; check the closure before believing it.
- `airframe_only` has no ADEP/ADES columns. That is comment 7, not a failure.

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

> **AMENDED 2026-08-31.** Four constraints inherited from the amendment:
>
> - **The prose rules in Global Constraints bind V2 from the first draft.** V1
>   needs an editorial pass (Task 14 §G) because it was written before those
>   rules existed. V2 has no such excuse — write it clean rather than writing it
>   long and cutting later.
> - **No V-measure.** It does not exist in the codebase after Task 12, and it is
>   not mentioned here.
> - **Report both intervals.** Every headline rate appears airborne and
>   gate-to-gate, with the per-side coverage stated once (`aobt` ~100%, `aibt`
>   ~50%). A release note that reports only the airborne rate repeats the exact
>   omission comment 4 was raised about.
> - **No provenance chapter.** One sentence in the reading guide, as in V1 §I.
>   The manifest is still written; it just is not a chapter.
>
> Also: `airframe_only` carries no ADEP/ADES numbers, so V2's downstream section
> compares `legacy` against `standard` and says why the midpoint is absent.

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

   **Three consumer-visible changes must appear here explicitly, not only in
   commit messages** — a reader of this page is the release's audience:

   - `FLT_ID` is now the track's *dominant* callsign, not the lexicographically
     smallest. Under legacy segmentation the two agree on every track, so
     historical data is unaffected.
   - **Unlabelled overflights now carry `""` where they carried NULL.** The
     column previously used `""` for unlabelled detected flights and NULL for
     unlabelled overflights — two spellings of "no callsign" in one column, so
     `WHERE FLT_ID IS NULL` and `WHERE FLT_ID = ''` answered the same question
     differently. A consumer filtering overflights on `IS NULL` now silently
     gets nothing back. Say so in those words.
   - Step 04 emits one event per zone crossing rather than one per callsign
     broadcast during it (Task 2b). Under legacy the two were identical.
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
    differences from these; **published data carries no marker of which
    segmentation produced it**, so distinguishing the two regimes depends on
    knowing the release date; and one more, which the study's own machinery
    produced:

    **The release's safety guards are keyed on configuration, because the data
    carries no provenance.** Both `flights.py` and `events.py` skip callsign
    resolution when the run stamps a legacy version string, so a correctly
    configured legacy re-run reproduces its released month byte for byte. But
    neither guard can ask the question it actually needs answered — *were the
    tracks I am reading built with the legacy rule?* — because no column records
    it. A legacy-stamped run over an `osn_tracks` rebuilt with `standard`
    therefore fans out while stamping the frozen version, and nothing in the
    output reveals it. `flights.py`'s `tracks_table == "osn_tracks"` clause
    distinguishes raw tracks from *clean* ones, not legacy-built from
    standard-built. Say plainly that this is the cost of shipping without a
    segmentation marker, and name the one shipped configuration it already
    affects: `benchmarks/event_bench.py`'s rungs `L00`–`L12` inherit
    `events_v0.0.2` and so run unresolved.

    **Production and the benchmark resolve the callsign over different rows at a
    month boundary.** Production resolves inside the reader, *after* the month
    filter, because moving it earlier defeats partition pruning. The benchmark
    resolves over its whole redirected table. The two therefore agree for every
    track that lies inside one month and can disagree for one that straddles a
    boundary. No track in either sample straddles one — V1's payoff runs three
    June days, V2 runs 2025-06-05 and 2024-06-05 — so nothing on this page is
    affected. A whole-month comparison would meet it, which is why it is written
    down rather than left for someone to rediscover.

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

Three edits, not one.

1. The workspace `CLAUDE.md` states that `tracks.py:_add_track_id` is frozen and
   marked `CRITICAL - DO NOT MODIFY`. As of Task 3 that is false in both the
   code and the release. Replace it with the versioned position: segmentation is
   a release decision, `standard` is what ships, `legacy` remains selectable and
   reproduces pre-release ids.

2. Record the **provenance blind spot** (ruling R20) under "Conventions that
   matter", because it is the rule a future contributor most needs and cannot
   infer: `osn_tracks` carries no marker of which segmentation produced a row,
   so the legacy-reproduction guards in `flights.py` and `events.py` key on the
   run's *configuration*, not on the data. A legacy-stamped run over
   standard-built tracks silently produces wrong output. Anyone reprocessing a
   released month must confirm the tracks were built with `legacy`, because
   nothing will check it for them.

3. `docs/pipeline_overview.rst:56-58` still describes `track_id` assignment
   purely as the legacy SHA-256/gap-threshold algorithm. It pre-dates this work
   and already sits in `CLAUDE.md`'s "Known inconsistencies", but it is a place
   a reader would plausibly meet the change, so correct it here.

4. Add the **callsign-resolution invariant** itself, which is now load-bearing
   across three modules: `flight_id` is used as a grouping and join key
   downstream and is never aggregated, so it must carry exactly one value per
   `track_id`. `resolve_flight_id` establishes that at the point the track table
   is read. It lives in `flights.py`; `events.py` imports it. **Do not copy it.**
   The whole study exists because two copies of a rule drifted.

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

## Amendment — 2026-08-31, ten further comments on V1

Ten comments on the revised V1, taken after Tasks 1–5 completed and Task 7's
code landed at `c380344` without producing results. Three were resolved with the
user on 2026-08-31 and those rulings are recorded below. The rest are
implemented by **Tasks 10–14**, plus in-place edits to Tasks 7 and 8.

**Comment → task map.** Every comment lands somewhere; this table is the
coverage check.

| # | Comment | Where it is handled |
|---|---|---|
| 1 | V-measure is introduced but never used — delete it everywhere | Task 12 (code), Task 14 §A |
| 2 | §4.2's last paragraph is too long | Task 14 §B |
| 3 | Does `traffic` forward/backward fill? | Task 12 (measurement), Task 14 §C |
| 4 | Tracks must include stand, taxi-out and taxi-in | Task 11 (metric), Task 7 (measured), Task 14 §D |
| 5 | Drop Chapter 7 | Task 14 §E |
| 6 | Replace it with a `recommended` parameter sweep | Task 10 (harness), Task 13 (run), Task 14 §E |
| 7 | ADEP/ADES on `recommended` and `legacy` only | Task 7 (amended), Task 14 §F |
| 8 | No claudisch language; sound natural; be to the point | Global Constraints, Task 14 §G |
| 9 | Final params missing from §9 "what shipped" | Task 14 §H |
| 10 | Chapter 10 provenance is not needed | Task 14 §I |

### Execution order — read this before dispatching anything

Tasks 10, 11 and 12 all modify files that `regenerate_track_v1.py` and
`regenerate_track_v2.py` declare as dependencies: `segmentation/base.py`,
`track_score.py`, `track_truth.py`, `track_diagnostics.py`. Each one alone marks
every job in both papers stale. Task 5 has just spent 8h20m refreshing exactly
those fingerprints.

So the three code tasks are **code and unit tests only, with no cluster run
between them**, and Task 13 pays the re-run cost once:

```
10 → 11 → 12   code only; no Spark, no S3
      ↓
     13        ONE combined re-run: every stale V1 job + the new sweep
      ↓
      7        the V2 study, amended
      ↓
     14        the V1 editorial pass — needs 13's and 7's numbers
      ↓
    8 → 9      write V2, publish
```

Running 10, 11 or 12 against the cluster individually is the one sequencing
error this amendment exists to prevent: three 8-hour re-runs to buy what one
buys.

### Decisions taken with the user (2026-08-31)

- **The taxi interval uses real block times, with a calibrated buffer as
  fallback.** Containment gains a second, wider interval
  `[t_off_block, t_in_block]` = `[aobt, aibt]` where APDF measured them, and
  `[t_off − B_dep, t_land + B_arr]` where it did not. `aobt` falls back to NM's
  `AOBT_3` and covers ~100% of flights; **`aibt` is APDF-only, has no fallback,
  and covers about half** (44,841 of ~89,500 on 2025-06-05/07). `B_dep` and
  `B_arr` are the *measured* median taxi times over the covered flights, per
  period — not chosen constants. Coverage is reported per side, never as one
  figure.
- **§8.2 stays, and recomputes nothing.** Comment 7 removes ADEP/ADES *runs* for
  the six non-shipping arms; it does not delete a finding already paid for. The
  eight-arm scatter showing that clustering quality fails to predict downstream
  accuracy stays on the CSVs already in `data/`, because it is what justifies
  not optimising `clean_match_pct` directly.
- **The sweep tests decoupling the callsign lookback.** `gap_minutes` currently
  does double duty in `recommended`: the general gap break *and* the bound on
  the callsign lookback. That is an implementation accident, so
  `callsign_lookback_minutes` becomes a real parameter defaulting to `None`,
  meaning "follow `gap_minutes`" — which reproduces today's behaviour exactly.

### Global Constraints — additions and corrections

Apply these to the Global Constraints section above before dispatching Task 10.

- **CORRECTION.** "S3 is a shared 100 GB bucket" is **wrong**. The quota is
  **200 GB** (`BUCKET_QUOTA_GB = 200.0` on `opdi` main). The scratch inventory
  script at `/home/jupyter/.claude/jobs/e2181584/tmp/clean_bucket.py:87`
  hardcodes `100e9` and has under-reported headroom by 100 GB all session — it
  said 1.52 GB free when ~101 GB was available. Task 10 Step 0 fixes it.
  Everything else in that bullet stands: single-object deletes only, never
  delete a prefix this study did not create.

- **NEW — prose.** *Write like someone who knows the subject, not like an
  assistant.* Checkable rules:
  - No "delve", "leverage", "robust", "comprehensive", "seamless", "crucial",
    "it's worth noting", "it is important to note", "that said".
  - No sentence whose only job is to announce the next sentence.
  - No three-item list whose third item is filler for rhythm.
  - No em-dash clause that restates the clause before it.
  - Active voice with a named actor: "the sweep found", not "it was found that".
  - Hedges need a number beside them. "Somewhat", "relatively", "fairly" are
    allowed only where a measurement justifies them.
  - A paragraph that survives deleting its first sentence did not need it.

- **NEW — V-measure is gone.** Not reported, not plotted, not mentioned, not
  computed in any code path this plan touches. Task 12 Step 3 removes the code;
  Task 14 §A removes the prose.

---

### Task 10: Decouple the callsign lookback; make the sweep harness arm-agnostic

**No cluster. No Spark job. Code and unit tests only.**

Two changes that together let Chapter 7 be rebuilt around `recommended` instead
of `legacy`. `track_sweep.py` hardcodes `rule = legacy()`, so it can sweep one
arm only; and `recommended`'s lookback bound is welded to `gap_minutes`, so the
axis the user most wants tested cannot be varied at all.

**Files:**
- Modify: `$OPDI/src/opdi/pipeline/segmentation/base.py:88-95` — new parameter
- Modify: `$OPDI/src/opdi/pipeline/segmentation/__init__.py` — export accessor
- Modify: `$OPDI/src/opdi/pipeline/segmentation/methods.py:455-460` — use it
- Modify: `$OPDI/src/opdi/config.py` — `SegmentationConfig` gains the same field
- Modify: `$OPDI/benchmarks/track_sweep.py` — `--method`, `--grid-lookback`
- Test: `$OPDI/tests/test_segmentation_lookback.py` (create)
- Fix (scratch, not committed): `/home/jupyter/.claude/jobs/e2181584/tmp/clean_bucket.py:87`

**Interfaces:**
- Produces: `SegmentationParams.callsign_lookback_minutes: float | None = None`
  and `segmentation.base.lookback_minutes(p) -> float`. Task 13 sweeps it;
  Task 14 §H reports it.
- Produces: `track_sweep.py --method {legacy,recommended,…}` and
  `--grid-lookback N [N …]`. Task 13 calls both.

- [ ] **Step 0: Fix the bucket quota constant**

```bash
sed -i 's/(100e9-total)/(200e9-total)/' /home/jupyter/.claude/jobs/e2181584/tmp/clean_bucket.py
grep -n "200e9" /home/jupyter/.claude/jobs/e2181584/tmp/clean_bucket.py
```

Expected: one line, the `BUCKET` print. Scratch tooling, not repo code — do not
commit it and do not look for it in git.

- [ ] **Step 1: Write the failing test**

```python
# $OPDI/tests/test_segmentation_lookback.py
"""`callsign_lookback_minutes` decouples A8's lookback bound from `gap_minutes`.

The bound exists because `break_expr` is evaluated over the whole airframe
window, and an unbounded `F.last` reaches back past a gap break into the
previous flight. `gap_minutes` became the bound only because it was to hand --
the two quantities answer different questions, and this pins the difference.
"""
from dataclasses import fields

from opdi.pipeline.segmentation import SegmentationParams
from opdi.pipeline.segmentation.base import lookback_minutes


def test_default_follows_gap_minutes():
    """None means "follow gap_minutes" -- today's behaviour, exactly."""
    p = SegmentationParams(gap_minutes=42.0)
    assert p.callsign_lookback_minutes is None
    assert lookback_minutes(p) == 42.0


def test_explicit_value_overrides():
    p = SegmentationParams(gap_minutes=30.0, callsign_lookback_minutes=5.0)
    assert lookback_minutes(p) == 5.0


def test_zero_is_honoured_not_treated_as_unset():
    """0.0 is falsy, and `or` would silently read it as unset.

    A zero lookback is a meaningful grid cell: it disables the callsign-change
    break entirely, which is the sweep's `airframe_only` corner. If this fails,
    the implementation used `or` instead of an `is None` check.
    """
    p = SegmentationParams(gap_minutes=30.0, callsign_lookback_minutes=0.0)
    assert lookback_minutes(p) == 0.0


def test_config_and_params_still_agree_field_for_field():
    """`from_config` raises TypeError when SegmentationConfig lacks a field.

    tests/test_segmentation_base.py already asserts the default sets match. This
    asserts the *new* field, so an edit to one dataclass cannot quietly leave
    the other behind.
    """
    from opdi.config import SegmentationConfig

    names = {f.name for f in fields(SegmentationConfig)}
    assert "callsign_lookback_minutes" in names
    assert SegmentationConfig().callsign_lookback_minutes is None
    assert SegmentationParams.from_config(SegmentationConfig()) == SegmentationParams()
```

- [ ] **Step 2: Run it to confirm it fails**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
.venv310/bin/python -m pytest tests/test_segmentation_lookback.py -v
```

Expected: FAIL — `cannot import name 'lookback_minutes'`.

- [ ] **Step 3: Add the parameter and its accessor**

In `base.py`, after `low_alt_ft` (line 90):

```python
    #: Bound on A8's callsign lookback, in minutes. ``None`` means "follow
    #: ``gap_minutes``", which is what the rule did when the bound was written
    #: and is therefore the only default that reproduces published behaviour.
    #:
    #: Separate from ``gap_minutes`` because the two answer different questions.
    #: ``gap_minutes`` asks how long a reception hole must be before it is a new
    #: flight. This asks how long a *callsign* stays valid for comparison across
    #: blank samples. Nothing says one number is right for both; they were the
    #: same number because one was to hand when the other was needed.
    callsign_lookback_minutes: float | None = None
```

Beside the other accessors:

```python
def lookback_minutes(p: "SegmentationParams") -> float:
    """A8's lookback bound: the explicit value, or ``gap_minutes`` when unset.

    ``is None`` rather than ``or``: ``0.0`` is a meaningful setting -- it
    disables the callsign-change break, which is the grid's ``airframe_only``
    corner -- and ``or`` would read it as unset.
    """
    if p.callsign_lookback_minutes is None:
        return p.gap_minutes
    return p.callsign_lookback_minutes
```

Export it from `__init__.py` alongside `gap_minutes`.

In `config.py`, `SegmentationConfig` gains the identical field with the
identical default:

```python
    callsign_lookback_minutes: float | None = None
```

In `methods.py`, `recommended`'s `expr` (around line 455) replaces
`p.gap_minutes` in the `recent` predicate — **and only there**:

```python
        recent = (
            F.unix_timestamp(F.col("_ts")) - F.unix_timestamp(prev_real_ts)
        ) / 60.0 < lookback_minutes(p)
```

`legacy().break_expr(p)` on the next line keeps `p.gap_minutes` untouched. The
two uses are now distinct, which is the whole point.

- [ ] **Step 4: Run the tests**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
.venv310/bin/python -m pytest tests/test_segmentation_lookback.py -v
.venv310/bin/python -m pytest tests/ -q
```

Both pass. The suite stood at **439** after `c380344`; report the new count. A
failure here is a real signal: `from_config` raises `TypeError` when
`SegmentationConfig` lacks a params field, so a half-applied change fails loudly.

- [ ] **Step 5: Make the sweep harness arm-agnostic**

In `track_sweep.py`, replace the `legacy` import:

```python
from opdi.pipeline.segmentation.methods import ARMS  # noqa: E402
```

Add two arguments beside the existing grid overrides:

```python
    ap.add_argument("--method", default="legacy", choices=sorted(ARMS),
                    help="which arm to sweep; the grid is the same for all of "
                         "them, but only `recommended` reads --grid-lookback")
    ap.add_argument("--grid-lookback", nargs="+", type=float, default=None,
                    help="callsign_lookback_minutes values. Omit to leave it "
                         "unset, i.e. following gap_minutes -- which is what "
                         "every cell of the legacy sweep did.")
```

Replace `rule = legacy()` with `rule = ARMS[args.method]()`.

The grid becomes four-dimensional, with `None` the sentinel for "unset":

```python
    lookback_grid = args.grid_lookback if args.grid_lookback else [None]
    grid = list(itertools.product(gap_grid, low_gap_grid, low_ft_grid, lookback_grid))
    # a low-altitude rule looser than the general one is inert
    grid = [c for c in grid if c[1] <= c[0]]
```

`CELL_KEYS` gains the fourth column, and each cell's `params` and `row` gain
`callsign_lookback_minutes`. **`--resume` compatibility matters**: the legacy
sweep's committed CSVs have no such column, so `cell_key` must read a missing
key as `None` rather than raising:

```python
CELL_KEYS = ("gap_minutes", "low_alt_gap_minutes", "low_alt_ft",
             "callsign_lookback_minutes")


def cell_key(row: dict) -> tuple:
    """The grid-cell identity, tolerant of CSVs written before the 4th axis.

    A row from a three-axis sweep has no `callsign_lookback_minutes` at all and
    must read as the unset cell -- otherwise `--resume` against any committed V1
    sweep file raises KeyError instead of skipping.
    """
    out = []
    for k in CELL_KEYS:
        v = row.get(k, "")
        out.append(None if v in ("", None, "None") else float(v))
    return tuple(out)
```

Also drop `v_measure` from the progress line — Task 12 removes the function and
a sweep printing it would not import:

```python
                print(
                    f"  [{i}/{len(todo)}] gap={g} lowgap={lg} lowalt={lft}ft "
                    f"lookback={lb}  clean={row['clean_match_pct']:.2f}%"
                )
```

- [ ] **Step 6: Test the harness change without a cluster**

```python
# append to $OPDI/tests/test_segmentation_lookback.py
def test_cell_key_reads_a_three_axis_row_as_the_unset_cell():
    """--resume against a committed V1 sweep CSV must skip, not crash."""
    import pathlib
    import sys

    sys.path.insert(
        0, str(pathlib.Path(__file__).resolve().parent.parent / "benchmarks")
    )
    from track_sweep import cell_key

    legacy_row = {"gap_minutes": "30", "low_alt_gap_minutes": "15",
                  "low_alt_ft": "5000"}
    assert cell_key(legacy_row) == (30.0, 15.0, 5000.0, None)
    assert cell_key({**legacy_row, "callsign_lookback_minutes": ""}) == (
        30.0, 15.0, 5000.0, None)
    assert cell_key({**legacy_row, "callsign_lookback_minutes": "5"}) == (
        30.0, 15.0, 5000.0, 5.0)
```

```bash
.venv310/bin/python -m pytest tests/test_segmentation_lookback.py -v
```

- [ ] **Step 7: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
uvx ruff check src/opdi benchmarks/track_sweep.py tests/test_segmentation_lookback.py
git add src/opdi benchmarks/track_sweep.py tests/test_segmentation_lookback.py
git commit -m "feat(segmentation): callsign lookback is its own parameter

A8 bounded its callsign lookback with gap_minutes because that value was to
hand, not because the two questions share an answer. gap_minutes asks how long a
reception hole must be before it is a new flight; the lookback asks how long a
callsign stays comparable across blank samples.

Defaults to None, meaning follow gap_minutes -- published behaviour, unchanged.
track_sweep.py gains --method and --grid-lookback so the sweep can run against
an arm other than legacy."
```

---

### Task 11: Score the whole turnaround, not just the airborne leg

**No cluster. No Spark job. Code and unit tests only.**

Comment 4, and it is a real blind spot rather than a presentation problem.
`overlap_join` matches a state vector to a flight when
`t_off <= event_time <= t_land` — the **airborne** interval. Every sample at the
stand, during taxi-out and during taxi-in therefore matches no flight and is
dropped before any metric sees it. `run_arm`'s own docstring
(`track_methods.py:309-314`) says so: *"anything about the aircraft on the ground
— taxi-out reception, for instance — is structurally absent from it."* The code
knew; the paper did not.

The fix is not a guessed buffer. `load_flight_intervals` **already carries
`aobt` and `aibt`** — measured off-block and in-block times — and simply does not
use them for containment.

**Files:**
- Modify: `$OPDI/benchmarks/track_truth.py` — gate interval, `bounds` argument
- Modify: `$OPDI/benchmarks/track_score.py` — `score_arm_gated`
- Test: `$OPDI/tests/test_track_gate_interval.py` (create)

**Interfaces:**
- Produces: `track_truth.gate_buffers(gt) -> (b_dep_s, b_arr_s)` — measured
  median taxi times, seconds.
- Produces: columns `t_off_block`, `t_in_block`, `gate_dep_measured`,
  `gate_arr_measured` on `load_flight_intervals`' output.
- Produces: `track_truth.attach_gate_interval(gt, b_dep_s, b_arr_s)`.
- Produces: `overlap_join(assign, gt, bounds=("t_off", "t_land"))` — the default
  is today's behaviour, so no existing caller changes.
- Produces: `track_score.score_arm_gated(matched, extents, matched_gate)` — the
  airborne row plus every gate metric under a `gate_` prefix.
- Tasks 7 and 13 consume these; Task 14 §D reports them.

- [ ] **Step 1: Write the failing tests**

```python
# $OPDI/tests/test_track_gate_interval.py
"""Containment over the gate-to-gate interval, so taxi and stand samples count.

The airborne interval [t_off, t_land] is what every V1 metric was computed over,
which means no V1 number says anything about whether taxi-out was attached to
the right flight. These tests pin the wider interval, and pin the property that
makes the two comparable: the gate interval always *contains* the airborne one,
so gate matching is a superset and can only ever add samples.
"""
import datetime as dt

import track_truth
from track_truth import overlap_join


def _ts(s):
    return dt.datetime.fromisoformat(s)


def test_gate_interval_contains_the_airborne_interval(spark):
    """The guard that makes the comparison sound.

    APDF is real operational data: a bad AOBT after its own take-off exists.
    Without least()/greatest() such a row yields a gate interval *narrower* than
    the airborne one, and gate matching would drop samples airborne matching
    kept -- inverting the finding.
    """
    gt = spark.createDataFrame(
        [
            # normal: off-block 12 min before take-off, in-block 6 after landing
            ("f1", "abc123", _ts("2025-06-05T10:00:00"), _ts("2025-06-05T11:00:00"),
             _ts("2025-06-05T09:48:00"), _ts("2025-06-05T11:06:00")),
            # corrupt: AOBT after ATOT, AIBT before ALDT
            ("f2", "def456", _ts("2025-06-05T14:00:00"), _ts("2025-06-05T15:00:00"),
             _ts("2025-06-05T14:05:00"), _ts("2025-06-05T14:55:00")),
        ],
        "flight_key string, icao24 string, t_off timestamp, t_land timestamp, "
        "aobt timestamp, aibt timestamp",
    )
    out = track_truth.attach_gate_interval(gt, b_dep_s=600, b_arr_s=300).collect()
    by = {r["flight_key"]: r for r in out}

    assert by["f1"]["t_off_block"] == _ts("2025-06-05T09:48:00")
    assert by["f1"]["t_in_block"] == _ts("2025-06-05T11:06:00")
    # clamped to the airborne bound, never inside it
    assert by["f2"]["t_off_block"] == _ts("2025-06-05T14:00:00")
    assert by["f2"]["t_in_block"] == _ts("2025-06-05T15:00:00")


def test_null_block_times_fall_back_to_the_measured_buffer(spark):
    """aibt is APDF-only and NULL for about half the sample. That half still
    needs an interval, and the buffer is a measured median -- not a constant
    someone liked the look of."""
    gt = spark.createDataFrame(
        [("f3", "abc123", _ts("2025-06-05T10:00:00"), _ts("2025-06-05T11:00:00"),
          None, None)],
        "flight_key string, icao24 string, t_off timestamp, t_land timestamp, "
        "aobt timestamp, aibt timestamp",
    )
    r = track_truth.attach_gate_interval(gt, b_dep_s=600, b_arr_s=300).collect()[0]
    assert r["t_off_block"] == _ts("2025-06-05T09:50:00")
    assert r["t_in_block"] == _ts("2025-06-05T11:05:00")
    assert r["gate_dep_measured"] is False
    assert r["gate_arr_measured"] is False


def test_gate_buffers_are_the_measured_medians(spark):
    """b_dep is median(t_off - aobt) over the flights where aobt is measured."""
    gt = spark.createDataFrame(
        [("f1", _ts("2025-06-05T10:00:00"), _ts("2025-06-05T11:00:00"),
          _ts("2025-06-05T09:50:00"), _ts("2025-06-05T11:05:00"), True, True),
         ("f2", _ts("2025-06-05T12:00:00"), _ts("2025-06-05T13:00:00"),
          _ts("2025-06-05T11:40:00"), _ts("2025-06-05T13:15:00"), True, True),
         ("f3", _ts("2025-06-05T14:00:00"), _ts("2025-06-05T15:00:00"),
          None, None, False, False)],
        "flight_key string, t_off timestamp, t_land timestamp, aobt timestamp, "
        "aibt timestamp, dep_measured boolean, arr_measured boolean",
    )
    b_dep, b_arr = track_truth.gate_buffers(gt)
    assert b_dep == 900.0    # median of 600 s and 1200 s
    assert b_arr == 750.0    # median of 300 s and 900 s


def test_taxi_sample_matches_gate_but_not_airborne(spark):
    """The finding, as a test. One sample during taxi-out.

    Under the airborne interval it belongs to no flight and vanishes from every
    metric. Under the gate interval it belongs to the flight it obviously
    belongs to.
    """
    assign = spark.createDataFrame(
        [("abc123", _ts("2025-06-05T09:52:00"), "trk1")],
        "icao24 string, event_time timestamp, track_id string",
    )
    gt = spark.createDataFrame(
        [("f1", "abc123", _ts("2025-06-05T10:00:00"), _ts("2025-06-05T11:00:00"),
          _ts("2025-06-05T09:48:00"), _ts("2025-06-05T11:06:00"), "apdf",
          "EBBR", "LEMD")],
        "flight_key string, icao24 string, t_off timestamp, t_land timestamp, "
        "t_off_block timestamp, t_in_block timestamp, t_source string, "
        "gt_adep string, gt_ades string",
    )
    assert overlap_join(assign, gt).count() == 0
    gated = overlap_join(assign, gt, bounds=("t_off_block", "t_in_block"))
    assert gated.count() == 1
    assert gated.collect()[0]["flight_key"] == "f1"
```

- [ ] **Step 2: Run them to confirm they fail**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
.venv310/bin/python -m pytest tests/test_track_gate_interval.py -v
```

Expected: FAIL — `attach_gate_interval` and `gate_buffers` do not exist, and
`overlap_join` takes no `bounds`.

- [ ] **Step 3: Implement in `track_truth.py`**

```python
def gate_buffers(gt: DataFrame) -> tuple:
    """Median taxi-out and taxi-in, in seconds, over the flights that measured them.

    These are the fallback where APDF has no block time. Measured rather than
    chosen: `aobt` covers ~100% of flights through NM's AOBT_3 fallback, but
    `aibt` is APDF-only and covers about half, so roughly half the arrival
    intervals are modelled. Taking the modelled half's duration from the
    measured half is the honest version of a buffer; picking ten minutes because
    it sounds like a taxi is not.

    Returns ``(b_dep_s, b_arr_s)``. A period with no measured flight on one side
    falls back to zero there, degrading the gate interval to the airborne one
    rather than inventing a duration.
    """
    row = gt.select(
        F.percentile_approx(
            F.when(F.col("dep_measured"),
                   F.unix_timestamp("t_off") - F.unix_timestamp("aobt")),
            0.5,
        ).alias("b_dep"),
        F.percentile_approx(
            F.when(F.col("arr_measured"),
                   F.unix_timestamp("aibt") - F.unix_timestamp("t_land")),
            0.5,
        ).alias("b_arr"),
    ).collect()[0]
    return (float(row["b_dep"] or 0.0), float(row["b_arr"] or 0.0))


def attach_gate_interval(gt: DataFrame, b_dep_s: float, b_arr_s: float) -> DataFrame:
    """Add the gate-to-gate interval beside the airborne one.

    ``least``/``greatest`` are not defensive padding. APDF is operational data
    and carries rows whose block time falls the wrong side of its own movement
    time; without the clamp such a row yields a gate interval *narrower* than
    the airborne interval, and gate matching would drop samples airborne
    matching kept. The clamp makes the gate interval a guaranteed superset, so
    the two metrics differ only by the samples the wider one adds.
    """
    return (
        gt.withColumn("gate_dep_measured", F.col("aobt").isNotNull())
        .withColumn("gate_arr_measured", F.col("aibt").isNotNull())
        .withColumn(
            "t_off_block",
            F.least(
                F.coalesce(
                    F.col("aobt"),
                    (F.unix_timestamp("t_off") - F.lit(b_dep_s)).cast("timestamp"),
                ),
                F.col("t_off"),
            ),
        )
        .withColumn(
            "t_in_block",
            F.greatest(
                F.coalesce(
                    F.col("aibt"),
                    (F.unix_timestamp("t_land") + F.lit(b_arr_s)).cast("timestamp"),
                ),
                F.col("t_land"),
            ),
        )
    )
```

`overlap_join` gains the parameter, defaulting to today's behaviour:

```python
def overlap_join(assign: DataFrame, gt: DataFrame,
                 bounds=("t_off", "t_land")) -> DataFrame:
```

Inside, the join predicate and the tie-break window read `bounds[0]`/`bounds[1]`
instead of literal `t_off`/`t_land`. The **select stays fixed and still emits
`t_off` and `t_land`**, because `boundary_error` needs the airborne boundaries
whichever interval did the matching. Add to the docstring:

```
    ``bounds`` selects the interval. The default is the airborne
    ``[t_off, t_land]``. Passing ``("t_off_block", "t_in_block")`` matches over
    the gate-to-gate interval instead, which is what includes taxi-out, taxi-in
    and stand samples. The emitted ``t_off``/``t_land`` are unchanged either
    way: they are the airborne boundaries, and boundary error is defined against
    them regardless of which interval decided membership.
```

Finally, `load_flight_intervals` calls `attach_gate_interval` on its result
(after the existing filters), using buffers computed from that same frame, and
adds `t_off_block`, `t_in_block`, `gate_dep_measured`, `gate_arr_measured` to
its final `select`.

- [ ] **Step 4: Implement `score_arm_gated` in `track_score.py`**

```python
def score_arm_gated(matched, extents, matched_gate) -> dict:
    """The airborne row, plus every gate-to-gate rate under a `gate_` prefix.

    Both are reported because they answer different questions and the paper
    needs both. The airborne metrics are what every V1 number was computed over
    and must stay comparable to; the gate metrics say whether the aircraft's
    time at the stand and on the taxiways ended up in the right track.

    Boundary error is computed once, from the airborne match. It is defined
    against `t_off`/`t_land`, so computing it twice would produce two columns
    with the same name and different meanings.
    """
    row = score_arm(matched, extents)
    row.update({f"gate_{k}": v for k, v in match_rates(matched_gate).items()})
    return row
```

`run_arm`'s default scorer stays `score_arm`; callers wanting gate metrics pass
their own closure, which builds `matched_gate` from the same `assign` and `gt`.

- [ ] **Step 5: Run the tests**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
.venv310/bin/python -m pytest tests/test_track_gate_interval.py -v
.venv310/bin/python -m pytest tests/ -q
```

Both pass. Report the count.

- [ ] **Step 6: Prove the airborne path did not move**

The design rests on gate metrics being *additive*. Prove it:

```python
# append to tests/test_track_gate_interval.py
def test_airborne_metrics_are_untouched_by_the_gate_columns(spark):
    """score_arm over a gt frame carrying gate columns must equal score_arm
    over the same frame without them.

    If this fails, the gate work changed numbers V1 already published and Task
    13's re-run will silently rewrite them.
    """
    from track_score import score_arm, track_extents

    assign = spark.createDataFrame(
        [("abc123", _ts("2025-06-05T10:10:00"), "trk1"),
         ("abc123", _ts("2025-06-05T10:50:00"), "trk1"),
         ("def456", _ts("2025-06-05T14:30:00"), "trk2")],
        "icao24 string, event_time timestamp, track_id string",
    )
    plain_schema = (
        "flight_key string, icao24 string, t_off timestamp, t_land timestamp, "
        "t_source string, gt_adep string, gt_ades string"
    )
    plain_rows = [
        ("f1", "abc123", _ts("2025-06-05T10:00:00"),
         _ts("2025-06-05T11:00:00"), "apdf", "EBBR", "LEMD"),
        ("f2", "def456", _ts("2025-06-05T14:00:00"),
         _ts("2025-06-05T15:00:00"), "apdf", "EHAM", "LFPG"),
    ]
    plain = spark.createDataFrame(plain_rows, plain_schema)
    widened = track_truth.attach_gate_interval(
        plain.withColumn("aobt", F.lit(None).cast("timestamp"))
             .withColumn("aibt", F.lit(None).cast("timestamp")),
        b_dep_s=600, b_arr_s=300,
    )

    extents = track_extents(assign)
    a = score_arm(overlap_join(assign, plain), extents)
    b = score_arm(overlap_join(assign, widened), extents)
    assert a == b, f"airborne metrics moved: {a} vs {b}"
```

Add `from pyspark.sql import functions as F` at the top of the test module.

- [ ] **Step 7: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
uvx ruff check benchmarks tests/test_track_gate_interval.py
git add benchmarks tests/test_track_gate_interval.py
git commit -m "feat(bench): score the turnaround, not only the airborne leg

overlap_join matched on [t_off, t_land], so every sample at the stand and on the
taxiways matched no flight and was dropped before any metric saw it. run_arm's
docstring already said so. No V1 number says anything about whether taxi-out was
attached to the right flight.

The interval now has a gate-to-gate variant built from APDF's measured block
times, falling back to the measured median taxi time where AIBT is absent --
about half the sample, since AIBT is APDF-only with no NM fallback. Clamped so
the gate interval always contains the airborne one, which keeps the two
comparable."
```

---

### Task 12: The `traffic` fill diagnostic, and the removal of V-measure

**No cluster. No Spark job. Code and unit tests only.**

Two small unrelated changes, batched because both are single-file edits to the
benchmark layer and both must land before Task 13's re-run.

**Comment 3, answered.** From
`/home/jupyter/work/opdi-workspace/traffic/src/traffic/core/flight.py`:

- `Flight.split()` (line 1462) fills **nothing**. It cuts on raw timestamp gaps.
- `Flight.filter()` (line 1845) takes `strategy=lambda x: x.bfill().ffill()` as
  its **default**.
- `Flight.resample()` (line 1712) defaults to `how="interpolate"` — numeric
  columns interpolated, the rest forward-filled (lines 1799-1806).

So the hypothesis is right about the workflow: a `Flight` reaching `.split()` in
idiomatic traffic use has been gap-filled and often resampled to 1 s. A3 applies
the rule to a raw frame where `baro_altitude` is NULL ~21% of the time, and A3's
predicate reads altitude on *both sides* of the candidate gap — the samples that
decide the split.

**That is not the dominant cause, and the paper must not claim it is.** A3 lost
on *merging*, and merging is under-splitting. Its mechanism is traffic's single
10-minute gap threshold meeting an aircraft that broadcasts continuously through
a turnaround: no gap, no split, and no amount of filling creates a gap that was
never there. Legacy's second rule — a shorter gap below 5,000 ft — is what
catches that case. The measurement below separates the two so §6.3 can be
written from evidence.

**Files:**
- Modify: `$OPDI/benchmarks/track_diagnostics.py` — new `gap_boundary_nulls`
- Modify: `$OPDI/benchmarks/track_score.py` — delete `vmeasure`
- Modify: `$OPDI/benchmarks/regenerate_track_v1.py` — new job
- Test: `$OPDI/tests/test_track_diagnostics_gaps.py` (create)

**Interfaces:**
- Produces: `track_diagnostics.gap_boundary_nulls(sv, gap_minutes=10.0) -> dict`
  with `n_gaps`, `n_null_either_side`, `null_pct`, `n_no_gap_turnarounds`.
  Task 13 runs it; Task 14 §C reports it.
- Removes: `track_score.vmeasure`. `score_arm` calls it today and must be
  updated in the same commit.

- [ ] **Step 1: Write the failing test**

```python
# $OPDI/tests/test_track_diagnostics_gaps.py
"""How often A3's split predicate cannot see the altitude it needs.

traffic's rule reads altitude on both sides of a candidate gap. On a raw OSN
frame that value is often NULL, and a NULL comparison is not a split decision --
it is the absence of one. This counts how often that happens, so section 6.3 can
say which part of A3's failure is the missing fill and which part is the
single-threshold design.
"""
import datetime as dt

from track_diagnostics import gap_boundary_nulls


def _ts(s):
    return dt.datetime.fromisoformat(s)


def test_counts_gaps_whose_boundary_altitude_is_null(spark):
    sv = spark.createDataFrame(
        [
            # a gap with altitude on both sides -- the predicate can decide
            ("a1", _ts("2025-06-05T10:00:00"), 30000.0, False),
            ("a1", _ts("2025-06-05T10:20:00"), 31000.0, False),
            # a gap with NULL on the far side -- it cannot
            ("a2", _ts("2025-06-05T10:00:00"), 30000.0, False),
            ("a2", _ts("2025-06-05T10:20:00"), None, False),
        ],
        "icao24 string, event_time timestamp, baro_altitude_ft double, "
        "on_ground boolean",
    )
    out = gap_boundary_nulls(sv, gap_minutes=10.0)
    assert out["n_gaps"] == 2
    assert out["n_null_either_side"] == 1
    assert out["null_pct"] == 50.0


def test_counts_turnarounds_with_no_gap_at_all(spark):
    """The failure no fill can fix: continuous broadcast through a turnaround.

    traffic has one threshold, on gap length. An aircraft on stand still
    broadcasting produces no gap, so the rule never splits and the two legs
    merge. Legacy catches this with its second rule, a shorter gap below
    5,000 ft. Counting these separates A3's two failure modes.
    """
    sv = spark.createDataFrame(
        [("b1", _ts("2025-06-05T10:00:00"), 300.0, True),
         ("b1", _ts("2025-06-05T10:02:00"), 300.0, True),
         ("b1", _ts("2025-06-05T10:04:00"), 300.0, True),
         ("b1", _ts("2025-06-05T10:06:00"), 300.0, True),
         ("b1", _ts("2025-06-05T10:08:00"), 300.0, True),
         ("b1", _ts("2025-06-05T10:11:00"), 300.0, True)],
        "icao24 string, event_time timestamp, baro_altitude_ft double, "
        "on_ground boolean",
    )
    out = gap_boundary_nulls(sv, gap_minutes=10.0)
    assert out["n_gaps"] == 0
    assert out["n_no_gap_turnarounds"] >= 1
```

- [ ] **Step 2: Run it to confirm it fails**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
.venv310/bin/python -m pytest tests/test_track_diagnostics_gaps.py -v
```

Expected: FAIL — `gap_boundary_nulls` does not exist.

- [ ] **Step 3: Implement it, then delete V-measure**

`gap_boundary_nulls` groups on `icao24`, orders on `event_time`, and over a
`rowsBetween(-1, -1)` lag computes the gap in minutes and the previous
`baro_altitude_ft`. A gap is a row where `gap > gap_minutes`. It is
**undecidable** when either its own or the previous altitude is NULL. A
"no-gap turnaround" is a maximal run of `on_ground = true` samples spanning more
than `gap_minutes` with **no** internal gap above the threshold — the case
traffic's single rule structurally cannot see.

Then remove V-measure.

**CORRECTED 2026-08-31 (ruling R46). Do not delete the `vmeasure` function.**
An earlier draft of this task said to. That was wrong, and would have deleted
two metrics the paper keeps. `track_score.vmeasure` computes **three** things
and returns them in one dict: `homogeneity`, `completeness`, and `v_measure`.
Homogeneity and completeness map onto merging and fragmentation, they are §4.1's
whole subject, and they stay. Only the harmonic mean goes.

So:

- **Rename** `vmeasure` to `homogeneity_completeness`. A function named
  `vmeasure` that does not return a V-measure is a worse defect than the one
  this comment is fixing.
- Delete the two lines computing `denom` and `v`, and the `"v_measure"` key from
  both the normal return and the `total == 0` early return.
- **Keep `_entropy`.** Homogeneity and completeness are entropy-based; it is
  load-bearing, not V-measure scaffolding.
- Update `score_arm`'s call site to the new name.
- `v_measure` from `track_sweep.py`'s progress line (Task 10 Step 5 did this —
  verify rather than repeat).
- Any `v_measure` key in `track_pipeline_v2.py`'s exported columns.

Also fix the docstring while renaming: it currently opens "Homogeneity,
completeness and their harmonic mean," which the rename falsifies. Say what the
two measures are for — one is the merging measure, the other the fragmentation
measure — since that is the reason they survived and the mean did not.

```bash
grep -rn "vmeasure\|v_measure" benchmarks/ src/ tests/
```

must return nothing, while `homogeneity` and `completeness` must still be
present and still be reported by `score_arm`. The committed CSVs keep their
`v_measure` column — they are historical records and are not edited by hand;
Task 13's re-run drops it naturally.

**One consequence for Task 13.** The Task 11 reviewer noted that `vmeasure`
sums floats in Python over `contingency(...).collect()` order, which is
partition-order dependent, so `homogeneity` and `completeness` can differ in
their last bits between runs of identical code. Task 13 Step 3 demands
byte-identical airborne columns — **compare those two columns with a tolerance,
not `==`**, or a re-run will look like a regression it is not. Every other
airborne column is exact and must be compared exactly.

- [ ] **Step 4: Run the tests**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
.venv310/bin/python -m pytest tests/test_track_diagnostics_gaps.py -v
.venv310/bin/python -m pytest tests/ -q
```

Any existing test asserting on `v_measure` is **deleted, not weakened** — the
metric is gone, so a test for it tests nothing. Report which ones went.

- [ ] **Step 5: Declare the new job in `regenerate_track_v1.py`**

One job per period, `traffic_fill_<period>`, writing
`traffic_fill_<period>.csv`. Its `code_paths` must include
`benchmarks/track_diagnostics.py`. It reads the same cleaned track table the
arms jobs read.

- [ ] **Step 6: Commit**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
uvx ruff check benchmarks tests/test_track_diagnostics_gaps.py
git add benchmarks tests
git commit -m "feat(bench): measure what A3's split predicate cannot see; drop V-measure

traffic's Flight.split fills nothing, but Flight.filter defaults to
bfill().ffill() and Flight.resample defaults to interpolation -- so the rule is
designed against a filled frame. OPDI applies it to a raw one where
baro_altitude is NULL about a fifth of the time, on the very samples that decide
the split. gap_boundary_nulls counts that, and separately counts turnarounds
with no gap at all, which is the failure no fill can fix.

V-measure went with it. It ranked the arms differently from the headline metric
at the top of the table, it chose nothing, and it collects the whole contingency
table to the driver to compute."
```

---

### Task 13: One combined re-run

**The expensive task, deliberately singular.** Tasks 10, 11 and 12 each changed
a file `regenerate_track_v1.py` fingerprints, so every V1 job is stale. Running
them one at a time costs three full re-runs; Task 5 established the shape of
one, at 8h20m.

**Preconditions, all three checked before starting:**

1. `kubectl -n eurocontrol get pods | grep -c Running` is 0.
2. S3 headroom ≥ 12 GB. **The quota is 200 GB**, not the 100 GB the scratch
   script assumed before Task 10 Step 0.
3. Tasks 10, 11 and 12 are committed.

- [ ] **Step 1: Establish the stale set, and record it**

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
export OPDI_PAPER_DIR=/home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v1
.venv310/bin/python benchmarks/regenerate_track_v1.py --check
```

Write the list into the report *before* running anything. Expect all fourteen,
plus the two new `traffic_fill_*` jobs.

- [ ] **Step 2: Re-run the stale jobs**

Serially — one Spark job at a time, driver port 7078. Follow Task 5's invocation
pattern.

- [ ] **Step 3: Verify the airborne numbers did not move**

**The most important check in this task.** Tasks 10–12 were designed to be
additive: the lookback defaults to `None` (reproducing `gap_minutes`), the gate
interval adds columns, and V-measure's removal computes strictly less. So every
surviving airborne metric must be **identical**, not merely close.

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan
git diff --stat papers/track-construction-v1/data/
git diff papers/track-construction-v1/data/arms_2025.csv
```

Expected: `v_measure` disappears, `gate_*` appear, and **every other cell is
byte-identical**. A moved `clean_match_pct` means one of the three tasks was not
additive after all — stop and find out which before continuing. Report the diff
either way.

- [ ] **Step 4: Sweep `recommended`, staged**

Staged as V1's legacy sweep was, because the four-axis product is 1,175 cells
and that is not a sweep, it is a weekend.

```bash
cd /home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2
D=/home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v1/data

# stage 1 -- the three legacy axes, at `recommended`, on 2025. 235 cells.
.venv310/bin/python -u benchmarks/track_sweep.py --method recommended \
  --period 2025 --results-dir $D --resume

# stage 2 -- the lookback axis alone, at stage 1's optimum. Substitute the
# winning triple; do not guess it.
.venv310/bin/python -u benchmarks/track_sweep.py --method recommended \
  --period 2025 --results-dir $D \
  --grid-gap <G*> --grid-low-alt-gap <LG*> --grid-low-alt-ft <LFT*> \
  --grid-lookback 0 5 10 15 30 60 120

# stage 3 -- confirm the winner on 2024.
.venv310/bin/python -u benchmarks/track_sweep.py --method recommended \
  --period 2024 --results-dir $D \
  --grid-gap <G*> --grid-low-alt-gap <LG*> --grid-low-alt-ft <LFT*> \
  --grid-lookback <LB*>
```

Outputs `sweep_recommended_2025_stage{1,2}.csv` and
`sweep_recommended_2024_stage3.csv`.

**If a winning value sits on a grid edge, extend the grid and re-run** — the
rule V1's Chapter 7 followed when `low_alt_ft` peaked at its boundary. An
optimum at the edge is not an optimum; it is a grid that stopped too early.

- [ ] **Step 5: Verify and commit**

```bash
.venv310/bin/python benchmarks/regenerate_track_v1.py --check   # exits 0
```

Nothing remains under `research/`. Commit the data in the portal worktree scoped
to `papers/track-construction-v1/data/` — **never `git add -A`** there; that
checkout carries 283 unrelated `_site` deletions that must not be staged.

---

### Task 14: The V1 editorial pass

Everything from the ten comments that is prose. Runs after Tasks 13 and 7,
because §D, §E and §F need numbers those produce.

**Files:**
- Modify: `$PORTAL/papers/track-construction-v1/index.qmd`

Ten lettered edits, each its own commit — a reviewer should be able to reject
one without unpicking the rest.

- [ ] **§A — Delete V-measure (comment 1)**

Remove the "**V-measure** is their harmonic mean…" paragraph at lines 465-479
entirely. Homogeneity and completeness stay: they map onto merging and
fragmentation, and they are used. In the arms table (lines ~731 and ~739), drop
the `v_measure` column and its `"V-measure"` header. Then:

```bash
grep -in "v-measure\|v_measure" papers/track-construction-v1/index.qmd
```

must return nothing. Not "nothing important" — nothing.

- [ ] **§B — Shorten §4.2's last paragraph (comment 2)**

The paragraph beginning "A flight that is both merged and fragmented counts as
**merged**…" runs about 120 words over four ideas: the tie-break, the
strictness, the rejected alternative, the lower-bound claim. Keep the tie-break
with its one-clause reason, and the lower-bound sentence. The rejected tolerant
alternative moves to a footnote or goes. Target: under 50 words.

- [ ] **§C — Rewrite §6.3 with the fill finding (comment 3)**

In this order: traffic's `split` fills nothing; its `filter` and `resample`
default to `bfill().ffill()` and interpolation, so the rule is designed against
a filled frame; OPDI applies it raw, where `baro_altitude` is NULL about a fifth
of the time, on the samples the predicate reads. Then the measurement from
`traffic_fill_<period>.csv`: what share of candidate gaps have an undecidable
boundary.

Then the correction, and do not bury it: **this is not the main cause.** A3 lost
on merging, merging is under-splitting, and the mechanism is one gap threshold
meeting a continuously-broadcasting turnaround — `n_no_gap_turnarounds` from the
same CSV. Filling cannot create a gap that was never there. Legacy's second rule
is what catches it.

The conclusion changes from "the reference approach does not transfer" to
something truer: it transfers badly because it is used outside the preprocessing
it assumes, and even inside that preprocessing one threshold cannot see a
turnaround with no gap.

- [ ] **§D — The turnaround is part of the track (comment 4)**

A change of framing across the paper, not one section's edit.

In §4.3 (`sec-containment`), add a subsection saying plainly that containment
ran on the airborne interval, that every stand and taxi sample therefore matched
no flight and left the metrics, and that it is now measured both ways. Give the
gate interval, the block-time sources, and the per-side coverage — `aobt` ~100%
via NM's fallback, `aibt` ~50% because it is APDF-only. State that the buffers
are measured medians, and give them per period.

Report `gate_clean_match_pct` beside `clean_match_pct` in the arms table. Where
the two disagree, that gap **is** the taxi-attachment quality — the new result
this comment buys.

In §6.7, rewrite the A7 verdict. The current text reads its positive departure
offsets as a boundary artefact. Under the gate interval it is not an artefact:
slicing off the initial climb also slices off the taxi-out and the stand, which
is the part of the track the user wants kept. Say that A7 fails the gate metric
by more than it fails the airborne one, and give both.

Then sweep the paper for the airborne assumption. `boundary_error`'s sign
convention already documents that OPDI tracks legitimately overhang ground
truth's interval on both sides — that passage now needs to say the overhang is
*wanted*, not tolerated.

- [ ] **§E — Replace Chapter 7 (comments 5 and 6)**

Delete `## Was legacy simply mistuned?` (`sec-sweep`) and its three
subsections. Replace with `## Tuning what actually ships {#sec-sweep}`, built on
Task 13 Step 4's CSVs: the three-axis grid at `recommended`, then the lookback
axis, then the 2024 confirmation.

Report where each axis peaks and whether the optimum is interior. State plainly
whether the sweep beats the shipped defaults, **including if it does not** — a
sweep finding the current settings already good is a result, and is the more
likely one.

The lookback axis gets its own paragraph: the only axis unique to
`recommended`, pinned to `gap_minutes` by accident rather than design, made real
by Task 10. If a decoupled value wins by enough to matter, say so and say what
shipping it would cost.

Keep whatever Chapter 7 said about *method* — grid staging, the edge-extension
rule — since Task 13 followed the same discipline. Drop the legacy numbers.

- [ ] **§F — ADEP/ADES on two arms (comment 7)**

Chapter 8's headline comparison becomes `legacy` vs `recommended`. Remove the
other six from the payoff tables.

**§8.2 stays** (user ruling, 2026-08-31). It reads the eight-arm CSVs already in
`data/` and recomputes nothing. Add one sentence saying exactly that: the
scatter is a historical measurement retained because it justifies not optimising
`clean_match_pct` directly, and it is not re-run.

- [ ] **§G — The prose pass (comment 8)**

Against the Global Constraints prose rules. Mechanical first:

```bash
grep -inE "delve|leverage|robust|comprehensive|seamless|crucial|it is worth noting|it's worth noting|it is important to note|that said" papers/track-construction-v1/index.qmd
```

Then the judgement pass, which matters more. Read each section's opening
sentence and ask whether it says anything or merely announces. Read each
em-dash clause and ask whether it restates the clause before it. The paper is
1,665 lines and was written under an instruction to explain every term to a
first-time reader. That instruction stands — but it produced padding, and
padding is what this comment is about. Explaining a term is not the same as
warming up to it.

Report the line count before and after. A real pass should remove a few hundred
lines without losing a single number or definition.

- [ ] **§H — Final parameters in §9 (comment 9)**

`## What shipped` (`sec-defaults`) names the method but not the numbers a reader
needs to reproduce it. Add a table: `gap_minutes`, `low_alt_gap_minutes`,
`low_alt_ft`, `callsign_lookback_minutes` — shipped value, the value the sweep
preferred, and whether they differ. Read from the config and the sweep CSVs as
inline R over `data/`, not typed: the no-typed-numbers rule applies here as
everywhere.

If Task 13's sweep found a better cell that did not ship, say why not, in one
sentence.

- [ ] **§I — Delete Chapter 10 (comment 10)**

Remove `## Provenance` (`sec-provenance`, lines ~1450-1516). The provenance
*system* stays — `_manifest.json` is still written and still stamps every
figure. What goes is the chapter about it. Keep one sentence in "How to read
this page" saying the numbers are stamped and where the manifest lives.

Check the anchor is not cross-referenced before deleting:

```bash
grep -n "sec-provenance" papers/track-construction-v1/index.qmd
```

- [ ] **§J — Render and verify**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/.claude/worktrees/track-construction-v1-plan/papers/track-construction-v1
OPDI_REPO_DIR=/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2 \
OPDI_PAPER_DIR=$PWD \
OPDI_PYTHON=/home/jupyter/work/opdi-workspace/opdi/.claude/worktrees/track-construction-v2/.venv310/bin/python \
  quarto render index.qmd --to html
```

Then again with `--to pdf`. **The PDF is a committed artifact and goes stale
silently** — it has embarrassed this paper once already. Grep the *rendered*
output, not the source, for `V-measure` and for the Chapter 7 heading.

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
