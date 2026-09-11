# Flight Events V4 — Re-run and Report on the Fixed Pipeline

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the three block-time fixes on `opdi` main, regenerate every figure in the `flight-events-v4` paper from the fixed pipeline through its own regeneration entrypoint, and publish a report in which **no number is typed from memory** — every figure traceable to a staged dataset with a provenance entry.

**Architecture:** Nothing new is built. The fixes are already implemented, tested and committed; this plan pushes them, re-cleans the period's tracks so the canonical table carries the corrected cleaning, drives `benchmarks/regenerate_events_v4.py` to rebuild all six staged CSVs plus the bridge JSON, then rewrites the paper's narrative where the measurement now contradicts it and converts the remaining hardcoded prose figures to inline R over those CSVs.

**Tech Stack:** PySpark on the OSN Kubernetes cluster (client-mode driver in this pod), pytest, Quarto + xelatex, `benchmarks/provenance.py` for figure provenance.

**Spec:** This document. Its factual basis is the measurement record in
`opdi-portal/.superpowers/sdd/2026-09-07-aobt-aibt-from-pbf/` and the commits
listed under *Prior work* below. Read those commit messages first: each carries
the measurement that justified it, and the paper must not restate any of them
from this plan rather than from the regenerated data.

## Global Constraints

* **Every figure in the paper comes from a staged dataset.** No number typed
  into prose, no number recalled from a prior session. Where a figure cannot be
  computed from a staged CSV it is either removed or labelled in the text as
  not reproducible from this run, with the reason. This is the user's explicit
  requirement and it is the acceptance criterion for Task 6.
* **The benchmark must never write a published table.** `event_bench.guard_writes()`
  refuses any write whose resolved path lacks `/research/`. Keep it.
* **Ground truth is always the LEFT side of a scoring join.** A milestone the
  pipeline never produced must count against it.
* **Never mutate a published `version` string.** `EventConfig.legacy()` must keep
  reproducing `events_v0.0.2` byte for byte; `tests/test_event_bench_guard.py`
  asserts the ladder's contract and must stay green (24 tests).
* **No Spark UDFs, no `applyInPandas`, no pandas in the Spark path.**
* **One distributed Spark job at a time from this pod.** The driver is
  client-mode inside the JupyterLab pod and only two ports are routed. Running a
  second concurrent job exhausts the `eurocontrol-quota` (30 CPU / 192 GB) and
  the new job gets zero executors — this happened on 2026-09-08 and cost twenty
  minutes. Wait for one job to exit before starting the next, and check
  `kubectl describe quota -n eurocontrol` if executors fail to register.
* **Container memory is capped at 16 GB.** `/sys/fs/cgroup/memory.max`; `free`
  reports the host's 251 GB and is misleading. Watch `anon` in
  `/sys/fs/cgroup/memory.stat`, not the total, which counts reclaimable cache.
* **Long Spark jobs hang in teardown after writing their CSVs** — every rung of
  this campaign has. Watch for `wrote .../ladder_2026.csv` in the log and then
  kill the process; do not wait for it to exit, and do not restart it.

### Prior work this plan depends on

Already implemented, tested (694 passing) and committed on
`feat/layouts-from-pbf`. **Do not reimplement these.**

| commit | what it fixes | measured effect |
|---|---|---|
| `c4ce9ae` | `mask_stale_broadcasts` could not tell a stationary aircraft from a stale broadcast | — |
| `c48ff5f` | that discriminator on by default | EBBR in-stand position survival 39.0% → 49.2% |
| `dae03ad` | `movement_window` read the broadcast `velocity`, absent on the ground; now derives groundspeed from position | AOBT bias **+720 s → +29 s** |
| `413e50e` | step 04 projected away `on_ground`, silently disabling `airport_admit_on_ground` | AOBT coverage **9.61% → 90.11%** |

The last is the root cause of the campaign's central puzzle. `airport_admit_on_ground`
is feature-gated on `"on_ground" in sv_f.columns`; the column was absent from
`process_month`'s projection, so the guard disabled the flag and the gate fell
back to `baro_altitude_c.isNotNull()`. Surface position messages carry no
altitude, so only the ~0.3% of in-stand samples with a barometric reading
survived — and coverage tracked *barometric availability* rather than reception.

**The numbers above are for orientation only. Nothing in the paper may cite
them from here; they must be recomputed by Task 4 and read from the staged
CSVs.** They are recorded so a reviewer can tell whether the re-run reproduces
them, and a large divergence is a finding to investigate rather than to accept.

---

## File Structure

**Modified in `opdi/`:** nothing. The fixes are committed. Task 1 only pushes.

**Modified in `opdi-portal/papers/flight-events-v4/`:**

| File | Change |
|---|---|
| `index.qmd` | Rewrite the off-block/on-block chapter and the cross-cutting findings the measurement now contradicts; convert every remaining hardcoded figure to inline R. |
| `data/*.csv`, `data/_manifest.json` | Regenerated by Task 4 through `provenance.py`. Never hand-copied. |

**New in `opdi-portal/`:** nothing.

---

### Task 1: Land the pipeline fixes

The fixes exist only on an unpushed local branch. Until they are on `main`,
nothing else in this plan is reproducible by anyone else.

**Files:**
- Modify: none (push only)

**Interfaces:**
- Consumes: nothing.
- Produces: `feat/layouts-from-pbf` on `origin`; a PR against `euctrl-pru/opdi` main.

- [ ] **Step 1: Confirm the branch is clean and the suite is green**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
git status --short | grep -v '^??'          # must print nothing
.venv310/bin/python -m pytest tests/ -q --ignore=tests/test_regenerate_track_v2.py
```
Expected: no uncommitted source changes; **694 passed**. `tests/test_regenerate_track_v2.py`
has 2 failures that pre-date all of this work and are unrelated — ignore them,
do not fix them, and say so in the PR body rather than leaving a reviewer to
wonder.

- [ ] **Step 2: Push**

```bash
git push -u origin feat/layouts-from-pbf
```

- [ ] **Step 3: Open the PR**

```bash
gh pr create --repo euctrl-pru/opdi --base main --head feat/layouts-from-pbf --draft \
  --title "Layout grid from a local OSM extract, and three block-time fixes" \
  --body-file -
```

The body must lead with the `on_ground` projection bug, because it is the one a
reviewer most needs to understand: state that `airport_admit_on_ground` had
never executed in production, that the guard `"on_ground" in sv_f.columns`
turns an omission into a silent no-op, and that the existing gate test in
`tests/test_layout.py` passed throughout because it hands the column straight
to the function. Give the measured before/after for coverage and note that the
runway milestones are byte-identical. Do not paste the whole measurement record.

- [ ] **Step 4: Commit nothing else in this task**

There is deliberately no code change here. If the suite is red, stop and report
rather than fixing forward — a green suite is the precondition for every number
this plan goes on to produce.

---

### Task 2: Re-clean the period's tracks into the canonical table

`research/tracks_clean_2026` was cleaned before `c48ff5f`, so it carries the old
masking. `regenerate_events_v4.py` reads that name via
`event_bench.PERIOD_TRACKS["2026"]["clean"]`, so unless the canonical table
carries the fix, the regeneration chain reproduces the old numbers.

A corrected copy already exists at `research/tracks_clean_2026_maskfix`, built
on 2026-09-08 and verified (EBBR in-stand position survival 49.2% against
39.0%). It is kept as the fallback.

**Files:**
- Modify: none.

**Interfaces:**
- Consumes: `research/tracks_2026`.
- Produces: `research/tracks_clean_2026`, cleaned with `stale_position_uses_last_pos_update=True`.

- [ ] **Step 1: Confirm the fallback copy exists before overwriting anything**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python - <<'PY'
import os
from pathlib import Path
for l in Path(".env").read_text().splitlines():
    if l.strip() and not l.startswith("#") and "=" in l:
        k, v = l.split("=", 1); os.environ.setdefault(k.strip(), v.strip())
import pyarrow.fs as pafs
fs = pafs.S3FileSystem(endpoint_override="https://s3.opensky-network.org",
    access_key=os.environ["AWS_ACCESS_KEY_ID"],
    secret_key=os.environ["AWS_SECRET_ACCESS_KEY"], scheme="https")
for t in ["research/tracks_clean_2026", "research/tracks_clean_2026_maskfix"]:
    f = [x for x in fs.get_file_info(pafs.FileSelector("eurocontrol/opdi/" + t,
         recursive=True, allow_not_found=True)) if x.type == pafs.FileType.File]
    print(f"{t}: {len(f)} files, {sum(x.size for x in f)/1073741824:.2f} GB")
PY
```
Expected: both present, ~10.6 GB each. **If the maskfix copy is absent, stop** —
it is the only rollback for this step.

- [ ] **Step 2: Re-clean**

```bash
nohup .venv310/bin/python -u benchmarks/clean_tracks.py \
  --period 2026 --target research/tracks_clean_2026 --executors 10 \
  > /tmp/reclean_canonical.log 2>&1 &
```

Roughly **3-4 hours** — the 2026-09-08 run took 4h 18m. It processes the whole
OPDI bounding box for three days. Poll with `tail` and a stage check; do not sit
on a monitor and do not restart it.

- [ ] **Step 3: Verify the fix took effect before spending cluster time on the ladder**

The unit tests prove the rule on synthetic data. This proves it on the table the
ladder will actually read.

```bash
.venv310/bin/python - <<'PY'
import sys
sys.path.insert(0, "src"); sys.path.insert(0, "benchmarks")
import osn_sample; osn_sample.load_dotenv()
spark = osn_sample.build_spark(6, "8g", distributed=False)
spark.sparkContext.setLogLevel("ERROR")
from pyspark.sql import functions as F
from pyspark.sql.functions import col
lay = spark.read.parquet("s3a://eurocontrol/opdi/hexaero_airport_layouts")
tr = spark.read.parquet("s3a://eurocontrol/opdi/research/tracks_clean_2026") \
       .filter(F.to_date("event_time") == F.lit("2026-06-05"))
cells = lay.filter((col("hexaero_apt_icao") == "EBBR")
                 & (col("hexaero_aeroway") == "parking_position")) \
           .select(col("hexaero_h3_id").alias("_h")).distinct()
j = tr.join(F.broadcast(cells), tr.h3_res_12 == col("_h"), "inner")
r = j.select(F.count("*").alias("n"),
             F.sum(F.when(col("lat").isNotNull(), 1).otherwise(0)).alias("with_pos"),
             F.sum(F.when(col("on_ground"), 1).otherwise(0)).alias("on_ground_true")).collect()[0]
print(f"EBBR in-stand samples {r['n']}, with position {r['with_pos']} "
      f"({100*r['with_pos']/r['n']:.1f}%), on_ground true {r['on_ground_true']}")
spark.stop()
PY
```
**Gate:** position survival must be near **49%**, not 39%, and `on_ground` must be
populated — the ladder needs that column and Task 4 depends on it. If it reads
~39%, the re-clean did not pick up `c48ff5f`; stop and report rather than
running a 16-hour ladder against unfixed tracks.

---

### Task 3: Build the 2026 flight list on the fixed tracks

`regenerate_events_v4.py` declares `flight_list_2026` as a stage whose input is
the cleaned track table, so Task 2 makes it stale by construction. Every
aerodrome-anchored family resolves ADEP/ADES through it.

**Files:**
- Modify: none.

**Interfaces:**
- Consumes: `research/tracks_clean_2026` (Task 2).
- Produces: `research/flight_list_2026`.

- [ ] **Step 1: Run it**

```bash
nohup .venv310/bin/python -u benchmarks/flight_list_2026.py --executors 12 \
  > /tmp/flight_list_2026.log 2>&1 &
```

- [ ] **Step 2: Check the row count against the previous build**

```bash
grep -aE 'rows|wrote' /tmp/flight_list_2026.log | tail -5
```
The 2026-09-07 build held **155,504 rows**, of which 2,181 name EBBR and 3,395
name LSZH. A count within a few percent is expected; a large change means the
re-clean altered track identity and every downstream number needs re-reading in
that light. **Report the number either way** — it is an input to Task 6's
provenance table.

---

### Task 4: Regenerate every figure through the entrypoint

This is the task the whole plan exists for, and the expensive one.

**Files:**
- Modify: `opdi-portal/papers/flight-events-v4/data/*` (written by the entrypoint).

**Interfaces:**
- Consumes: Tasks 2 and 3.
- Produces: `bridge_2026.json`, `ladder_2026.csv`, `inventory_2026.csv`,
  `per_airport_2026.csv`, `per_airport_by_detector_2026.csv`,
  `pooling_rules_2026.csv`, `resolution_2026.csv`, `rings_2026.csv`,
  `runway_2026.csv`, and `_manifest.json` entries for each.

- [ ] **Step 1: See what the entrypoint considers stale**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python benchmarks/regenerate_events_v4.py --check
```
Expected: every job STALE, exit 1. This costs no cluster time and confirms the
dependency fingerprints noticed Tasks 2 and 3.

- [ ] **Step 2: Run the chain**

```bash
nohup .venv310/bin/python -u benchmarks/regenerate_events_v4.py \
  > /tmp/regen_v4.log 2>&1 &
```

**Budget honestly: this is a long run.** `jobs()` defines `ground_truth_2026`,
then `ladder_2026`, then `compare_2026`. The ladder job passes no `--runs`
filter, so it runs **all eight rungs** V00–V07, and a single rung took ~2 hours
on 2026-09-08. Expect **12–18 hours** end to end.

**Do not shorten it by running only `V07_shipped`.** The paper's ladder table
attributes each behaviour to its rung; with only the top rung staged, every
intermediate row renders as an em dash and the attribution chapter says nothing.
If the full run is not affordable, that is a decision for the user, not a
silent narrowing — see *Open items*.

- [ ] **Step 3: Watch it without disturbing it**

Poll every 15 minutes or so with `tail -c 400 /tmp/regen_v4.log | tr '\r' '\n' | tail -2`
and `cat /sys/fs/cgroup/memory.stat | grep '^anon '`. Liveness is **log
freshness**, not driver CPU or driver I/O — executors run remotely on
Kubernetes, so the driver's counters stay flat while the job is healthy. A log
silent for more than ten minutes is the real stall signal.

- [ ] **Step 4: Confirm every output has a provenance entry**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/papers/flight-events-v4
.venv310/bin/python - <<'PY'
import json, glob, os
m = json.load(open("data/_manifest.json"))
names = set(m) if isinstance(m, dict) else {e.get("output") for e in m}
for f in sorted(glob.glob("data/*.csv") + glob.glob("data/*.json")):
    b = os.path.basename(f)
    if b == "_manifest.json":
        continue
    print(f"  {b:36s} {'in manifest' if b in names else 'NO MANIFEST ENTRY'}")
PY
```
An output with no manifest entry must be reported in the paper as unverified
rather than shown as fact. That rule exists because three staged CSVs were once
found to derive from tables written days earlier by different parameters.

- [ ] **Step 5: Sanity-check the headline before building a paper on it**

```bash
.venv310/bin/python -c "
import pandas as pd
d = pd.read_csv('data/ladder_2026.csv')
b = d[((d.det_type=='off-block')&(d.milestone=='AOBT'))|((d.det_type=='on-block')&(d.milestone=='AIBT'))]
print(b[['rung','milestone','coverage_pct','n_detected','bias_s','within_60s_pct']].to_string(index=False))
r = d[((d.det_type=='ATOT')&(d.milestone=='ATOT'))|((d.det_type=='ALDT')&(d.milestone=='ALDT'))]
print(r[['rung','milestone','coverage_pct','bias_s']].drop_duplicates().to_string(index=False))
"
```
**What working looks like** — falsifiable, and if these do not hold the paper
reports what it found rather than what was expected:
* AOBT and AIBT coverage on `V07_shipped` are **near 90% and 88%**, not near 9%.
* `ATOT` is **89.77% at +25 s** and `ALDT` **95.92% at +5 s**, unchanged — the
  fixes touch no other family, and a change here is a regression to investigate.
* The `V06 → V07` step is where the block family moves, since that is the rung
  the shipped configuration switches on.

---

### Task 5: Rewrite what the measurement contradicts

The chapter currently attributes the block-time shortfall to reception. That is
now measurably wrong, and leaving it would be the paper's most serious error.

**Files:**
- Modify: `opdi-portal/papers/flight-events-v4/index.qmd`

**Interfaces:**
- Consumes: the CSVs from Task 4.
- Produces: prose consistent with them.

- [ ] **Step 1: Replace the reception claim with the measured attribution**

The chapter must say, computing every figure inline from the staged CSVs:

* Coverage was limited by a **defect, not by reception**. `airport_admit_on_ground`
  was feature-gated on a column step 04 projected away, so the gate fell back to
  requiring a barometric altitude that surface messages do not carry.
* The evidence that it was never reception: all twenty study aerodromes sit at
  **97.5–100% ground detection** in `opensky-airport-coverage/data/ranking_tier_a_2026.csv`,
  measured as visible reports between AOBT and ATOT — the same taxi phases being
  scored. Cite that file; it is outside the staged data, so state it as an
  external reference with its path rather than as a figure from this run.
* What each fix was worth, read from the ladder rather than asserted.

- [ ] **Step 2: Correct the Annex A commentary**

Any sentence claiming an aerodrome is reception-limited must be re-derived from
`per_airport_2026.csv`. Aerodromes previously at 0.00% are the sharpest case: if
they now report coverage, the sentence explaining their zero is not merely stale
but was wrong about the cause.

- [ ] **Step 3: State the residual honestly**

Coverage is fixed; timing is not uniformly good. Report the per-aerodrome bias
spread from the staged CSV and name the aerodromes at the bad end. Do not
present a coverage win as though it settled accuracy.

- [ ] **Step 4: Update the breaking-changes chapter**

Consumers of `events_v0.2.0` will see airport-event volume rise sharply — the
2026-09-09 isolated measurement put it near 23× for one day, driven by
`entry-`/`exit-taxiway` and `parking_position`. Recompute the real figure from
`inventory_2026.csv` and say what it means for table size and for anyone
counting events.

---

### Task 6: Purge every remaining number that does not come from a dataset

The user's explicit requirement, and the acceptance criterion for the whole plan.

**Files:**
- Modify: `opdi-portal/papers/flight-events-v4/index.qmd`

- [ ] **Step 1: Enumerate what is still hardcoded**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/papers/flight-events-v4
python3 - <<'PY'
import re
lines = open("index.qmd").read().split("\n")
inchunk = False
for i, l in enumerate(lines, 1):
    if l.startswith("```{"): inchunk = True; continue
    if l.strip() == "```":   inchunk = False; continue
    if inchunk: continue
    hits = re.findall(r"\d{1,3}\.\d{1,2}\s?%|\b\d{2,3},\d{3}\b", l)
    if hits:
        print(f"L{i:>5}: {','.join(hits)}")
        print(f"        {l.strip()[:110]}")
PY
```

An earlier pass converted 11 claims and left ~21. Expect roughly that many, plus
whatever Task 5's rewrite introduced.

- [ ] **Step 2: Convert every measurement claim to inline R**

Use the accessors the tables already use (`lad`, `pera`, `perd`, `prules`, `res`,
`rwy`, `br`) so a sentence and the table beneath it read the same value. Where
the same figure appears in two chapters, compute it once and reference it —
duplicated literals are how the paper came to state "four of the twenty" when
the data said three.

- [ ] **Step 3: Deal with figures that cannot be computed**

Three kinds, and they are not the same:

1. **Arithmetic in a worked example** (91.7% of 30,000 in the exclusion-box
   illustration) — correct by construction, leave it, and it is not a
   measurement.
2. **Measurements from another study or extract** (the 2.83% APDF null rate on
   the June 2025 extract; V3's 0.39% for `take-off`) — cannot come from this
   run's CSVs. Keep, and label in the sentence itself as measured elsewhere,
   naming the source.
3. **Before-values from a previous build of this same paper** — these are the
   dangerous ones. The CSVs they came from have been overwritten, so they are
   unverifiable. **Remove them or stage them.** If a before/after comparison is
   worth keeping, stage the previous run's CSV under `data/` with its own
   manifest entry so the comparison is reproducible; otherwise cut the claim.
   Do not leave an unverifiable number in the text with a note.

- [ ] **Step 4: Prove the property rather than asserting it**

Add a check the render itself runs, so the requirement cannot silently rot:

```r
#| label: no-orphan-figures
#| include: false
# Every percentage in the prose must be produced by inline R over a staged CSV.
# This chunk fails the render if a bare literal percentage appears outside a
# code chunk -- the paper's own guarantee that its numbers came from data.
src <- readLines("index.qmd")
inchunk <- FALSE; offenders <- character()
for (i in seq_along(src)) {
  l <- src[i]
  if (grepl("^```\\{", l)) { inchunk <- TRUE; next }
  if (identical(trimws(l), "```")) { inchunk <- FALSE; next }
  if (inchunk) next
  if (grepl("`r ", l, fixed = TRUE)) next          # computed: fine
  if (grepl("<!-- literal-ok", l, fixed = TRUE)) next  # explicitly excepted
  if (grepl("[0-9]{1,3}\\.[0-9]{1,2} ?%", l)) offenders <- c(offenders, sprintf("L%d: %s", i, trimws(l)))
}
if (length(offenders)) stop("hardcoded figures in prose:\n", paste(offenders, collapse = "\n"))
```

Every legitimately-static figure from Step 3 gets an explicit
`<!-- literal-ok: measured on the June 2025 extract, see §3 -->` marker on its
line. The marker is the point: it converts "someone typed a number" into "someone
decided this number is static, and said why".

---

### Task 7: Render, audit, and publish

**Files:**
- Modify: `opdi-portal/papers/flight-events-v4/flight-events-v4.pdf`

- [ ] **Step 1: Render against the real chain**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/papers
OPDI_RENDER=check quarto render flight-events-v4/index.qmd --to pdf
```
`check` fails fast if any staged figure is stale against the checked-out code.
Use it rather than `allow-stale`: the whole point of this plan is that the
figures match the pipeline.

- [ ] **Step 2: Audit the rendered PDF**

Read it. Check: every table populated; no stray `NA` or em dash where data
exists; no sentence contradicting the table beside it; the ladder showing all
eight rungs rather than dashes for six of them.

- [ ] **Step 3: Commit and update the PR**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal
git add papers/flight-events-v4
git commit -m "papers: AOBT/AIBT re-measured on the fixed pipeline"
git push
```
PR #14 (`papers/flight-events-v4-blocks`) is open and is the right place — its
current numbers are what this supersedes. Update its description to say so.

- [ ] **Step 4: Record the correction in the run log**

Append to `opdi/benchmarks/EVENTS_RUN_LOG.md`: the `on_ground` projection
defect, that `airport_admit_on_ground` never executed in production between its
merge and `413e50e`, and that any block-time figure published from a run in that
window understates coverage by roughly an order of magnitude.

---

## Verification

**Without the cluster:**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python -m pytest tests/ -q --ignore=tests/test_regenerate_track_v2.py
.venv310/bin/python benchmarks/regenerate_events_v4.py --check
```
Expected: 694 passed; `--check` exits 0 once Task 4 has run.

**The contract that must hold:**

```bash
.venv310/bin/python -c "
from opdi.config import EventConfig, OPDIConfig
from opdi.pipeline.events import TRACK_COLUMNS
assert 'on_ground' in TRACK_COLUMNS
assert EventConfig.legacy().events_version == 'events_v0.0.2'
assert OPDIConfig.for_environment('opensky').cleaning.stale_position_uses_last_pos_update
print('contract ok')
"
```

**What "done" looks like:**

* The paper renders under `OPDI_RENDER=check` — every figure current against the
  checked-out pipeline.
* The `no-orphan-figures` chunk passes, so every prose percentage is either
  computed or explicitly marked static with a reason.
* AOBT and AIBT coverage read near 90% and 88% from `ladder_2026.csv`, and
  `ATOT`/`ALDT` are unchanged at 89.77%/95.92%.
* Every aerodrome in `per_airport_2026.csv` reports non-zero block coverage —
  ENVA and EGNT were at exactly 0.00% and are the test of whether the fix
  reached the whole network.

---

## Open items, stated rather than assumed

* **The full ladder is 12–18 hours.** Eight rungs at roughly two hours each.
  That is the honest cost of an attribution table that is not mostly em dashes.
  If it is not affordable, the alternative is to run `V00_v3_shipped` and
  `V07_shipped` only (~4 hours) and state in the ladder chapter that the
  intermediate rungs were not re-run and their deltas are carried from the
  previous build — which makes them unverifiable and, by this plan's own
  standard, candidates for removal. **This is a decision for the user.**
* **Task 2 overwrites `research/tracks_clean_2026`.** Deliberate: the
  regeneration entrypoint reads that name, and a paper that regenerates from a
  differently-named table is not reproducible by anyone else. The pre-fix
  contents are superseded, and `research/tracks_clean_2026_maskfix` is the
  rollback.
* **PR #14's published numbers become wrong the moment this lands.** Its paper
  attributes the shortfall to reception. Update its description rather than
  leaving a merged PR asserting a cause the data contradicts.
* **Timing is not fixed by any of this.** Per-aerodrome bias ranged widely on
  2026-09-09 — some aerodromes in the tens of seconds, others in the hundreds.
  Whether that is a second detector defect or a genuine reception effect is
  **not established**, and the paper should say so rather than implying the
  block family is now sound.
* **The airport-event volume increase is unmeasured at scale.** An isolated
  one-day test suggested ~23×. If that holds for the published tables it is an
  operational consideration for anyone storing or querying them, and
  `inventory_2026.csv` is where the real figure comes from.
