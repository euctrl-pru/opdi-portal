# AOBT/AIBT from a local OSM extract — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `off-block` (AOBT) and `on-block` (AIBT) answer movements at every
large and medium aerodrome in the OPDI bounding box, by building the
`hexaero_airport_layouts` H3 grid from a local `europe-latest.osm.pbf` extract
with pyosmium instead of querying the public Overpass and Nominatim services.

**Architecture:** One function is replaced, not the pipeline.
`retrieve_osm_data(icao) -> GeoDataFrame` is the only place step 00b talks to
the network; everything downstream of it — `fill_missing_width`,
`convert_to_polygon`, `polygon_to_h3`, `HEXAERO_SCHEMA` — is pure and stays
exactly as it is. A new `pbf_source.py` reads the extract **once**, keeps only
`aeroway` features, assigns each to the aerodrome whose own OSM boundary
(`aeroway=aerodrome`, matched on its `icao` tag) contains it — falling back to
a runway-extent bounding box only for aerodromes OSM maps as a bare node — and
serves per-airport GeoDataFrames from memory. `hexagonify_airport` gains a `source`
parameter so the Overpass path remains available and the two can be compared.

**Tech Stack:** pyosmium (PBF reading, geometry building), shapely, geopandas,
pandas, h3 v4, PySpark (the existing write path only), pytest.

**Spec:** `opdi/docs/industrialization-plan.md` (on `opdi` main) — §1 for which
milestone needs what, §3 for the volume, runtime and traps this plan must
respect, §3.4 for why the place-based query is being replaced. Read it first;
this plan implements its §3 and §5.

## Global Constraints

* **Everything Spark-side stays native Spark.** This plan adds no UDF and no
  `applyInPandas`. The PBF work is **driver-side reference generation**, like
  the rest of step 00; it produces a pandas frame that the existing
  `process_all` writes.
* **`retrieve_osm_data`'s contract is the seam and must not change.** It returns
  a GeoDataFrame whose `reset_index()` yields columns `element`, `id`,
  `geometry`, `aeroway`, and optionally `width`, `ref`, `surface`, `length`.
  `hexagonify_airport` renames `id -> osm_id` and `element -> type`. A
  replacement that omits `element` or `id` silently produces a table with null
  `hexaero_osm_id`, which nothing downstream checks.
* **Output schema is fixed:** `HEXAERO_SCHEMA` in
  `src/opdi/reference/h3_airport_layouts.py:58` — twelve `hexaero_*` columns.
  `COLUMN_TYPES` at `:73` casts them. Do not add or reorder columns.
* **`process_airport` must not write.** It builds and returns; `process_all`
  writes once. This was fixed in `euctrl-pru/opdi#6` after a per-airport
  `mode="overwrite"` took the published table from twenty airports to one. Any
  new code that writes per airport reintroduces that incident.
* **Never write `hexaero_airport_layouts` from an ad-hoc script.** Go through
  `process_all`, or write to a `research/` name. The published table is
  currently **restored to 15 airports** and is still missing 16 of its
  original 20.
* **New runtime dependencies go in `docker/Dockerfile`** or executors fail at
  import. pyosmium is driver-side only, but the Dockerfile is still the place
  it is declared.
* **The venv has no `pip`.** Install with
  `VIRTUAL_ENV=/home/jupyter/work/opdi-workspace/opdi/.venv310 uv pip install <pkg>`.
* **Do not query the public Overpass API during this work.** It has already
  refused this host; that refusal is why this plan exists.

### Fixed decisions

| Decision | Value |
|---|---|
| Extract | `https://download.geofabrik.de/europe-latest.osm.pbf` (~30 GB; 385 GB free on `/`) |
| Extract location | `/home/jupyter/work/osm/europe-latest.osm.pbf` |
| Test extract | `https://download.geofabrik.de/europe/luxembourg-latest.osm.pbf` (~40 MB) |
| Aeroway tags | `AEROWAY_TAGS` at `h3_airport_layouts.py:48` — taxiway, runway, apron, hangar, threshold, parking_position, deicing_pad |
| H3 resolution | 12 (`config.h3.airport_layout_resolution`) |
| Airport scope | large + medium in the OPDI bbox — 1,353 aerodromes |
| Branch | `feat/layouts-from-pbf` off `opdi` main |

---

## File Structure

**New in `opdi/src/opdi/reference/`:**

| File | Responsibility |
|---|---|
| `pbf_source.py` | Read a `.osm.pbf` once, keep `aeroway` features, build shapely geometries, assign each feature to an aerodrome (by containment in OSM's own aerodrome polygon; bounding box only where there is none), and serve per-airport GeoDataFrames. The only new IO-heavy unit. |

**Modified in `opdi/src/opdi/reference/`:**

| File | Change |
|---|---|
| `h3_airport_layouts.py` | `hexagonify_airport(..., source=None)`; `AirportLayoutGenerator(..., pbf_path=None)` builds a `PbfLayoutSource` once and passes it down. `retrieve_osm_data` untouched, still the Overpass path. |

**New tests:** `opdi/tests/test_pbf_source.py`.

**Modified:** `opdi/docker/Dockerfile` (declare pyosmium).

`pbf_source.py` is its own module rather than more functions in
`h3_airport_layouts.py` because it is the only part that touches a 30 GB file
and the only part with a cache; the rest of that module is pure geometry and is
tested as such.

---

### Task 1: Environment, extract, and a fast test fixture

The 30 GB download is slow and everything else is blocked on knowing pyosmium
works here. This task ends with a **small** extract that every later test uses,
and the big one downloading in the background.

**Files:**
- Modify: `opdi/docker/Dockerfile`
- Create: nothing yet

**Interfaces:**
- Consumes: nothing.
- Produces: `pyosmium` importable; `/home/jupyter/work/osm/luxembourg-latest.osm.pbf`;
  `/home/jupyter/work/osm/europe-latest.osm.pbf` (may still be downloading).

- [ ] **Step 1: Install pyosmium**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
VIRTUAL_ENV=$PWD/.venv310 uv pip install osmium
.venv310/bin/python -c "import osmium; print(osmium.version.pyosmium_release)"
```
Expected: a version string, e.g. `4.0.2`. If `uv` is missing, it is at
`/usr/local/bin/uv`.

- [ ] **Step 2: Record the dependency**

Add to `opdi/docker/Dockerfile`, beside the other pip installs:

```dockerfile
# Driver-side only: step 00b reads a local .osm.pbf extract instead of the
# public Overpass API, which rate-limits hard enough to make a 1,353-airport
# build impossible. Executors never import this.
RUN pip install --no-cache-dir osmium
```

- [ ] **Step 3: Fetch the small extract used by every test below**

```bash
mkdir -p /home/jupyter/work/osm && cd /home/jupyter/work/osm
curl -L --retry 5 -o luxembourg-latest.osm.pbf \
  https://download.geofabrik.de/europe/luxembourg-latest.osm.pbf
ls -la luxembourg-latest.osm.pbf
```
Expected: ~40 MB. Luxembourg is used because it contains **ELLX**, a real
aerodrome with stands, taxiways and a runway, and it parses in seconds.

- [ ] **Step 4: Start the Europe extract downloading in the background**

```bash
cd /home/jupyter/work/osm
nohup curl -sL --retry 5 --retry-delay 10 -C - \
  -o europe-latest.osm.pbf https://download.geofabrik.de/europe-latest.osm.pbf \
  > /tmp/geofabrik.log 2>&1 &
```
`-C -` resumes, so an interrupted download is not restarted from zero. Check
progress with `ls -la /home/jupyter/work/osm/`. **Do not block on this**; Tasks
2-4 use Luxembourg.

- [ ] **Step 5: Verify pyosmium reads the small extract**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python -c "
import osmium
n = 0
for obj in osmium.FileProcessor('/home/jupyter/work/osm/luxembourg-latest.osm.pbf').with_filter(osmium.filter.KeyFilter('aeroway')):
    n += 1
print('aeroway objects:', n)
"
```
Expected: a few thousand. **If `osmium.FileProcessor` does not exist**, the
installed pyosmium is 3.x, whose API is the handler class
(`osmium.SimpleHandler` with `way`/`area` methods). Record which API is present
in the commit message — Task 2 is written against `FileProcessor` (4.x) and
must be adapted if 3.x is installed.

- [ ] **Step 6: Commit**

```bash
git add docker/Dockerfile
git commit -m "build: declare pyosmium for the local OSM extract path"
```

---

### Task 2: Read aeroway features out of a PBF

The core of the replacement. Reads the extract once, keeps only `aeroway`
features, and builds shapely geometries — the same shapes
`ox.features.features_from_place` returned.

**Files:**
- Create: `opdi/src/opdi/reference/pbf_source.py`
- Test: `opdi/tests/test_pbf_source.py`

**Interfaces:**
- Consumes: `AEROWAY_TAGS` from `h3_airport_layouts`.
- Produces:
  - `read_aeroway_features(pbf_path: str) -> gpd.GeoDataFrame` with columns
    `element` (`"way"`/`"area"`/`"node"`), `id` (int), `geometry` (shapely),
    `aeroway` (str), `width`, `ref`, `surface`, `length` (str or None).

- [ ] **Step 1: Write the failing test**

Create `opdi/tests/test_pbf_source.py`:

```python
"""Reading aeroway geometry out of a local OSM extract.

Every test uses the Luxembourg extract, which is ~40 MB and contains ELLX --
a real aerodrome with a runway, taxiways and stands. Using a real extract
rather than a synthetic one is deliberate: the failure modes this code has are
about how OSM actually encodes aprons and stands (closed ways that are areas,
multipolygon relations, missing width tags), and a hand-built fixture would
encode this author's assumptions about that rather than the reality.
"""
import os
import pytest

pytest.importorskip("osmium")

from opdi.reference.pbf_source import read_aeroway_features

PBF = "/home/jupyter/work/osm/luxembourg-latest.osm.pbf"

pytestmark = pytest.mark.skipif(
    not os.path.exists(PBF), reason="Luxembourg extract not downloaded (Task 1)"
)


def test_reads_the_aeroway_families_the_layout_grid_needs():
    gdf = read_aeroway_features(PBF)
    kinds = set(gdf["aeroway"])
    assert "runway" in kinds
    assert "taxiway" in kinds
    assert "parking_position" in kinds, (
        "stands are the whole point: AOBT/AIBT are anchored on parking_position"
    )


def test_every_feature_carries_the_identity_the_schema_needs():
    """`hexagonify_airport` renames `id`->`hexaero_osm_id` and
    `element`->`hexaero_type`. A source that omits either yields a table with
    nulls there and nothing downstream checks it."""
    gdf = read_aeroway_features(PBF)
    for col in ("element", "id", "geometry", "aeroway"):
        assert col in gdf.columns, f"missing {col}"
    assert gdf["id"].notna().all()
    assert gdf["element"].isin(["node", "way", "area", "relation"]).all()


def test_geometries_are_usable_shapes():
    """`convert_to_polygon` switches on `geom_type` and buffers LineStrings by
    width. Anything it cannot name is silently dropped later."""
    gdf = read_aeroway_features(PBF)
    types = set(gdf.geometry.geom_type)
    assert types <= {"LineString", "Polygon", "MultiPolygon", "Point"}, types
    assert gdf.geometry.is_valid.all() or gdf.geometry.isna().sum() == 0
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /home/jupyter/work/opdi-workspace/opdi && .venv310/bin/python -m pytest tests/test_pbf_source.py -v`
Expected: FAIL with `ModuleNotFoundError: No module named 'opdi.reference.pbf_source'`

- [ ] **Step 3: Implement the reader**

Create `opdi/src/opdi/reference/pbf_source.py`:

```python
"""Aeroway geometry from a local ``.osm.pbf`` extract.

Step 00b used to reach the public Overpass API once per airport, through
``osmnx.features_from_place``, which first resolves the airport *name* through
Nominatim and then sends Overpass a polygon predicate. That is two rate-limited
services per aerodrome, and it fails silently: a name that does not resolve
returns no data rather than an error, which is why five of twenty airports came
back empty during the flight-events-v4 campaign -- one of them an airport that
demonstrably has stands. At 1,353 aerodromes the public endpoint simply refuses.

Reading a Geofabrik extract removes both services. The file is read once, all
``aeroway`` features are kept, and each is assigned to an aerodrome from
reference data we already hold. See ``docs/industrialization-plan.md`` §3.
"""
from typing import List, Optional

import geopandas as gpd
import osmium
import pandas as pd
from shapely import wkb as shapely_wkb

from opdi.reference.h3_airport_layouts import AEROWAY_TAGS

#: Tags copied off each feature. `width`, `ref`, `surface` and `length` are what
#: `hexagonify_airport` reads; anything else would be dropped by the schema.
_TAGS = ("aeroway", "width", "ref", "surface", "length")

_WKB = osmium.geom.WKBFactory()


def _tags_of(obj) -> dict:
    return {k: obj.tags.get(k) for k in _TAGS}


def read_aeroway_features(pbf_path: str) -> gpd.GeoDataFrame:
    """Every ``aeroway`` feature in *pbf_path*, as shapely geometry.

    Returns the same shape ``retrieve_osm_data`` returns after
    ``reset_index()``: ``element``, ``id``, ``geometry`` plus the tag columns.
    Closed ways tagged as areas (aprons, stands) come back as polygons and open
    ways (taxiway and runway centrelines) as linestrings, which is exactly what
    ``convert_to_polygon`` expects to switch on.
    """
    rows: List[dict] = []

    # `with_areas()` makes osmium assemble multipolygon relations and closed
    # ways into areas, which is how OSM encodes an apron. Without it those
    # features arrive as bare ways and an apron becomes a thin buffered line.
    fp = (
        osmium.FileProcessor(pbf_path)
        .with_areas()
        .with_filter(osmium.filter.KeyFilter("aeroway"))
    )
    for obj in fp:
        if obj.tags.get("aeroway") not in AEROWAY_TAGS:
            continue
        try:
            if isinstance(obj, osmium.osm.Area):
                geom = shapely_wkb.loads(_WKB.create_multipolygon(obj), hex=True)
                element, oid = "area", obj.orig_id()
            elif isinstance(obj, osmium.osm.Way):
                geom = shapely_wkb.loads(_WKB.create_linestring(obj), hex=True)
                element, oid = "way", obj.id
            elif isinstance(obj, osmium.osm.Node):
                geom = shapely_wkb.loads(_WKB.create_point(obj), hex=True)
                element, oid = "node", obj.id
            else:
                continue
        except Exception:
            # A feature whose nodes are outside the extract cannot be built.
            # Skipping it is right: it is geometry we do not have, and the
            # alternative is a partial shape that would rasterise to cells in
            # the wrong place.
            continue
        if geom is None or geom.is_empty:
            continue
        rows.append({"element": element, "id": int(oid), "geometry": geom,
                     **_tags_of(obj)})

    if not rows:
        raise ValueError(f"no aeroway features found in {pbf_path}")
    return gpd.GeoDataFrame(pd.DataFrame(rows), geometry="geometry", crs="EPSG:4326")
```

- [ ] **Step 4: Run to verify it passes**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py -v`
Expected: PASS, three tests.

If `create_multipolygon` raises on some areas, the `except Exception: continue`
swallows them — check the kept count is plausible (Luxembourg should yield
hundreds of aeroway features) rather than trusting the tests alone.

- [ ] **Step 5: Commit**

```bash
git add src/opdi/reference/pbf_source.py tests/test_pbf_source.py
git commit -m "feat(reference): read aeroway geometry from a local OSM extract"
```

---

### Task 3: Assign each feature to an aerodrome

**Revised 2026-09-07.** The original version of this task derived a bounding box
per aerodrome from its runway extent and resolved overlaps with a
nearest-aerodrome guard. That is replaced. **OSM tags aerodromes itself** --
`aeroway=aerodrome` carrying an `icao=` tag -- so the airport's own boundary
polygon is *in the extract*, keyed by exactly the identifier the layout table
is indexed on. That is the same polygon Overpass was fetching, except that
Overpass had to geocode the airport's *name* through Nominatim to find it,
which is the step that failed silently and returned nothing for five of twenty
airports during the flight-events-v4 campaign.

Assignment therefore becomes **containment in the aerodrome's own polygon**,
which deletes three judgement calls: the runway-extent box, the `1.5 km`
margin, and the nearest-aerodrome tie-break that existed only because
rectangles overlap where real boundaries do not.

**Measured on the 34.9 GB Europe extract before this task was written:**

| | count |
|---|---|
| `aeroway=aerodrome` features carrying an `icao` tag | 3,281 (2,485 ways, 310 relations, **486 nodes**) |
| distinct ICAO codes among them | **3,254** |
| of the 20 study aerodromes, present | **20/20** |
| of the original 20 the published table held, present | **20/20** |

3,254 comfortably exceeds the 1,353 aerodromes in scope, and every airport this
campaign cares about is present. But **486 of those features are nodes**, and a
node has no area, so it can contain nothing.

**So the bbox is retained, as the fallback and only as the fallback.** An
aerodrome whose OSM feature is a bare node, or which has no `icao`-tagged
feature at all, falls back to the runway-extent box with the nearest-aerodrome
guard -- the code the first attempt at this task already wrote, preserved at
`.superpowers/sdd/2026-09-07-aobt-aibt-from-pbf/task-3-box-fallback.patch`.
Reuse it rather than rewriting it; it was reviewed as far as it went and its
overlap test is mutation-verified.

**Why the fallback must stay narrow.** The box is the weaker method: it is a
rectangle around runway thresholds, so it admits whatever else sits in that
rectangle. Applying it only where there is no polygon confines that weakness to
the aerodromes that have no better option, instead of imposing it everywhere.

**This is also a large performance win, which is a consequence rather than the
reason.** The box path was measured at 0.53 s per `features_for` call against a
1,353-aerodrome box set -- about 12 minutes of pure filtering for a full build,
because each call compares against every other aerodrome. A spatial join
assigns every feature in one pass.

**Files:**
- Modify: `opdi/src/opdi/reference/pbf_source.py`
- Test: `opdi/tests/test_pbf_source.py`

**Interfaces:**
- Consumes: `read_aeroway_features` (Task 2); `oa_airports` and `oa_runways`
  via `StorageManager`.
- Produces:
  - `read_aerodromes(pbf_path) -> gpd.GeoDataFrame` with columns `icao`,
    `name`, `element`, `id`, `geometry` -- one row per `aeroway=aerodrome`
    feature carrying a non-empty `icao` tag whose geometry could be assembled
    as a polygon. Nodes are **excluded** here and handled by the fallback.
  - `airport_boxes(storage, airport_types=None) -> pd.DataFrame` with columns
    `ident`, `lat_min`, `lat_max`, `lon_min`, `lon_max`, `apt_lat`, `apt_lon`
    -- unchanged from the preserved patch.
  - `class PbfLayoutSource` with
    `__init__(self, pbf_path: str, storage, airport_types=None)`,
    `features_for(self, apt_icao: str) -> gpd.GeoDataFrame`, and an
    `assignment_report(self) -> pd.DataFrame` recording, per aerodrome in
    scope, which method assigned it (`polygon` / `bbox` / `none`) and how many
    features it received.

- [ ] **Step 1: Write the failing tests**

Append to `opdi/tests/test_pbf_source.py`. Keep the existing convention: a test
that reads the Luxembourg extract carries `@needs_luxembourg`; a test that
builds its own synthetic `.osm` fixture in `tmp_path` carries no decorator.

```python
@needs_luxembourg
def test_aerodrome_polygons_are_read_with_their_icao_code():
    """ELLX is mapped as a closed way tagged `aeroway=aerodrome`, `icao=ELLX`.

    This is the polygon Overpass used to fetch by geocoding the airport's name.
    Reading it from the extract makes the key exact rather than a name lookup,
    which is the failure that returned nothing for five of twenty airports.
    """
    ad = read_aerodromes(PBF)
    assert "ELLX" in set(ad["icao"])
    row = ad[ad["icao"] == "ELLX"].iloc[0]
    assert row.geometry.geom_type in ("Polygon", "MultiPolygon")
    assert row.geometry.area > 0


@needs_luxembourg
def test_features_are_assigned_by_containment_in_the_aerodrome_polygon(spark):
    """The whole point of the change: a feature belongs to the airport whose
    boundary encloses it, not to the airport whose rectangle it happens to
    fall in."""
    src = PbfLayoutSource(PBF, _Storage(spark))
    ellx = src.features_for("ELLX")
    assert len(ellx) > 0
    assert set(ellx["aeroway"]) <= AEROWAY_TAGS_SET
    # Every returned feature really is inside the polygon it was assigned to.
    poly = read_aerodromes(PBF).set_index("icao").loc["ELLX"].geometry
    assert ellx.geometry.representative_point().within(poly).all()


@needs_luxembourg
def test_an_aerodrome_absent_from_the_extract_returns_nothing(spark):
    src = PbfLayoutSource(PBF, _Storage(spark))
    assert len(src.features_for("EBBR")) == 0


def test_an_aerodrome_with_only_a_node_falls_back_to_its_bbox(tmp_path, spark):
    """486 of the extract's icao-tagged aerodrome features are bare nodes, and
    a node encloses nothing. Those aerodromes must still get a grid, by the
    runway-extent box -- otherwise switching to polygons would silently drop
    every airport OSM has not drawn a boundary for.

    The fixture is synthetic so the two paths can be exercised side by side:
    one aerodrome mapped as a polygon, one as a bare node, each with a stand.
    """
    ...


def test_the_polygon_path_wins_where_both_are_available(tmp_path, spark):
    """An aerodrome with a polygon must NOT also pick up features its box would
    have caught but its boundary excludes. Otherwise the fallback quietly
    re-imposes the weakness the polygon was adopted to remove."""
    ...


def test_the_source_reads_the_extract_once(spark):
    """1,353 airports must not mean 1,353 passes over a 35 GB file."""
    src = PbfLayoutSource(PBF, _Storage(spark))
    src.features_for("ELLX")
    before = src._read_count
    src.features_for("ELLX")
    src.features_for("EBBR")
    assert src._read_count == before, "the extract was re-read"
```

Write the two `...` bodies out in full -- they are the tests that pin the
fallback boundary, and they are the reason this task can be reviewed at all.
Build the fixtures as `.osm` XML in `tmp_path`, the same way Task 2's synthetic
fixtures are built.

- [ ] **Step 2: Run to verify they fail**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py -v`
Expected: FAIL with `ImportError: cannot import name 'read_aerodromes'`

- [ ] **Step 3: Implement `read_aerodromes`**

Same shape as `read_aeroway_features`, but selecting `aeroway=aerodrome` and
requiring a non-empty `icao` tag. It needs `.with_areas()`, because an
aerodrome boundary is a closed way or a multipolygon relation. Normalise the
code with `.strip().upper()` -- OSM values are free text.

Two traps carried over from Task 2, both already solved there and both applying
again here: build geometry inside a `try` and **count** what fails per reason
rather than swallowing it silently; and remember that `with_areas()` also emits
the original closed `Way`, so take the assembled `Area` and ignore the way.

- [ ] **Step 4: Implement assignment, computed once**

In `PbfLayoutSource._load`, after reading features and aerodromes, resolve
every feature's aerodrome **in one pass** and cache it:

1. Spatial-join the features' representative points against the aerodrome
   polygons (`geopandas.sjoin`, predicate `within`). A representative point is
   used rather than the whole geometry because a taxiway may cross the
   boundary, and a feature is either this airport's or it is not.
2. A feature landing in more than one polygon (airports do overlap in OSM, and
   a few boundaries are drawn twice) is assigned deterministically to the
   **smallest-area** polygon containing it, ties broken by `icao` ascending --
   the smaller boundary is the more specific one.
3. Aerodromes in scope with **no polygon** get the box path from the preserved
   patch, applied only to features that step 1 left unassigned. Do not let the
   box claim a feature that a polygon already owns.
4. Record the outcome per aerodrome for `assignment_report()`.

`features_for` then becomes a lookup on the cached assignment. Drop the helper
columns (`_lat`, `_lon`, join artefacts, the index columns `sjoin` adds) on the
way out, on every path including the empty ones -- the seam contract is
`element`, `id`, `geometry`, `aeroway`, `width`, `ref`, `surface`, `length`.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py -v`

- [ ] **Step 6: Report the assignment split**

Not a commit gate, but it is what Task 6 needs to know before a full build.
Using Luxembourg, report: how many aerodromes were assigned by polygon, by
box, and not at all; and what fraction of the extract's aeroway features ended
up assigned to *some* aerodrome, broken down by the seven families. **A large
unassigned fraction is a finding, not a nuisance** -- it would mean OSM
boundaries are tighter than the features that belong to the airport, and Task 5
would then be comparing against an Overpass baseline built on the same
boundaries, so it would not catch it.

- [ ] **Step 7: Commit**

```bash
git add src/opdi/reference/pbf_source.py tests/test_pbf_source.py
git commit -m "feat(reference): assign PBF aeroway features by OSM aerodrome polygon, bbox as fallback"
```

---

### Task 4: Wire the source into the layout generator

Both paths must coexist so the two can be compared and the Overpass path
remains available.

**Files:**
- Modify: `opdi/src/opdi/reference/h3_airport_layouts.py`
- Test: `opdi/tests/test_pbf_source.py`

**Interfaces:**
- Consumes: `PbfLayoutSource.features_for` (Task 3).
- Produces:
  - `hexagonify_airport(apt_icao, resolution=12, source=None) -> pd.DataFrame`
  - `AirportLayoutGenerator(spark, config, resolution=None, log_dir=..., storage=None, pbf_path=None)`

- [ ] **Step 1: Write the failing test**

Append to `opdi/tests/test_pbf_source.py`:

```python
def test_hexagonify_uses_the_pbf_source_when_given_one(spark):
    """The seam. `hexagonify_airport` is unchanged apart from where its
    features come from: same widths, same polygon conversion, same H3, same
    twelve columns."""
    from opdi.reference.h3_airport_layouts import HEXAERO_SCHEMA, hexagonify_airport

    src = PbfLayoutSource(PBF, _Storage(spark))
    df = hexagonify_airport("ELLX", resolution=12, source=src)

    assert len(df) > 0
    assert list(df.columns) == [f.name for f in HEXAERO_SCHEMA.fields]
    assert set(df["hexaero_apt_icao"]) == {"ELLX"}
    assert (df["hexaero_res"] == 12).all()
    assert "parking_position" in set(df["hexaero_aeroway"])
```

- [ ] **Step 2: Run to verify it fails**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py::test_hexagonify_uses_the_pbf_source_when_given_one -v`
Expected: FAIL with `TypeError: hexagonify_airport() got an unexpected keyword argument 'source'`

- [ ] **Step 3: Add the parameter**

In `h3_airport_layouts.py`, change the signature at `:270` and the first line
of the body at `:285`:

```python
def hexagonify_airport(apt_icao: str, resolution: int = 12, source=None) -> pd.DataFrame:
```

```python
    # `source` is a PbfLayoutSource when step 00b is running off a local
    # extract. `retrieve_osm_data` -- the Overpass path -- stays reachable and
    # unchanged, so the two can be compared on the same airport.
    raw = source.features_for(apt_icao) if source is not None else retrieve_osm_data(apt_icao)
    if raw is None or len(raw) == 0:
        raise ValueError(f"No data returned for {apt_icao}")
    df = raw.reset_index(drop=source is not None)
```

`drop=source is not None` matters: the Overpass frame carries `element`/`id` in
its **index**, so `reset_index()` must promote them; the PBF frame already has
them as columns and a plain `reset_index()` would add a spurious `index` column.

Then in `AirportLayoutGenerator.__init__`, after `self.resolution = ...`:

```python
        # When set, step 00b reads this local extract instead of the public
        # Overpass API. See docs/industrialization-plan.md §3.
        self.pbf_path = pbf_path
        self._pbf_source = None
```

and add `pbf_path: Optional[str] = None,` to its signature. In
`process_airport`, replace the `hexagonify_airport` call:

```python
            if self.pbf_path and self._pbf_source is None:
                from opdi.reference.pbf_source import PbfLayoutSource

                self._pbf_source = PbfLayoutSource(self.pbf_path, self.storage)
            df = hexagonify_airport(
                apt_icao, resolution=self.resolution, source=self._pbf_source
            )
```

- [ ] **Step 4: Run the test and the existing generator tests**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py tests/test_h3_airport_layouts.py -v`
Expected: PASS. `test_h3_airport_layouts.py` asserts `process_airport` writes
nothing and `process_all` writes once; neither may regress.

- [ ] **Step 5: Commit**

```bash
git add src/opdi/reference/h3_airport_layouts.py tests/test_pbf_source.py
git commit -m "feat(layouts): step 00b can build from a local OSM extract"
```

---

### Task 4B: Pre-filter the extract, because the container has 16 GB

**Added 2026-09-07, after Task 5 was OOM-killed three times.**

**What happened.** `hexagonify_airport` against the full Europe extract died
three times with no output. The cause is not the code: this container's cgroup
caps memory at **17,179,869,184 bytes (16 GB)** — `free` reports the host's
251 GB and is misleading — and `/sys/fs/cgroup/memory.events` records **15
`oom_kill` events**.

The specific trigger is `with_areas()`. Assembling multipolygons requires
osmium to hold **node locations for the whole file**, and Europe has billions
of nodes. Its default `flex_mem` index does that in RAM and is killed well
before it finishes. The two Europe scans that *succeeded* earlier — the aeroway
census (348 s) and the aerodrome tag census (508 s) — both ran **without**
`with_areas()`, which is why the limit went unnoticed until the gate.

**What was tried and rejected.** pyosmium supports disk-backed node indexes.
`with_locations("sparse_file_array,...")` does hold memory down — measured peak
RSS **3.1 GB**, a fifth of the cap — so the approach is memory-correct. But it
is far too slow: the index reached **57 GB on disk**, and the run was still in
the area-assembly pass after **79 minutes** with no areas emitted, with the
cgroup sitting at 15 GB of 16 GB in page cache. Random lookups across a 57 GB
on-disk index are the bottleneck. Killed and rejected. **Do not retry this.**

**The fix.** Filter the extract *once* down to only what aeroway geometry
needs, and let everything afterwards run on a small file with the default
in-memory index. This is what `osmium tags-filter` would do, and the CLI is not
installed here, so it is done in pyosmium.

The prize is not only that it fits: the filtered file is read in **seconds**
rather than 9 minutes a pass, so Task 5's comparison, Task 6's 1,353-aerodrome
build, and every future rebuild all become cheap. The 34.9 GB original stays on
disk as the source of truth.

**Files:**
- Create: `opdi/src/opdi/reference/pbf_filter.py`
- Test: `opdi/tests/test_pbf_filter.py`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `filter_aeroway_pbf(src_path: str, dst_path: str) -> dict` — writes the
    filtered extract and returns counts (`nodes`, `ways`, `relations`,
    `bytes_in`, `bytes_out`, `seconds`).

- [ ] **Step 1: Write the failing test**

Create `opdi/tests/test_pbf_filter.py`. Use the Luxembourg extract, where the
answer is checkable against Task 2's measured counts.

```python
@needs_luxembourg
def test_the_filtered_extract_yields_the_same_aeroway_features(tmp_path):
    """The filter is only correct if nothing downstream can tell the
    difference. Task 2 measured Luxembourg at 116 taxiway, 5 runway, 57 apron,
    6 hangar, 194 parking_position, 0 threshold, 0 deicing_pad -- so the
    filtered file must reproduce that exactly, not approximately."""
    dst = tmp_path / "aeroway-lux.osm.pbf"
    stats = filter_aeroway_pbf(PBF, str(dst))
    assert dst.exists() and stats["bytes_out"] < stats["bytes_in"]

    before = read_aeroway_features(PBF)
    after = read_aeroway_features(str(dst))
    assert (after["aeroway"].value_counts().sort_index()
            .equals(before["aeroway"].value_counts().sort_index()))
    assert set(zip(after["element"], after["id"])) == set(zip(before["element"], before["id"]))


@needs_luxembourg
def test_aerodrome_polygons_survive_the_filter(tmp_path):
    """Task 3 assigns features by containment in the `aeroway=aerodrome`
    polygon. If the filter drops the boundary, or keeps it as an unassemblable
    fragment, every feature becomes unassigned and the grid silently empties."""
    dst = tmp_path / "aeroway-lux.osm.pbf"
    filter_aeroway_pbf(PBF, str(dst))
    before, after = read_aerodromes(PBF), read_aerodromes(str(dst))
    assert set(after["icao"]) == set(before["icao"])
    for icao in before["icao"]:
        a = after.set_index("icao").loc[icao].geometry
        b = before.set_index("icao").loc[icao].geometry
        assert a.equals(b) or a.symmetric_difference(b).area < 1e-12


def test_a_relation_member_way_is_kept_even_though_it_is_untagged(tmp_path):
    """The trap. An apron mapped as a multipolygon relation has member ways
    that carry no `aeroway` tag of their own. Filter on the tag alone and the
    members vanish, the relation cannot be assembled, and the apron disappears
    -- at every airport that maps aprons this way, silently."""
    ...
```

Write the third test out in full against a synthetic `.osm` fixture: a relation
tagged `aeroway=apron` whose two member ways are untagged, plus their nodes.
Assert the relation still assembles to a polygon after filtering. **Verify by
mutation that it fails if member ways are not collected.**

- [ ] **Step 2: Run to verify they fail**

Run: `.venv310/bin/python -m pytest tests/test_pbf_filter.py -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'opdi.reference.pbf_filter'`

- [ ] **Step 3: Implement the filter**

Three passes, because a PBF is ordered nodes → ways → relations and a relation
is therefore read *after* the ways it references:

1. **Pass A** — ways and relations. A way tagged `aeroway` goes into
   `keep_ways` and its node refs into `keep_nodes`. A relation tagged `aeroway`
   goes into `keep_rels` and its **member way ids** into `member_ways`.
2. **Pass B** — ways again. Any way in `member_ways` not already kept goes into
   `keep_ways` and contributes its node refs. This pass exists solely because
   relations are read last; without it the member ways of every multipolygon
   apron are missing.
3. **Pass C** — write, in file order, with `osmium.SimpleWriter`: nodes in
   `keep_nodes` **or** carrying an `aeroway` tag themselves (a
   `parking_position` is often a bare node), then ways in `keep_ways`, then
   relations in `keep_rels`.

Use `osmium.index.IdSet` for the four id sets — verified present in pyosmium
4.3.1 and far cheaper than Python sets at tens of millions of ids.

Do **not** use `with_areas()` anywhere in this module. That is the thing being
worked around; the filter deals in raw objects only.

- [ ] **Step 4: Run the tests**

Run: `.venv310/bin/python -m pytest tests/test_pbf_filter.py -v`

- [ ] **Step 5: Build the filtered Europe extract**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python -c "
from opdi.reference.pbf_filter import filter_aeroway_pbf
print(filter_aeroway_pbf('/home/jupyter/work/osm/europe-latest.osm.pbf',
                         '/home/jupyter/work/osm/aeroway-europe.osm.pbf'))
"
```

Three passes over 34.9 GB at roughly 350 s each, so expect **20-30 minutes**.
Run it once, in the background, and do not restart it. Watch RSS: it must stay
well under 16 GB, and if it climbs toward the cap, stop and report rather than
letting the OOM killer end it.

- [ ] **Step 6: Prove the filtered extract is equivalent, at Europe scale**

The unit tests prove equivalence for Luxembourg. This proves it for the file
that will actually be used:

```bash
.venv310/bin/python -c "
import osmium, collections
c = collections.Counter()
for o in osmium.FileProcessor('/home/jupyter/work/osm/aeroway-europe.osm.pbf').with_filter(osmium.filter.KeyFilter('aeroway')):
    c[o.tags.get('aeroway')] += 1
for k in ('taxiway','runway','apron','hangar','threshold','parking_position','deicing_pad'):
    print(f'{k:20s} {c.get(k,0)}')
"
```

**Gate — these are the counts measured on the full 34.9 GB extract and every
one must match exactly:**

| family | expected |
|---|---|
| taxiway | 67,826 |
| runway | 12,195 |
| apron | 15,555 |
| hangar | 18,399 |
| threshold | 1,510 |
| parking_position | 37,600 |
| deicing_pad | 36 |

A shortfall in any family means the filter dropped geometry and **must not be
worked around** — Task 5 would then be grading a filter bug rather than the
change of source. Also confirm the count of distinct `icao` codes on
`aeroway=aerodrome` features is **3,254**, unchanged.

- [ ] **Step 7: Commit**

```bash
git add src/opdi/reference/pbf_filter.py tests/test_pbf_filter.py
git commit -m "feat(reference): pre-filter the OSM extract to aeroway geometry

with_areas() over the 34.9 GB Europe extract needs node locations for the whole
file and is OOM-killed under this container's 16 GB cgroup cap. A disk-backed
node index holds memory to 3.1 GB but reached 57 GB on disk and had not
finished assembling areas after 79 minutes.

Filtering once to aeroway ways, their relation members and the nodes they
reference gives a file small enough for the default in-memory index, read in
seconds instead of nine minutes a pass."
```

---

### Task 5: Prove the PBF path agrees with the Overpass path

Before rebuilding 1,353 aerodromes from a new source, show it produces what the
old one did. **This is the acceptance gate for the whole approach** — the claim
"without losing anything" in `industrialization-plan.md` §3.4 is currently
unverified, and this task is what verifies it.

**Files:**
- Create: `opdi/benchmarks/compare_layout_sources.py`

**Interfaces:**
- Consumes: `PbfLayoutSource`, `hexagonify_airport`.
- Produces: a printed comparison; no table is written.

- [ ] **Step 1: Write the comparison script**

Create `opdi/benchmarks/compare_layout_sources.py`:

```python
"""Does the local extract produce the same layout grid as Overpass did?

Compares H3 cell sets per aeroway type for airports whose Overpass-built grid
was saved during the flight-events-v4 campaign. Cell identity is exact, so the
comparison is a set operation rather than a tolerance.

Run:
    .venv310/bin/python benchmarks/compare_layout_sources.py \
        --pbf /home/jupyter/work/osm/europe-latest.osm.pbf \
        --parts /home/jupyter/work/osm/overpass_baseline \
        --airports EBBR LSZH EICK EGGD
"""
import argparse
import glob
import os
import sys

import pandas as pd

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--pbf", required=True)
    ap.add_argument("--parts", required=True,
                    help="directory of <ICAO>.parquet built from Overpass")
    ap.add_argument("--airports", nargs="+", required=True)
    args = ap.parse_args()

    import osn_sample
    osn_sample.load_dotenv()
    spark = osn_sample.build_spark(2, "8g", distributed=False)
    spark.sparkContext.setLogLevel("ERROR")

    from opdi.config import OPDIConfig
    from opdi.reference.h3_airport_layouts import hexagonify_airport
    from opdi.reference.pbf_source import PbfLayoutSource
    from opdi.utils.storage import StorageManager

    storage = StorageManager(spark, OPDIConfig.for_environment("opensky"))
    src = PbfLayoutSource(args.pbf, storage)

    print(f"{'airport':8s} {'aeroway':18s} {'overpass':>9s} {'pbf':>9s} "
          f"{'shared':>9s} {'only_op':>8s} {'only_pbf':>8s}")
    for apt in args.airports:
        part = os.path.join(args.parts, f"{apt}.parquet")
        if not os.path.exists(part):
            print(f"{apt}: no Overpass baseline, skipped")
            continue
        old = pd.read_parquet(part)
        new = hexagonify_airport(apt, resolution=12, source=src)
        for way in sorted(set(old["hexaero_aeroway"]) | set(new["hexaero_aeroway"])):
            a = set(old.loc[old["hexaero_aeroway"] == way, "hexaero_h3_id"])
            b = set(new.loc[new["hexaero_aeroway"] == way, "hexaero_h3_id"])
            print(f"{apt:8s} {way:18s} {len(a):9d} {len(b):9d} "
                  f"{len(a & b):9d} {len(a - b):8d} {len(b - a):8d}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
```

- [ ] **Step 2: Run it on every aerodrome that has a baseline**

Not four. **Fifteen** — every Overpass baseline that survives from the
flight-events-v4 campaign. The extra fourteen are nearly free: the source pays
~18 minutes once to read the extract, then answers each airport in about a
millisecond, so restricting the comparison buys nothing and costs coverage.

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python benchmarks/compare_layout_sources.py \
  --pbf /home/jupyter/work/osm/europe-latest.osm.pbf \
  --parts /home/jupyter/work/osm/overpass_baseline \
  --airports EBBR EDDS EFHK EGGD EICK ENVA ESSA LEIB LFLL LFPO LHBP LKPR LOWW LPFR LSZH
```

**Fifteen is also what makes all seven families testable.** Measured over the
baselines:

| family | baseline cells | aerodromes carrying it |
|---|---|---|
| taxiway | 60,102 | all 15 |
| apron | 31,155 | all 15 |
| parking_position | 26,011 | all 15 |
| runway | 20,674 | all 15 |
| hangar | 2,304 | 12 |
| threshold | 183 | 8 — EBBR EDDS EGGD EICK ENVA LHBP LPFR LSZH |
| deicing_pad | 164 | **2 — EBBR and EDDS only** |

`threshold` and `deicing_pad` do not exist in the Luxembourg extract, so Tasks
2-4 never exercised them. **This is the only point in the plan where either is
checked before it reaches the published table**, and `deicing_pad` rests
entirely on EBBR and EDDS. Dropping either airport from the run silently
removes a whole family from the acceptance gate.

- [ ] **Step 3: Judge the result against a stated bar**

**Pass:** for `parking_position`, `taxiway`, `apron` and `runway`, the shared
share is **> 90%** of the Overpass cells at every airport, and `only_pbf` is
not wildly larger than `only_op` (which would mean the box is capturing a
neighbour).

**Expect some difference and do not treat it as failure**: the extract and the
Overpass snapshot were taken on different days, and OSM changes. A *systematic*
loss of one aeroway type is a real defect — most likely `with_areas()` not
assembling that type — and must be fixed before Task 6.

If a type is missing entirely from the PBF side, check whether it is encoded as
a closed way rather than a relation, and whether the `KeyFilter` is dropping it.

- [ ] **Step 4: Commit the script and the result**

```bash
git add benchmarks/compare_layout_sources.py
git commit -m "bench: compare the local-extract layout grid against the Overpass one"
```

Record the printed table in the commit message. It is the evidence for §3.4's
claim, which is currently written as a proposal precisely because nobody has run
this.

---

### Task 6: Rebuild the layout table for every large and medium aerodrome

**Files:**
- Modify: `opdi/src/opdi/runner.py:140-150`

**Interfaces:**
- Consumes: everything above.
- Produces: `hexaero_airport_layouts` covering ~1,353 aerodromes.

- [ ] **Step 1: Let the runner pass a PBF path**

In `_step_00b_airport_layouts`:

```python
def _step_00b_airport_layouts(spark, config, **kwargs):
    """Step 00b: Airport ground layouts (OSM -> H3)."""
    print("\n--- 00b: Airport ground layouts (OSM -> H3) ---")
    from opdi.reference.h3_airport_layouts import AirportLayoutGenerator

    # A local extract when one is configured; the public Overpass API
    # otherwise. The extract is strongly preferred: Overpass rate-limits hard
    # enough that a 1,353-aerodrome build cannot complete against it.
    layout_gen = AirportLayoutGenerator(
        spark, config, pbf_path=kwargs.get("pbf_path") or os.environ.get("OPDI_OSM_PBF")
    )
```

Add `import os` at the top of `runner.py` if absent.

- [ ] **Step 2: Confirm the Europe extract finished downloading**

```bash
ls -la /home/jupyter/work/osm/europe-latest.osm.pbf
```
Expected: ~30 GB and no longer growing. A truncated file makes
`read_aeroway_features` raise or return a partial set — check the size before
spending an hour on a build.

- [ ] **Step 3: Build, to a research table first**

Do **not** write the published table on the first full run. Point the storage
at a research name, exactly as the benchmark does:

```bash
cd /home/jupyter/work/opdi-workspace/opdi
OPDI_OSM_PBF=/home/jupyter/work/osm/europe-latest.osm.pbf \
  .venv310/bin/python - <<'PY'
import os, sys, time
sys.path.insert(0, "src"); sys.path.insert(0, "benchmarks")
import osn_sample; osn_sample.load_dotenv()
spark = osn_sample.build_spark(4, "16g", distributed=False)
spark.sparkContext.setLogLevel("ERROR")
from opdi.config import OPDIConfig
from opdi.reference.h3_airport_layouts import AirportLayoutGenerator
from opdi.utils.storage import StorageManager

# Redirect the write to a research name for this first build.
orig = StorageManager._s3_path
StorageManager._s3_path = lambda self, t: orig(
    self, "research/hexaero_airport_layouts_pbf" if t == "hexaero_airport_layouts" else t
)
gen = AirportLayoutGenerator(spark, OPDIConfig.for_environment("opensky"),
                             log_dir="OPDI_live/logs/pbf",
                             pbf_path=os.environ["OPDI_OSM_PBF"])
t0 = time.time()
ok, bad = gen.process_all()
print("built %d, failed %d, %.0f s" % (len(ok), len(bad), time.time() - t0))
PY
```

Expected: on the order of **1,300 airports** and well under an hour — there is
no network in the loop. Compare against the volume in
`industrialization-plan.md` §3.1: ~9,843 cells per airport, so ~13 M rows.

- [ ] **Step 4: Check the result before publishing**

```bash
.venv310/bin/python - <<'PY'
import os, sys
from pathlib import Path
for l in Path(".env").read_text().splitlines():
    if l.strip() and not l.startswith("#") and "=" in l:
        k, v = l.split("=", 1); os.environ.setdefault(k.strip(), v.strip())
import pyarrow.dataset as pds, pyarrow.fs as pafs
fs = pafs.S3FileSystem(endpoint_override="https://s3.opensky-network.org",
    access_key=os.environ["AWS_ACCESS_KEY_ID"],
    secret_key=os.environ["AWS_SECRET_ACCESS_KEY"], scheme="https")
t = pds.dataset("eurocontrol/opdi/research/hexaero_airport_layouts_pbf",
                filesystem=fs, format="parquet").to_table(
    columns=["hexaero_apt_icao", "hexaero_aeroway"]).to_pandas()
print("airports:", t["hexaero_apt_icao"].nunique(), "rows:", len(t))
print(t["hexaero_aeroway"].value_counts().to_string())
STUDY = "EBBR LSZH EICK EFHK LEIB EDDS LFLL LHBP LFPO LPFR ESSA ENVA LOWW LKPR EDDP LPPT EGGD EGNT EGCC UGKO".split()
have = set(t.loc[t["hexaero_aeroway"] == "parking_position", "hexaero_apt_icao"])
print("study airports with stands: %d/20" % len(set(STUDY) & have))
print("missing:", sorted(set(STUDY) - have))
PY
```

**Gate**, all four parts, and none of them is optional:

1. All twenty study aerodromes have `parking_position` cells.
2. The original twenty (`EBBR EDDF EDDM EGKK EGLL EHAM EIDW EKCH ENGM EPWA
   ESSA LEBL LEMD LFPG LGAV LIRF LOWW LPPT LSZH LTFM`) are all present — that
   set is what the published table held before it was damaged, and this build
   is also its restoration.
3. **All seven `AEROWAY_TAGS` families are present network-wide, with cell
   counts of a plausible order.** The grid is the whole hexaero map, not just
   the stands AOBT/AIBT ride on, and a family lost across all 1,353 aerodromes
   would otherwise sail through a stands-only check. The raw feature census
   over the Europe extract gives the denominator to sanity-check against:

   | family | features in the extract |
   |---|---|
   | taxiway | 67,826 |
   | runway | 12,195 |
   | apron | 15,555 |
   | hangar | 18,399 |
   | threshold | 1,510 |
   | parking_position | 37,600 |
   | deicing_pad | 36 |

   `threshold` and `deicing_pad` are absent from the Luxembourg extract used in
   Tasks 2-4, so this is the **first** point at which either is exercised at
   all. A zero for one of them here is a defect, not a quirk of the data.
4. The extract is intact: `europe-latest.osm.pbf` is exactly
   **34,940,824,103 bytes**. A truncated PBF can parse without raising and
   yield a partial feature set, which looks identical to the
   missing-aerodrome symptom this plan exists to repair. The first download
   of this file died silently at 25.8 GB.

- [ ] **Step 5: Publish**

Only after step 4 passes, re-run step 3 without the `_s3_path` redirect so
`process_all` writes `hexaero_airport_layouts` itself. Delete
`OPDI_live/logs/pbf` first so it is a fresh run and the single write is an
`overwrite` rather than an `append`.

- [ ] **Step 6: Commit**

```bash
git add src/opdi/runner.py
git commit -m "feat(runner): step 00b builds from a local OSM extract when configured"
```

---

### Task 7: Measure AOBT/AIBT, and say what it comes to

The point of all of it. **Report what it measures, not what it was hoped to
be** — the fix and the geometry only matter if the numbers move.

**Files:**
- Modify: `opdi-portal/papers/flight-events-v4/index.qmd`

**Interfaces:**
- Consumes: the published layout table (Task 6); `airport_admit_on_ground`,
  merged.
- Produces: measured per-aerodrome AOBT/AIBT coverage.

- [ ] **Step 1: Re-run the shipped rung**

One distributed job at a time from this pod.

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python -u benchmarks/event_bench.py \
  --period 2026 --ladder v4 --airports study \
  --runs V07_shipped --out-name ladder_2026.csv \
  --executors 10 --driver-memory 16g \
  --results-dir /tmp/v4_blocks
```

The run may hang in Spark teardown **after** writing its CSVs — that has
happened on every rung of this campaign. Watch for `wrote .../ladder_2026.csv`
rather than for the process to exit, then kill it.

- [ ] **Step 2: Score per aerodrome and per detector**

```bash
.venv310/bin/python -u benchmarks/events_compare.py \
  --period 2026 --ladder v4 --rung V07_shipped --airports study \
  --executors 10 --driver-memory 16g --results-dir /tmp/v4_blocks
```

This writes `per_airport_2026.csv` (pooled) and
`per_airport_by_detector_2026.csv` (split), both already used by Annex A.

- [ ] **Step 3: Read the result against the stated expectation**

Compare `AOBT`/`AIBT` coverage against the campaign's baseline: **~1% at
nineteen of twenty aerodromes, 55% at LSZH**.

* A large rise at the fifteen aerodromes that previously had **no stand
  geometry** is the geometry working.
* A rise at EBBR specifically is the `on_ground` fix working: 2,013 aircraft sit
  in its stand polygons and 57 reached the detector before it.
* **Do not expect parity with `ATOT`/`ALDT` (~90%).** Reception is the floor and
  no geometry lifts it — LSZH reports ground altitude on 12.8% of in-stand
  samples against EBBR's 0.1%. Set the claim from the measurement.

- [ ] **Step 4: Update the paper**

Stage the CSVs into `opdi-portal/papers/flight-events-v4/data/` and re-render:

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal/papers
OPDI_RENDER=allow-stale quarto render flight-events-v4/index.qmd --to pdf
```

Rewrite the off-block/on-block chapter's limitation, which currently says the
family is reception-bound. That is only the residual: the dominant causes were
missing stand geometry at fifteen of twenty aerodromes and an altitude gate that
discarded 99.9% of stand samples. Both are now fixed, and the chapter should say
what each was worth.

- [ ] **Step 5: Commit and open a PR**

```bash
cd /home/jupyter/work/opdi-workspace/opdi-portal
git add papers/flight-events-v4
git commit -m "papers: AOBT/AIBT measured with stand geometry at every study aerodrome"
git push -u origin papers/flight-events-v4-blocks
gh pr create --draft --repo euctrl-pru/opdi-portal --base main \
  --head papers/flight-events-v4-blocks --title "papers: AOBT/AIBT measured"
```

---

## Verification

**Unit, no cluster, no network beyond the Luxembourg extract:**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python -m pytest tests/test_pbf_source.py tests/test_h3_airport_layouts.py \
                              tests/test_layout.py -q
```
Expected: PASS. The layout tests pin the `on_ground` gate; the generator tests
pin that `process_airport` writes nothing and `process_all` writes once.

**Contract check — the seam did not move:**

```bash
.venv310/bin/python -c "
from opdi.reference.h3_airport_layouts import HEXAERO_SCHEMA, hexagonify_airport
import inspect
assert 'source' in inspect.signature(hexagonify_airport).parameters
print([f.name for f in HEXAERO_SCHEMA.fields])
print('seam ok')
"
```

**What "working" looks like — falsifiable predictions.** If these do not hold,
say so rather than reporting the numbers as a success:

* The PBF build covers **≥ 1,200** of the 1,353 aerodromes. Fewer means the
  boxes or the area assembly are dropping airports.
* **All twenty study aerodromes have `parking_position` cells**, and so do the
  original twenty the published table held.
* Task 5's comparison shows **> 90%** shared cells per aeroway type at four
  aerodromes of different sizes.
* AOBT/AIBT coverage rises materially above ~1% at the fifteen aerodromes that
  had no stands. **If it does not, the geometry was not the binding constraint
  and the paper must say so** — the block-time detector would then be reception-
  bound in the way the chapter currently claims, and this plan will have
  established that rather than assumed it.

---

## Open items, stated rather than assumed

* ~~pyosmium's API version is unverified.~~ **Resolved (Task 1):** pyosmium
  **4.3.1** is installed and exposes the full 4.x API — `FileProcessor`,
  `filter.KeyFilter`, `geom.WKBFactory`, `osm.Area`. No `SimpleHandler`
  fallback is needed.
* **The 1.5 km box margin is a judgement, not a measurement** — but it now
  applies only to the fallback. Task 3 was revised to assign features by
  containment in OSM's own `aeroway=aerodrome` polygon, matched on its `icao`
  tag; the box is used only where no polygon exists (486 of the extract's
  3,281 `icao`-tagged aerodrome features are bare nodes). The margin's weakness
  is therefore confined to the aerodromes that have no better option instead of
  being imposed everywhere. It is still checked only indirectly, by Task 5.
* ~~Nearest-aerodrome assignment is O(airports) per airport.~~ **Resolved by
  the same revision.** The box path was measured at 0.53 s per `features_for`
  call against a 1,353-aerodrome box set — about 12 minutes of pure filtering
  for a full build. The polygon path is a single spatial join over all
  features. The nearest-aerodrome loop survives only on the fallback, where it
  runs against a much smaller set.
* **Containment could be tighter than reality.** A feature belonging to an
  airport but drawn outside its OSM boundary is now unassigned where the box
  would have caught it. Task 5's comparison against the Overpass baselines
  cannot fully detect this, because Overpass derived its search area from the
  same boundary — so Task 3 Step 6 reports the unassigned fraction per family
  directly, and a large one is a finding rather than a nuisance.
* **§3.4's response-caching proposal is deliberately not implemented.** It
  existed to stop a re-run re-querying Overpass; with a local extract there is
  no HTTP request to cache, and the file on disk *is* the cache. The bbox
  proposal from that section survives only as Task 3's fallback; OSM's own
  aerodrome polygon replaced it as the primary assignment method.
* **The extract is a snapshot.** Cells will differ from an Overpass build made
  on another day, and nothing in the event schema records which layout vintage
  produced an event. `industrialization-plan.md` §3.5 raises this; it is not
  solved here.
* **The published table is still missing 16 of its original 20 aerodromes**
  until Task 6 completes. That is a live degradation and Task 6 is its repair,
  not merely an improvement.
