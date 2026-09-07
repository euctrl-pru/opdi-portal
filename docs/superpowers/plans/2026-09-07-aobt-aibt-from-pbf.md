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
`aeroway` features, assigns each to an aerodrome by geometry, and serves
per-airport GeoDataFrames from memory. `hexagonify_airport` gains a `source`
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
| `pbf_source.py` | Read a `.osm.pbf` once, keep `aeroway` features, build shapely geometries, assign each feature to an aerodrome, and serve per-airport GeoDataFrames. The only new networked/IO-heavy unit. |

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

The extract has no notion of which airport a taxiway belongs to. Overpass got
this from the place polygon; here it comes from reference data we already hold.
**This is the task that can silently corrupt the table** — a feature assigned to
the wrong aerodrome becomes a stand at an airport that does not have it.

**Files:**
- Modify: `opdi/src/opdi/reference/pbf_source.py`
- Test: `opdi/tests/test_pbf_source.py`

**Interfaces:**
- Consumes: `read_aeroway_features` (Task 2); `oa_airports` and `oa_runways`
  via `StorageManager`.
- Produces:
  - `airport_boxes(storage, airport_types=None) -> pd.DataFrame` with columns
    `ident`, `lat_min`, `lat_max`, `lon_min`, `lon_max`, `apt_lat`, `apt_lon`
  - `class PbfLayoutSource` with
    `__init__(self, pbf_path: str, storage, airport_types=None)` and
    `features_for(self, apt_icao: str) -> gpd.GeoDataFrame`

- [ ] **Step 1: Write the failing tests**

Append to `opdi/tests/test_pbf_source.py`:

```python
from opdi.reference.pbf_source import PbfLayoutSource, airport_boxes


class _Storage:
    """`oa_airports` and `oa_runways` as the generator reads them."""

    def __init__(self, spark):
        self._t = {
            "oa_airports": spark.createDataFrame(
                [("ELLX", 49.6233, 6.2044, "large_airport"),
                 ("EBBR", 50.9014, 4.4844, "large_airport")],
                "ident string, latitude_deg double, longitude_deg double, type string",
            ),
            "oa_runways": spark.createDataFrame(
                [("ELLX", 49.6266, 6.1867, 49.6200, 6.2247),
                 ("EBBR", 50.9010, 4.4700, 50.9060, 4.5000)],
                "airport_ident string, le_latitude_deg double, le_longitude_deg double, "
                "he_latitude_deg double, he_longitude_deg double",
            ),
        }

    def table_exists(self, name):
        return name in self._t

    def read_table(self, name):
        return self._t[name]


def test_a_box_is_built_from_the_runway_extent(spark):
    """Runways bound an airport's long axis, so their extent plus a margin
    encloses the aprons, stands and taxiways that sit between them. The margin
    is what makes it an envelope rather than a line."""
    boxes = airport_boxes(_Storage(spark)).set_index("ident")
    ellx = boxes.loc["ELLX"]
    assert ellx.lat_min < 49.6200 and ellx.lat_max > 49.6266
    assert ellx.lon_min < 6.1867 and ellx.lon_max > 6.2247


def test_features_are_assigned_to_the_nearest_aerodrome_only(spark):
    """The guard against the failure this task can cause. A box can overlap a
    neighbouring airfield, and `hexagonify_airport` stamps `apt_icao` on
    whatever it is given -- so a feature inside two boxes must go to the
    aerodrome it is actually closest to, and to no other."""
    src = PbfLayoutSource(PBF, _Storage(spark))
    ellx = src.features_for("ELLX")
    ebbr = src.features_for("EBBR")
    assert len(ellx) > 0, "ELLX is in the Luxembourg extract and has aeroways"
    assert len(ebbr) == 0, "EBBR is in Belgium; nothing in this extract is its"
    assert set(ellx["aeroway"]) <= set(AEROWAY_TAGS_SET)


def test_the_source_reads_the_extract_once(spark):
    """1,353 airports must not mean 1,353 passes over a 30 GB file."""
    src = PbfLayoutSource(PBF, _Storage(spark))
    src.features_for("ELLX")
    before = src._read_count
    src.features_for("ELLX")
    src.features_for("EBBR")
    assert src._read_count == before, "the extract was re-read"
```

Add near the imports of that file:

```python
from opdi.reference.h3_airport_layouts import AEROWAY_TAGS
AEROWAY_TAGS_SET = set(AEROWAY_TAGS)
```

- [ ] **Step 2: Run to verify they fail**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py -v`
Expected: FAIL with `ImportError: cannot import name 'PbfLayoutSource'`

- [ ] **Step 3: Implement assignment**

Append to `opdi/src/opdi/reference/pbf_source.py`:

```python
import math

from pyspark.sql import functions as F

#: Margin around the runway extent, in kilometres. Aprons, stands and hangars
#: sit off the runway axis; 1.5 km covers them at the largest aerodromes without
#: reaching a neighbouring field at typical separations.
BOX_MARGIN_KM = 1.5

#: Fallback half-size when an aerodrome has no usable runway coordinates, so it
#: still gets a box rather than being dropped.
FALLBACK_HALF_KM = 3.0

_KM_PER_DEG_LAT = 111.0


def _deg_lon(km: float, lat: float) -> float:
    return km / (_KM_PER_DEG_LAT * max(0.05, math.cos(math.radians(lat))))


def airport_boxes(storage, airport_types=None) -> pd.DataFrame:
    """One bounding box per aerodrome, from its runway extent.

    Overpass derived the search area from a geocoded place polygon. That is the
    step being removed, so the area has to come from reference data instead:
    ``oa_runways`` holds both thresholds of every runway, and a runway bounds
    the airport's long axis.
    """
    airport_types = airport_types or ["large_airport", "medium_airport"]
    apt = (
        storage.read_table("oa_airports")
        .filter(F.col("type").isin(airport_types))
        .select("ident", "latitude_deg", "longitude_deg")
        .toPandas()
        .rename(columns={"latitude_deg": "apt_lat", "longitude_deg": "apt_lon"})
    )
    rwy = (
        storage.read_table("oa_runways")
        .select("airport_ident", "le_latitude_deg", "le_longitude_deg",
                "he_latitude_deg", "he_longitude_deg")
        .toPandas()
    )
    lat = pd.concat([rwy["le_latitude_deg"], rwy["he_latitude_deg"]])
    lon = pd.concat([rwy["le_longitude_deg"], rwy["he_longitude_deg"]])
    ident = pd.concat([rwy["airport_ident"], rwy["airport_ident"]])
    ext = (
        pd.DataFrame({"ident": ident, "lat": lat, "lon": lon})
        .dropna()
        .groupby("ident")
        .agg(lat_min=("lat", "min"), lat_max=("lat", "max"),
             lon_min=("lon", "min"), lon_max=("lon", "max"))
        .reset_index()
    )
    out = apt.merge(ext, on="ident", how="left")

    have = out["lat_min"].notna()
    m_lat = BOX_MARGIN_KM / _KM_PER_DEG_LAT
    out.loc[have, "lat_min"] -= m_lat
    out.loc[have, "lat_max"] += m_lat
    out.loc[have, "lon_min"] -= [
        _deg_lon(BOX_MARGIN_KM, v) for v in out.loc[have, "apt_lat"]
    ]
    out.loc[have, "lon_max"] += [
        _deg_lon(BOX_MARGIN_KM, v) for v in out.loc[have, "apt_lat"]
    ]

    # No runway coordinates: a square around the aerodrome point, so it is
    # still built rather than silently absent from the table.
    miss = ~have
    f_lat = FALLBACK_HALF_KM / _KM_PER_DEG_LAT
    out.loc[miss, "lat_min"] = out.loc[miss, "apt_lat"] - f_lat
    out.loc[miss, "lat_max"] = out.loc[miss, "apt_lat"] + f_lat
    out.loc[miss, "lon_min"] = out.loc[miss, "apt_lon"] - [
        _deg_lon(FALLBACK_HALF_KM, v) for v in out.loc[miss, "apt_lat"]
    ]
    out.loc[miss, "lon_max"] = out.loc[miss, "apt_lon"] + [
        _deg_lon(FALLBACK_HALF_KM, v) for v in out.loc[miss, "apt_lat"]
    ]
    return out


class PbfLayoutSource:
    """Per-airport aeroway features, served from one pass over the extract.

    The extract is read on first use and held in memory. Europe's aeroway
    features are a small fraction of the file -- tens of megabytes as
    geometry -- so this is affordable, and the alternative (a pass per airport)
    would be 1,353 passes over 30 GB.
    """

    def __init__(self, pbf_path: str, storage, airport_types=None):
        self.pbf_path = pbf_path
        self._boxes = airport_boxes(storage, airport_types)
        self._features: Optional[gpd.GeoDataFrame] = None
        self._read_count = 0

    def _load(self) -> gpd.GeoDataFrame:
        if self._features is None:
            self._features = read_aeroway_features(self.pbf_path)
            self._read_count += 1
            reps = self._features.geometry.representative_point()
            self._features["_lat"] = reps.y.to_numpy()
            self._features["_lon"] = reps.x.to_numpy()
        return self._features

    def features_for(self, apt_icao: str) -> gpd.GeoDataFrame:
        """Features inside *apt_icao*'s box **and** nearer to it than to any
        other aerodrome.

        The second half is the guard. Boxes overlap where aerodromes are close,
        and ``hexagonify_airport`` stamps ``apt_icao`` on whatever it is handed,
        so without it a neighbour's stands would be published as this airport's.
        """
        feats = self._load()
        row = self._boxes[self._boxes["ident"] == apt_icao]
        if row.empty:
            return feats.iloc[0:0]
        r = row.iloc[0]
        inside = feats[
            feats["_lat"].between(r.lat_min, r.lat_max)
            & feats["_lon"].between(r.lon_min, r.lon_max)
        ]
        if inside.empty:
            return inside.drop(columns=["_lat", "_lon"], errors="ignore")

        b = self._boxes
        d_this = (inside["_lat"] - r.apt_lat) ** 2 + (
            (inside["_lon"] - r.apt_lon) * math.cos(math.radians(r.apt_lat))
        ) ** 2
        nearest_is_this = pd.Series(True, index=inside.index)
        for _, o in b[b["ident"] != apt_icao].iterrows():
            d_other = (inside["_lat"] - o.apt_lat) ** 2 + (
                (inside["_lon"] - o.apt_lon) * math.cos(math.radians(o.apt_lat))
            ) ** 2
            nearest_is_this &= d_this <= d_other
        return inside[nearest_is_this].drop(columns=["_lat", "_lon"], errors="ignore")
```

- [ ] **Step 4: Run to verify they pass**

Run: `.venv310/bin/python -m pytest tests/test_pbf_source.py -v`
Expected: PASS, six tests.

- [ ] **Step 5: Commit**

```bash
git add src/opdi/reference/pbf_source.py tests/test_pbf_source.py
git commit -m "feat(reference): assign PBF aeroway features to aerodromes by runway extent"
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
        --parts /home/jupyter/.claude/jobs/f9b282a2/tmp/osm_parts \
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

- [ ] **Step 2: Run it on four aerodromes of different sizes**

```bash
cd /home/jupyter/work/opdi-workspace/opdi
.venv310/bin/python benchmarks/compare_layout_sources.py \
  --pbf /home/jupyter/work/osm/europe-latest.osm.pbf \
  --parts /home/jupyter/.claude/jobs/f9b282a2/tmp/osm_parts \
  --airports EBBR LSZH EICK EGGD
```

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

**Gate:** all twenty study aerodromes must have stands, and the original twenty
(`EBBR EDDF EDDM EGKK EGLL EHAM EIDW EKCH ENGM EPWA ESSA LEBL LEMD LFPG LGAV
LIRF LOWW LPPT LSZH LTFM`) must all be present — that set is what the published
table held before it was damaged, and this build is also its restoration.

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

* **pyosmium's API version is unverified.** This plan is written against 4.x
  (`osmium.FileProcessor`, `osmium.filter.KeyFilter`, `.with_areas()`). If 3.x
  installs, Task 2 needs the `osmium.SimpleHandler` form instead. Task 1 Step 5
  detects which.
* **The 1.5 km box margin is a judgement, not a measurement.** It is checked
  only indirectly, by Task 5's cell comparison. An aerodrome with far-flung
  cargo stands could lose them; the symptom would be `only_op` cells clustered
  away from the runway.
* **Nearest-aerodrome assignment is O(airports) per airport** — 1,353² distance
  comparisons over a few hundred thousand features. If that is slow, restrict
  the inner loop to aerodromes whose boxes actually overlap this one.
* **§3.4's response-caching proposal is deliberately not implemented.** It
  existed to stop a re-run re-querying Overpass; with a local extract there is
  no HTTP request to cache, and the file on disk *is* the cache. The bbox
  proposal from that section survives in a different form: the box is what
  assigns a feature to an aerodrome (Task 3) rather than what limits a query.
* **The extract is a snapshot.** Cells will differ from an Overpass build made
  on another day, and nothing in the event schema records which layout vintage
  produced an event. `industrialization-plan.md` §3.5 raises this; it is not
  solved here.
* **The published table is still missing 16 of its original 20 aerodromes**
  until Task 6 completes. That is a live degradation and Task 6 is its repair,
  not merely an improvement.
