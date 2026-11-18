"""Tests for the generators in tools/: data dictionary, figures and validation summary."""
import csv
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import draw_validation_figures as figures  # noqa: E402
import gen_data_dictionary  # noqa: E402
import gen_validation_summary  # noqa: E402
from check_site import MEASURE_COLUMNS  # noqa: E402

STUB = Path(__file__).resolve().parent / "stub_opdi"


def test_dictionary_tables_from_schema(tmp_path):
    assert gen_data_dictionary.main(["--opdi", str(STUB), "--root", str(tmp_path)]) == 0
    md = (tmp_path / "content/data/_generated/flight_list_columns.md").read_text()
    assert "| Column | Type | Unit | Description |" in md
    assert "Departure airport \\| ICAO code." in md
    events = (tmp_path / "content/data/_generated/flight_events_columns.md").read_text()
    assert "`event_id`" in events and "`flight_version`" in events and "`adep`" not in events
    # Event columns that share a name with a flight list column are still
    # columns of the events file, with their own description.
    assert "`flight_id`" in events
    assert "Version of the event method." in events
    assert "Version of the flight row method." not in events
    rows = list(csv.DictReader((tmp_path / "assets/data/dictionary.csv").open()))
    assert {r["table"] for r in rows} == {"flight_list", "flight_events"}
    assert gen_data_dictionary.main(["--opdi", str(STUB), "--root", str(tmp_path), "--check"]) == 0
    (tmp_path / "assets/data/dictionary.csv").write_text("stale\n")
    assert gen_data_dictionary.main(["--opdi", str(STUB), "--root", str(tmp_path), "--check"]) == 1


def row(page, measure_id, value, headline="0"):
    base = {c: "" for c in MEASURE_COLUMNS}
    base.update(page=page, measure_id=measure_id, label=measure_id, value=value, headline=headline)
    return base


def test_upsert_replaces_rows_by_page_and_measure(tmp_path):
    path = tmp_path / "validation_measures.csv"
    figures.upsert_measures([row("holding", "f1", "0.70"), row("holding", "recall", "0.71")], path)
    figures.upsert_measures([row("holding", "f1", "0.74"), row("track-identity", "f1", "0.99")], path)
    rows = list(csv.DictReader(path.open()))
    assert [(r["page"], r["measure_id"], r["value"]) for r in rows] == [
        ("holding", "f1", "0.74"), ("holding", "recall", "0.71"), ("track-identity", "f1", "0.99")]
    assert list(rows[0].keys()) == list(MEASURE_COLUMNS)


def test_summary_lists_unvalidated_items(tmp_path):
    measures = tmp_path / "validation_measures.csv"
    r = row("holding", "f1", "0.74", headline="1")
    r.update(item="holding-start;holding-end", kind="event", unit="", n="346", reference="Hand-corrected labels")
    figures.upsert_measures([r], measures)
    out = tmp_path / "summary.md"
    assert gen_validation_summary.main(["--measures", str(measures), "--out", str(out)]) == 0
    text = out.read_text()
    assert "`holding-start`" in text and "0.74" in text
    assert "holding.qmd#validation-method" in text
    line = next(l for l in text.splitlines() if "`first_seen`" in l)
    assert "Not validated" in line


def test_every_event_type_has_a_methods_page():
    import check_site
    pages = gen_validation_summary.METHOD_PAGES["event"]
    assert [name for name in check_site.EVENT_TYPES if name not in pages] == []
    for page in set(pages.values()):
        assert (check_site.ROOT / "content" / "methods" / f"{page}.qmd").is_file(), page


def test_summary_links_unvalidated_items_to_their_page(tmp_path):
    measures = tmp_path / "validation_measures.csv"
    figures.upsert_measures([row("holding", "f1", "0.74", headline="1")], measures)
    out = tmp_path / "summary.md"
    assert gen_validation_summary.main(["--measures", str(measures), "--out", str(out)]) == 0
    line = next(l for l in out.read_text().splitlines() if "`line-up`" in l)
    assert "Not validated" in line and "runway-milestones.qmd#limitations" in line


def test_item_figure_labels_come_from_the_staged_table():
    import gen_item_figures as g
    row = {"availability_own_label": "partial", "availability_ref_label": "high", "accuracy_label": "good"}
    own = g.own("p", "m", "l", "ATOT")
    ref = g.found("p", "m", "l", "ATOT")
    acc = g.w60("p", "m", "l", "ATOT")
    med = g.median("p", "m", "l", "ATOT")
    assert [g.badge(f, row) for f in (own, ref, acc, med)] == ["partial", "high", "good", ""]


def test_item_figures_link_to_the_airport_tables():
    import gen_item_figures as g
    assert "airports.qmd#take-off" in g.airport_line("take-off-landing")
    assert "airports.qmd#all-items" in g.airport_line("tops")


def test_item_figures_are_up_to_date():
    import gen_item_figures as g
    assert g.main(["--check"]) == 0


def test_accuracy_tables_are_up_to_date():
    import gen_accuracy_tables as g
    assert g.main(["--check"]) == 0


def test_every_staged_item_has_a_name_and_a_page():
    import gen_accuracy_tables as g
    items = {r["item"] for r in g.read(g.ROOT, "item_accuracy.csv")}
    assert items <= set(g.ITEM_NAME)
    for page in set(g.ITEM_PAGE.values()):
        assert (g.ROOT / "content" / "algorithms" / f"{page}.qmd").is_file(), page


def lookup(scope):
    import json
    import gen_accuracy_tables as g
    return json.loads((g.ROOT / f"assets/pages/accuracy_lookup_{scope}.json").read_text(encoding="utf-8"))


def test_lookup_has_every_airport_and_main_item():
    import gen_accuracy_tables as g
    data = lookup("airports")
    union = ({r["airport"] for r in g.read(g.ROOT, "airport_accuracy_europe.csv")}
             | {r["airport"] for r in g.read(g.ROOT, "airport_accuracy_world.csv")})
    assert {a["icao"] for a in data["airports"]} == union
    for a in data["airports"]:
        for m in data["main"]:
            assert m["item"] in data["rows"][a["icao"]], (a["icao"], m["item"])


def test_lookup_rows_match_staged_csv():
    rows = lookup("airports")["rows"]
    assert rows["LFPG"]["ATOT"] == [95.6, "high", 2154, 0, 2046, 98.8, "high", 2022,
                                    "94.5 % within 60 s", 34, "good"]
    assert rows["LTFM"]["ATOT"][10] == "too few compared"


def test_lookup_flags_follow_the_tables():
    airports = {a["icao"]: a for a in lookup("airports")["airports"]}
    assert "ground" in airports["LTFM"]["flags"]
    assert "no_times" in airports["LGIR"]["flags"]
    assert "take-off" in airports["LMML"]["limited"]


def test_lookup_empty_cells_are_null():
    for by_item in lookup("airports")["rows"].values():
        for row in by_item.values():
            for pos in (0, 2, 4, 5, 7, 9):
                assert row[pos] is None or isinstance(row[pos], (int, float)), row


def test_lookup_overall_has_every_item():
    import gen_accuracy_tables as g
    overall = lookup("overall")["overall"]
    pooled = {r["item"] for r in g.read(g.ROOT, "item_accuracy.csv") if r["days"] == g.POOLED}
    assert set(overall) == pooled
    assert len(overall["ATOT"]) == 2 and overall["ATOT"][0][0] == 0


def test_lookup_files_stay_small():
    import gen_accuracy_tables as g
    for scope in ("overall",):
        size = (g.ROOT / f"assets/pages/accuracy_lookup_{scope}.json").stat().st_size
        assert size < 880_664, (scope, size)


def test_lookup_include_keeps_anchors():
    import gen_accuracy_tables as g
    text = g.lookup_include("airports")
    for anchor, *_ in g.MAIN:
        assert f"[]{{#{anchor}}}" in text
    assert 'src="../../assets/js/accuracy-lookup.js"' in text
    assert "[]{#take-off}" not in g.lookup_include("overall")


def test_item_group_follows_the_sidebar():
    import gen_accuracy_tables as g
    assert g.item_group("take-off-landing") == "Runway and stands"
    assert g.item_group("airports") == "Flight list"
    assert g.item_group("processing-steps") == "Other"


def test_stale_generated_tables_are_reported(tmp_path):
    import shutil
    import gen_accuracy_tables as g
    for rel in ("assets/accuracy", "assets/pages", "content/accuracy/_generated", "content/algorithms/_generated"):
        shutil.copytree(g.ROOT / rel, tmp_path / rel)
    assert g.main(["--check", "--root", str(tmp_path)]) == 0
    (tmp_path / "content/accuracy/_generated/old.md").write_text(g.HEADER + "x", encoding="utf-8")
    assert g.main(["--check", "--root", str(tmp_path)]) == 1


def test_limitation_anchor_covers_every_item_page():
    import re
    import gen_accuracy_tables as g
    assert set(g.ITEM_PAGE.values()) <= set(g.LIMITATION_ANCHOR)
    ids = set(re.findall(r"\{#(lim-[a-z-]+)", (g.ROOT / "content/limitations.qmd").read_text(encoding="utf-8")))
    assert set(g.LIMITATION_ANCHOR.values()) <= ids, set(g.LIMITATION_ANCHOR.values()) - ids


def test_airports_payload_merges_both_lists():
    import gen_accuracy_tables as g
    data = lookup("airports")
    assert data["scope"] == "airports"
    codes = [a["icao"] for a in data["airports"]]
    assert len(codes) == len(set(codes))
    union = ({r["airport"] for r in g.read(g.ROOT, "airport_accuracy_europe.csv")}
             | {r["airport"] for r in g.read(g.ROOT, "airport_accuracy_world.csv")})
    assert set(codes) == union
    airports = {a["icao"]: a for a in data["airports"]}
    assert airports["LTFM"]["ranks"] == {"europe": 1, "world": 16}
    assert airports["LTFM"]["source"] == "europe"
    assert "ground" in airports["LTFM"]["flags"]
    assert airports["KATL"]["ranks"]["europe"] is None
    assert airports["KATL"]["where"] == "outside Europe"
    assert airports["KATL"]["source"] == "world"
    assert airports["KATL"]["movements"] == {"nm": 230, "opdi": 7241}
    # LTFM's rows are the European ones (with airport records as the reference).
    assert data["rows"]["LTFM"]["ATOT"][3] == 0
    for a in data["airports"]:
        for m in data["main"]:
            assert m["item"] in data["rows"][a["icao"]], (a["icao"], m["item"])


def test_items_carry_limits():
    for scope in ("airports", "overall"):
        items = lookup(scope)["items"]
        assert items["ATOT"]["limits"] == "../limitations.qmd#lim-take-off-landing", scope
        assert all(it["limits"].startswith("../limitations.qmd#lim-") for it in items.values()), scope


AIRPORTS_JSON_BOUND = 1_491_316  # 1.2 x the 1,242,763 bytes of the first build


def test_airports_lookup_stays_small():
    import gen_accuracy_tables as g
    size = (g.ROOT / "assets/pages/accuracy_lookup_airports.json").stat().st_size
    assert size < AIRPORTS_JSON_BOUND, size


def test_airports_include_keeps_anchors():
    import gen_accuracy_tables as g
    text = g.lookup_include("airports")
    for anchor, *_ in g.MAIN:
        assert f"[]{{#{anchor}}}" in text
    assert 'src="../../assets/js/accuracy-lookup.js"' in text
    assert 'data-scope="airports"' in text
    assert "accuracy_lookup_airports.json" in text


def test_old_scopes_are_not_generated():
    import gen_accuracy_tables as g
    wanted = {p.name for p in g.build(g.ROOT)}
    assert not {n for n in wanted if n.endswith("lookup.md") or n.endswith(".json")} - {"airports_lookup.md", "overall_lookup.md", "accuracy_lookup_airports.json", "accuracy_lookup_overall.json"}
    assert "accuracy_lookup_airports.json" in wanted
